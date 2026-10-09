;;;; adapter.lisp --- the Telegram adapter: the platform, the poll lap, the door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane host — rooms, lanes, the gate, the digest, the status line, the
;;;; answer, the write-back — is the kit's (host.lisp, room.lisp). This file
;;;; is what Telegram contributes: how a message, an edit, a delete and a
;;;; typing beat are spelled (rest.lisp, bound into a PLATFORM), how a
;;;; mention is written, where the chat is, and the getUpdates long poll
;;;; that turns updates into candidates.
;;;;
;;;; Thread topology (kit rule):
;;;;   - channel-telegram-poll (supervised lap): one getUpdates long poll
;;;;     per lap, then admission through the host. This thread owns the
;;;;     offset.
;;;;   - the host's :FRAME fold and channel-telegram-deliver pool: the kit's.

(in-package #:nodecode-channel-telegram)

(nlk:access (host channel-host))

(defstruct (telegram-adapter (:copier nil)
                             (:constructor %make-telegram-adapter))
  (host nil)
  (poll-timeout 10 :type real)
  (poll-interval-ms 1000 :type integer)
  ;; The bot's own identity for the chat contract and the mention strip:
  ;; channels.telegram.bot_username, else getMe's; the id only ever comes
  ;; from getMe.
  (bot-username nil :type (or null string))
  (bot-user-id nil :type (or null string))
  ;; channels.telegram.allowed_updates: the update kinds the poll asks for.
  (allowed-updates +default-allowed-updates+ :type list)
  ;; getUpdates offset, owned by the poll thread.
  (offset nil))

(nlk:access (adapter telegram-adapter))

(defvar *telegram-adapter* nil
  "The live Telegram adapter, or NIL when the lane is stopped.")

;;; --- what Telegram contributes to the host --------------------------------------

(defun telegram-strip-mention (text username)
  "TEXT with our own @handle removed, whatever case it was typed in."
  ;; The model should read the ask, not the handle that routed it.
  (when (and (stringp username) (plusp (length username)))
    (let ((handle (if (char= #\@ (char username 0))
                      username
                      (concatenate 'string "@" username))))
      (setf text (remove-all handle text :test #'char-equal))))
  (nlk:trimmed text))

;;; Data, so an operator's layer can reword it; the ids above it are the
;;; adapter's.
(defparameter +telegram-api-primer+
  "Telegram's Bot API is open to you as the bot, from eval:
  (nct:request \"POST\" PATH &key body headers timeout) => (:status N :body VALUE)
PATH is any Bot API method, e.g. \"/setMessageReaction\"; a JSON body is a keyword plist: (:chat_id <chat> :message_id 12 :reaction #((:type \"emoji\" :emoji \"👍\"))). A reply carrying something comes back as (:ok T :result …), arrays as vectors, null as :NULL. A file upload is an alist: ((\"chat_id\" . \"<chat>\") (\"document\" . #p\"/path\")). A write that only acknowledges — Telegram’s {\"ok\":true,\"result\":true} — comes back with an empty :body, since a boolean says the request was accepted and nothing about what is there; where the object can be read it carries :state too, the object read fresh afterwards. That covers the pin and chat verbs (getChat), the member verbs (getChatMember) and the command menu (getMyCommands); :read \"/getChat?chat_id=…\" names another read and :read nil none. No read here confirms a MESSAGE: a reaction, an edit and a deletion cannot be checked from this door, so say what you sent and never that it is there. Bots cannot read chat history — the ids in the user lines are the handles you have. The adapter posts your final answer as a reply to the ask: do not send it again yourself."
  "What the lane contract says about reaching Telegram itself.")

(defun telegram-where-text (candidate bot-username bot-user-id)
  "Where the chat is and who the bot is, for the lane contract: the chat,
the topic, the kind, the bot's handle and id — whichever are known; an
unknown identity is left out, not invented."
  (let ((target (channel-target candidate)))
    (format nil "Telegram chat ~a~@[, topic ~a~]~@[ (~a)~].~
~:[~@[ The bot's user id is ~a.~]~;~:* The bot is @~a~@[, user id ~a~].~]"
            (or (getf target :channel-id) "unknown")
            (getf target :thread-id)
            (source-field candidate "chat_kind")
            bot-username
            bot-user-id)))

(defun telegram-platform (&key bot-username bot-user-id)
  "The Telegram PLATFORM: Bot API plans, rich messages in 16384-character
chunks, the @handle mention, per-chat message ids (so an address carries
the chat), a plain-text footer, a typing beat that lives ~5 s, reactions
set in one call, and the command menu as setMyCommands."
  ;; Command answers reply in the chat: Telegram has no return path of its own
  ;; for a message.
  (make-platform
   :id "telegram"
   :name "Telegram"
   :noun "chat"
   :owner-label "Telegram user id"
   :session-prefix "telegram"
   :contract-section "telegram-chat"
   :text-limit +telegram-message-text-limit+
   :typing-refresh-ms 4000
   :plan-message #'send-message-plan
   :plan-edit #'edit-message-plan
   :plan-delete #'delete-message-plan
   :plan-typing #'typing-plan
   :plan-reaction #'reaction-plans
   :plan-commands (lambda (entries &key timeout-seconds)
                    (list (set-my-commands-plan
                           entries :timeout-seconds timeout-seconds)))
   :message-id-of #'telegram-message-id
   :address-of #'telegram-address
   :strip-mention (lambda (text) (telegram-strip-mention text bot-username))
   :where-text (lambda (candidate)
                 (telegram-where-text candidate bot-username bot-user-id))
   :api-primer +telegram-api-primer+
   :seams-primer "  nct:telegram-handle-update (adapter update) — every raw getUpdates entry within channels.telegram.allowed_updates (default: messages only; widen it for edited_message, message_reaction, callback_query, my_chat_member, ...), on the poll thread: return fast. update is the hash table Telegram sent."
   ;; A control press is answered with answerCallbackQuery.
   :plan-control-ack #'answer-callback-plans
   :plan-file (lambda (target pathname &key content reply-to voice timeout-seconds)
                "A photo renders inline and anything else is a document, and the file's
own bytes say which; a minute is the floor, a file being heavier than a message."
                (telegram-file-plan target pathname :content content :reply-to reply-to
                                    :voice voice
                                    :timeout-seconds (max 60 (or timeout-seconds 0))))))

;;; --- the door ----------------------------------------------------------------------------

(defun request (method path &rest arguments &key body headers timeout retry
                (read (read-back-path path body))
                &aux (host (and *telegram-adapter* (telegram-adapter-host *telegram-adapter*))))
  "One Telegram Bot API call as the bot: METHOD PATH, answered as
(:status N :body VALUE [:error TEXT]) — the shape the eval snippet prints
legibly, see NCK:CALL."
  ;; PATH is the method, "/sendMessage"; it joins
  ;; onto the token-bearing base URL. BODY is a keyword plist (a JSON object),
  ;; an NLK:JSON-OBJECT, or a multipart alist (("chat_id" . "1")
  ;; ("document" . pathname)). Any method the token allows; nothing here
  ;; narrows it. The token stays on the executor and never appears in the
  ;; answer. Blocks on the caller's thread.
  ;;
  ;; A Bot API write that only acknowledges — Telegram answers it {"ok":true,
  ;; "result":true} — comes back with an empty :BODY, because a boolean says
  ;; the request was accepted and nothing about what is there. Where the
  ;; object can be read it comes back carrying :STATE as well, the object read
  ;; fresh afterwards. READ picks which object: it defaults to
  ;; READ-BACK-PATH's answer for this method and body, a string names another
  ;; read, and NIL asks for none.
  ;;
  ;; A message is the case that has no read. A reaction, an edit and a
  ;; deletion answer with that acknowledgement and nothing here confirms them,
  ;; so they carry no :STATE and what is said of them is what was sent, never
  ;; that it is there. TIMEOUT and RETRY default as NCK:CALL's do.
  (declare (ignore headers timeout retry))
  (unless (and host host.executor)
    (error "the telegram channel is not running"))
  (apply #'call (wrap-telegram-executor host.executor) method path :read read arguments))

;;; --- ingress (poll thread) --------------------------------------------------------------

(defun telegram-file-fetcher (adapter file-id)
  "A thunk that returns the bytes behind FILE-ID, or signals with the
whole truth in its message — never the token, never the URL."
  ;; Built when the update is admitted, on the poll thread; called when the
  ;; ask's turn is prepared, on the kit's delivery pool, so no download ever
  ;; parks the poll. The ceiling a file may take is the kit's, read off the
  ;; size the entry declares before this runs.
  (lambda (&aux (result (execute-plan (host-executor adapter.host) (get-file-plan file-id)))
                (path (and result.ok-p (nlk:json-value result.body :string "result" "file_path"))))
    ;; One getFile call names the download path.
    (unless path
      (error "Telegram did not return a download path for this file"))
    (nlk:with-handlers ((error ()
                          (error "the download from Telegram failed")))
      (dex:get (telegram-file-url path)
               :force-binary t :connect-timeout 10 :read-timeout 30))))

(defun attach-telegram-file-fetchers (adapter candidate)
  "CANDIDATE's attachment entries — the ones the adapter put there with a
file id, on the message and on the message it replies to — each gain the
thunk that fetches the file, bound to ADAPTER."
  ;; Called once, as the update is admitted; nothing here touches the network.
  (dolist (holder (list candidate (nlk:json-value candidate :object "reply")))
    (let ((attachments (nlk:json-value holder :any "attachments")))
      (when (vectorp attachments)
        (loop for attachment across attachments
              when (and (nlk:json-value attachment :string "file_id")
                        (null (gethash "fetch" attachment)))
                do (setf (gethash "fetch" attachment)
                         (telegram-file-fetcher
                          adapter
                          (nlk:json-value attachment :string "file_id")))))))
  candidate)

(defun telegram-handle-update (adapter update)
  "One raw getUpdates entry UPDATE, as Telegram sent it, on the poll
thread: a message update maps to the shared candidate and goes through the
host; a callback-query update — a press on a control a lane posted — is
relayed to the kit, which answers it and acts; every other kind falls
through."
  ;; An attachment gains its fetcher
  ;; here — the file id now, the bytes when the ask's turn is prepared — so the
  ;; thread never waits on a download. Exported as the seam a layer advises —
  ;; (hook 'nct:telegram-handle-update key fn) sees edits, reactions, callback
  ;; queries, membership changes, whatever channels.telegram.allowed_updates
  ;; asks for — with the poll's own rule: return fast, this thread is the one
  ;; that polls. Returns the candidate admitted, or NIL.
  (let ((candidate (map-telegram-message
                    (gethash "message" update)
                    :bot-user-id adapter.bot-user-id
                    :bot-username adapter.bot-username))
        (callback (gethash "callback_query" update)))
    (cond
      (candidate
       (handle-candidate adapter.host (attach-telegram-file-fetchers adapter candidate))
       candidate)
      ((hash-table-p callback)
       (telegram-handle-callback adapter callback)))))

(defun telegram-handle-callback (adapter callback)
  "One callback_query UPDATE: a press on an inline control a lane posted."
  ;; Parses the press into the kit's normal payload and relays it — the kit
  ;; answers the query and acts; this thread only reads the hash table.
  ;; Returns the payload, or NIL for a press missing its sender, message or
  ;; data.
  (let ((data (gethash "data" callback)))
    (when (and (nlk:json-value callback :object "from")
               (nlk:json-value callback :object "message" "chat") (stringp data))
      (let ((payload (list :id (number-string (gethash "id" callback))
                           :data data
                           :user-id (number-string (nlk:json-value callback :any "from" "id"))
                           :user-name (or (nlk:json-value callback :string "from" "username")
                                          (nlk:json-value callback :string "from" "first_name"))
                           :message-id (number-string
                                        (nlk:json-value callback :any "message" "message_id"))
                           :channel-id (number-string
                                        (nlk:json-value callback :any "message" "chat" "id")))))
        (control-pressed adapter.host payload)
        payload))))

(defun telegram-poll-lap (adapter stop-p)
  "One getUpdates long poll."
  ;; Returns the supervisor verdict: seconds to wait before the next lap.
  (when (funcall stop-p) (return-from telegram-poll-lap :stop))
  (let ((result (execute-plan
                 (host-executor adapter.host)
                 (get-updates-plan adapter.offset
                                   :poll-timeout-seconds
                                   adapter.poll-timeout
                                   :allowed-updates
                                   adapter.allowed-updates)))
        (interval (max 0.025
                       (min 30 (/ adapter.poll-interval-ms
                                  1000.0)))))
    (when (funcall stop-p) (return-from telegram-poll-lap :stop))
    (cond
      ((not (execution-ok-p result))
       (set-channel-status "telegram" :connected nil
                           :detail result.error)
       ;; A 409 is a duplicate poller holding this token. Loud, persistent,
       ;; and slow — never a hot conflict loop.
       (let ((conflict-p (= 409 result.status)))
         (warn (if conflict-p "~a" "telegram getUpdates failed: ~a") result.error)
         (if conflict-p 5.0 (max interval 1.0))))
      (t
       (set-channel-status "telegram" :state :running :connected t
                           :detail nil)
       (let ((updates (nlk:json-value result.body :any "result")))
         (when (vectorp updates)
           ;; The offset moves first, over every update: one that wedges
           ;; its handler is not polled again.
           (setf adapter.offset (next-update-offset updates adapter.offset))
           (loop for update across updates
                 when (hash-table-p update)
                   ;; One boundary per update, outside the seam, so advice
                   ;; that signals costs that update and not the poll.
                   do (handler-case (telegram-handle-update adapter update)
                        (error (condition)
                          (warn "telegram update handling signalled: ~a"
                                condition))))))
       interval))))

;;; --- the channel entry ---------------------------------------------------------------------

(defun start-channel (section &key executor)
  "Start the Telegram lane from its channels.telegram config SECTION."
  ;; Returns a stop thunk. EXECUTOR overrides the live Bot API executor — the
  ;; scripted-executor test seam.
  ;;
  ;; Fail-closed: refuses unless at least one of allowed_chats / allowed_users
  ;; is populated. allowed_users says who may talk; owner says whose
  ;; instructions are standing policy (the kit's room, AUTHORITY) and is
  ;; warned about, not required, when a multi-person chat leaves it unset. An
  ;; owner may always talk: the ids join allowed_users when that list is the
  ;; gate.
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "telegram")) section))
         (token (resolve-channel-secret section "token"))
         (configured-users (getf settings :allowed-users))
         (owners (resolve-owners "telegram" (getf settings :owner)
                                 configured-users "allowed_chats"))
         (allowed-users (and configured-users (union configured-users owners :test #'string=)))
         (bot-username (getf settings :bot-username))
         (bot-user-id nil)
         (api-base (config-string section "api_base" +telegram-api-base+))
         (request-timeout (config-integer section "request_timeout_seconds"
                                          60 :min 1)))
    (require-non-empty-allowlist "telegram"
                                 "allowed_chats" (getf settings :allowed-chats)
                                 "allowed_users" allowed-users)
    (let ((executor (or executor (make-telegram-executor
                                  :api-base api-base :token token))))
      ;; Mention policy needs the bot's @username; hydrate via getMe when
      ;; the config does not pin one. Loud on failure: with require_mention
      ;; every group message would be rejected until it is fixed.
      (when (and (getf settings :require-mention) (null bot-username))
        (let ((result (execute-plan executor (bot-plan "/getMe" (nlk:json-object)
                                                       request-timeout t "get_me"))))
          (if result.ok-p
              (setf bot-username (nlk:json-value result.body :string "result" "username")
                    bot-user-id (number-string
                                 (nlk:json-value result.body :any "result" "id")))
              (warn "telegram: could not hydrate bot username (~a); with ~
                     require_mention every group message will be rejected ~
                     until channels.telegram.bot_username is set"
                    result.error))))
      (let* ((host (make-host-from-section
                    section (telegram-platform :bot-username bot-username
                                               :bot-user-id bot-user-id)
                    :executor executor
                    :owners owners
                    :soul-path (soul-path section)
                    :request-timeout 60
                    ;; The declared members name the policy's own keys; the
                    ;; rest are read here.
                    :policy (apply #'telegram-inbound-policy
                                   :allowed-users allowed-users
                                   :bot-username bot-username
                                   :allow-private-chats (config-boolean
                                                         section "allow_private_chats" t)
                                   :allow-group-chats (config-boolean section "allow_group_chats" t)
                                   :allow-forum-topics (config-boolean
                                                        section "allow_forum_topics" t)
                                   settings)))
             (adapter
               (%make-telegram-adapter
                :host host
                ;; Short window by design: the parked getUpdates read is
                ;; also the lane's stop latency (see GET-UPDATES-PLAN).
                :poll-timeout (config-integer
                               section "get_updates_timeout_seconds" 2
                               :min 0)
                :poll-interval-ms (config-integer
                                   section "poll_interval_ms" 1000 :min 25)
                :allowed-updates (or (config-string-list section
                                                         "allowed_updates")
                                     +default-allowed-updates+)
                :bot-username bot-username
                :bot-user-id bot-user-id)))
        (run-host host "poll" (lambda (stop-p) (telegram-poll-lap adapter stop-p))
                  :on-start (lambda ()
                              (setf *telegram-adapter* adapter
                                    *telegram-file-root*
                                    (format nil "~a/file/bot~a"
                                            (string-right-trim "/" api-base) token)))
                  :on-stop (lambda ()
                             ;; A stale stop thunk racing a respawned lane must
                             ;; not clear the fresh adapter's binding.
                             (when (eq *telegram-adapter* adapter)
                               (setf *telegram-adapter* nil
                                     *telegram-file-root* nil))))))))
