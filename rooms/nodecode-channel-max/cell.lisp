;;;; cell.lisp --- the MAX adapter: the platform, the plans, the poll lap, the door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane host -- rooms, lanes, the gate, the digest, the answer, the
;;;; write-back -- is the kit's (host.lisp, room.lisp). This file is what MAX
;;;; contributes: how a message is spelled (the request plans, below), where a
;;;; conversation sits, and the long-poll lap that turns new updates into
;;;; candidates. MAX's bot API takes the token as the access_token query
;;;; parameter, so every plan path is built here, with the token in it; the
;;;; dexador executor sends what the plan says.
;;;;
;;;; Thread topology (kit rule: no network I/O on a thread that is not ours):
;;;;   - channel-max-poll (supervised lap): GET /updates with the marker the
;;;;     last read ended on, every update through MAX-CANDIDATE, on the lap
;;;;     thread: return fast.
;;;;   - the kit's :FRAME fold and delivery pool: the kit's.

(in-package #:nodecode-channel-max)

(nlk:access (chunk text-chunk) (plan request-plan) (result execution)
            (host channel-host))

(defparameter +default-base+ "https://platform-api2.max.ru"
  "MAX's bot API. channels.max.api_base names another for a test or a mirror.")

(defparameter +max-text-limit+ 3900
  "Characters one MAX message carries: MAX allows 4000, less the mark that
says an answer goes on.")

(defvar *token* nil
  "The bot token every plan path carries. Set at start and at probe; the
delivery threads read it, so it is a global rather than a binding.")

(defvar *base* +default-base+
  "The API base the plan paths hang off, beside *TOKEN*.")



(defvar *max-adapter* nil
  "The live MAX adapter, or NIL when the lane is stopped.")

(defparameter +max-api-primer+
  "MAX is open to you as the bot, over its own API, from eval: (channel-max:request \"me\") reads the bot; (channel-max:request \"send\" :chat_id \"<id>\" :text \"...\") posts a message; (channel-max:request \"chats\") lists the dialogs. A message is named by its mid; a chat by its chat_id. MAX draws Markdown and allows 4000 characters. The adapter posts your final answer: do not post it again yourself."
  "What the lane contract says about reaching MAX itself.")

(defparameter +seams-primer+
  "  channel-max:max-candidate (update) -- every update the lap reads, on the lap
thread: return fast."
  "The adapter's own advisable seam.")

;;; --- reading JSON the lenient way -------------------------------------------------

(defun max-field (object key)
  "KEY's value in OBJECT as a string, whatever JSON type it arrived as, or
NIL. Chat and user ids arrive as numbers in MAX's answers."
  (let ((value (and (hash-table-p object) (gethash key object))))
    (typecase value
      (string value)
      (integer (princ-to-string value))
      (null nil)
      (t (princ-to-string value)))))

(defun max-object (object key)
  (let ((value (and (hash-table-p object) (gethash key object))))
    (and (hash-table-p value) value)))

(defun max-true-p (object key)
  (and (hash-table-p object) (eq t (gethash key object))))

(defun max-update-type (update)
  (max-field update "update_type"))

;;; --- paths and plans ---------------------------------------------------------------

(defun max-path (path &rest pairs)
  "PATH under the API base with the token and PAIRS as its query, escaped.
A pair with a NIL value is left out."
  (with-output-to-string (out)
    (format out "~a?access_token=~a" path (quri:url-encode (or *token* "")))
    (loop for (key value) on pairs by #'cddr
          when value do (format out "&~a=~a" key (quri:url-encode (princ-to-string value))))))

(defun max-chat (target)
  "The chat a TARGET plist names."
  (getf target :channel-id))

(defun text-of (chunk)
  "The words CHUNK carries, whichever of the kit's shapes it is."
  (if (stringp chunk) chunk (or (ignore-errors (text-chunk-text chunk)) "")))

(defun message-plan (target chunk &key reply-to card files (timeout-seconds 60)
                     &allow-other-keys)
  "One message of CHUNK's words in TARGET's chat, replying under REPLY-TO.
CARD and FILES are drawn nowhere: MAX here carries words alone."
  (declare (ignore card files))
  (let ((body (nlk:json-object "text" (text-of chunk) "format" "markdown")))
    (when reply-to
      (setf (gethash "link" body)
            (nlk:json-object "type" "reply" "mid" reply-to)))
    (rest-plan "POST" (max-path "/messages" "chat_id" (max-chat target))
               "max_send" nil (or timeout-seconds 60) body)))

(defun edit-plan (target message-id text &key (timeout-seconds 60) &allow-other-keys)
  "An answer's card, edited in place: MAX edits a message it sent."
  (declare (ignore target))
  (rest-plan "PUT" (max-path "/messages" "message_id" message-id)
             "max_edit" nil (or timeout-seconds 60)
             (nlk:json-object "text" (or text "") "format" "markdown")))

(defun delete-plan (target message-id &key (timeout-seconds 60) &allow-other-keys)
  (declare (ignore target))
  (rest-plan "DELETE" (max-path "/messages" "message_id" message-id)
             "max_delete" nil (or timeout-seconds 60)))

(defun typing-plan (target &key (timeout-seconds 30) &allow-other-keys)
  "MAX's typing indicator in TARGET's chat; it clears itself after a few
seconds, and a sent message clears it."
  (rest-plan "POST" (max-path (format nil "/chats/~a/actions" (max-chat target)))
             "max_typing" nil (or timeout-seconds 30)
             (nlk:json-object "action" "typing_on")))

(defun message-id-of (body)
  "The mid a send answered with, so the lane can edit what it posted."
  (or (max-field (max-object (max-object body "message") "body") "mid")
      (max-field (max-object body "message") "mid")
      (max-field (max-object body "message") "message_id")
      (max-field body "message_id")))

(defstruct (max-adapter (:copier nil))
  "One MAX lane's live facts: the host it speaks through, the bot it is, and the marker its next read starts on."
  (host nil)
  (bot-id "" :type string)
  (username "" :type string)
  (marker nil)
  (marker-path nil)
  (poll-seconds 3 :type real))

(defun updates-plan (marker &key (timeout 30))
  "GET /updates from MARKER: MAX holds the answer for TIMEOUT seconds while
the chat is quiet, so the lap costs one request per window, not one per
second."
  (rest-plan "GET" (max-path "/updates" "timeout" timeout "limit" 100 "marker" marker)
             "max_updates" nil (+ timeout 20)))

;;; --- what a person said -------------------------------------------------------------

(defun max-mention-p (text username)
  "Whether TEXT names @USERNAME, the bot's own MAX name."
  (let ((name (string-left-trim "@" (or username ""))))
    (and (stringp text) (plusp (length name))
         (ppcre:scan (format nil "(?i)(^|[^\\w@])@~a(?!\\w)"
                              (ppcre:quote-meta-chars name))
                      text)
         t)))

(defun max-strip-mention (text username)
  "TEXT without @USERNAME: the model reads the words; the name says where they went."
  (let ((name (string-left-trim "@" (or username ""))))
    (if (plusp (length name))
        (nlk:trimmed (ppcre:regex-replace
                      (format nil "(?i)@~a" (ppcre:quote-meta-chars name))
                      (or text "") ""))
        (or text ""))))

(defun max-candidate (update &key username)
  "One message update as the kit's candidate, or NIL for anything else: a
bot's own message, an edit, an update with no words. A dialog is addressed
to the bot by being one; a group's message by naming it."
  (let* ((message (max-object update "message"))
         (body (max-object message "body"))
         (sender (max-object message "sender"))
         (recipient (max-object message "recipient"))
         (raw (nlk:trimmed (or (max-field body "text") (max-field message "text") "")))
         (text (max-strip-mention raw username))
         (user-id (or (max-field sender "user_id") (max-field update "user_id")))
         (chat-id (max-field recipient "chat_id"))
         (dialog-p (equal "dialog" (string-downcase (or (max-field recipient "chat_type") "")))))
    (when (and (member (max-update-type update) '("message_created" "bot_started")
                       :test #'string=)
               user-id chat-id (not (max-true-p sender "is_bot"))
               (plusp (length text)))
      (nlk:json-object
       "text" text
       "source"
       (nlk:json-object
        "platform" "max"
        "chat_kind" (if dialog-p "dm" "group")
        "channel_id" chat-id
        "user_id" user-id
        "user_name" (or (max-field sender "name") user-id)
        "message_id" (or (max-field body "mid") (max-field message "message_id") "")
        "reply_to_message_id" (max-field (max-object message "link") "mid")
        "addressed" (or dialog-p (max-mention-p raw username))
        "is_bot" nil)))))

(defun max-inbound-policy (&key allowed-users username)
  "The kit policy over MAX: people by user id, dialogs and groups both, a
group's message only when it names the bot."
  (make-inbound-policy
   :allowed-users allowed-users
   :dm-policy :enabled
   :group-policy :enabled
   :require-mention t
   :mention-target username
   :mention-test (lambda (text target)
                   (declare (ignore target))
                   (max-mention-p text username))
   :allow-bots :none
   :channel-match :exact))

;;; --- the platform ------------------------------------------------------------------

(defun max-platform (&key book bot-name)
  "The MAX PLATFORM: markdown, 3900 characters to a message, a card edited
where it was posted, a typing indicator, no reactions and no buttons."
  (declare (ignore book bot-name))
  (make-platform
   :id "max"
   :name "MAX"
   :noun "chat"
   :owner-label "MAX user id"
   :session-prefix "max"
   :contract-section "max-channel"
   :text-limit +max-text-limit+
   :typing-refresh-ms 60000
   :plan-message (lambda (target chunk &rest keys) (apply #'message-plan target chunk keys))
   :plan-edit (lambda (target message-id text &rest keys)
                (apply #'edit-plan target message-id text keys))
   :plan-delete (lambda (target message-id &rest keys)
                  (apply #'delete-plan target message-id keys))
   :plan-typing (lambda (target &rest keys) (apply #'typing-plan target keys))
   :message-id-of #'message-id-of
   :answer-body (lambda (text footer) (format nil "~a~%~%~a" text footer))
   :where-text (lambda (candidate)
                 (format nil "a MAX ~a. The bot answers as ~a, in the chat the ask came from. MAX draws Markdown and allows 4000 characters."
                         (if (equal "dm" (max-field candidate "chat_kind")) "dialog" "group chat")
                         (or (and *max-adapter* (max-adapter-username *max-adapter*)) "the bot")))
   :choices nil
   :api-primer +max-api-primer+
   :seams-primer +seams-primer+))

;;; --- the lap -----------------------------------------------------------------------

(defun execute (adapter plan)
  (execute-plan (host-executor (max-adapter-host adapter)) plan))

(defun marker-path (&optional (bot-id ""))
  (nlk:home (format nil "channels/max/~a.marker" (if (plusp (length bot-id)) bot-id "bot"))))

(defun kept-marker (path)
  (let ((kept (and (probe-file path) (nlk:trimmed (uiop:read-file-string path)))))
    (and (plusp (length kept)) (every #'digit-char-p kept) (parse-integer kept))))

(defun keep-marker (adapter)
  (when (and (max-adapter-marker-path adapter) (max-adapter-marker adapter))
    (handler-case
        (nlk:write-file-atomically (max-adapter-marker-path adapter)
                                   (princ-to-string (max-adapter-marker adapter)))
      (error (condition) (warn "max: the marker was not kept: ~a" condition)))))

(defun max-poll-lap (adapter stop-p)
  "One read of MAX's updates from the kept marker. => seconds to wait before
the next lap, or :FATAL for a token MAX refuses."
  (when (funcall stop-p) (return-from max-poll-lap :stop))
  (let ((result (execute adapter (updates-plan (max-adapter-marker adapter)))))
    (when (funcall stop-p) (return-from max-poll-lap :stop))
    (cond
      ((member (execution-status result) '(401 403))
       (set-channel-status "max" :state :degraded :connected nil
                                 :detail "MAX refused the bot token: give the bot a live token and restart")
       :fatal)
      ((not (execution-ok-p result))
       (set-channel-status "max" :connected nil :detail (execution-error result))
       (warn "max read failed: ~a" (execution-error result))
       10.0)
      (t
       (set-channel-status "max" :state :running :connected t)
       (let ((updates (handler-case (nlk:json-array (execution-body result) "updates")
                        (error () #()))))
         (loop for update across updates
               do (handler-case
                      (let ((candidate (max-candidate update :username (max-adapter-username adapter))))
                        (when candidate
                          (handle-candidate (max-adapter-host adapter) candidate)))
                    (error (condition) (warn "max update signalled: ~a" condition))))
         (let ((marker (max-field (execution-body result) "marker")))
           (when marker (setf (max-adapter-marker adapter) (parse-integer marker))))
         (keep-marker adapter)
         (max-adapter-poll-seconds adapter))))))

;;; --- the door ----------------------------------------------------------------------

(defun request (op &rest arguments &key &allow-other-keys)
  "One MAX call from eval: OP names it (\"me\", \"send\", \"chats\"), the rest
are its query arguments. => (:status N :body VALUE [:error TEXT])."
  (unless (and *max-adapter* (max-adapter-host *max-adapter*))
    (error "the max channel is not running"))
  (let* ((host (max-adapter-host *max-adapter*))
         (path (cond ((string= op "me") "/me")
                     ((string= op "send") "/messages")
                     ((string= op "chats") "/chats")
                     (t (format nil "/~a" op))))
         (pairs (loop for (key value) on arguments by #'cddr
                      append (list (string-downcase (symbol-name key)) value)))
         (body (when (string= op "send") (nlk:json-object "text" (getf arguments :text))))
         (plans (if (string= op "send")
                    (list (rest-plan "POST" (apply #'max-path path (remove :text pairs :key #'car))
                                     "max_send" nil 60 body))
                    (list (rest-plan "GET" (apply #'max-path path pairs) op nil 60))))
         (result (execute-plan (host-executor host) (first plans))))
    (list* :status (execution-status result) :body (execution-body result)
           (and (execution-error result) (list :error (execution-error result))))))

;;; --- the door the kit opens ---------------------------------------------------------

(defun start-channel (section &key executor)
  "Start the MAX lane from its channels.max config SECTION. => a stop thunk."
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "max")) section))
         (token (resolve-channel-secret section "bot_token"))
         (base (let ((named (config-string section "api_base")))
                 (if (plusp (length named)) named +default-base+))))
    (setf *token* token *base* base)
    (let* ((raw (or executor (make-dexador-executor :base-url base
                                                     :redact-prefixes (list token))))
           (me (execute-plan raw (rest-plan "GET" (max-path "/me") "max_me" nil 30)))
           (bot-id (and (execution-ok-p me) (max-field (execution-body me) "user_id"))))
      (unless bot-id
        (config-error "channels.max: MAX did not take the bot token (~a)"
                      (or (execution-error me) "no user_id in /me")))
      (let* ((username (or (max-field (execution-body me) "username")
                           (config-string section "username")))
             (named (getf settings :allowed-users))
             (owners (resolve-owners "max" (getf settings :owner) named "allowed_users"))
             (allowed (and named (union named owners :test #'string=)))
             (host (progn
                     (require-non-empty-allowlist "max" "allowed_users" allowed)
                     (make-host-from-section
                      section (max-platform :bot-name username)
                      :executor raw
                      :owners owners
                      :policy (max-inbound-policy :allowed-users allowed :username username)
                      :soul-path (soul-path section)
                      :request-timeout 60)))
             (path (marker-path bot-id))
             (adapter (make-max-adapter :host host :bot-id bot-id
                                         :username (or username "")
                                         :marker (ignore-errors (kept-marker path))
                                         :marker-path path
                                         :poll-seconds (max 1 (or (getf settings :poll-seconds) 3)))))
        (run-host host "poll" (lambda (stop-p) (max-poll-lap adapter stop-p))
                  :on-start (lambda () (setf *max-adapter* adapter))
                  :on-stop (lambda () (when (eq *max-adapter* adapter)
                                        (setf *max-adapter* nil))))))))

;;; --- what the token can see ---------------------------------------------------------

(defun probe-channel (section &key executor (timeout 60))
  "What the token in SECTION can see, as lines: the bot it is, and whether
its updates are readable. Never carries the token."
  (declare (ignore timeout))
  (let* ((lines '())
         (token (resolve-channel-secret section "bot_token"))
         (base (let ((named (config-string section "api_base")))
                 (if (plusp (length named)) named +default-base+))))
    (flet ((say (control &rest args) (push (apply #'format nil control args) lines)))
      (setf *token* token *base* base)
      (let* ((exec (or executor (make-dexador-executor :base-url base
                                                        :redact-prefixes (list token))))
             (me (execute-plan exec (rest-plan "GET" (max-path "/me") "max_me" nil 30))))
        (if (execution-ok-p me)
            (let ((body (execution-body me)))
              (say "bot: ~a (user ~a)~@[, @~a~]" (or (max-field body "name") "unnamed")
                   (or (max-field body "user_id") "unknown")
                   (max-field body "username"))
              (let ((updates (execute-plan exec (updates-plan nil :timeout 0))))
                (if (execution-ok-p updates)
                    (say "updates: ~d readable"
                         (length (handler-case (nlk:json-array (execution-body updates) "updates")
                                   (error () #()))))
                    (say "updates: ~a" (execution-error updates)))))
            (say "token: MAX refused it (~a)" (or (execution-error me) "no answer")))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(nlk:define-section ("channels" "max")
  (:guide "make a bot in MAX (masterbot) and copy its token into an environment variable or a file of mode 600, named here; list the MAX user ids the bot may answer; the bot reads updates by long polling, so nothing needs to be exposed. MAX is a Russian messenger, so give the bot an account of its own.")
  (:check #'probe-channel)
  (:one-of "bot_token_env" "bot_token_file")
  ("bot_token_env" :env :doc "environment variable holding the bot token")
  ("bot_token_file" :path :doc "file holding the bot token")
  ("allowed_users" :list :required t :doc "MAX user ids whose messages the bot answers")
  ("owner" :list :doc "the operator's MAX user id, whose word is standing policy")
  ("username" :string :doc "the bot's @name in MAX, when its token's /me carries none")
  ("api_base" :string :doc "the API base, https://platform-api2.max.ru unless named")
  ("poll_seconds" :integer :default 3 :min 1 :doc "seconds between two reads when a read came back empty")
  ("soul_file" :path :doc "a SOUL.md whose text is the channel's standing persona")
  ("turn_budget_minutes" :integer :default 0 :doc "minutes an ask's turn may run; 0 caps nothing"))
