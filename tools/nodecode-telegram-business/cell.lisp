;;;; cell.lisp --- the manual, the three tools, /biz, and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :telegram-business) answers the primer below, and every request's
;;;; help section carries one line naming it. All Telegram I/O is the Bot
;;;; API over NLK:HTTP, one seam; the state is one private JSON file.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "telegram-business": {"bot_token_env": "TELEGRAM_BUSINESS_BOT_TOKEN",
;;;;                          "persona": "You are drafting ...",
;;;;                          "draft_ttl_hours": 24}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-telegram-business)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Telegram Business mode is available through the nodecode-telegram-business cell: three tools,
which a person can also call from eval in the telegram-business package.
  tg_business_poll  {timeout, limit}
      What arrived on the business connection: new customer messages, connection changes, and
      the owner's button presses. Call it with no arguments for what is waiting; timeout holds
      the call open (long polling). Its answer carries the persona to draft in. The owner's
      Send, Edit and Discard presses are applied here: a Send delivers the draft, an Edit makes
      the owner's next DM the outgoing text. So poll after every draft to learn what they
      chose. A customer event names the connection_id, chat_id and message_id a draft needs,
      and says draft=false when the owner paused drafting for that connection.
  tg_business_draft  {connection_id, chat_id, customer_message_id, customer_text, text}
      Draft the reply yourself, in the owner's voice and the customer's language, then hand it
      here: it DMs the owner with Send, Edit and Discard buttons and records the draft. Nothing
      reaches the customer until the owner acts. One pending draft per customer chat: a newer
      draft supersedes the older one.
  tg_business_send  {draft_id, text}
      Deliver a recorded draft now, for an owner who approved it in conversation rather than by
      button, or with their own text in text. It refuses an expired, discarded, superseded or
      already-sent draft, and refuses when Telegram does not grant the connection the right to
      reply for the owner.
The owner's /biz command shows the connections, the drafts waiting and the muted chats. Never
send a reply the owner did not approve: the whole point is that they see it first. ERROR:
TELEGRAM-BUSINESS-ERROR names the problem; a refused send says why in its text."
  "What (help :telegram-business) answers while the cell runs.")

;;; --- reading the section -------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the bot token the section names and the persona
that has a default: read once, here."
  (declare (ignore table))
  (let* ((env (nonblank (getf values :bot-token-env)))
         (plist (copy-list values)))
    (unless env
      (nlk:config-error "bot_token_env names no environment variable: set it to the variable holding the BotFather bot token"))
    (let ((token (nonblank (nle::credential-env env))))
      (unless token
        (nlk:config-error "bot_token_env names ~a, which is unset or empty" env))
      (setf (getf plist :token) token))
    (setf (getf plist :persona)
          (or (nonblank (getf values :persona)) +default-persona+))
    plist))

;;; --- tg_business_poll ----------------------------------------------------------

(defun poll-updates (call)
  "The tg_business_poll tool: read the connection, apply the owner's presses,
and answer the events worth the model's look."
  (running-settings)
  (with-telegram-business-lock
    (answering
      (let* ((timeout (max 0 (min 50 (or (business-poll-timeout call) 0))))
             (limit (max 1 (min 100 (or (business-poll-limit call) 50))))
             (result (api-call "getUpdates"
                               :payload (nlk:json-object
                                         "offset" (or (part "offset") 0)
                                         "timeout" timeout
                                         "limit" limit
                                         "allowed_updates" (allowed-updates))
                               :extra-timeout timeout))
             (updates (if (vectorp result) result (vector)))
             (events '())
             (last -1))
        (loop for update across updates
              do (let ((id (nlk:json-value update :integer "update_id")))
                   (when (and (integerp id) (> id last)) (setf last id)))
                 (setf events (nconc events (handle-update update))))
        (when (>= last 0)
          (setf (part "offset") (1+ last)))
        (save-state)
        (nlk:encode-json-object
         (nlk:json-object
          "events" (coerce events 'vector)
          "persona" (setting :persona)
          "now" (nlk:unix-now)
          "pending_drafts" (length (pending-drafts))
          "paused_chats" (coerce (paused-list) 'vector)
          "connections"
          (coerce (loop for record in (connections-list)
                        collect (nlk:json-object
                                 "connection_id" (or (field record "connection_id") "")
                                 "user_name" (or (field record "user_name") "")
                                 "owner_chat_id" (or (field record "owner_chat_id") 0)
                                 "enabled" (enabled-p record)
                                 "can_reply" (can-reply-p record)
                                 "drafting" (auto-draft-p record)))
                  'vector)))))))

;;; --- tg_business_draft ---------------------------------------------------------

(defun customer-name (chat-id)
  "The name the events carried for CHAT-ID, else the chat id."
  (or (gethash chat-id (names)) chat-id))

(defun draft-reply (call)
  "The tg_business_draft tool: record the drafted reply and DM it to the owner
with Send, Edit and Discard."
  (running-settings)
  (with-telegram-business-lock
    (answering
      (let* ((connection-id (nlk:trimmed (or (business-draft-connection-id call) "")))
             (chat-id (chat-key (business-draft-chat-id call)))
             (text (nlk:trimmed (or (business-draft-text call) "")))
             (customer-text (nlk:trimmed (or (business-draft-customer-text call) "")))
             (conn (connection connection-id)))
        (unless (plusp (length connection-id))
          (return-from draft-reply
            (refuse "no connection: name the connection_id a poll event carries")))
        (unless conn
          (return-from draft-reply
            (refuse "no business connection ~s is known here: call tg_business_poll first, so the bot reports its connection" connection-id)))
        (unless (plusp (length text))
          (return-from draft-reply
            (refuse "the draft is empty: write the reply the owner would send")))
        (unless (and chat-id (plusp (length chat-id)))
          (return-from draft-reply
            (refuse "no chat: name the customer chat_id a poll event carries")))
        (let ((owner-chat (field conn "owner_chat_id")))
          (when (null owner-chat)
            (return-from draft-reply
              (refuse "connection ~s names no owner DM chat yet: call tg_business_poll so Telegram reports it" connection-id)))
          (let* ((superseded
                   (loop for record in (pending-drafts)
                         when (and (field record "status")
                                   (string= (field record "status") "pending")
                                   (string= (or (field record "connection_id") "") connection-id)
                                   (string= (or (field record "customer_chat_id") "") chat-id))
                           collect (progn (settle record "superseded")
                                          (or (field record "draft_id") ""))))
                 (record (new-table))
                 (id (new-draft-id))
                 (expires (+ (nlk:unix-now) (* 3600 (setting :draft-ttl-hours)))))
            (setf (field record "draft_id") id
                  (field record "connection_id") connection-id
                  (field record "owner_user_id") (field conn "owner_user_id")
                  (field record "owner_chat_id") owner-chat
                  (field record "customer_chat_id") chat-id
                  (field record "customer_message_id") (or (business-draft-customer-message-id call) 0)
                  (field record "customer_name") (customer-name chat-id)
                  (field record "customer_text") (nlk:clip customer-text (setting :max-customer-text-chars))
                  (field record "draft_text") text
                  (field record "status") "pending"
                  (field record "created_at") (nlk:unix-now)
                  (field record "expires_at") expires)
            (setf (gethash id (drafts)) record)
            (let ((owner-message (send-owner-message (field record "owner_chat_id")
                                                      (render-draft record)
                                                      (draft-keyboard record))))
              (setf (field record "owner_message_id") (or owner-message 0))
              (save-state)
              (nlk:encode-json-object
               (nlk:json-object
                "draft_id" id
                "status" "pending"
                "owner_message_id" (or owner-message 0)
                "expires_at" expires
                "can_reply" (can-reply-p conn)
                "superseded" (coerce superseded 'vector)
                "note" (if owner-message
                            "the owner has it: call tg_business_poll to learn what they chose"
                            "recorded, but Telegram refused the owner DM: check the bot token, and that the owner started the bot"))))))))))

;;; --- tg_business_send ----------------------------------------------------------

(defun send-reply (call)
  "The tg_business_send tool: deliver a recorded draft now, on the owner's behalf."
  (running-settings)
  (with-telegram-business-lock
    (answering
      (let* ((draft-id (nlk:trimmed (or (business-send-draft-id call) "")))
             (override (nlk:trimmed (or (business-send-text call) "")))
             (record (draft draft-id)))
        (unless record
          (return-from send-reply (refuse "no draft ~s is recorded here" draft-id)))
        (when (draft-expired-p record)
          (settle record "expired")
          (rerender record "expired")
          (save-state)
          (return-from send-reply
            (refuse "draft ~a has expired: draft it again" draft-id)))
        (unless (member (field record "status") +answerable+ :test #'string=)
          (return-from send-reply
            (refuse "draft ~a is already ~a" draft-id (field record "status"))))
        (unless (can-reply-p (connection (field record "connection_id")))
          (return-from send-reply
            (refuse "Telegram does not let this connection reply for the owner: enable Send-on-your-behalf in Telegram Business >> Chatbots, or copy the text")))
        (let* ((text (if (plusp (length override)) override (field record "draft_text")))
               (status (if (plusp (length override)) "edited" "sent")))
          (multiple-value-bind (ok note)
              (deliver-approved record text status)
            (save-state)
            (unless ok
              (return-from send-reply (refuse "~a" note)))
            (nlk:encode-json-object
             (nlk:json-object
              "draft_id" draft-id
              "status" status
              "chat_id" (or (field record "customer_chat_id") "")
              "sent_text" text
              "note" (format nil "~a to ~a" note (or (field record "customer_chat_id") ""))))))))))

;;; --- /biz ----------------------------------------------------------------------

(defun biz-status ()
  (let ((records (connections-list)))
    (if (null records)
        "No Telegram Business connection has reported in yet. Connect the bot in Telegram Business >> Chatbots, then call tg_business_poll once."
        (with-output-to-string (out)
          (format out "Telegram Business:~%")
          (dolist (conn records)
            (format out "  ~a - ~a, reply-for-you ~:[off~;on~], drafting ~:[paused~;on~]~%"
                    (or (nonblank (field conn "user_name")) (or (field conn "connection_id") "?"))
                    (if (enabled-p conn) "connected" "disabled")
                    (can-reply-p conn) (auto-draft-p conn)))
          (format out "~d draft~:p pending; muted chats: ~{~a~^, ~}~%"
                  (length (pending-drafts)) (paused-list))))))

(defun biz-text (words)
  (let ((sub (string-downcase (or (first words) "")))
        (arg (second words)))
    (cond
      ((string= sub "") (biz-status))
      ((member sub '("pause" "resume") :test #'string=)
       (let ((on (string= sub "resume"))
             (touched 0))
         (dolist (conn (connections-list))
           (when (enabled-p conn)
             (setf (field conn "auto_draft") on)
             (incf touched)))
         (save-state)
         (if (zerop touched)
             "You don't have any active business connections."
             (format nil "~a  (~d connection~:p)"
                     (if on "Resumed drafting." "Paused drafting.") touched))))
      ((and (member sub '("off" "on") :test #'string=) (nonblank arg))
       (let ((chat (nlk:trimmed arg)))
         (if (not (ppcre:scan "\\A-?[0-9]+\\z" chat))
             (format nil "'~a' isn't a chat id. Telegram chat ids are numbers, e.g. /biz off 123456789." chat)
             (let ((was (paused-p chat)))
               (if (string= sub "off")
                   (progn (setf (gethash chat (paused)) t) (save-state)
                          (if was
                              (format nil "No change: chat ~a was already muted." chat)
                              (format nil "Chat ~a muted: its messages are still read, no draft is made." chat)))
                   (progn (remhash chat (paused)) (save-state)
                          (if was
                              (format nil "Chat ~a re-enabled." chat)
                              (format nil "No change: chat ~a was not muted." chat))))))))
      (t
       (format nil "Usage:~%  /biz             show status~%  /biz pause       pause drafting (messages are still read)~%  /biz resume      resume drafting~%  /biz off <id>    mute drafting for one customer chat~%  /biz on  <id>    re-enable it")))))

(defun run-command (args session-id)
  "The /biz command: status, pause, resume, off <chat_id>, on <chat_id>."
  (declare (ignore session-id))
  (running-settings)
  (with-telegram-business-lock
    (handler-case (biz-text (and args (nlk:split-words args)))
      (telegram-business-error (condition)
        (format nil "ERROR: ~a" (telegram-business-error-detail condition))))))

;;; --- the cell ------------------------------------------------------------------

(defun start ()
  "Load the state, and write it back on the way down."
  (setf *state* (read-state))
  (nle:on-stop (lambda ()
                 (ignore-errors (save-state))
                 (setf *state* nil))))

(nle:define-cell telegram-business
  (:section ("telegram-business")
    (:guide "a BotFather bot connected to your Telegram Business account (Telegram >> Business >> Chatbots), with its token's variable named in bot_token_env; then ask what your customers wrote")
    ("bot_token_env" :env :required t
     :doc "the environment variable holding the business bot's token (TELEGRAM_BUSINESS_BOT_TOKEN)")
    ("persona" :string
     :doc "the instruction drafts follow: voice, length, language; empty, the shipped secretary persona")
    ("max_customer_text_chars" :integer :default 4000 :min 1
     :doc "how much of a customer message is kept and quoted; longer text is cut")
    ("draft_ttl_hours" :integer :default 24 :min 1
     :doc "how long a draft may wait for the owner before it expires")
    ("timeout" :integer :default 15 :min 1
     :doc "how long one Bot API request may take, in seconds, before a long poll's own wait"))
  (:settings #'read-settings)
  (:help :telegram-business
         "telegram-business: tg_business_poll, tg_business_draft and tg_business_send draft replies to Telegram Business customers and deliver one only after the owner approves it"
         *manual*)
  (:tool "tg_business_poll" (nlk:contract 'business-poll) #'poll-updates)
  (:tool "tg_business_draft" (nlk:contract 'business-draft) #'draft-reply)
  (:tool "tg_business_send" (nlk:contract 'business-send) #'send-reply)
  (:command "biz" 'run-command
            :description "Telegram Business: status, pause, resume, mute a chat"
            :argument-hint "[pause|resume|off <chat_id>|on <chat_id>]")
  (:start #'start))
