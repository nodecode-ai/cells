;;;; cell.lisp --- Telegram's one tool, one command and one hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verb through the manual: while the cell runs,
;;;; (help :telegram-notify) answers the primer below, and every request's
;;;; help section carries one line naming it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "telegram-notify": {"bot_token_env": "TELEGRAM_BOT_TOKEN",
;;;;                       "chat_id": "123456789",
;;;;                       "notify_on": ["turn-end", "turn-failed"]}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from madpower2000's Hermes Agent plugin (see NOTICE): the Bot API
;;;; call, the token and chat pair, the lifecycle messages a turn's start,
;;;; completion and failure raised, the bounded final answer and approval
;;;; request text, and the duplicate suppression of one turn's one message —
;;;; now read off the organism's own frames.

(in-package #:nodecode-telegram-notify)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Telegram notify sends a message to the operator's Telegram chat, through a bot of their
own, from the nodecode-telegram-notify cell. One tool, which a person can also call from
eval as (telegram-notify:notify ...).
  telegram_notify  {message, chat_id, silent}
      message is the body, plain text, at most 4096 characters: it is read on a lock
      screen, so lead with the fact and leave the narration to the transcript.
      chat_id sends to another chat the bot can see; empty means the configured chat.
      silent delivers it without a notification sound, for something that can wait.
      The answer says which message the bot posted, or its own words for the refusal (a
      wrong token, a chat the bot was never started in, an empty message).
Send one for what the person should know now and is not watching for: a long job finished,
a deploy went out, a check failed. Do not send one per turn, and never as a substitute
for answering: the answer itself is what the transcript carries.
When the section lists notify_on, the cell also messages by itself on the events it names
— turn-start, turn-end, turn-failed and questions — and a turn's message carries the
turn's final answer when include_answer is set. (hermes) names the events."
  "What (help :telegram-notify) answers while the cell runs.")

(defparameter +api-root+ "https://api.telegram.org/bot"
  "The Bot API's one host: a message leaves for nowhere else.")

(defparameter +max-text+ 4096
  "Telegram's own ceiling on a message body.")

(defparameter +max-title-chars+ 250
  "The ceiling on the configured headline, so one line stays one line.")

(defparameter +events+ '("turn-start" "turn-end" "turn-failed" "questions")
  "The lifecycle events notify_on may name.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a message cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (telegram-notify-error (condition)
       (nle:failure (format nil "ERROR: ~a" (telegram-notify-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Telegram: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun started-p ()
  "Whether the section is read and the cell's verbs may run."
  (and *telegram-notify* t))

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the token the section names: read once, here."
  (declare (ignore table))
  (let* ((token-env (or (nonblank (getf values :bot-token-env)) "TELEGRAM_BOT_TOKEN"))
         (chat-id (nonblank (getf values :chat-id)))
         (events (mapcar (lambda (event) (string-downcase (nlk:trimmed event)))
                         (or (getf values :notify-on) '())))
         (plist (copy-list values)))
    (dolist (event events)
      (unless (member event +events+ :test #'string=)
        (nlk:config-error "notify_on names ~s, which is not one of ~{~a~^, ~}" event +events+)))
    (unless chat-id
      (nlk:config-error "chat_id is empty: send the bot one message, run /telegram-notify chats to read the id it answers with, and set it in /settings"))
    (setf (getf plist :bot-token-env) token-env
          (getf plist :chat-id) chat-id
          (getf plist :bot-token)
          (or (nonblank (nle::credential-env token-env))
              (nlk:config-error "~a is unset or empty: put the bot token @BotFather gave you in it, or name another variable in bot_token_env" token-env))
          (getf plist :notify-on) (remove-duplicates events :test #'string=)
          (getf plist :include-answer) (if (member :include-answer values) (getf values :include-answer) t)
          (getf plist :answer-max-chars) (min +max-text+ (or (getf values :answer-max-chars) 3200))
          (getf plist :max-chars) (min +max-text+ (or (getf values :max-chars) 3900))
          (getf plist :title) (nlk:clip (or (nonblank (getf values :title)) "Nodecode") +max-title-chars+)
          (getf plist :timeout) (or (getf values :timeout) 8))
    plist))

;;; --- one HTTPS exchange ---------------------------------------------------------

(defun clip-text (text)
  "TEXT under Telegram's ceiling, saying what was cut."
  (let ((text (or text ""))
        (limit (setting :max-chars)))
    (if (<= (length text) limit)
        text
        (concatenate 'string (subseq text 0 (max 0 (- limit 3))) "..."))))

(defun api-call (method payload)
  "One HTTPS POST of PAYLOAD to METHOD of the bot API => (values OBJECT STATUS)."
  (handler-case
      (multiple-value-bind (text status)
          (nlk:http :post (format nil "~a~a/~a" +api-root+ (setting :bot-token) method)
                    :headers '(("content-type" . "application/json")
                               ("accept" . "application/json"))
                    :content (nlk:encode-json-object payload)
                    :timeout (setting :timeout)
                    :connect-timeout (setting :timeout))
        (values (ignore-errors (nlk:decode-json text)) status))
    (error (condition)
      (fail "the Bot API did not answer: ~a" (nlk:one-line (princ-to-string condition))))))

(defun api-error (object)
  "Telegram's own words for a call it refused, or NIL for one it took."
  (when (and (hash-table-p object)
             (not (eq t (nlk:json-value object :boolean "ok"))))
    (or (nonblank (nlk:json-value object :string "description"))
        "the Bot API rejected the call")))

(defun send-message (text &key chat-id silent)
  "Send TEXT to CHAT-ID, the section's chat when empty => (values MESSAGE-ID NIL)
or (values NIL WHY-IT-FAILED). One call, to Telegram, and nothing else."
  (let* ((chat-id (or (nonblank chat-id) (setting :chat-id)))
         (payload (nlk:json-object "chat_id" chat-id
                                    "text" (clip-text text)
                                    "disable_web_page_preview" t
                                    :when silent "disable_notification" t)))
    (multiple-value-bind (object status) (api-call "sendMessage" payload)
      (let ((error (api-error object)))
        (cond (error (values nil error))
              ((and (integerp status) (<= status 299) (hash-table-p object))
               (values (or (nlk:json-value (nlk:json-value object :object "result")
                                            :integer "message_id")
                           0)
                       nil))
              (t (values nil (format nil "HTTP ~a from the Bot API" status))))))))

(defun discover-chats ()
  "The chats the bot has seen a message from => a list of (ID . LABEL), oldest first."
  (multiple-value-bind (object status) (api-call "getUpdates" (nlk:json-object "timeout" 0))
    (declare (ignore status))
    (nlk:when-let (error (api-error object))
      (fail "Telegram refused the chat list: ~a" error))
    (let ((found '()))
      (loop for update across (or (nlk:json-array object "result") #())
            for chat = (nlk:json-value update :object "message" "chat")
            when chat
              do (let ((id (nlk:json-value chat :integer "id")))
                   (when id
                     (pushnew (cons (princ-to-string id)
                                    (or (nlk:json-value chat :string "title")
                                        (nlk:json-value chat :string "username")
                                        (nlk:json-value chat :string "first_name")
                                        ""))
                              found :test #'equal :key #'car))))
      (nreverse found))))

;;; --- telegram_notify --------------------------------------------------------------

(defun send-notification (call)
  "The telegram_notify tool: one message, from the model."
  (answering
    (let ((message (nlk:trimmed (or (notification-message call) ""))))
      (unless (plusp (length message))
        (return-from send-notification
          (refuse "the message is empty: say the one fact the person should read")))
      (multiple-value-bind (id error)
          (send-message message
                         :chat-id (notification-chat-id call)
                         :silent (notification-silent call))
        (if error
            (nle:failure (format nil "ERROR: Telegram refused the message: ~a" error))
            (nlk:encode-json-object
             (nlk:json-object "sent" t
                               "message_id" id
                               "chat" (or (nonblank (notification-chat-id call))
                                          (setting :chat-id))
                               "title" (setting :title))))))))

(defun notify (message &rest pairs)
  "The tool's own body, from eval: (telegram-notify:notify \"the deploy is live\" :silent t).
=> the answer the model would read: the message id, or the refusal in words."
  (send-notification
   (decode-notification
    (apply #'nlk:make-json-object
           "message" message
           (loop for (key value) on pairs by #'cddr
                 append (list (string-downcase (substitute #\_ #\- (symbol-name key))) value))))))

;;; --- what the operator reads --------------------------------------------------------

(defun notify-line ()
  "The events that message by themselves, in words."
  (let ((events (setting :notify-on)))
    (if events
        (format nil "a message on ~{~a~^, ~}" events)
        "none of its own: set notify_on to turn-start, turn-end, turn-failed or questions in /settings")))

(defun status ()
  "What /telegram-notify status answers: what a message would use, in words."
  (if (not (started-p))
      (format nil "Telegram notify is not running: turn the nodecode-telegram-notify cell on in /cells, ~
or (restart-cells \"nodecode-telegram-notify\").")
      (format nil "~{~a~^~%~}"
              (list "Telegram notifications (nodecode-telegram-notify):"
                    (format nil "  bot token  ~a (set)" (setting :bot-token-env))
                    (format nil "  chat       ~a" (setting :chat-id))
                    (format nil "  title      ~a" (setting :title))
                    (format nil "  by itself  ~a" (notify-line))
                    (format nil "  answer     ~a"
                            (if (setting :include-answer)
                                (format nil "the turn's final answer, up to ~a characters"
                                        (setting :answer-max-chars))
                                "not included: include_answer is off"))
                    (format nil "  endpoint   ~a<token>/sendMessage" +api-root+)
                    "  The model sends one with the telegram_notify tool; /telegram-notify test sends one now."))))

(defun test-message ()
  "Send one test message, and answer what the API said."
  (handler-case
      (multiple-value-bind (id error)
          (send-message (format nil "~a · a test message from Nodecode. Nothing is wrong: this is ~
what a notification from this machine looks like." (setting :title)))
        (if error
            (format nil "Telegram refused the test message: ~a" error)
            (format nil "A test message went (message ~a). Check your Telegram." id)))
    (telegram-notify-error (condition)
      (format nil "No message went: ~a" (telegram-notify-error-detail condition)))
    (error (condition)
      (format nil "No message went: ~a" (nlk:one-line (princ-to-string condition))))))

(defun chat-list ()
  "The chats the bot can see, one id and label a line."
  (handler-case
      (let ((chats (discover-chats)))
        (if chats
            (format nil "~{~a~^~%~}"
                    (cons "Chats this bot has heard from (put one in chat_id):"
                          (loop for (id . label) in chats
                                collect (format nil "  ~a  ~a" id (if (plusp (length label)) label "(no name)")))))
            "This bot has heard from no chat yet: send it any message in Telegram, then run /telegram-notify chats again."))
    (telegram-notify-error (condition)
      (format nil "No chat list: ~a" (telegram-notify-error-detail condition)))
    (error (condition)
      (format nil "No chat list: ~a" (nlk:one-line (princ-to-string condition))))))

(defun status-dialog ()
  "The rows the bare /telegram-notify offers under its answer."
  (nle:list-dialog
   :title "Telegram notify"
   :summary "A Telegram message when a turn ends, fails or needs an answer."
   :rows (list (nle:ask-row "Send a test message" "Nothing to fill in" "/telegram-notify test"
                             :verb "send")
               (nle:ask-row "List the chats this bot can see" "Nothing to fill in"
                             "/telegram-notify chats" :verb "list"))))

(defun run-command (args session-id)
  "The /telegram-notify command: status by default, one test message or the chat list on a verb."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((or (string= verb "") (string= verb "status"))
           (values (status) (and (started-p) (status-dialog))))
          ((string= verb "test")
           (values (if (started-p) (test-message) (status)) (status-dialog)))
          ((member verb '("chats" "chat-id" "discover-chat-id") :test #'string=)
           (values (if (started-p) (chat-list) (status)) (status-dialog)))
          (t (values "Usage: /telegram-notify [status|test|chats]. status says what a message would use; ~
test sends one now; chats lists the ids the bot can see."
                      (and (started-p) (status-dialog)))))))

;;; --- the organism's own frames ----------------------------------------------------
;; The hook point a live frame passes through, once, before any shell sees it: a
;; turn's start, its final answer, its end or failure arrives there
;; (NLK:FRAME-FACT), and a question the agent is waiting on is a frame of its own
;; (organism.questions). A message goes out off the publishing thread — the
;; operator's phone is not worth a stalled frame — and only for the events the
;; section names, one per turn.

(defvar *answers* nil
  "Turn id -> the turn's final assistant text, kept until its end frames it.")

(defvar *notified* nil
  "The event keys that already went, so one turn is one message.")

(defun remember-answer (turn-id text)
  "Keep TEXT as TURN-ID's final answer; the hash is bounded, and an old turn's
answer is worth nothing once its end has passed."
  (when (and turn-id text)
    (when (> (hash-table-count *answers*) 64)
      (clrhash *answers*))
    (setf (gethash turn-id *answers*) text)))

(defun claim-notification (key)
  "True the first time KEY is claimed, NIL after: one turn, one message."
  (when (and key (not (gethash key *notified*)))
    (when (> (hash-table-count *notified*) 256)
      (clrhash *notified*))
    (setf (gethash key *notified*) t)
    t))

(defun started-notify-p (event)
  (and (started-p) (member event (setting :notify-on) :test #'string=) t))

(defun session-label (session-id)
  "The name a person knows SESSION-ID by: its directory's own name, else its id."
  (or (and session-id
           (nlk:when-let (cwd (ignore-errors (nlk:find-session-cwd session-id)))
             (ignore-errors (nlk:folder-name cwd))))
      session-id
      "a session"))

(defun turn-message (type payload turn-id session-id)
  "The message a terminal or opening turn fact TYPE becomes: the headline, the
session, and the answer it produced when the section asks for it."
  (let* ((label (session-label session-id))
         (title (setting :title))
         (answer (and (setting :include-answer) (gethash turn-id *answers*))))
    ;; An answer rides one message, never the next turn's.
    (when answer (remhash turn-id *answers*))
    (cond
      ((equal type "turn.started")
       (format nil "~a · turn started~%~a" title label))
      ((equal type "turn.failed")
       (format nil "~a · turn failed~%~a~@[~%~a~]"
               title label
               (nlk:first-line (nlk:json-value payload :string "detail") 300)))
      (answer
       (format nil "~a · turn done~%~a~%~%~a"
               title label (nlk:clip answer (setting :answer-max-chars))))
      (t (format nil "~a · turn done~%~a~%Ready for input." title label)))))

(defun question-message (question)
  "One open question, as the message reads it: what it asks, its detail, its choices."
  (let ((title (nonblank (nlk:json-value question :string "title")))
        (detail (nonblank (nlk:json-value question :string "detail")))
        (choices (nlk:json-array question "choices")))
    (with-output-to-string (out)
      (write-string (or title "Nodecode needs an answer") out)
      (when detail (format out "~%~a" (nlk:clip detail 200)))
      (when (and choices (plusp (length choices)))
        (format out "~%Choose: ~{~a~^ / ~}" (coerce choices 'list))))))

(defun publish-message (text)
  "Send TEXT off this thread, swallowing what a dead network says: a message is
never worth a frame's failure."
  (nlk:spawn "telegram-notify-message"
    (handler-case (send-message text)
      (error () nil)))
  t)

(defun consider-frame (op)
  "OP as a notification: a turn's start, end or failure, and a question that waits."
  (multiple-value-bind (type payload metadata turn-id) (nlk:frame-fact op)
    (declare (ignore metadata))
    (cond
      ((equal type "turn.assistant_message_completed")
       (nlk:when-let (text (nlk:fact-message-content payload))
         (remember-answer turn-id text)))
      ((member type '("turn.started" "turn.completed" "turn.failed") :test #'equal)
       (let ((event (second (assoc type '(("turn.started" "turn-start")
                                          ("turn.completed" "turn-end")
                                          ("turn.failed" "turn-failed"))
                                    :test #'equal))))
         (when (and (started-notify-p event)
                    (claim-notification (format nil "~a:~a" event turn-id)))
           (publish-message (turn-message type payload turn-id (getf op :session-id))))))
      ((equal (getf op :kind) "organism.questions")
       (when (started-notify-p "questions")
         (loop for question across (nlk:json-array (getf op :payload) "questions")
               for id = (nlk:json-value question :string "question_id")
               when (claim-notification (format nil "question:~a" id))
                 do (publish-message (format nil "⚠️ ~a · needs an answer~%~a"
                                             (setting :title) (question-message question)))))))))

(defun observe-frame (op next)
  "The :frame hook: what the section asks to be told about becomes a message, and
every frame goes on to the shells whatever this decides."
  (handler-case (when (started-p) (consider-frame op))
    (error () nil))
  (funcall next op))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell telegram-notify
  (:section ("telegram-notify")
    (:guide "make a bot with @BotFather in Telegram and copy its token; send the bot one message; export the token in a variable (TELEGRAM_BOT_TOKEN) and put the chat id in chat_id (run /telegram-notify chats to read it); next step: /telegram-notify test")
    ("bot_token_env" :env :default "TELEGRAM_BOT_TOKEN"
     :doc "the environment variable holding the bot token @BotFather gave you")
    ("chat_id" :string :required t
     :doc "the Telegram chat a message goes to: an id the bot has seen, from /telegram-notify chats")
    ("notify_on" :list
     :doc "the events that message by themselves: any of turn-start, turn-end, turn-failed, questions. Empty, the default, messages nothing on its own")
    ("include_answer" :boolean :default t
     :doc "put a turn's final answer in the message its end sends")
    ("answer_max_chars" :integer :default 3200 :min 0
     :doc "how much of that answer one message carries")
    ("max_chars" :integer :default 3900 :min 1
     :doc "the ceiling on any one message, under Telegram's own 4096")
    ("title" :string :default "Nodecode"
     :doc "the name a message's headline carries")
    ("timeout" :integer :default 8 :min 1
     :doc "how long one Bot API request may take, in seconds"))
  (:settings #'read-settings)
  (:help :telegram-notify
         "telegram-notify: telegram_notify sends a Telegram message to the operator's chat"
         *manual*)
  (:tool "telegram_notify" (nlk:contract 'notification) #'send-notification)
  (:command "telegram-notify" 'run-command
            :description "Telegram notify: what a message would use, a test message, or the chats the bot sees"
            :argument-hint "status | test | chats")
  (:hook :frame #'observe-frame)
  (:start (lambda ()
            (setf *answers* (make-hash-table :test #'equal)
                  *notified* (make-hash-table :test #'equal))
            (nle:on-stop (lambda ()
                           (setf *answers* nil
                                 *notified* nil))))))
