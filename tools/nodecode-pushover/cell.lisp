;;;; cell.lisp --- Pushover's one tool, one command and one hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verb through the manual: while the cell runs,
;;;; (help :pushover) answers the primer below, and every request's help
;;;; section carries one line naming it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "pushover": {"app_token_env": "PUSHOVER_APP_TOKEN",
;;;;                "user_key_env": "PUSHOVER_USER_KEY",
;;;;                "notify_on": ["turn-end", "turn-failed", "questions"]}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from lightx's Hermes Agent plugin (see NOTICE): the messages
;;;; endpoint and its fields, the user key and application token pair, the
;;;; 1024-character body, the per-device filter, and the lifecycle alerts a
;;;; finished turn, a failed one, a question that needs an answer and a
;;;; command approval raised — now read off the organism's own frames.

(in-package #:nodecode-pushover)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Pushover sends a notification to the operator's phone, tablet or desktop, through the
nodecode-pushover cell. One tool, which a person can also call from eval as (pushover:notify ...).
  pushover_notify  {message, title, priority, device, sound, url, url_title, retry, expire}
      message is the body, plain text, at most 1024 characters: it is read on a lock screen, so
      lead with the fact and leave the narration to the transcript. title is a short headline
      over it. priority is -2 (no banner, no sound), -1 (quiet), 0 (normal, the default),
      1 (high) or 2 (emergency: Pushover repeats it every retry seconds, 60 by default, until the
      person acknowledges it, and gives up after expire seconds, 3600 by default) — keep 2 for
      something that cannot wait. device names one of the account's devices, empty reaches them
      all. sound names one of Pushover's sounds. url and url_title attach one link.
      The answer says the request Pushover gave the push, or its own words for the refusal
      (a wrong token, an unknown device, an empty message).
Send one for what the person should know now and is not watching for: a long job finished, a
deploy went out, a check failed. Do not send one per turn, and never as a substitute for
answering: the answer itself is what the transcript carries.
When the section lists notify_on, the cell also pushes by itself on the events it names —
turn-end, turn-failed and questions — and the operator's /pushover command says which."
  "What (help :pushover) answers while the cell runs.")

(defparameter +endpoint+ "https://api.pushover.net/1/messages.json"
  "Pushover's messages endpoint: the one host a push leaves for.")

(defparameter +max-message-chars+ 1024
  "Pushover's own ceiling on a message body.")

(defparameter +max-title-chars+ 250
  "Pushover's own ceiling on a title.")

(defparameter +events+ '("turn-end" "turn-failed" "questions")
  "The lifecycle events notify_on may name.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a push cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (pushover-error (condition)
       (nle:failure (format nil "ERROR: ~a" (pushover-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Pushover: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun started-p ()
  "Whether the section is read and the cell's verbs may run."
  (and *pushover* t))

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the two secrets the section names: read once, here."
  (declare (ignore table))
  (let* ((app-env (or (nonblank (getf values :app-token-env)) "PUSHOVER_APP_TOKEN"))
         (user-env (or (nonblank (getf values :user-key-env)) "PUSHOVER_USER_KEY"))
         (priority (or (getf values :priority) 0))
         (events (mapcar (lambda (event) (string-downcase (nlk:trimmed event)))
                         (or (getf values :notify-on) '())))
         (plist (copy-list values)))
    (dolist (event events)
      (unless (member event +events+ :test #'string=)
        (nlk:config-error "notify_on names ~s, which is not one of ~{~a~^, ~}" event +events+)))
    (when (or (< priority -2) (> priority 2))
      (nlk:config-error "priority ~a is outside Pushover's -2 to 2" priority))
    (setf (getf plist :app-token-env) app-env
          (getf plist :user-key-env) user-env
          (getf plist :app-token)
          (or (nonblank (nle::credential-env app-env))
              (nlk:config-error "~a is unset or empty: export the application token from pushover.net/apps in it, or name another variable in app_token_env" app-env))
          (getf plist :user-key)
          (or (nonblank (nle::credential-env user-env))
              (nlk:config-error "~a is unset or empty: export your Pushover user key in it, or name another variable in user_key_env" user-env))
          (getf plist :device) (or (nonblank (getf values :device)) "")
          (getf plist :title) (or (nonblank (getf values :title)) "Nodecode")
          (getf plist :notify-on) (remove-duplicates events :test #'string=)
          (getf plist :priority) priority
          (getf plist :timeout) (or (getf values :timeout) 8))
    plist))

;;; --- one HTTPS exchange ---------------------------------------------------------

(defun form-body (fields)
  "FIELDS — (NAME . VALUE) pairs — as an application/x-www-form-urlencoded
body, empty members left out. Pushover reads its parameters form-encoded."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in fields
                when (and (stringp value) (plusp (length value)))
                  collect (format nil "~a=~a" name (quri:url-encode value :encoding :utf-8)))))

(defun clip-message (text)
  "TEXT under Pushover's body ceiling, saying what was cut."
  (let ((text (or text "")))
    (if (<= (length text) +max-message-chars+)
        text
        (concatenate 'string (subseq text 0 (- +max-message-chars+ 3)) "..."))))

(defun post-message (fields)
  "One HTTPS POST of FIELDS to Pushover => (values OBJECT STATUS)."
  (handler-case
      (multiple-value-bind (text status)
          (nlk:http :post +endpoint+
                    :headers '(("content-type" . "application/x-www-form-urlencoded")
                               ("accept" . "application/json"))
                    :content (form-body fields)
                    :timeout (setting :timeout)
                    :connect-timeout (setting :timeout))
        (values (ignore-errors (nlk:decode-json text)) status))
    (error (condition)
      (fail "Pushover did not answer: ~a" (nlk:one-line (princ-to-string condition))))))

(defun api-errors (object)
  "Pushover's own words for a push it refused, or NIL for one it took."
  (let ((errors (and (hash-table-p object) (nlk:json-array object "errors"))))
    (when (and errors (plusp (length errors)))
      (format nil "~{~a~^; ~}" (coerce errors 'list)))))

(defun send-push (title message &key priority device sound url url-title retry expire)
  "Send one push => (values REQUEST-ID NIL) or (values NIL WHY-IT-FAILED)."
  (let* ((priority (or priority (setting :priority)))
         (fields (list (cons "token" (setting :app-token))
                       (cons "user" (setting :user-key))
                       (cons "message" (clip-message message))
                       (cons "title" (nlk:clip (or title "") +max-title-chars+))
                       (cons "priority" (princ-to-string priority))
                       (cons "device" (or (nonblank device) (setting :device)))
                       (cons "sound" (or (nonblank sound) ""))
                       (cons "url" (or (nonblank url) ""))
                       (cons "url_title" (or (nonblank url-title) "")))))
    (when (= priority 2)
      ;; An emergency push repeats until it is acknowledged: Pushover requires
      ;; the interval and the ceiling with it.
      (setf fields (append fields
                           (list (cons "retry" (princ-to-string (or retry 60)))
                                 (cons "expire" (princ-to-string (or expire 3600)))))))
    (multiple-value-bind (object status) (post-message fields)
      (let ((errors (api-errors object)))
        (cond (errors (values nil errors))
              ((and (integerp status) (<= status 299) (hash-table-p object))
               (values (or (nlk:json-value object :string "request") "") nil))
              (t (values nil (format nil "HTTP ~a~@[: ~a~]"
                                     status
                                     (and (hash-table-p object)
                                          (nlk:clip (nlk:encode-json-object object) 300))))))))))

;;; --- pushover_notify --------------------------------------------------------------

(defun send-notification (call)
  "The pushover_notify tool: one push, from the model."
  (answering
    (let ((message (nlk:trimmed (or (notification-message call) "")))
          (title (or (nonblank (notification-title call)) (setting :title)))
          (priority (notification-priority call)))
      (unless (plusp (length message))
        (return-from send-notification
          (refuse "the message is empty: say the one fact the person should read")))
      (when (and priority (or (< priority -2) (> priority 2)))
        (return-from send-notification
          (refuse "priority ~a is outside Pushover's -2 to 2" priority)))
      (multiple-value-bind (request error)
          (send-push title message
                     :priority priority
                     :device (notification-device call)
                     :sound (notification-sound call)
                     :url (notification-url call)
                     :url-title (notification-url-title call)
                     :retry (notification-retry call)
                     :expire (notification-expire call))
        (if error
            (nle:failure (format nil "ERROR: Pushover refused the push: ~a" error))
            (nlk:encode-json-object
             (nlk:json-object "sent" t
                              "request" request
                              "title" title
                              "priority" (or priority (setting :priority))
                              "device" (or (nonblank (notification-device call))
                                           (setting :device)
                                           "every device"))))))))

(defun notify (message &rest pairs)
  "The tool's own body, from eval: (pushover:notify \"the deploy is live\" :title \"Deploy\").
=> the answer the model would read: the request id, or the refusal in words."
  (send-notification
   (decode-notification
    (apply #'nlk:make-json-object
           "message" message
           (loop for (key value) on pairs by #'cddr
                 append (list (string-downcase (substitute #\_ #\- (symbol-name key))) value))))))

;;; --- what the operator reads --------------------------------------------------------

(defun notify-line ()
  "The events that push by themselves, in words."
  (let ((events (setting :notify-on)))
    (if events
        (format nil "a push on ~{~a~^, ~}" events)
        "none of its own: set notify_on to turn-end, turn-failed or questions in /settings")))

(defun status ()
  "What /pushover status answers: what a push would use, in words."
  (if (not (started-p))
      (format nil "Pushover is not running: turn the nodecode-pushover cell on in /cells, ~
or (restart-cells \"nodecode-pushover\").")
      (format nil "~{~a~^~%~}"
              (list "Pushover notifications (nodecode-pushover):"
                    (format nil "  app token  ~a (set)" (setting :app-token-env))
                    (format nil "  user key   ~a (set)" (setting :user-key-env))
                    (format nil "  device     ~a" (if (plusp (length (setting :device)))
                                                        (setting :device)
                                                        "every device on the account"))
                    (format nil "  title      ~a" (setting :title))
                    (format nil "  priority   ~a" (setting :priority))
                    (format nil "  by itself  ~a" (notify-line))
                    (format nil "  endpoint   ~a" +endpoint+)
                    "  The model sends one with the pushover_notify tool; /pushover test sends one now."))))

(defun test-push ()
  "Send one test push, and answer what the API said."
  (handler-case
      (multiple-value-bind (request error)
          (send-push (setting :title)
                     (format nil "A test push from Nodecode. Nothing is wrong: this is what a ~
notification from this machine looks like."))
        (if error
            (format nil "Pushover refused the test push: ~a" error)
            (format nil "A test push went (request ~a). Check your phone." request)))
    (pushover-error (condition)
      (format nil "No push went: ~a" (pushover-error-detail condition)))
    (error (condition)
      (format nil "No push went: ~a" (nlk:one-line (princ-to-string condition))))))

(defun status-dialog ()
  "The rows the bare /pushover offers under its answer."
  (nle:list-dialog
   :title "Pushover"
   :summary "A push notification to your phone."
   :rows (list (nle:ask-row "Send a test push" "Nothing to fill in" "/pushover test"
                             :verb "send"))))

(defun run-command (args session-id)
  "The /pushover command: status by default, one test push on `test'."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((or (string= verb "") (string= verb "status"))
           (values (status) (and (started-p) (status-dialog))))
          ((string= verb "test")
           (values (if (started-p) (test-push) (status)) (status-dialog)))
          (t (values "Usage: /pushover [status|test]. status says what a push would use; test sends one now."
                      (and (started-p) (status-dialog)))))))

;;; --- the organism's own frames ----------------------------------------------------
;; The hook point a live frame passes through, once, before any shell sees it:
;; a turn that ended or failed carries its fact there (NLK:FRAME-FACT), and a
;; question the agent is waiting on is a frame of its own (organism.questions).
;; A push is sent off the publishing thread — the operator's phone is not
;; worth a stalled frame — and only for the events the section names.

(defvar *asked-questions* nil
  "The question ids that already pushed, once the section asks for questions.")

(defvar *last-turn* nil
  "The turn id that already pushed its end, so one turn is one notification.")

(defun started-notify-p (event)
  (and (started-p) (member event (setting :notify-on) :test #'string=) t))

(defun session-label (session-id)
  "The name a person knows SESSION-ID by: its directory's own name, else its id."
  (or (and session-id
           (nlk:when-let (cwd (ignore-errors (nlk:find-session-cwd session-id)))
             (ignore-errors (nlk:folder-name cwd))))
      session-id
      "a session"))

(defun publish-push (title message)
  "Send TITLE and MESSAGE off this thread, swallowing what a dead network says:
a push is never worth a frame's failure."
  (nlk:spawn "pushover-push"
    (handler-case (send-push title message)
      (error () nil)))
  t)

(defun question-message (question)
  "One open question, as the push reads it: what it asks, its detail, its choices."
  (let ((title (nonblank (nlk:json-value question :string "title")))
        (detail (nonblank (nlk:json-value question :string "detail")))
        (choices (nlk:json-array question "choices")))
    (with-output-to-string (out)
      (write-string (or title "Nodecode needs an answer") out)
      (when detail (format out "~%~a" (nlk:clip detail 200)))
      (when (and choices (plusp (length choices)))
        (format out "~%Choose: ~{~a~^ / ~}" (coerce choices 'list))))))

(defun consider-frame (op)
  "OP as a notification: a turn's end or failure, and a question that waits."
  (multiple-value-bind (type payload metadata turn-id) (nlk:frame-fact op)
    (declare (ignore metadata))
    (cond
      ((and type (member type '("turn.completed" "turn.failed") :test #'equal))
       (let ((event (if (equal type "turn.failed") "turn-failed" "turn-end")))
         (when (and (started-notify-p event) (not (equal turn-id *last-turn*)))
           (setf *last-turn* turn-id)
           (let ((label (session-label (getf op :session-id))))
             (if (equal type "turn.failed")
                 (publish-push (format nil "~a: turn failed" (setting :title))
                               (format nil "Turn failed: ~a~@[: ~a~]"
                                       label
                                       (nlk:first-line (nlk:json-value payload :string "detail") 120)))
                 (publish-push (format nil "~a: ready for input" (setting :title))
                               (format nil "Ready for input: ~a" label)))))))
      ((equal (getf op :kind) "organism.questions")
       (when (started-notify-p "questions")
         (loop for question across (nlk:json-array (getf op :payload) "questions")
               for id = (nlk:json-value question :string "question_id")
               when (and id (not (gethash id *asked-questions*)))
                 do (setf (gethash id *asked-questions*) t)
                    (publish-push (format nil "~a: needs an answer" (setting :title))
                                  (question-message question))))))))

(defun observe-frame (op next)
  "The :frame hook: what the section asks to be told about becomes a push, and
every frame goes on to the shells whatever this decides."
  (handler-case (when (started-p) (consider-frame op))
    (error () nil))
  (funcall next op))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell pushover
  (:section ("pushover")
    (:guide "create an application at pushover.net/apps and copy its API token; your user key is on the pushover.net front page; export each in a variable (PUSHOVER_APP_TOKEN, PUSHOVER_USER_KEY) in the shell that starts Nodecode; next step: /pushover test")
    ("app_token_env" :env :default "PUSHOVER_APP_TOKEN"
     :doc "the environment variable holding the application's API token from pushover.net/apps")
    ("user_key_env" :env :default "PUSHOVER_USER_KEY"
     :doc "the environment variable holding your user key, on pushover.net's front page")
    ("device" :string :default ""
     :doc "the name of one device to deliver to; empty reaches every device on the account")
    ("title" :string :default "Nodecode"
     :doc "the headline a push carries")
    ("notify_on" :list
     :doc "the events that push by themselves: any of turn-end, turn-failed, questions. Empty, the default, pushes nothing on its own")
    ("priority" :integer :default 0 :min -2
     :doc "the priority of an automatic push: -2 silent, -1 quiet, 0 normal, 1 high, 2 emergency (repeats until acknowledged)")
    ("timeout" :integer :default 8 :min 1
     :doc "how long one Pushover request may take, in seconds"))
  (:settings #'read-settings)
  (:help :pushover
         "pushover: pushover_notify sends a push notification to the operator's phone"
         *manual*)
  (:tool "pushover_notify" (nlk:contract 'notification) #'send-notification)
  (:command "pushover" 'run-command
            :description "Pushover: what a push would use, or a test push to your phone"
            :argument-hint "status | test")
  (:hook :frame #'observe-frame)
  (:start (lambda ()
            (setf *asked-questions* (make-hash-table :test #'equal)
                  *last-turn* nil)
            (nle:on-stop (lambda ()
                           (setf *asked-questions* nil
                                 *last-turn* nil))))))
