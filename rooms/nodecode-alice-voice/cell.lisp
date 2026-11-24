;;;; cell.lisp --- a Yandex Alice skill's voice requests, answered in one session.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of bablobanov's hermes-alice-voice (MIT). The same private
;;;; skill, the same bearer-in-the-path secret, the same plain words, the
;;;; same "the answer arrives where the session speaks, not in Alice". What
;;;; changed is where it lives: the original opened a loopback HTTP server
;;;; of its own beside a gateway; here the endpoint is one gateway route of
;;;; the running organism, and the injection is the kernel's own in-process
;;;; ingress, NLE:SUBMIT, instead of a plugin context call.
;;;;
;;;; Inbound only: nothing here dials out, so the cell needs no key of its
;;;; own and no network but the operator's reverse proxy pointing at the
;;;; gateway's port.

(defpackage #:nodecode-alice-voice
  (:use #:cl)
  (:export #:*inject* #:*cache* #:*read-body* #:handle-request #:route-path
           #:route-answer #:alice-request #:run-slash #:response-text #:reset!
           #:cache-size))

(in-package #:nodecode-alice-voice)

(nlk:define-peripheral alice-voice :not-running t)

;;; --- the words ---------------------------------------------------------------
;;; Alice speaks these; they are the skill's own replies, not the model's.
;;; The original named Telegram as the destination of the answer; Nodecode
;;; answers in the session, which is a channel room or the shell itself.

(defparameter *ack* "Запрос принят для обработки. Ответ будет в Nodecode"
  "Scheduling only: it never claims the turn ran or that an answer arrived.")
(defparameter *denied* "Запрос отклонён")
(defparameter *not-sent* "Запрос не отправлен. Попробуйте позже")
(defparameter *empty* "Назовите задачу для Nodecode. Ответ будет в Nodecode")
(defparameter *help-reply* "Назовите задачу. Управление и подтверждения доступны только в Nodecode")
(defparameter *bye* "До встречи")
(defparameter *later* "Ответ будет в Nodecode")

(defparameter *empty-ask* "Назовите задачу для Nodecode. Ответ будет в Nodecode")

(defparameter *help-words* '("помощь" "что ты умеешь" "help"))
(defparameter *bye-words* '("выход" "хватит" "стоп" "exit"))
(defparameter *later-words* '("ответ готов" "дальше"))

(defparameter *max-body* 16384
  "The original's ceiling on one request body; past it, 413.")
(defparameter *max-text* 4096
  "The original's ceiling on one utterance.")
(defparameter *cache-limit* 32
  "How many accepted requests are remembered for deduplication, newest first.
Retries arrive with the same message id; the cache is in memory only, so a
restart forgets it, as the original's did.")

;;; --- state -------------------------------------------------------------------

(defvar *cache* '()
  "((SKILL ACCOUNT SESSION MESSAGE) . (STATUS . REPLY)) newest first, at most
*CACHE-LIMIT* entries: what makes a retried message id one turn, not two.")
(defvar *registered* nil
  "The path the running cell put on the gateway, or NIL while stopped.")
(defvar *message-counter* (get-universal-time)
  "The message id /alice say gives the next synthetic request.")
(defvar *lock* (bt2:make-lock :name "alice-voice")
  "One lock over *CACHE*: the gateway answers requests on several threads.")

(defun reset! ()
  "Forget every remembered request; the registered path is the cell's start
and stop, not this."
  (bt2:with-lock-held (*lock*) (setf *cache* '()))
  (setf *message-counter* (get-universal-time)))

(defun cache-size ()
  (bt2:with-lock-held (*lock*) (length *cache*)))

;;; --- the shape of one request -------------------------------------------------

(defun field (object key)
  "OBJECT's KEY, or NIL: OBJECT need not be an object at all."
  (and (hash-table-p object) (gethash key object)))

(defun identifier-p (value)
  "A short non-empty string, the only thing the payload may name with."
  (and (stringp value) (plusp (length value)) (<= (length value) 256)))

(defun digest-p (value)
  "A 64-character hex secret with at least four distinct characters, as the
original required of ALICE_WEBHOOK_SECRET."
  (and (stringp value)
       (= 64 (length value))
       (every (lambda (c) (digit-char-p c 16)) value)
       (>= (length (remove-duplicates value :test #'char=)) 4)))

(defun reply (text &key end-session)
  "The Alice protocol's answer object: one spoken text, and whether the
skill session ends here."
  (nlk:json-object "version" "1.0"
                    "response" (nlk:json-object "text" text
                                                 "end_session" (and end-session t))))

(defun response-text (answer)
  (field (field answer "response") "text"))

;;; --- the section ---------------------------------------------------------------

(defun read-settings (values table)
  "VALUES, what the declaration derived, checked into the plist the cell runs
on. A missing or malformed secret refuses: it is the path of every request
and the only credential the endpoint has."
  (declare (ignore table))
  (let ((secret (getf values :secret))
        (skill (getf values :skill-id))
        (session (getf values :session))
        (owner (getf values :owner-user-id)))
    (unless (digest-p secret)
      (nlk:config-error "alice-voice: secret must be a 64-character hex string (32 random bytes); ~
generate one with `openssl rand -hex 32` and give the same value to the skill's webhook URL"))
    (unless (identifier-p skill)
      (nlk:config-error "alice-voice: skill_id is the private skill's id, as the skill sends it"))
    (unless (identifier-p session)
      (nlk:config-error "alice-voice: session names the Nodecode session the requests go into"))
    (when (and owner (plusp (length owner)) (not (identifier-p owner)))
      (nlk:config-error "alice-voice: owner_user_id is the skill account to accept, or empty for any"))
    (list :secret secret
          :skill-id skill
          :session session
          :owner-user-id (or owner ""))))

(defun route-path ()
  "The path the skill posts to: the secret is the bearer, so the path is one."
  (format nil "/alice/~a" (setting :secret)))

;;; --- injection ------------------------------------------------------------------

(defun ensure-session (session)
  "SESSION standing by, so a first request has somewhere to land."
  (when (and (nlk:store-open-p) (not (nlk:session-exists-p session)))
    (nlk:standby-session :id session
                         :cwd (namestring (ensure-directories-exist (nlk:home "alice/"))))))

(defun inject-into (session text command-id)
  "TEXT into SESSION through the one in-process ingress. T when the kernel
took it: started, queued behind a running turn, steered, or already admitted
under the same command id."
  (ensure-session session)
  (member (nlk:active-input-admission-disposition
           (nle:submit session text :command-id command-id :source "alice" :source-id "alice-voice"))
          '(:started :queued :steered :duplicate)))

(defvar *inject* #'inject-into
  "How an utterance reaches its session: (SESSION TEXT COMMAND-ID) => true
when it was taken. A test rebinds it; nothing else does.")

(defun command-id (key)
  "The kernel's idempotency key for one Alice message."
  (format nil "alice-~{~a~^-~}" key))

;;; --- the dispatch ------------------------------------------------------------------

(defun accept (session account text)
  "TEXT into the configured session, once per message id."
  (let* ((key (list (setting :skill-id) account (field session "session_id")
                     (field session "message_id")))
         (known (bt2:with-lock-held (*lock*) (assoc key *cache* :test #'equal))))
    (if known
        (values (second known) (cddr known))
        (let ((taken (handler-case (funcall *inject* (setting :session) text (command-id key))
                       (error () nil))))
          (multiple-value-bind (status value)
              (if taken (values 200 (reply *ack*)) (values 503 (reply *not-sent*)))
            (when taken
              (bt2:with-lock-held (*lock*)
                (push (list* key status value) *cache*)
                (when (> (length *cache*) *cache-limit*)
                  (setf *cache* (subseq *cache* 0 *cache-limit*)))))
            (values status value))))))

(defun control-word-p (word words)
  (member (string-downcase (string-trim '(#\Space #\Tab #\Newline) word)) words
          :test #'equal))

(defun handle-text (session request account)
  "The words of one utterance: help, exit and the two reminders answer here,
never in the model; anything else becomes a turn."
  (let ((command (field request "command"))
        (original (field request "original_utterance")))
    (cond
      ((or (not (stringp command)) (not (stringp original)))
       (values 400 (reply *denied*)))
      ((or (> (length command) *max-text*) (> (length original) *max-text*))
       (values 400 (reply *denied*)))
      ((or (uiop:string-prefix-p "/" (string-left-trim '(#\Space) command))
           (some (lambda (c) (or (< (char-code c) 32) (= (char-code c) 127))) command)
           (some (lambda (c) (or (< (char-code c) 32) (= (char-code c) 127))) original))
       ;; A slash command would be one the operator never typed: a voice
       ;; request cannot reach /cells or /settings.
       (values 403 (reply *denied*)))
      (t
       (let ((text (if (plusp (length (string-trim '(#\Space #\Tab) original))) original command)))
         (cond
           ((zerop (length (string-trim '(#\Space #\Tab #\Newline) text)))
            (values 200 (reply *empty*)))
           ((control-word-p command *help-words*) (values 200 (reply *help-reply*)))
           ((control-word-p command *bye-words*) (values 200 (reply *bye* :end-session t)))
           ((member (string-right-trim "?.!" (string-downcase (string-trim '(#\Space #\Tab) command)))
                    *later-words* :test #'equal)
            (values 200 (reply *later*)))
           (t (accept session account text))))))))

(defun handle-request (path body)
  "PATH and the decoded BODY of one skill request => (values STATUS ANSWER).
The checks are the original's, in its order: the running bridge, the secret
path, the protocol version, the skill, the account, the message ids, then
the words."
  (cond
    ((null *registered*) (values 503 (reply *not-sent*)))
    ((not (equal path (route-path))) (values 403 (reply *denied*)))
    ((not (hash-table-p body)) (values 400 (reply *denied*)))
    ((not (equal "1.0" (field body "version"))) (values 400 (reply *denied*)))
    (t
     (let* ((session (field body "session"))
            (request (field body "request"))
            (account (field (field session "user") "user_id"))
            (owner (setting :owner-user-id)))
       (cond
         ((or (not (hash-table-p session)) (not (hash-table-p request))
              (not (equal (field session "skill_id") (setting :skill-id)))
              (not (identifier-p account))
              (and (plusp (length owner)) (not (equal account owner))))
          (values 403 (reply *denied*)))
         ((or (not (identifier-p (field session "session_id")))
              (not (integerp (field session "message_id")))
              (minusp (field session "message_id"))
              (not (equal (field request "type") "SimpleUtterance")))
          (values 400 (reply *denied*)))
         (t (handle-text session request account)))))))

;;; --- the route ---------------------------------------------------------------------

(defun header (env name)
  (let ((headers (getf env :headers)))
    (and (hash-table-p headers) (gethash name headers))))

(defun body-value (env)
  "ENV's body as a Lisp value, or a keyword naming why it is not one."
  (let* ((stream (getf env :raw-body))
         (length (getf env :content-length)))
    (cond
      ((null stream) :absent)
      ((or (not (integerp length)) (minusp length)) :bad-length)
      ((> length *max-body*) :too-large)
      ((not (search "application/json" (or (header env "content-type") "") :test #'char-equal))
       ;; Bodies are parsed only when declared JSON, as the gateway's own
       ;; routes do.
       :not-json)
      (t
       (let* ((text (if (eq 'character (stream-element-type stream))
                        (make-string length)
                        (make-array length :element-type '(unsigned-byte 8))))
              (read (read-sequence text stream)))
         (if (= read length)
             (handler-case (nlk:decode-json text :whole t)
               (error () :unreadable))
             :truncated))))))

(defun route-answer (env)
  "The skill's webhook, answered on the gateway's own port: /alice/<secret>,
POST, one JSON body, one JSON answer in the Alice protocol."
  (let* ((raw (or (getf env :request-uri) ""))
         (path (subseq raw 0 (or (position #\? raw) (length raw))))
         (body (body-value env)))
    (multiple-value-bind (status value)
        (case body
          (:too-large (values 413 (reply *denied*)))
          (:not-json (values 415 (reply *denied*)))
          ((:absent :bad-length :truncated :unreadable) (values 400 (reply *denied*)))
          (t (handle-request path body)))
      (let ((octets (nlk:encode-json-octets value)))
        (list status (list :content-type "application/json; charset=utf-8"
                           :content-length (length octets))
              octets)))))

;;; --- the operator's /alice -----------------------------------------------------------

(defun alice-request (text &key (message-id (incf *message-counter*)))
  "A skill-shaped payload carrying TEXT, as the private skill would send it:
the operator's own console, so /alice say proves the whole path."
  (let ((owner (setting :owner-user-id)))
    (nlk:json-object
     "version" "1.0"
     "session" (nlk:json-object "skill_id" (setting :skill-id)
                                 "user" (nlk:json-object "user_id" (if (plusp (length owner))
                                                                        owner "console"))
                                 "session_id" "console"
                                 "message_id" message-id
                                 "new" nil)
     "request" (nlk:json-object "type" "SimpleUtterance"
                                 "command" text
                                 "original_utterance" text))))

(defun status-text ()
  "What /alice answers with nothing after it: the one secret the operator has
to paste into the skill, and the session it feeds."
  (let ((owner (setting :owner-user-id)))
    (format nil "alice-voice: the skill posts to https://<your proxy>/~a and the requests become turns in session ~s~@[; only account ~a is accepted~]. ~d request~:p remembered. Keep the path private: it is the only credential."
            (route-path) (setting :session)
            (and (plusp (length owner)) owner)
            (cache-size))))

(defun run-slash (args session-id)
  "`/alice` answers where the endpoint stands; `/alice say TEXT` sends one
utterance down the same path a voice request takes."
  (declare (ignore session-id))
  (let* ((line (nlk:trimmed (or args "")))
         (space (position #\Space line))
         (verb (if space (subseq line 0 space) line))
         (rest (if space (nlk:trimmed (subseq line (1+ space))) "")))
    (cond
      ((member verb '("" "status") :test #'string-equal) (status-text))
      ((string-equal verb "say")
       (if (plusp (length rest))
           (multiple-value-bind (status answer) (handle-request (route-path) (alice-request rest))
             (format nil "~d ~a" status (or (response-text answer) "")))
           "alice: /alice say <what you would say to the assistant>"))
      (t (format nil "alice: ~a is not a verb; status, or say <text>" verb)))))

;;; --- the manual -------------------------------------------------------------------------

(defparameter *primer*
  "A private Yandex Alice skill sends one recognized utterance to /alice/<secret> on this
Nodecode's gateway port; the cell turns it into a turn in one session you name, and the
answer arrives wherever that session speaks (its shell, or the channel room it is bound
to) -- never spoken by Alice, which only says the request was accepted.

Set up: secret is 64 hex characters (`openssl rand -hex 32`), the same value the skill's
webhook URL carries and nothing else repeats; skill_id is the private skill's id; session
is the Nodecode session the requests go into, created on first use. owner_user_id is
optional -- empty accepts any Alice account that holds the path, which is the only
credential: a leaked URL is a leaked key. Your HTTPS reverse proxy must forward to the
gateway port with Host: 127.0.0.1:<port>, because the gateway refuses any other Host.

Try it without a voice: /alice status, and /alice say <text>, which sends one utterance
down the same path. Leading slash commands and control characters are refused; ordinary
words can still ask the agent to use tools, under the session's own approvals.")

;;; --- the cell -----------------------------------------------------------------------------

(nle:define-cell alice-voice
  (:section ("alice-voice")
    (:guide "secret: the 64-hex value the skill's webhook URL carries, `openssl rand -hex 32`; skill_id: the private skill's id; session: the Nodecode session the requests land in; owner_user_id: optional, the one Alice account to accept")
    ("secret" :secret :required t
              :doc "the bearer in the webhook path, 64 hex characters")
    ("skill_id" :string :required t
                :doc "the private Alice skill's id")
    ("session" :string :required t :default "alice"
               :doc "the Nodecode session every request becomes a turn in")
    ("owner_user_id" :string :default ""
                     :doc "the Alice account to accept; empty accepts any account holding the path"))
  (:settings 'read-settings)
  (:command "alice" 'run-slash
            :description "The Yandex Alice voice endpoint: where it stands, or one utterance down it")
  (:help :alice-voice "alice-voice: a Yandex Alice skill's voice requests, answered as turns in one session" *primer*)
  (:start (lambda ()
            (reset!)
            (setf *registered* (nle:route (route-path) #'route-answer :public t))
            (nle:on-stop (lambda ()
                           (when *registered*
                             (nle:route *registered* nil)
                             (setf *registered* nil))))
            (nle:notice (format nil "alice-voice: /alice/<secret> answers the skill into session ~a"
                                (setting :session))
                        :key "nodecode-alice-voice"))))
