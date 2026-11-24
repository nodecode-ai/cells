;;;; cell.lisp --- a question asked without waiting, and the answer that comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of web3blind/hermes-deferred-questions (MIT). The
;;;; original posted a question through one of the host gateway's chat
;;;; transports, held the answer in SQLite, and admitted the answer back into
;;;; the agent's own chat when it arrived. A Nodecode session is the chat, so
;;;; the same three parts are here without a transport: the agent enqueues a
;;;; question and is free at once, the operator answers with /dq (or by
;;;; saying so in the conversation), and the answer reaches the session at
;;;; its next model request, once, as a block of its own -- a running turn
;;;; reads it at its next round, an idle session at its next turn.
;;;;
;;;; What the original calls binding, this keeps: one answer per question,
;;;; the first one wins, the answer names its question's id, and it is only
;;;; handed back to the session that asked. Expiry is absolute and checked
;;;; before an answer is stored; an idempotency key makes one question of two
;;;; writes. The ledger is one JSON file under the home, mode 0600, holding
;;;; the questions and their answers unencrypted, as the original's does.

(in-package #:nodecode-deferred-questions)

;;; --- the words ---------------------------------------------------------------

(defparameter +actions+ '("request" "get" "list" "cancel")
  "What the deferred_questions tool may ask for: enqueue a question, read one
back, read the newest ones, or cancel one that still waits.")

(defparameter *framing*
  "[deferred answers: ~d. The operator answered questions you asked earlier; nothing is waiting for them any more. Use the answer and carry on, and ask the next question rather than blocking on it.]"
  "The first line of what a model request carries: how many answers arrived
since the last one, in the one ~d a caller fills. A layer may reword it.")

(defparameter +primer+
  "A deferred question is asked and left: the turn that asks it does not wait.
The answer arrives in a later request of the same session, in a block headed
[deferred answers: N], and the question is finished from then on. The
nodecode-deferred-questions cell keeps the ledger; its verbs answer a string,
and a refusal is ERROR: DEFERRED-QUESTIONS-ERROR with the reason.
  (dq:request :question \"Which date works?\" :options '(\"Friday\" \"Monday\"))
      => asked #1; the operator answers with /dq 1 Friday
  (dq:request :question \"Which branch?\" :allow-text t :expires \"+7d\")
  (dq:board)                  the ledger: what waits first, then the rest
  (dq:show 1)                 one question, its options and its answer
  (dq:answer 1 \"Friday\")      record an answer as if the operator gave it
  (dq:cancel 1)               cancel one that still waits
An answer is a number when the question offers options, or free text when it
allows that; a question takes one answer and then stands answered. A question
expires at the absolute time its :expires names (\"+72h\", \"+7d\" or an ISO
time; deferred-questions.expires_hours when it names none) and an answer after
that is refused. The same :key twice is one question, not two.
The model tool deferred_questions does the same four things: request, get,
list, cancel. /dq in a shell shows what waits, /dq ID ANSWER answers one,
/dq answer ID TEXT does the same in words, /dq cancel ID drops one, /dq show ID
reads one, /dq list lists them."
  "What (help :deferred-questions) answers while the cell runs.")

;;; --- the ledger's file ---------------------------------------------------------

(defparameter +fields+
  ;; (KEY WIRE DEFAULT) -- one place the file's shape is written. `options' is
  ;; the one array, read and written on its own below.
  '((:id "id" 0) (:key "key" "") (:question "question" "") (:allow-text "allow_text" t)
    (:session "session" "") (:state "state" "pending") (:answer "answer" "")
    (:answer-from "answer_from" "") (:answered-at "answered_at" 0)
    (:delivered "delivered" 0) (:expires-at "expires_at" 0) (:created "created" 0))
  "The question's fields: symbol, wire name, and its default.")

(defparameter +states+ '("pending" "answered" "cancelled" "expired")
  "The four states a question is in: waiting, answered, cancelled by the
operator or the model, or past its expiry.")

(defun ledger-path ()
  "The ledger's one file: under the home, mode 0600."
  (nlk:home "deferred-questions/questions.json"))

(defun field-default (key)
  "The default value of the field KEY."
  (third (assoc key +fields+)))

(defun blank-item ()
  "A question with every field at its default, and no options."
  (append (loop for (key nil default) in +fields+ append (list key default))
          (list :options '())))

(defun item-field (item key)
  "ITEM's KEY, or its default when it has none."
  (let ((value (getf item key)))
    (if (eq key :allow-text)
        (if (null value) nil t)
        (or value (field-default key)))))

(defun wired (item key)
  "ITEM's KEY as the file writes it: a boolean as JSON true or false."
  (let ((value (item-field item key)))
    (if (eq key :allow-text) (if value t :false) value)))

(defun option-list (value)
  "VALUE as the option labels a question carries: a vector or a list of
strings, trimmed, no empties, no duplicates, at most deferred-questions.max_options."
  (let* ((raw (cond ((vectorp value) (coerce value 'list))
                     ((listp value) value)
                     ((null value) '())
                     (t (list value))))
         (labels (remove-if (lambda (label) (zerop (length label)))
                            (mapcar (lambda (label) (nlk:trimmed (princ-to-string label))) raw)))
         (labels (remove-duplicates labels :test #'string=))
         (cap (setting :max-options)))
    (when (> (length labels) cap)
      (fail "a question carries at most ~d options, not ~d: deferred-questions.max_options says so"
            cap (length labels)))
    (dolist (label labels labels)
      (when (> (length label) 150)
        (fail "an option is at most 150 characters: ~s" (nlk:clip label 60))))))

(defun item-to-json (item)
  "ITEM as the JSON object the file holds."
  (apply #'nlk:make-json-object
         (append (loop for (key wire default) in +fields+
                       append (list wire (wired item key)))
                 (list "options" (coerce (item-field item :options) 'vector)))))

(defun json-to-item (object)
  "The question OBJECT, every field defaulted."
  (let ((item (blank-item)))
    (loop for (key wire default) in +fields+
          do (setf (getf item key)
                   (cond ((integerp default) (or (nlk:json-value object :integer wire) default))
                         ((stringp default) (or (nlk:json-value object :string wire) default))
                         (t (not (null (nlk:json-value object :boolean wire)))))))
    (setf (getf item :options)
          (remove-if-not #'stringp
                         (coerce (or (nlk:json-value object :array "options") #()) 'list)))
    item))

(defun read-ledger ()
  "The ledger: (list :next-id N :items ITEMS). A missing or torn file is an
empty ledger, never an error."
  (let* ((text (nlk:read-text (ledger-path)))
         (object (and text (ignore-errors (nlk:decode-json text))))
         (items (and object (nlk:json-value object :array "items"))))
    (list :next-id (or (and object (nlk:json-value object :integer "next_id")) 1)
          :items (loop for raw across (or items #()) when (hash-table-p raw)
                       collect (json-to-item raw)))))

(defun forget-old (ledger &optional (now (nlk:unix-now)))
  "Drop LEDGER's finished questions older than deferred-questions.retention_days.
An answered question stays until it has been handed to its session."
  (let ((window (* 86400 (setting :retention-days))))
    (setf (getf ledger :items)
          (remove-if (lambda (item)
                       (let ((state (item-field item :state)))
                         (and (not (string= state "pending"))
                              (or (not (string= state "answered"))
                                  (plusp (item-field item :delivered)))
                              (< (max (item-field item :answered-at)
                                      (item-field item :created))
                                 (- now window)))))
                     (getf ledger :items)))
    ledger))

(defun write-ledger (ledger)
  "LEDGER as its file: atomic, 0600, its folder 0700."
  (nlk:write-file-atomically
   (ledger-path)
   (nlk:encode-json-object
    (nlk:make-json-object
     "version" 1
     "next_id" (getf ledger :next-id 1)
     "items" (coerce (mapcar #'item-to-json (getf (forget-old ledger) :items)) 'vector)))
   :mode #o600 :directory-mode #o700)
  ledger)

(defmacro with-ledger ((ledger) &body body)
  "BODY with LEDGER bound, read and written back under the cell's lock. A
refusal inside BODY writes nothing."
  `(with-deferred-questions-lock
     (let ((,ledger (read-ledger)))
       (prog1 (progn ,@body)
         (write-ledger ,ledger)))))

;;; --- time --------------------------------------------------------------------

(defun at-text (unix)
  "UNIX as a short stamp an operator reads: 2026-11-10T03:00."
  (and unix (plusp unix) (subseq (nlk:iso-time unix :millis nil) 0 16)))

(defun relative-text (unix now)
  "UNIX read against NOW: \"in 18h\", \"now\", \"2h ago\"."
  (let ((seconds (- unix now)))
    (cond ((< (abs seconds) 60) "now")
          ((plusp seconds) (format nil "in ~a" (nlk:duration-text seconds :coarse t)))
          (t (format nil "~a ago" (nlk:duration-text (- seconds) :coarse t))))))

(defun parse-expiry (text &optional (now (nlk:unix-now)))
  "The absolute instant TEXT names: a delay (\"+72h\", \"+7d\", \"+90m\"), or an
ISO time; with no TEXT, deferred-questions.expires_hours from NOW."
  (let ((text (nlk:trimmed (or text ""))))
    (cond ((string= text "") (+ now (* 3600 (setting :expires-hours))))
          ((char= #\+ (char text 0))
           (let ((seconds (nlk:parse-duration (subseq text 1))))
             (unless seconds
               (fail "~s is not a delay: write \"+72h\", \"+7d\" or \"+90m\"" text))
             (+ now seconds)))
          (t
           (let* ((padded (if (= 16 (length text)) (concatenate 'string text ":00") text))
                  (universal (nlk:iso-universal padded)))
             (unless universal
               (fail "~s is not a time: a delay (\"+72h\", \"+7d\") or an ISO time (\"2026-11-15T09:00\")"
                     text))
             (- universal nlk:+unix-epoch+))))))

;;; --- the ledger's arithmetic -----------------------------------------------------

(defun expire-items (ledger &optional (now (nlk:unix-now)))
  "The questions of LEDGER whose absolute expiry has passed stop waiting."
  (dolist (item (getf ledger :items) ledger)
    (when (and (string= (item-field item :state) "pending")
               (<= (item-field item :expires-at) now))
      (setf (getf item :state) "expired")))
  ledger)

(defun waiting-items (ledger)
  "The questions of LEDGER still waiting for an answer, oldest first."
  (remove-if-not (lambda (item) (string= (item-field item :state) "pending"))
                 (getf ledger :items)))

(defun sorted-items (ledger)
  "LEDGER's questions: the ones still waiting, oldest first, then the rest,
newest first."
  (append (waiting-items ledger)
          (reverse (remove-if (lambda (item) (string= (item-field item :state) "pending"))
                              (getf ledger :items)))))

(defun item-by-id (ledger id)
  "LEDGER's question ID, or NIL."
  (find id (getf ledger :items) :key (lambda (item) (item-field item :id))))

(defun find-item (ledger id)
  "LEDGER's question ID, or a refusal naming what the ledger holds."
  (or (item-by-id ledger id)
      (fail "no question #~a in the ledger: (dq:board) lists ~d of them" id
            (length (getf ledger :items)))))

(defun id-number (value)
  "VALUE, or the integer it spells, or a refusal."
  (cond ((integerp value) value)
        ((and (stringp value) (plusp (length (nlk:trimmed value))))
         (let ((parsed (ignore-errors (parse-integer (nlk:trimmed value) :junk-allowed t))))
           (or parsed (fail "~s is no question id: /dq lists the ids" value))))
        (t (fail "~s is no question id: /dq lists the ids" value))))

(defun key-for (question now)
  "The idempotency key a question written without one gets: its first words
and the day, so two writes of one question are one question."
  (let* ((words (remove-if (lambda (word) (zerop (length word)))
                            (uiop:split-string (string-downcase (or question ""))
                                               :separator " -,.;:!?")))
         (words (or words (list "question")))
         (slug (format nil "~{~a~^-~}" (subseq words 0 (min 6 (length words))))))
    (format nil "~a-~a" (subseq slug 0 (min 48 (length slug)))
            (subseq (nlk:iso-time now :millis nil) 0 10))))

(defun question-line (item now)
  "One question as the board tells it."
  (let ((answer (item-field item :answer))
        (created (item-field item :created)))
    (format nil "#~d [~a] ~a~a~a"
            (item-field item :id) (item-field item :state)
            (nlk:clip (item-field item :question) 160)
            (if (plusp (length answer)) (format nil " -> ~s" answer) "")
            (if (plusp created) (format nil " (asked ~a)" (relative-text created now)) ""))))

(defun option-lines (item)
  "ITEM's options as an answer sheet, or NIL when it has none."
  (loop for option in (item-field item :options)
        for index from 1
        collect (format nil "   ~d. ~a" index (nlk:clip option 150))))

(defun announced-state (item)
  "The suffix the ledger adds to a question that is no longer waiting."
  (let ((state (item-field item :state)))
    (if (string= state "pending") "" (format nil " [~a]" state))))

;;; --- the verbs -------------------------------------------------------------------

(define-verb request (&key question options (allow-text t) (expires "+72h") key session)
  "Ask a question and move on. OPTIONS are the fixed answers the operator
picks by number, ALLOW-TEXT whether free text is accepted as well, EXPIRES
\"+72h\", \"+7d\" or an ISO time, KEY an idempotency key (the same key again
answers the same question). => what was asked, as text."
  (let ((text
          (with-ledger (ledger)
            (expire-items ledger)
            (let* ((now (nlk:unix-now))
                   (question (nlk:trimmed (or question "")))
                   (given (nlk:trimmed (or key "")))
                   (labels (option-list options))
                   (expires-at (parse-expiry expires now)))
              (when (zerop (length question))
                (fail "a question needs words: :question \"Which date works?\""))
              (when (> (length question) 1200)
                (fail "a question is at most 1200 characters: this one is ~d" (length question)))
              (when (and (null labels) (not allow-text))
                (fail "a question with no options must allow text, or nothing can answer it"))
              (when (<= expires-at now)
                (fail "~a is not ahead of now: a question needs a time to stand until"
                      (at-text expires-at)))
              (let* ((wanted (if (plusp (length given)) given (key-for question now)))
                     (same (find wanted (getf ledger :items)
                                 :key (lambda (item) (item-field item :key))
                                 :test #'string=)))
                (if same
                    (format nil "asked #~d again (same key ~s): ~a~a -- /dq show ~d"
                            (item-field same :id) wanted
                            (nlk:clip (item-field same :question) 160)
                            (announced-state same) (item-field same :id))
                    (let ((item (blank-item)))
                      (setf (getf item :id) (getf ledger :next-id 1)
                            (getf item :key) wanted
                            (getf item :question) question
                            (getf item :options) labels
                            (getf item :allow-text) (and allow-text t)
                            (getf item :session) (or session "")
                            (getf item :created) now
                            (getf item :expires-at) expires-at
                            (getf item :state) "pending"
                            (getf ledger :next-id) (1+ (getf ledger :next-id 1))
                            (getf ledger :items) (append (getf ledger :items) (list item)))
                      (format nil "asked #~d: ~a~a~%(the operator answers with /dq ~d ANSWER~a; it stands until ~a)"
                              (item-field item :id) (nlk:clip question 300)
                              (if labels (format nil "~%~{~a~^~%~}" (option-lines item)) "")
                              (item-field item :id)
                              (if (item-field item :allow-text)
                                  ", a number or free text" ", a number")
                              (at-text expires-at)))))))))
    (announce)
    text))

(define-verb answer (id value &key (from "operator"))
  "Record the operator's answer to question ID: VALUE is a number when the
question offers options, or free text when it allows that. The first answer
wins; a cancelled, expired or already answered question refuses."
  (let ((value (nlk:trimmed (or value "")))
        (id (id-number id)))
    (when (zerop (length value))
      (fail "an answer needs words: /dq answer ~a TEXT" id))
    (let ((text
            (with-ledger (ledger)
              (expire-items ledger)
              (let* ((now (nlk:unix-now))
                     (item (find-item ledger id))
                     (state (item-field item :state)))
                (when (string= state "answered")
                  (fail "#~d was answered ~a (~a): one answer stands, and this is not it"
                        id (at-text (item-field item :answered-at))
                        (relative-text (item-field item :answered-at) now)))
                (unless (string= state "pending")
                  (fail "#~d is ~a, not waiting: (dq:board) shows what still waits" id state))
                (let ((stored (resolve-answer item value))
                      (options (length (item-field item :options))))
                  (setf (getf item :state) "answered"
                        (getf item :answer) stored
                        (getf item :answer-from) (or from "operator")
                        (getf item :answered-at) now)
                  (format nil "answered #~d: ~a -> ~s~a; it reaches the session that asked at its next request"
                          id (nlk:clip (item-field item :question) 160) stored
                          (if (string= stored value)
                              ""
                              (format nil " (option ~a of ~d)" value options))))))))
      (announce)
      text)))

(define-verb show (id)
  "One question, its options, its answer and where it stands."
  (with-ledger (ledger)
    (expire-items ledger)
    (let* ((now (nlk:unix-now))
           (item (find-item ledger (id-number id)))
           (state (item-field item :state))
           (answer (item-field item :answer))
           (delivered (item-field item :delivered))
           (session (item-field item :session))
           (lines (list (format nil "#~d [~a] ~a" (item-field item :id) state
                                 (nlk:clip (item-field item :question) 400)))))
      (setf lines (append lines (option-lines item)))
      (when (and (null (item-field item :options)) (item-field item :allow-text))
        (setf lines (append lines (list "   or free text"))))
      (setf lines (append lines
                           (list (format nil "asked ~a by ~a, ~a"
                                         (relative-text (item-field item :created) now)
                                         (if (plusp (length session))
                                             (subseq session 0 (min 12 (length session)))
                                             "any session")
                                         (if (string= state "expired")
                                             "expired"
                                             (format nil "expires ~a"
                                                     (at-text (item-field item :expires-at))))))))
      (when (plusp (length answer))
        (setf lines (append lines
                            (list (format nil "answered ~s by ~a ~a" answer
                                          (item-field item :answer-from)
                                          (relative-text (item-field item :answered-at) now))))))
      (when (plusp delivered)
        (setf lines (append lines
                            (list (format nil "it reached the session that asked ~a"
                                          (relative-text delivered now))))))
      (when (string= state "pending")
        (setf lines (append lines (list (format nil "answer: /dq answer ~d TEXT" (item-field item :id))))))
      (format nil "~{~a~^~%~}" lines))))

(define-verb board (&key (which "all") (limit 20))
  "The ledger as it stands: WHICH = all (the default), pending, or answered."
  (let ((which (string-downcase (nlk:trimmed (or which "all")))))
    (unless (member which '("all" "pending" "answered") :test #'string=)
      (fail "~s is not a view: all, pending or answered" which))
    (with-ledger (ledger)
      (expire-items ledger)
      (let* ((now (nlk:unix-now))
             (items (if (string= which "all")
                        (sorted-items ledger)
                        (remove-if-not (lambda (item) (string= (item-field item :state) which))
                                       (sorted-items ledger))))
             (shown (subseq items 0 (min (length items) (max 1 limit)))))
        (if (null shown)
            (format nil "no ~a question~:p in the ledger: (dq:request :question \"...\") asks one"
                    (if (string= which "all") "" which))
            (format nil "~d question~:p (~d waiting)~a:~%~{~a~^~%~}"
                    (length (getf ledger :items))
                    (length (waiting-items ledger))
                    (if (string= which "all") "" (format nil ", ~a view" which))
                    (mapcar (lambda (item) (question-line item now)) shown)))))))

(define-verb cancel (id)
  "Cancel a question that still waits: no answer will be taken for it."
  (let ((id (id-number id)))
    (let ((text
            (with-ledger (ledger)
              (expire-items ledger)
              (let ((item (find-item ledger id)))
                (unless (string= (item-field item :state) "pending")
                  (fail "#~d is ~a, so there is nothing to cancel" id (item-field item :state)))
                (setf (getf item :state) "cancelled")
                (format nil "cancelled #~d: ~a" id (nlk:clip (item-field item :question) 160))))))
      (announce)
      text)))

;;; --- the answer that comes back ---------------------------------------------------

(defun resolve-answer (item value)
  "VALUE as it will be stored: a number the question offers options for
becomes that option's own words, and a question that allows no free text
takes nothing else."
  (let* ((options (item-field item :options))
         (index (ignore-errors (parse-integer value :junk-allowed t))))
    (cond ((and index (<= 1 index (length options)))
           (nth (1- index) options))
          ((and index options)
           (fail "~d is not one of the ~d options #~d offers: 1 to ~d"
                 index (length options) (item-field item :id) (length options)))
          ((item-field item :allow-text) value)
          (options
           (fail "#~d takes a number from 1 to ~d, not free text: ~s"
                 (item-field item :id) (length options) (nlk:clip value 60)))
          (t (fail "#~d offers nothing to answer" (item-field item :id))))))

(defun ready-items (ledger session)
  "The answered questions of LEDGER this SESSION has not been handed yet:
those it asked, and those asked with no session named."
  (remove-if-not (lambda (item)
                    (and (string= (item-field item :state) "answered")
                         (zerop (item-field item :delivered))
                         (let ((asked (item-field item :session)))
                           (or (zerop (length asked))
                               (null session)
                               (string= asked session)))))
                  (sorted-items ledger)))

(defun answer-block (item)
  "One answered question as the model reads it."
  (let ((from (item-field item :answer-from)))
    (format nil "#~d Q: ~a~%   A: ~a~@[ (from ~a)~]"
            (item-field item :id)
            (nlk:clip (item-field item :question) 400)
            (item-field item :answer)
            (and (plusp (length from)) from))))

(defun continuation (session)
  "What one model request carries: the answers SESSION has not been handed,
marked handed so they arrive once. => the text, or NIL."
  (when (setting :deliver-in-round)
    (with-ledger (ledger)
      (expire-items ledger)
      (let ((ready (ready-items ledger session)))
        (when ready
          (let ((text (nlk:clip
                       (format nil "~a~%~{~a~^~%~}"
                               (format nil *framing* (length ready))
                               (mapcar #'answer-block ready))
                       4000))
                (now (nlk:unix-now)))
            (dolist (item ready) (setf (getf item :delivered) now))
            text))))))

(defun deliver (op)
  "At a model request: the answers that arrived since the last one arrive in
the round about to be asked, and are handed over once."
  (when (and (nlk:store-open-p) (getf op :turn) (getf op :session-id))
    (let ((text (ignore-errors (continuation (getf op :session-id)))))
      (when text
        (nlk:record-turn-context-prompt (getf op :turn) :text text)))))

(defun announce ()
  "Say on the board how many questions wait for the operator, or clear that."
  (let ((waiting (ignore-errors (length (waiting-items (read-ledger))))))
    (nle:notice (if (and waiting (plusp waiting))
                    (format nil "deferred questions: ~d waiting -- /dq shows what waits"
                            waiting)
                    nil)
                :key "nodecode-deferred-questions")))

;;; --- the tools -----------------------------------------------------------------

(nlk:define-contract deferred-call
  (:doc "Ask the operator a question and move on: this turn does not wait for the answer. It reaches a later request of this session, once. Also reads back the questions and their answers.")
  ("action" :text :required t :options +actions+
   :doc "request: enqueue a question; get: one question and its answer; list: the newest questions; cancel: cancel one that still waits")
  ("id" :integer :doc "the question's id, for get and cancel")
  ("question" :text :doc "the question itself, for request: one sentence the operator can answer")
  ("options" :array :of :string :doc "fixed answers the operator picks by number, in order; leave it out for a free-text question")
  ("allow_text" :boolean :default t :doc "whether a free-text answer is accepted as well as an option number")
  ("expires" :text :default "+72h" :doc "how long the question stands: \"+72h\", \"+7d\" or an ISO time")
  ("idempotency_key" :text :default "" :doc "the same key again answers the same question rather than adding a second")
  ("limit" :integer :default 20 :doc "how many questions list returns"))

(defun current-session ()
  "The session a tool call runs in, or NIL when the turn names none."
  (let ((turn (ignore-errors (nle:turn))))
    (and turn (getf turn :session-id))))

(defun tool-call (call)
  "deferred_questions: the one tool, its four actions."
  (let ((action (deferred-call-action call)))
    (cond ((string= action "request")
           (request :question (deferred-call-question call)
                    :options (deferred-call-options call)
                    :allow-text (deferred-call-allow-text call)
                    :expires (deferred-call-expires call)
                    :key (deferred-call-idempotency-key call)
                    :session (current-session)))
          ((string= action "get") (show (deferred-call-id call)))
          ((string= action "list") (board :limit (deferred-call-limit call)))
          (t (cancel (deferred-call-id call))))))

;;; --- /dq -----------------------------------------------------------------------

(defun head-and-rest (args)
  "ARGS split at its first blank: the head word, and the rest trimmed."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (c) (member c '(#\Space #\Tab))) text)))
    (values (string-downcase (subseq text 0 space))
            (if space (nlk:trimmed (subseq text space)) ""))))

(defun ledger-summary ()
  "The ledger as /dq tells it: what waits, and what is answered but not yet read."
  (let* ((ledger (read-ledger))
         (waiting (waiting-items ledger))
         (ready (remove-if-not (lambda (item)
                                  (and (string= (item-field item :state) "answered")
                                       (zerop (item-field item :delivered))))
                                (getf ledger :items))))
    (format nil "deferred questions: ~d waiting~a~%~{~a~^~%~}"
            (length waiting)
            (if ready (format nil ", ~d answered and not yet read" (length ready)) "")
            (if waiting
                (mapcar (lambda (item) (question-line item (nlk:unix-now))) waiting)
                (list "none: the agent asks one with its deferred_questions tool when it needs an answer")))))

(defun panel ()
  "The dialog bare /dq answers: each waiting question, and the whole ledger."
  (let ((waiting (waiting-items (read-ledger))))
    (nle:list-dialog
     :title "Deferred questions" :summary (format nil "~d waiting" (length waiting))
     :rows (append
            (loop for item in waiting
                  collect (nle:ask-row
                           (format nil "Answer #~d ~a" (item-field item :id)
                                   (nlk:clip (item-field item :question) 60))
                           "Answer"
                           (format nil "/dq answer ~d" (item-field item :id))
                           :placeholder (if (item-field item :options)
                                            "a number, or words"
                                            "words")
                           :verb "answer"))
            (list (nle:run-row "Every question" "/dq list"
                               :detail "the ledger, what waits first"))))))

(defun run-slash (args session-id)
  "/dq: what waits, and the way to answer it. /dq ID ANSWER answers one,
/dq answer ID TEXT the same in words, /dq show ID reads one, /dq cancel ID
drops one, /dq list lists the ledger."
  (declare (ignore session-id))
  (multiple-value-bind (head rest) (head-and-rest args)
    (cond ((string= head "") (values (ledger-summary) (panel)))
          ((string= head "list") (board))
          ((string= head "pending") (board :which "pending"))
          ((string= head "show") (show (id-number rest)))
          ((string= head "cancel") (cancel (id-number rest)))
          ((string= head "answer")
           (multiple-value-bind (id value) (head-and-rest rest)
             (answer (id-number id) value)))
          ((ignore-errors (parse-integer head :junk-allowed t))
           (answer (id-number head) rest))
          (t (ledger-summary)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell deferred-questions
  (:section ("deferred-questions")
    (:guide "the agent asks a question and moves on; the answer reaches the session at its next request, and /dq is where the operator gives it")
    ("expires_hours" :integer :default 72 :min 1
     :doc "how long a question stands when the caller names no expiry")
    ("max_options" :integer :default 24 :min 1
     :doc "the most fixed answers one question may offer")
    ("retention_days" :integer :default 30 :min 0
     :doc "how long finished questions stay in the ledger")
    ("deliver_in_round" :boolean :default t
     :doc "hand the answers to the model at its next request, rather than only when it asks"))
  (:tool "deferred_questions" (nlk:contract 'deferred-call) 'tool-call)
  (:help :deferred-questions "deferred-questions: ask the operator a question and move on; the answer comes back at a later request, /dq answers it" +primer+)
  (:hook :round (nle:observer #'deliver "deferred-questions"))
  (:command "dq" 'run-slash
            :catches 'deferred-questions-error
            :description "Deferred questions: what waits, and the answer to one"
            :argument-hint "[ID ANSWER | answer ID TEXT | show ID | cancel ID | list]")
  (:start (lambda ()
            (announce)
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-deferred-questions"))))))
