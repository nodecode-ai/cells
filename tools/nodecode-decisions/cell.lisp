;;;; cell.lisp --- the two tools, the manual, the section, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original runs its submissions on a private event loop and keeps the
;;;; finished ones in memory, so a tool call never waits for an endpoint and a
;;;; second call reads the answer back. The same two verbs, the same choice of
;;;; background or foregound, and the same short memory: one thread per job,
;;;; and a registry that is this process's alone.

(in-package #:nodecode-decisions)

(defparameter +manual+
  "Decisions asks yes/no questions about a text and answers, for each, the probability that
it is true: (is this a support ticket? 0.97), (is it urgent? 0.12). Two tools reach it.
  decisions_submit   input is the text (at most 256 KiB with its questions); questions is
                     one to 64 of them, each an object whose type is \"predicate\" and whose
                     instructions is the question; background (the default) answers a job
                     id at once, and background false waits for the probabilities.
  decisions_status   job_id reads a submission of this session back: running, completed with
                     the endpoint's own reply object (every member it sent, not only the
                     answers), or failed with the reason.
An answer is {\"type\": \"predicate\", \"name\": null, \"probability\": 0.87}; the
probabilities are the endpoint's, and a question asked in the negative reads worse than the
same one asked in the affirmative. Nothing is retried: a failed submission says whether the
outcome is uncertain, and asking again is a second call. Four submissions run at once and
eight times as many finished ones are kept, all in memory: (restart-cells) and a relaunch
keep none of them."
  "What (help :decisions) answers while the cell runs.")

;;; --- the job registry ----------------------------------------------------------------

(defvar *jobs* (make-hash-table :test #'equal)
  "Job id -> (OWNER . FIELDS): FIELDS the JSON object a caller reads back.")

(defvar *order* '()
  "Job ids in the order they were admitted, oldest first: what retention reads.")

(defvar *running* 0
  "Jobs admitted and not yet finished, against +CAPACITY+.")

(defvar *spawn* nil
  "How a background job runs: NIL for a thread of its own; a test binds a
function that runs it on this one, so the answer is a fact rather than a wait.")

(defun owner ()
  "The session a call belongs to: its jobs are that session's alone."
  (or (getf (nle:turn) :session-id) ""))

(defun job-id ()
  "A fresh job id: 32 hexadecimal digits, as the original's were."
  (format nil "~(~{~2,'0x~}~)" (loop repeat 16 collect (random 256))))

(defun job-fields (id)
  "The FIELDS of job ID, or NIL when it is gone."
  (let ((job (gethash id *jobs*)))
    (and job (cdr job))))

(defun job-state (id)
  (let ((fields (job-fields id)))
    (and fields (gethash "state" fields))))

(defun trim ()
  "Keep the newest +RETENTION+ finished jobs; the older ones go."
  (let ((finished (remove-if-not (lambda (id) (let ((state (job-state id)))
                                                (and state (not (equal "running" state)))))
                                 *order*)))
    (dolist (id (butlast finished (min +retention+ (length finished))))
      (remhash id *jobs*)
      (setf *order* (delete id *order* :test #'equal)))))

(defun finish (id state &key answers error)
  "Job ID moved to STATE: ANSWERS (the decoded reply) when it completed, ERROR when
it failed. A job already finished keeps what it said; the running count moves once."
  (with-decisions-lock
    (let ((fields (job-fields id)))
      (when (and fields (equal "running" (gethash "state" fields)))
        (setf (gethash "state" fields) state)
        (cond (answers (setf (gethash "answers" fields) answers))
              (error (setf (gethash "error" fields) error)))
        (decf *running*)
        (trim)))))

(defun admit ()
  "A new job under the ceiling; => its id. Past the ceiling the call is refused, not queued."
  (with-decisions-lock
    (when (>= *running* +capacity+)
      (fail "decisions: ~d submissions are already running; wait for one of them before submitting another"
            +capacity+))
    (let ((id (job-id)))
      (setf (gethash id *jobs*) (cons (owner) (nlk:json-object "job_id" id "state" "running"))
            *order* (append *order* (list id)))
      (incf *running*)
      id)))

(defun run-job (id settings body count)
  "The request job ID is: its answer or its failure lands on the job, and nothing
signals out of the thread that runs it."
  (handler-case (finish id "completed" :answers (post settings body count))
    (decisions-error (condition) (finish id "failed" :error (decisions-error-detail condition)))
    (error () (finish id "failed"
                      :error "decisions: an internal error; the outcome is uncertain and it was not retried"))))

(defun run-in-background (function)
  "FUNCTION on a thread of its own, or on this one when a test says so."
  (if *spawn*
      (funcall *spawn* function)
      (bt2:make-thread function :name "nodecode-decisions request")))

(defun job-text (id)
  "Job ID as the caller reads it: the JSON object, without the owner it is filed under."
  (with-decisions-lock
    (let ((fields (job-fields id)))
      (unless fields
        (fail "decisions: job ~a is gone: finished jobs are kept ~d deep, and a restart keeps none"
              id +retention+))
      (nlk:encode-json-object fields))))

;;; --- the two tools ---------------------------------------------------------------

(defun tool-submit (call)
  "decisions_submit: the questions asked about the input, as a job or as the answers."
  (let* ((settings (running-settings))
         (input (decision-submission-input call))
         (questions (decision-submission-questions call))
         (background (decision-submission-background call))
         (count (length questions)))
    (require-key settings)
    (when (zerop count)
      (fail "decisions: ask at least one question about the input"))
    (when (> count +max-questions+)
      (fail "decisions: at most ~d questions in one submission, got ~d" +max-questions+ count))
    (let* ((body (request-octets settings input questions))
           (id (admit)))
      (if background
          (progn (run-in-background (lambda () (run-job id settings body count)))
                 (job-text id))
          (progn (run-job id settings body count)
                 (job-text id))))))

(defun tool-status (call)
  "decisions_status: a submission of this session, by the id decisions_submit answered."
  (running-settings)
  (let ((id (decision-status-job-id call)))
    (with-decisions-lock
      (let ((job (gethash id *jobs*)))
        (unless (and job (equal (car job) (owner)))
          (fail "decisions: no job ~s in this session: another session's jobs are not read here, finished ones are kept ~d deep, and a restart keeps none"
                id +retention+))
        (nlk:encode-json-object (cdr job))))))

;;; --- /decisions ------------------------------------------------------------------

(defun run-command (args session-id)
  "/decisions: the submissions this session started, oldest first, and what became of
them. Typed after a job id, it answers that one job's whole record."
  (declare (ignore session-id))
  (running-settings)
  (let ((asked (nlk:trimmed (or args ""))))
    (if (plusp (length asked))
        (job-text asked)
        (let ((mine (with-decisions-lock
                      (loop for id in *order*
                            for job = (gethash id *jobs*)
                            when (and job (equal (car job) (owner)))
                              collect (cons id (cdr job))))))
          (if (null mine)
              "decisions: no submissions in this session yet; decisions_submit starts one, and decisions_status reads it back"
              (with-output-to-string (out)
                (format out "decisions: ~d submission~:p in this session~%" (length mine))
                (loop for (id . fields) in mine
                      do (format out "  ~a  ~a~@[  ~a~]~%"
                                 id (gethash "state" fields)
                                 (let ((error (gethash "error" fields)))
                                   (and error (nlk:one-line error :cap 160)))))))))))

;;; --- the entry -------------------------------------------------------------------

(defun install ()
  "A start keeps no job from the last one: the registry is this process's, and a
restart is a fresh one."
  (with-decisions-lock
    (clrhash *jobs*)
    (setf *order* '()
          *running* 0))
  (nle:on-stop (lambda ()
                 (with-decisions-lock
                   (clrhash *jobs*)
                   (setf *order* '()
                         *running* 0)))))

(nle:define-cell decisions
  (:section ("decisions")
    (:guide "paste a Decisions API key into api_key, or name the variable holding it in api_key_env; endpoint and model already point at the OpenAI Decisions API; next step: ask a yes/no question about a piece of text")
    ("endpoint" :string :default "https://api.openai.com/v1/decisions"
     :doc "the Decisions endpoint: an absolute http(s) URL, no query, no credential, no fragment")
    ("model" :string :default "gpt-6-luna"
     :doc "the model identifier the endpoint accepts")
    ("api_key" :secret
     :doc "the API key, written here; empty reads the variable api_key_env names")
    ("api_key_env" :env :default "OPENAI_API_KEY"
     :doc "the environment variable holding the key, when it is not written above")
    ("timeout_seconds" :integer :default 120 :min 1
     :doc "one submission's ceiling in seconds, at most 600; a call past it is not retried"))
  (:settings #'read-settings)
  (:tool "decisions_submit" (nlk:contract 'decision-submission) #'tool-submit)
  (:tool "decisions_status" (nlk:contract 'decision-status) #'tool-status)
  (:help :decisions
         "decisions: yes/no probability answers about a text, from a Decisions endpoint (decisions_submit, decisions_status)"
         +manual+)
  (:command "decisions" 'run-command
            :description "The decisions submissions this session started")
  (:start #'install))
