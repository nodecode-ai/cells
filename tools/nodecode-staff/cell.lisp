;;;; cell.lisp --- Staff: a roster of specialists, a board of jobs, and a way
;;;; out to a coworker that is not this process.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's five tools and two commands over one job file
;;;; (tools.py, store.py, __init__.py:44-78):
;;;;
;;;;   staff_assign      one specialist answers one brief, on the model this
;;;;                     session is on -- a side call, not an agent turn
;;;;   staff_status      the roster and the recent jobs, or one job whole
;;;;   staff_delegate    the brief handed to a coworker, over the user's own
;;;;                     webhook and/or a shared folder
;;;;   staff_collect     the answer back: from results/<id>.* or pasted
;;;;   staff_handoffs    the handoffs and their state
;;;;   /staff            the same five, typed
;;;;
;;;; Two things are renamed rather than carried over. The tools are
;;;; staff_* where the original had grokbot_delegate, grokbot_collect and
;;;; grokbot_status, because the bridge is any webhook or shared folder and not
;;;; one brand's bot; and the original's two commands (/staff and /grokbot) are
;;;; one here, /staff, since a cell's command is its own name.

(in-package #:nodecode-staff)

;;; --- the manual -------------------------------------------------------------

(defparameter *manual*
  "Staff is a small team the model confers with: a roster of specialists, each
with its own instructions, and a board of the jobs it gave them.
  staff_assign {specialist, task, title, context}
      one specialist answers one brief, in a single side call on this session's
      own model: no session is started, no tool runs inside it, and the answer
      is handed back whole. specialist='chief' restates the goal as 3-7 work
      items and names the specialist for each; then assign those one by one.
      The job is kept on the board, so staff_status can read the answer again.
  staff_status {job_id, limit}
      the roster, the recent jobs and how a handoff is configured; with a job
      id, that job whole: its task, its answer, or why it failed.
  staff_delegate {task, specialist, title, reply_hint}
      the brief handed to a coworker outside this process, over the webhook the
      staff section names (POSTed as JSON) and/or a shared folder it names
      (written as handoffs/<id>.json). It answers an id; collect the answer
      later, never twice.
  staff_collect {job_id, result}
      the answer to a handoff: read from results/<job_id>.json|md|txt in the
      shared folder, or taken as `result' when the coworker had nowhere to
      write. The job is marked done.
  staff_handoffs {status, limit}
      the handoffs on the board, pending, done or failed, with the paths that
      are configured.
A person reaches the same five with /staff: /staff lists the team, /staff status
[job_id] reads the board, /staff <specialist> <task> gives one brief, /staff
delegate <task> hands work over, /staff handoffs lists the handoffs, and /staff
collect <job_id> [result] takes one answer back. The roster is the built-in team
until the staff section's `specialists' names another."
  "What (help :staff) answers while the cell runs.")

(defparameter +usage-line+
  "Usage: /staff [list] · /staff status [job_id] · /staff <specialist> <task> · /staff delegate <task> · /staff handoffs · /staff collect <job_id> [result]"
  "What /staff says when it is given something it does not read as a verb.")

;;; --- what the cell runs on ---------------------------------------------------

(defun max-output-tokens ()
  "One specialist answer's completion ceiling, from the section."
  (or (getf (running-settings) :max-output-tokens) 1200))

(defun webhook-timeout ()
  "One webhook exchange's deadline, from the section."
  (or (getf (running-settings) :webhook-timeout-seconds) 15))

(defun webhook-url ()
  "The webhook the section names, or the one the environment names."
  (let ((said (trim (getf (running-settings) :webhook-url))))
    (if (plusp (length said)) said
        (trim (uiop:getenv "GROKBOT_STAFF_WEBHOOK_URL")))))

(defun inbox-dir ()
  "The shared folder the section names, or empty."
  (trim (getf (running-settings) :inbox-dir)))

(defun roster ()
  "The team this cell runs on."
  (specialists (getf (running-settings) :specialists-raw)))

(defun session-id ()
  "The session this call runs in, when the image is inside one."
  (and (boundp 'nlk:*scribe-session-id*) nlk:*scribe-session-id*))

;;; --- one specialist's answer ------------------------------------------------

(defun ask-specialist (specialist team task context)
  "SPECIALIST's answer to TASK with CONTEXT, one side call on this session's
model. The user message is the original's: the task, and the context under a
heading when there is any."
  (nle:complete (specialist-system-prompt specialist team)
                (if (plusp (length context))
                    (format nil "~a~%~%Context:~%~a" task context)
                    task)
                :max-tokens (max-output-tokens)
                :session-id (session-id)))

;;; --- the board, as text a person reads ---------------------------------------

(defun roster-text (team)
  "TEAM as the lines /staff prints: one specialist each."
  (format nil "Staff (~d):~%~{~a~^~%~}"
          (length team)
          (mapcar (lambda (specialist)
                    (format nil "  ~a - ~a~@[ (~a)~]"
                            (first specialist) (second specialist)
                            (let ((focus (third specialist)))
                              (and (plusp (length focus)) focus))))
                  team)))

(defun job-line (job)
  (format nil "  ~a · ~a · ~a · ~a · ~a"
          (getf job :id) (getf job :specialist) (getf job :status)
          (let ((title (trim (getf job :title)))) (if (plusp (length title)) title "(untitled)"))
          (getf job :created-at)))

(defun jobs-text (jobs)
  (if (null jobs)
      "No jobs on the board yet."
      (format nil "Jobs (newest first):~%~{~a~^~%~}" (mapcar #'job-line jobs))))

(defun bridge-line ()
  "How a handoff is configured, as the status answer says it -- never the
webhook's path or query, which carry its token."
  (let ((url (webhook-url)) (inbox (inbox-dir)))
    (cond
      ((and (zerop (length url)) (zerop (length inbox)))
       "Handoff: not configured (set webhook_url or inbox_dir).")
      (t (format nil "Handoff: ~{~a~^ · ~}"
                 (append (unless (zerop (length url))
                           (list (format nil "webhook ~a" (redact-url url))))
                         (unless (zerop (length inbox))
                           (list (format nil "inbox ~a" inbox)))))))))

(defun job-text (job id)
  "JOB whole, or the line saying the board holds no ID."
  (if (null job)
      (format nil "staff_status: the board holds no job ~s." id)
      (with-output-to-string (out)
        (format out "~a · ~a · ~a~%" (getf job :id)
                (let ((who (trim (getf job :specialist))))
                  (if (plusp (length who)) who "chief"))
                (getf job :status))
        (format out "~a~%" (getf job :title))
        (format out "task: ~a~%" (getf job :task))
        (loop for (label key) in '(("Answer" :output) ("Result" :result)
                                   ("Error" :error) ("Delivery" :delivery))
              for text = (trim (getf job key))
              when (plusp (length text)) do (format out "~a:~%~a~%" label text))
        (format out "created ~a, updated ~a" (getf job :created-at) (getf job :updated-at)))))

;;; --- the handoff -------------------------------------------------------------

(defun handoff-payload (job request)
  "JOB as the JSON string a handoff sends or writes."
  (let ((hint (trim (delegate-request-reply-hint request))))
    (nlk:encode-json-object
     (nlk:json-object "id" (getf job :id)
                       "created_at" (getf job :created-at)
                       "specialist" (getf job :specialist)
                       "title" (getf job :title)
                       "task" (getf job :task)
                       "reply_hint" (if (plusp (length hint)) hint
                                        (format nil "Reply with the result. If you can write files, ~
                                                     save it as results/~a.md in the shared inbox folder; ~
                                                     otherwise the user will paste it back."
                                                (getf job :id)))))))

;;; --- the answers -------------------------------------------------------------

(defun answer-assign (request)
  "staff_assign's answer: what the specialist said, or why it was not asked."
  (let* ((team (roster))
         (named (assign-request-specialist request))
         (specialist (find-specialist team named))
         (task (trim (assign-request-task request)))
         (context (trim (assign-request-context request))))
    (cond
      ((null specialist)
       (format nil "staff_assign: no specialist called ~s. The roster is ~a."
               named (roster-line team)))
      (t
       (let* ((title (let ((said (trim (assign-request-title request))))
                       (if (plusp (length said)) said (clamp task 80))))
              (job (new-job :kind "local" :status "running"
                            :specialist (first specialist) :title title :task task)))
         (add-job job)
         (handler-case
             (let ((answer (ask-specialist specialist team task context)))
               (update-job (getf job :id) :status "done" :output answer)
               (format nil "~a · ~a · job ~a~%~%~a"
                       (first specialist) (getf job :status) (getf job :id) answer))
           (error (condition)
             (update-job (getf job :id) :status "failed"
                         :error (let ((said (format nil "~a" condition)))
                                  (clamp said 500)))
             (format nil "staff_assign: ~a could not answer (~a); job ~a is on the board, failed."
                     (first specialist) (type-of condition) (getf job :id)))))))))

(defun answer-board (request)
  "staff_status's answer: one job whole, or the roster with the board."
  (let ((id (trim (board-request-job-id request))))
    (if (plusp (length id))
        (job-text (find-job id) id)
        (format nil "~a~%~%~a~%~a"
                (roster-text (roster))
                (jobs-text (list-jobs :kind "local"
                                       :limit (board-request-limit request)))
                (bridge-line)))))

(defun answer-handoffs (request)
  "staff_handoffs' answer: the handoffs, and the paths configured for them."
  (let* ((status (trim (handoffs-request-status request)))
         (wanted (if (member status '("pending" "done" "failed") :test #'string=)
                     status nil))
         (jobs (list-jobs :kind "handoff" :status wanted
                          :limit (handoffs-request-limit request))))
    (format nil "~a~%~a~%Collect an answer with staff_collect job_id=<id>, or staff_collect result=<pasted text>."
            (bridge-line)
            (if (null jobs) "No handoffs on the board yet."
                (format nil "Handoffs (~a):~%~{~a~^~%~}"
                        (or wanted "all") (mapcar #'job-line jobs))))))

(defun answer-delegate (request)
  "staff_delegate's answer: the id it handed out, or why nothing was sent."
  (let* ((task (trim (delegate-request-task request)))
         (named (trim (delegate-request-specialist request)))
         (team (roster))
         (specialist (and (plusp (length named)) (find-specialist team named)))
         (url (webhook-url))
         (inbox (inbox-dir)))
    (cond
      ((and (zerop (length url)) (zerop (length inbox)))
       (format nil "staff_delegate: ~a" +setup-hint+))
      ((and (plusp (length named)) (null specialist))
       (format nil "staff_delegate: no specialist called ~s. The roster is ~a."
               named (roster-line team)))
      (t
       (let* ((title (let ((said (trim (delegate-request-title request))))
                       (if (plusp (length said)) said (clamp task 80))))
              (job (new-job :kind "handoff" :status "pending"
                            :specialist (if specialist (first specialist) "chief")
                            :title title :task task))
              (payload (handoff-payload job request))
              (deliveries '())
              (sent nil))
         (when (plusp (length url))
           (multiple-value-bind (ok detail) (post-webhook url payload (webhook-timeout))
             (push (format nil "webhook ~a: ~a" (if ok "accepted" "refused") detail) deliveries)
             (setf sent (or sent ok))))
         (when (plusp (length inbox))
           (multiple-value-bind (ok detail) (write-inbox-handoff inbox payload (getf job :id))
             (push (if ok (format nil "inbox written: ~a" detail)
                       (format nil "inbox: ~a" detail))
                   deliveries)
             (setf sent (or sent ok))))
         (let ((lines (nreverse deliveries)))
           (setf job (plist-set job :status (if sent "pending" "failed")))
           (setf job (plist-set job :delivery (format nil "~{~a~^; ~}" lines)))
           (add-job job)
           (format nil "~a · job ~a~%~{~a~^~%~}~%~a"
                   (if sent "handed off" "nothing was delivered")
                   (getf job :id)
                   (mapcar (lambda (line) (format nil "  ~a" line)) lines)
                   (if sent
                     (format nil "Collect the answer with staff_collect job_id=~a when the coworker has replied."
                             (getf job :id))
                     +setup-hint+))))))))

(defun answer-collect (request)
  "staff_collect's answer: the answer that came back, or that none has."
  (let ((id (trim (collect-request-job-id request))))
    (cond
      ((zerop (length id)) "staff_collect: a job id is needed, as staff_delegate answered it.")
      (t
       (let ((job (find-job id)))
         (cond
           ((or (null job) (not (string= "handoff" (getf job :kind))))
            (format nil "staff_collect: no handoff job ~s is on the board." id))
           (t
            (let* ((pasted (trim (collect-request-result request)))
                   (from-paste (plusp (length pasted)))
                   (answer (if from-paste pasted (read-inbox-result (inbox-dir) id))))
              (cond
                ((null answer)
                 (format nil "staff_collect: ~a is still ~a: nothing in results/~a.*. ~
                              Save the answer as results/~a.md in the shared folder, or call ~
                              staff_collect again with result=<the text>."
                         id (getf job :status) id id))
                (t
                 (update-job id :status "done" :result (clamp answer +max-result+))
                 (format nil "~a · done (from ~a)~%~%~a" id
                         (if from-paste "the pasted answer" "the shared folder")
                         answer)))))))))))

;;; --- the boundaries ------------------------------------------------------------

(nlk:define-contract assign-request
  (:doc "Give one specialist on the staff team a brief and take its answer. The
specialist answers in one side call on this session's own model, under its own
instructions: no session is started and no tool runs. Use specialist='chief'
first for a goal that needs splitting, then give each work item to the
specialist it names. Answers are saved on the board for staff_status.")
  ("specialist" :text :required t
   :doc "the specialist's id, as staff_status lists the roster")
  ("task" :text :required t
   :doc "what the specialist should do, in full: it sees this and the context, nothing else")
  ("title" :text :default ""
   :doc "one short line for the board; the task's first words when empty")
  ("context" :text :default ""
   :doc "material the specialist should have: quotes, numbers, links, what was already tried"))

(nlk:define-contract board-request
  (:doc "Read the staff board: the roster, the recent jobs and how a handoff is
configured -- or one job whole, with the specialist's answer, when a job id is
given.")
  ("job_id" :text :default ""
   :doc "one job id, to read that job in full")
  ("limit" :integer :default 10
   :doc "how many of the newest jobs the board shows (1-200)"))

(nlk:define-contract delegate-request
  (:doc "Hand a long-running brief to a coworker outside this process, over the
webhook the staff section names and/or a shared folder it names. It answers at
once with a job id; the answer comes back later through staff_collect. With
neither a webhook nor a folder configured it says what to set, and sends
nothing.")
  ("task" :text :required t
   :doc "the whole brief for the coworker: it does not see this conversation")
  ("specialist" :text :default ""
   :doc "optional: the specialist id whose work this is, which frames the handoff")
  ("title" :text :default ""
   :doc "one short line for the board")
  ("reply_hint" :text :default ""
   :doc "how the coworker should answer; the shared-folder convention when empty"))

(nlk:define-contract collect-request
  (:doc "Take back the answer to a handoff: read from results/<job_id>.json, .md
or .txt in the shared folder, or from a result pasted here. The job is marked
done.")
  ("job_id" :text :required t
   :doc "the id staff_delegate answered with")
  ("result" :text :default ""
   :doc "the answer, pasted, when the coworker had nowhere to write it"))

(nlk:define-contract handoffs-request
  (:doc "List the handoffs on the board, and the paths configured for sending
one.")
  ("status" :text :default "all" :options '("pending" "done" "failed" "all")
   :doc "which handoffs to list")
  ("limit" :integer :default 20
   :doc "how many to list (1-200)"))

;;; --- the command ---------------------------------------------------------------

(defun split-words (text)
  "TEXT as (values FIRST REST): its first word, and everything after it."
  (let* ((text (trim text))
         (space (position #\Space text)))
    (if space
        (values (subseq text 0 space) (trim (subseq text (1+ space))))
        (values text ""))))

(defun decode (name &rest pairs)
  "PAIRS as the contract NAME reads them."
  (nlk:contract-decode (nlk:contract name) (apply #'nlk:make-json-object pairs)))

(defun run-command (args session-id)
  "ARGS as /staff reads them; answers the text a shell shows."
  (declare (ignore session-id))
  (multiple-value-bind (word rest) (split-words args)
    (let ((word (string-downcase word)))
      (cond
        ((member word '("" "list" "team") :test #'string=) (roster-text (roster)))
        ((string= word "status") (answer-board (decode 'board-request "job_id" rest)))
        ((string= word "handoffs")
         (answer-handoffs (decode 'handoffs-request
                                  "status" (if (plusp (length rest)) (string-downcase rest) "all"))))
        ((string= word "collect")
         (multiple-value-bind (id result) (split-words rest)
           (answer-collect (decode 'collect-request "job_id" id "result" result))))
        ((string= word "delegate")
         (answer-delegate (decode 'delegate-request "task" rest)))
        ((member word '("help" "?") :test #'string=) +usage-line+)
        (t
         (let ((specialist (find-specialist (roster) word)))
           (if (null specialist)
               (format nil "No specialist called ~a.~%~%~a" word +usage-line+)
               (let ((answer (answer-assign (decode 'assign-request
                                                    "specialist" (first specialist)
                                                    "task" rest))))
                 (if (plusp (length rest)) answer
                     (format nil "~a~%~%~a" answer +usage-line+))))))))))

;;; --- what the section declares, and what it cannot -----------------------------

(defun read-settings (values table)
  "The settings the cell runs on: the values the `staff' declaration derived,
plus the roster -- a JSON list of objects a flat section cannot carry -- and the
webhook URL an environment variable may stand in for."
  (let ((env (trim (uiop:getenv "GROKBOT_STAFF_WEBHOOK_URL"))))
    (list :specialists-raw (if (hash-table-p table)
                               (nlk:json-value table :any "specialists")
                               nil)
          :webhook-url (or (getf values :webhook-url) (and (plusp (length env)) env))
          :inbox-dir (or (getf values :inbox-dir) "")
          :state-dir (or (getf values :state-dir) "")
          :webhook-timeout-seconds (or (getf values :webhook-timeout-seconds) 15)
          :max-output-tokens (or (getf values :max-output-tokens) 1200))))

;;; --- what the model calls --------------------------------------------------------

(defun tool-answer (name function request)
  "FUNCTION's answer to REQUEST, or the refusal a model can read: a tool never
throws a condition at the turn that called it."
  (handler-case (funcall function request)
    (staff-error (condition)
      (format nil "~a: ~a" name (staff-error-detail condition)))
    (error (condition)
      (format nil "~a failed: ~a" name condition))))

(defun tool-assign (request) (tool-answer "staff_assign" #'answer-assign request))
(defun tool-board (request) (tool-answer "staff_status" #'answer-board request))
(defun tool-delegate (request) (tool-answer "staff_delegate" #'answer-delegate request))
(defun tool-collect (request) (tool-answer "staff_collect" #'answer-collect request))
(defun tool-handoffs (request) (tool-answer "staff_handoffs" #'answer-handoffs request))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell staff
  (:section ("staff")
    (:guide "nothing to fetch: the roster answers on your own model; webhook_url or inbox_dir is the optional handoff, and state_dir is where the board is kept; next step: /staff lists the team")
    ("webhook_url" :secret
     :doc "your own webhook routine, POSTed a job as JSON; https only (http for localhost); the environment variable GROKBOT_STAFF_WEBHOOK_URL answers where this is unset")
    ("inbox_dir" :path
     :doc "a shared folder a coworker reads: handoffs/<id>.json is written, results/<id>.json|md|txt read back")
    ("state_dir" :path
     :doc "the folder the job board is kept in; the staff folder under the Nodecode home when unset")
    ("webhook_timeout_seconds" :integer :default 15 :min 1
     :doc "one webhook exchange's deadline, in seconds")
    ("max_output_tokens" :integer :default 1200 :min 64
     :doc "one specialist answer's completion ceiling"))
  ;; `specialists' is a JSON list of objects -- id, title, focus, prompt -- which
  ;; a flat declaration cannot carry, so the section's own object is read beside
  ;; it. The environment fallback for the webhook URL is read there too.
  (:settings 'read-settings)
  (:help :staff
         "staff: staff_assign, staff_status, staff_delegate, staff_collect and /staff - a roster of specialists, a job board, and a handoff"
         *manual*)
  (:command "staff" 'run-command
            :description "The staff team: list, status, assign, delegate, collect"
            :argument-hint "[list|status [job_id]|<specialist> <task>|delegate <task>|handoffs|collect <job_id> [result]]")
  (:tool "staff_assign" (nlk:contract 'assign-request) 'tool-assign)
  (:tool "staff_status" (nlk:contract 'board-request) 'tool-board)
  (:tool "staff_delegate" (nlk:contract 'delegate-request) 'tool-delegate)
  (:tool "staff_collect" (nlk:contract 'collect-request) 'tool-collect)
  (:tool "staff_handoffs" (nlk:contract 'handoffs-request) 'tool-handoffs)
  (:start (lambda ()
            ;; The board is one file in one folder: a folder that cannot be made
            ;; is the start refusing, not a job lost later.
            (ensure-directories-exist (jobs-file)))))
