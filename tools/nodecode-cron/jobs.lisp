;;;; jobs.lisp --- the job record, the registry document, and its row in the store.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A job IS a durable session: `cron-<id>', made when the job is added, the
;;;; place every fire lands. Its transcript is the run ledger -- (cron:runs)
;;;; is a query over the session's terminal turn facts, and a follow-up is
;;;; the operator resuming that session -- so there is no executions table,
;;;; no output directory, no notepad: what hermes keeps beside the job, the
;;;; organism keeps as the job's own history, with RECALL-LOG reaching it
;;;; from any session.
;;;;
;;;; What a session cannot hold is the schedule and its cursor, and those are
;;;; ONE registry document: every job, as JSON, in one session-state row on
;;;; a session id no session.created fact names (the KV keys on (session,
;;;; key) and requires no session, so the row never shows in the picker).
;;;; One owner per store, so the in-image list is the working copy and the
;;;; row is written whole after every mutation, under *LOCK*. Times are
;;;; stored as unix seconds and handled as universal time.

(in-package #:nodecode-cron)

(nlk:define-record (job (:copier nil))
  "One scheduled prompt."
  ;; STATE is :ACTIVE, :PAUSED or :DONE (a one-shot that fired, or an ISO time
  ;; that passed). NEXT-AT is the due instant the ticker sleeps toward, NIL
  ;; when there is none. LAST is the newest outcome as a plist (:at :status
  ;; :line :turn-id). TIMEOUT is seconds, NIL for the section's default.
  ;; ORIGIN is the session that added the job. FAILURES is the run of failed
  ;; fires since the last one that answered: at the section's
  ;; pause_after_failures the job pauses itself (SETTLE-FIRE).
  (id "" :type string)
  (name "" :type string)
  (schedule nil :type (or null schedule))
  (prompt "" :type string)
  (cwd nil :type (or null string))
  (session "" :type string)
  (state :active :type keyword)
  (next-at nil :type (or null integer))
  (fires 0 :type integer)
  (last nil :type list)
  (failures 0 :type integer)
  (created-at 0 :type integer)
  (origin nil :type (or null string))
  (timeout nil :type (or null integer))
  ;; A settled answer waiting to be sent to ORIGIN as its next request:
  ;; (:input :answer :at). Kept on the row until the ingress admits it,
  ;; so a restart in between loses nothing.
  (report nil :type list)
  ;; NIL for an ordinary job; the wake job's own clock and gates as a plist
  ;; (wake.lisp): :MIN :MAX :GAP seconds, :ALLOWANCE dollars a day or NIL,
  ;; :CALLS, :CURSOR, :FIRED-AT, :ASKED, :WHY, :CAUSE, :WAKES, :SAID,
  ;; :SAID-AT, :OFF-BY, :OFF-AT.
  (wake nil :type list))

;;; No session.created fact names it, so it never appears in the picker.
(defparameter +registry-session+ "cron"
  "The session id the registry row lives on.")

(defparameter +registry-key+ "jobs")

(defvar *jobs* '()
  "Every job, newest first. Guarded by *LOCK*.")

;;; --- ids --------------------------------------------------------------------

(defun slug (text)
  "TEXT as an id: lowercase, runs of anything but letters and digits as one
dash, trimmed, at most 32 characters; \"job\" when nothing survives."
  (let* ((trimmed (string-trim "-" (ppcre:regex-replace-all "[^a-z0-9]+" (string-downcase text) "-")))
         (cut (string-trim "-" (subseq trimmed 0 (min (length trimmed) 32)))))
    (if (plusp (length cut)) cut "job")))

(defun find-job (id)
  (find id *jobs* :key #'job-id :test #'string=))

(defun require-job (id)
  "The job named ID, or a refusal listing the ids there are."
  (unless (stringp id)
    (fail "a job is named by its id string; ~a" (ids-text)))
  (or (find-job id)
      (fail "no job ~s; ~a" id (ids-text))))

(defun ids-text ()
  (if *jobs*
      (format nil "the jobs are ~{~a~^, ~}" (mapcar #'job-id *jobs*))
      "there are no jobs yet"))

;;; --- JSON -------------------------------------------------------------------

(defun job-json (job)
  (nlk:json-object
   "id" job.id
   "name" job.name
   "schedule" (let* ((schedule job.schedule) (kind schedule.kind) (at schedule.at))
                (nlk:json-object "kind" (string-downcase (symbol-name kind))
                                 :when (eq kind :interval) "seconds" schedule.seconds
                                 :when (eq kind :cron) "expr" schedule.expr
                                 :when (eq kind :once) "at" (unix-from-universal at)
                                 "display" schedule.display))
   "prompt" job.prompt
   :opt "cwd" job.cwd
   "session" job.session
   "state" (string-downcase (symbol-name job.state))
   :opt "next_at" (and job.next-at (unix-from-universal job.next-at))
   "fires" job.fires
   :opt "last" (let ((last job.last))
                 (and last
                      (nlk:json-object "at" (unix-from-universal (getf last :at))
                                       "status" (getf last :status)
                                       :opt "line" (getf last :line)
                                       :opt "turn_id" (getf last :turn-id))))
   :when (plusp job.failures) "failures" job.failures
   "created_at" (unix-from-universal job.created-at)
   :opt "origin" job.origin
   :opt "timeout" job.timeout
   :opt "report" (let ((report job.report))
                   (and report
                        (nlk:json-object "input" (getf report :input)
                                         "answer" (getf report :answer)
                                         "at" (unix-from-universal (getf report :at)))))
   :opt "wake" (wake-json job.wake)))

;;; The wake plist as JSON and back: instants as unix seconds, the cause as
;;; a word, every absent key absent.
(defparameter +wake-integer-keys+ '((:min "min") (:max "max") (:gap "gap") (:calls "calls")
                                    (:asked "asked") (:wakes "wakes")))
(defparameter +wake-instant-keys+ '((:cursor "cursor") (:fired-at "fired_at") (:said-at "said_at")
                                    (:off-at "off_at")))
(defparameter +wake-string-keys+ '((:why "why") (:said "said") (:off-by "off_by")))

(defun wake-json (wake)
  (when wake
    (let ((table (nlk:json-object)))
      (loop for (key name) in +wake-integer-keys+
            for value = (getf wake key)
            when value do (setf (gethash name table) value))
      (loop for (key name) in +wake-instant-keys+
            for value = (getf wake key)
            when value do (setf (gethash name table) (unix-from-universal value)))
      (loop for (key name) in +wake-string-keys+
            for value = (getf wake key)
            when value do (setf (gethash name table) value))
      (nlk:when-let (allowance (getf wake :allowance))
        (setf (gethash "allowance" table) allowance))
      (nlk:when-let (cause (getf wake :cause))
        (setf (gethash "cause" table) (string-downcase (symbol-name cause))))
      table)))

(defun wake-from-json (object)
  (when object
    (append
     (loop for (key name) in +wake-integer-keys+
           for value = (nlk:json-value object :integer name)
           when value append (list key value))
     (loop for (key name) in +wake-instant-keys+
           for value = (nlk:json-value object :integer name)
           when value append (list key (universal-from-unix value)))
     (loop for (key name) in +wake-string-keys+
           for value = (nlk:json-value object :string name)
           when value append (list key value))
     (nlk:when-let (allowance (nlk:json-value object :number "allowance"))
       (list :allowance allowance))
     (nlk:when-let (cause (nlk:json-value object :string "cause"))
       (list :cause (cond ((equal cause "asked") :asked)
                          ((equal cause "now") :now)
                          (t :timer)))))))

(defun job-from-json (object &aux (state (nlk:json-value object :string "state")))
  (make-job
   :id (nlk:json-value object :string "id")
   :name (or (nlk:json-value object :string "name") "")
   :schedule (let* ((schedule (nlk:json-value object :object "schedule"))
                    (kind (nlk:json-value schedule :string "kind"))
                    (display (or (nlk:json-value schedule :string "display") "")))
               (cond ((equal kind "interval")
                      (make-schedule :kind :interval
                                     :seconds (nlk:json-value schedule :integer "seconds")
                                     :display display))
                     ((equal kind "cron")
                      (cron-schedule (nlk:json-value schedule :string "expr") display))
                     ((equal kind "once")
                      (once-schedule (universal-from-unix (nlk:json-value schedule :integer "at"))
                                     display))
                     (t (error "cron registry: unknown schedule kind ~s" kind))))
   :prompt (or (nlk:json-value object :string "prompt") "")
   :cwd (nlk:json-value object :string "cwd")
   :session (nlk:json-value object :string "session")
   :state (cond ((equal state "paused") :paused)
                ((equal state "done") :done)
                (t :active))
   :next-at (nlk:when-let (at (nlk:json-value object :integer "next_at")) (universal-from-unix at))
   :fires (or (nlk:json-value object :integer "fires") 0)
   :last (let ((last (nlk:json-value object :object "last")))
           (and last
                (list :at (universal-from-unix (nlk:json-value last :integer "at"))
                      :status (nlk:json-value last :string "status")
                      :line (nlk:json-value last :string "line")
                      :turn-id (nlk:json-value last :string "turn_id"))))
   :failures (or (nlk:json-value object :integer "failures") 0)
   :created-at (universal-from-unix (or (nlk:json-value object :integer "created_at") 0))
   :origin (nlk:json-value object :string "origin")
   :timeout (nlk:json-value object :integer "timeout")
   :report (let* ((report (nlk:json-value object :object "report"))
                  (input (nlk:json-value report :string "input"))
                  (answer (nlk:json-value report :string "answer"))
                  (at (nlk:json-value report :integer "at")))
             (and input answer at
                  (list :input input :answer answer :at (universal-from-unix at))))
   :wake (wake-from-json (nlk:json-value object :object "wake"))))

;;; --- the store row -------------------------------------------------------------

(defun load-jobs ()
  "Replace *JOBS* with the registry row's jobs; none when there is no row or
no store (a test image)."
  (with-cron-lock
    (setf *jobs*
          (let ((text (and (nlk:store-open-p)
                           (nlk:session-state-get +registry-session+ +registry-key+)))
                (jobs '()))
            ;; Newest first; an unreadable job is dropped with a warning.
            (when text
              (loop for object across (nlk:json-array (nlk:decode-json text) "jobs")
                    do (handler-case (push (job-from-json object) jobs)
                         (error (condition)
                           (warn "cron registry: dropping an unreadable job: ~a" condition)))))
            jobs))
    (length *jobs*)))

(defun persist-jobs ()
  "Write *JOBS* whole to the registry row."
  ;; No store, no write: the list still serves the image it is in.
  (with-cron-lock
    (when (nlk:store-open-p)
      ;; The registry document: every job, oldest first, as one JSON text.
      (nlk:session-state-put
       +registry-session+ +registry-key+
       (nlk:encode-json-object
        (nlk:json-object "jobs" (coerce (mapcar #'job-json (reverse *jobs*)) 'vector)))))
    t))

;;; --- text -------------------------------------------------------------------------

(defun last-text (job &aux (last job.last))
  "The newest outcome as one phrase: `ok 09-07 09:00 \"Top story: ...\"'."
  (if (null last)
      "never fired"
      (format nil "~a ~a~@[ ~s~]"
              (getf last :status)
              (subseq (local-text (getf last :at)) 5)
              (getf last :line))))

(defun next-text (job)
  "The next fire as local wall time and the time left before it: what every
job listing, record and resume answer carries."
  (if job.next-at (at-text job.next-at) "-"))

(defun jobs-text ()
  "Every job, one line each, oldest first; what (cron:jobs) answers."
  (with-cron-lock
    (if (null *jobs*)
        "no jobs; (cron:add \"every day at 9am\" \"...\") schedules one"
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (job)
                          (format nil "~a  ~(~a~)  ~a  next ~a  last ~a"
                                  job.id job.state (schedule-display job.schedule)
                                  (next-text job) (last-text job)))
                        (reverse *jobs*))))))

(defun job-text (job)
  "The whole record; what (cron:show id) answers."
  (format nil "job ~a~%  name: ~a~%  state: ~(~a~)~%  schedule: ~a~%  next: ~a~%  ~
               fires: ~d~%  last: ~a~@[, ~d failed in a row~]~%  session: ~a~%  cwd: ~a~%  ~
               timeout: ~a~%  origin: ~a~%  report: ~a~%  prompt: ~a"
          job.id job.name job.state
          (schedule-display job.schedule) (next-text job) job.fires
          (last-text job) (and (> job.failures 1) job.failures) job.session (or job.cwd "-")
          (if job.timeout (duration-text job.timeout) "default")
          (or job.origin "-")
          (if job.report
              (format nil "waiting to be sent to ~a (since ~a)"
                      job.origin (local-text (getf job.report :at)))
              "none pending")
          job.prompt))
