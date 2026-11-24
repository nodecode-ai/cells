;;;; cell.lisp --- what every turn did, and which of them finished green but
;;;; went wrong.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a flight recorder for an agent: every tool call, model
;;;; round and approval is recorded, each finished run is judged against the
;;;; rules in rules.lisp and, for a scheduled job, against what that job
;;;; usually costs and does, and a flagged run raises a paragraph naming what
;;;; to check. Nothing leaves the machine and nothing is dialled.
;;;;
;;;; THE SEAMS. Hermes pushes one callback per event; Nodecode publishes one
;;;; durable fact per thing that happened, on the :FRAME point, and each fact
;;;; already carries what the original's hooks were handed: a tool call and
;;;; its result (arguments, result, duration, failed), a model round
;;;; (turn.usage, with the token counts), the assistant's answer
;;;; (turn.assistant_message_completed) and the turn's end (turn.completed,
;;;; turn.failed, turn.cancelled). So one hook replaces the original's ten,
;;;; and a run is a turn.
;;;;
;;;; A JOB is a scheduled turn: the cron cell runs every job in its own
;;;; session, cron-<id>, so a run whose session starts with cron- is a job run
;;;; and gets the three baseline rules as well. Every other turn is a chat and
;;;; gets the rules that need no history.
;;;;
;;;; WHAT IS A STEP. Nodecode's model calls one tool, eval, and the verbs
;;;; inside it (sh, edit, write-file) are where the work happens, so a call's
;;;; form is what the edit and check rules read -- the argument a person would
;;;; name anyway. A cell's own tools are read the same way, by name.
;;;;
;;;; Config, a top-level key:
;;;;   "flightlog": {"notice": true, "keep_runs": 200}
;;;; Nothing is required: recording starts when the cell starts.

(in-package #:nodecode-flightlog)

;;; --- what is recording right now ---------------------------------------------

(defvar *open-runs* (make-hash-table :test #'equal)
  "The runs in flight, by turn id.")

(defvar *calls* (make-hash-table :test #'equal)
  "Each tool call's arguments by turn and call id, until its result lands.")

(defparameter +answer-chars+ 4000
  "How much of a turn's answer the record keeps: enough to judge the claim,
short enough that a store stays a record and not a transcript.")

(defparameter +target-chars+ 100
  "How much of a call's one interesting argument a step keeps.")

(defparameter +detail-chars+ 120
  "How much of a failure's own words a step keeps.")

(defun new-id (prefix)
  "A short id nothing else in the store shares."
  (let ((text (nlk:make-durable-id prefix)))
    (subseq text (max 0 (- (length text) 8)))))

(defun flatten (text)
  "TEXT on one line, whitespace runs collapsed, trimmed."
  (nlk:one-line (or text "")))

(defun clip (text limit)
  (let ((text (flatten text)))
    (if (> (length text) limit) (subseq text 0 limit) text)))

;;; --- one call's arguments ----------------------------------------------------

(defparameter +target-keys+
  '("command" "cmd" "form" "path" "file" "file_path" "query" "url" "pattern" "name" "code")
  "The one argument a person cares about, in the order the original tried them.")

(defun argument-text (arguments)
  "ARGUMENTS as the text a step records: the call's JSON as it was written."
  (typecase arguments
    (null "")
    (string arguments)
    (hash-table (ignore-errors (nlk:encode-json-object arguments)))
    (t (princ-to-string arguments))))

(defun target-of (arguments)
  "The one argument a person cares about -- the command, the form, the path --
on one line, or the whole call's text when it names none of them."
  (let* ((text (argument-text arguments))
         (object (ignore-errors (nlk:decode-json text))))
    (clip (if (hash-table-p object)
              (or (loop for key in +target-keys+
                        for value = (ignore-errors (nlk:json-value object :string key))
                        when (and value (plusp (length value))) return value)
                  (loop for value being the hash-values of object
                        when (and (stringp value) (plusp (length value))) return value)
                  text)
              text)
          +target-chars+)))

(defun argkey-of (arguments)
  "A call's near-identical-arguments key: case and whitespace do not count."
  (let ((text (string-downcase (flatten (argument-text arguments)))))
    (subseq (remove #\Space text) 0 (min 200 (length (remove #\Space text))))))

(defun detail-of (result)
  "A failure's own first line, in a few words."
  (let ((text (typecase result
                (null "")
                (string result)
                (t (or (ignore-errors (nlk:encode-json-object result)) (princ-to-string result))))))
    (clip text +detail-chars+)))

;;; --- recording ---------------------------------------------------------------

(defun open-run-for (turn session)
  "TURN's run, opened on first sight of the turn."
  (or (gethash turn *open-runs*)
      (setf (gethash turn *open-runs*)
            (let* ((session (or session ""))
                   (job (and (plusp (length session))
                             (uiop:string-prefix-p "cron-" session)
                             session)))
              (make-run :id (new-id "r") :session session :job job)))))

(defun note-event (run event)
  "EVENT appended to RUN's record, its sequence number given here."
  (let ((event (list* :seq (incf (run-seq run)) event)))
    (setf (run-events run) (append (run-events run) (list event)))
    event))

(defun note-call (turn payload)
  "Remember what a tool was called on, until its result names the call again."
  (let ((call-id (nlk:json-value payload :string "call-id")))
    (when (and call-id (plusp (length call-id)))
      (setf (gethash (format nil "~a/~a" turn call-id) *calls*)
            (argument-text (nlk:json-value payload :string "arguments"))))))

(defun call-arguments (turn payload)
  (let ((call-id (nlk:json-value payload :string "call-id")))
    (prog1 (and call-id (gethash (format nil "~a/~a" turn call-id) *calls*))
      (when call-id (remhash (format nil "~a/~a" turn call-id) *calls*)))))

(defun note-result (turn run payload)
  "One tool result as a step: what ran, on what, how long, and how it went."
  (let* ((arguments (call-arguments turn payload))
         (failed (nlk:json-value payload :boolean "failed"))
         (result (nlk:json-value payload :string "result")))
    (incf (run-tool-calls run))
    (when failed (incf (run-tool-errors run)))
    (note-event run (list :kind "tool"
                          :name (or (nlk:json-value payload :string "tool-name") "")
                          :status (if failed "error" "ok")
                          :argkey (argkey-of arguments)
                          :target (target-of arguments)
                          :duration-ms (nlk:json-value payload :integer "duration-ms")
                          :detail (if failed (detail-of result) "")
                          :ts (nlk:unix-now)))))

(defun note-usage (run payload)
  "One model round: the tokens it spent, and the model it ran on."
  (let ((tokens (or (nlk:json-value payload :integer "total-tokens") 0))
        (model (or (nlk:json-value payload :string "model") "")))
    (when (and (stringp model) (plusp (length model)))
      (setf (run-model run) model))
    (incf (run-tokens run) tokens)
    (note-event run (list :kind "model" :name model :status "ok" :tokens tokens
                          :ts (nlk:unix-now)))))

(defun note-answer (run payload)
  "The answer a turn gave, which the claim rules read."
  (let ((text (nlk:fact-message-content payload)))
    (when text
      (setf (run-answer run) (if (> (length text) +answer-chars+)
                                  (subseq text 0 +answer-chars+)
                                  text)))))

;;; --- judging a finished run ---------------------------------------------------

(defun finish-run (turn run outcome)
  "RUN ended OUTCOME: judged, stored, and raised when it earned a finding."
  (setf (run-outcome run) outcome
        (run-ended run) (nlk:unix-now))
  (remhash turn *open-runs*)
  (remhash turn *calls*)
  (let* ((job (run-job run))
         (baseline (and job (baseline-from (healthy-rows job))))
         (silenced (if job (silenced-rules job) '()))
         (found (evaluate run baseline silenced))
         (findings (loop for item in found
                         for summary = (template-report found)
                         collect (list* :id (new-id "f") :report summary item)))
         (row (record-run (encode-run run findings))))
    (when (and findings (ignore-errors (setting :notice)))
      (nle:notice (note-text findings (run-id run))
                  :level :warning
                  :key "nodecode-flightlog"))
    row))

(defun fold-frame (op)
  "One durable fact, folded into the run it belongs to. Never signals: a
recorder that breaks a turn is worse than one that missed a step."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when (and (stringp type) (stringp turn) (plusp (length turn)))
      (with-flightlog-lock
        (let ((run (open-run-for turn (getf op :session-id))))
          (cond
            ((equal type "turn.tool_call_started") (note-call turn payload))
            ((equal type "turn.tool_result") (note-result turn run payload))
            ((equal type "turn.usage") (note-usage run payload))
            ((equal type "turn.assistant_message_completed") (note-answer run payload))
            ((equal type "turn.completed") (finish-run turn run "ok"))
            ((equal type "turn.failed") (finish-run turn run "failed"))
            ((equal type "turn.cancelled") (finish-run turn run "interrupted"))))))))

(defun observe-frame (op next)
  "The :FRAME point: a durable fact is folded into its run, every other frame
passes untouched, and the frame itself lands whatever this cell decides."
  (let ((answer (funcall next op)))
    (when (getf op :durable-p)
      (nlk:with-handlers ((error () nil))
        (fold-frame op)))
    answer))

;;; --- what a report says -------------------------------------------------------

(defun ago-text (unix)
  "How long ago UNIX was, in the one unit a person reads."
  (let ((seconds (round (- (nlk:unix-now) (or unix 0)))))
    (cond ((null unix) "-")
          ((>= seconds 86400) (format nil "~dd ago" (floor seconds 86400)))
          ((>= seconds 3600) (format nil "~dh ago" (floor seconds 3600)))
          ((>= seconds 60) (format nil "~dm ago" (floor seconds 60)))
          ((< seconds 10) "just now")
          (t (format nil "~ds ago" seconds)))))

(defun duration-text (milliseconds)
  (cond ((null milliseconds) "")
        ((>= milliseconds 1000) (format nil "~,1f s" (/ milliseconds 1000.0)))
        (t (format nil "~d ms" milliseconds))))

(defun tokens-text (count)
  (if (>= count 1000) (format nil "~,1fk" (/ count 1000.0)) (format nil "~d" count)))

(defun unacked-findings (row)
  (remove-if (lambda (item) (getf item :acked)) (run-findings-of row)))

(defun verdict-text (row)
  "The one line a listing puts before a run."
  (let ((findings (unacked-findings row)))
    (cond ((plusp (length findings)) (format nil "⚠ ~a" (headline (getf (first findings) :rule))))
          ((equal "ok" (row-string row "outcome")) "✓ Looks fine")
          (t (format nil "✗ Ended ~a" (row-string row "outcome"))))))

(defun who-text (row)
  (let ((job (row-string row "job")))
    (if (plusp (length job)) (format nil "job ~a" job) "chat")))

(defun run-line (row)
  (format nil "~a~%   ~a · ~a · run ~a · ~d tool~:p · ~a tokens"
          (verdict-text row) (who-text row) (ago-text (nlk:json-value row :number "ended"))
          (row-string row "id") (row-integer row "tool_calls")
          (tokens-text (row-integer row "tokens"))))

(defun overview-text (&optional (limit 5))
  "The /flightlog answer: what ran lately and what needs a look."
  (let ((rows (recent-runs :limit limit)))
    (if (null rows)
        "Flightlog has no runs yet: a run is recorded when a turn ends. Nothing has to be set."
        (multiple-value-bind (runs flagged) (runs-count-since (* 24 3600))
          (format nil "~a~2%~{~a~^~2%~}~2%Open one: /flightlog why ~a"
                  (format nil "Flightlog · last 24h: ~d run~:p, ~a"
                          runs (if (plusp flagged)
                                   (format nil "~d need~:p a look" flagged)
                                   "all look fine"))
                  (mapcar #'run-line rows)
                  (let ((first-flagged (find-if (lambda (row) (plusp (length (unacked-findings row)))) rows)))
                    (row-string (or first-flagged (first rows)) "id")))))))

(defun step-text (event)
  "One step of a timeline, as a person reads it: the mark, the thing, how long."
  (let* ((status (getf event :status))
         (bad (member status '("error" "failed" "denied" "timeout") :test #'equal))
         (kind (getf event :kind))
         (mark (if bad "✗" (if (equal kind "model") "·" "✓")))
         (text (cond ((equal kind "tool")
                      (format nil "~a ~@[~a ~]" (getf event :name) (getf event :target)))
                     ((equal kind "model")
                      (format nil "model round~@[ · ~a~]" (getf event :name)))
                     (t (getf event :kind))))
         (duration (duration-text (getf event :duration-ms)))
         (detail (and bad (getf event :detail))))
    (format nil "~a ~a~@[ · ~a~]~@[ — ~a~]" mark text
            (and duration (plusp (length duration)) duration)
            (and detail (plusp (length detail)) detail))))

(defun steps-text (events &key (limit 25))
  "The first and last few steps of a long run, every failed one kept in between."
  (let ((steps (mapcar #'step-text (remove-if (lambda (event) (equal (getf event :kind) "session"))
                                              events))))
    (when (null steps) (return-from steps-text '()))
    (if (<= (length steps) limit)
        steps
        (let ((head (subseq steps 0 5))
              (tail (subseq steps (- (length steps) 5))))
          (append head (list (format nil "… ~d more steps" (- (length steps) 10))) tail)))))

(defun report-text (row)
  "One run's incident report and timeline: /flightlog why RUN."
  (let* ((findings (run-findings-of row))
         (lead (first findings))
         (events (run-events-of row))
         (models (count "model" events :key (lambda (event) (getf event :kind)) :test #'equal))
         (seconds (/ (- (or (nlk:json-value row :number "ended") 0)
                        (or (nlk:json-value row :number "started") 0))
                     1.0)))
    (with-output-to-string (out)
      (format out "~a~%" (verdict-text row))
      (cond
        (lead
         (format out "~a~%~%" (getf lead :summary))
         (format out "Why it matters: ~a" (impact (getf lead :rule)))
         (dolist (item (rest findings))
           (format out "~%Also: ~a. ~a" (string-downcase (headline (getf item :rule))) (getf item :summary)))
         (let ((explain (getf lead :report)))
           (when (and explain (plusp (length explain))) (format out "~2%~a" explain))))
        (t (format out "No problems found in this run.")))
      (format out "~2%What happened · ~,1f s · ~d model call~:p · ~d tool~:p · ~a tokens"
              seconds models (row-integer row "tool_calls") (tokens-text (row-integer row "tokens")))
      (dolist (line (steps-text events))
        (format out "~%  ~a" line))
      (when lead
        (format out "~2%False alarm? /flightlog ack ~a" (getf lead :id))))))

(defun jobs-text ()
  "Each job's baseline: what it usually costs and does."
  (let ((jobs (remove-duplicates (remove-if-not (lambda (row) (plusp (length (row-string row "job"))))
                                                (runs-of *store*))
                                  :key (lambda (row) (row-string row "job")) :test #'equal)))
    (if (null jobs)
        "No scheduled job has run yet: a job run is a turn in a cron-<id> session."
        (format nil "~{~a~^~%~}"
                (loop for row in jobs
                      for job = (row-string row "job")
                      for baseline = (baseline-from (healthy-rows job))
                      collect (if baseline
                                  (format nil "~a  baseline over ~d healthy run~:p: median ~a tools, ~a tokens, ~a s"
                                          job (getf baseline :n)
                                          (number-text (float (getf baseline :median-tools)))
                                          (tokens-text (round (float (getf baseline :median-tokens))))
                                          (number-text (/ (float (or (getf baseline :median-duration-ms) 0)) 1000)))
                                  (format nil "~a  no healthy runs yet" job)))))))

;;; --- /flightlog, and the tool the model reads it with -------------------------

(defun dialog ()
  "The rows /flightlog alone answers with."
  (nle:list-dialog
   :title "Flightlog"
   :summary "Recent runs, and why a run was flagged"
   :rows (append
          (loop for row in (recent-runs :limit 3)
                collect (nle:ask-row (row-string row "id")
                                     (format nil "~a · ~a" (verdict-text row) (who-text row))
                                     "/flightlog" :placeholder "why <run>"
                                     :verb (format nil "why ~a" (row-string row "id"))))
          (list (nle:ask-row "Each job's baseline" "Press Enter" "/flightlog jobs" :verb "jobs")))))

(defun run-command (args session-id)
  "One /flightlog invocation: no argument is the overview, `why RUN' one run's
report, `jobs' each baseline, `ack FINDING' silences a rule for its job, and
`purge' forgets everything."
  (declare (ignore session-id))
  (handler-case
      (let* ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab))))
             (verb (string-downcase (or (first words) "")))
             (rest (rest words)))
        (flet ((refuse (text) (values text nil)))
          (cond
            ((string= verb "")
             (values (overview-text) (dialog)))
            ((string= verb "why")
             (if (null rest)
                 (refuse "Which run? /flightlog why <run id>. /flightlog lists the recent ones.")
                 (let ((rows (find-runs (first rest))))
                   (if rows
                       (values (report-text (car (last rows))) nil)
                       (refuse (format nil "No run ~a. /flightlog lists the recent ones." (first rest)))))))
            ((string= verb "jobs") (values (jobs-text) nil))
            ((string= verb "ack")
             (if (null rest)
                 (refuse "Which finding? The report for a flagged run names its id.")
                 (let ((item (ack-finding (first rest))))
                   (if item
                       (values (format nil "Acked ~a (~a); that rule stays quiet for this job."
                                       (getf item :id) (getf item :rule))
                               nil)
                       (refuse (format nil "No finding ~a." (first rest)))))))
            ((string= verb "purge")
             (if (equal (second words) "confirm")
                 (progn (purge-store) (values "Purged: every run, finding and silence is gone." nil))
                 (refuse "This deletes every recorded run. /flightlog purge confirm to do it.")))
            (t (values (overview-text) (dialog))))))
    (error (condition) (values (format nil "ERROR: flightlog: ~a" condition) nil))))

(nlk:define-contract flightlog-query
  (:doc "Read this machine's flight recorder: what recent turns did, which of them finished green but went wrong, and the timeline of one run. Call it when a person asks what ran recently, why a run was flagged, or whether a scheduled job is behaving the way it used to. Nothing here leaves the machine.")
  ("run" :string :default ""
         :doc "a run id, or a prefix of one, for its report and timeline; empty lists the newest runs")
  ("limit" :integer :default 5
           :doc "how many recent runs a listing answers with"))

(defun tool-runs (query)
  "The flightlog_runs tool: a listing, or one run's report."
  (handler-case
      (let ((reference (or (flightlog-query-run query) "")))
        (if (plusp (length (nlk:trimmed reference)))
            (let ((rows (find-runs reference)))
              (if rows (report-text (car (last rows)))
                  (format nil "No run ~a. /flightlog lists the recent ones." reference)))
            (overview-text (flightlog-query-limit query))))
    (error (condition) (nle:failure (format nil "ERROR: flightlog: ~a" condition)))))

(defparameter +primer+
  "Flightlog records every turn on this machine -- each tool call, model round and answer --
and flags the ones that finished green but went wrong: a claim that no check proved, a
failure the answer swallowed, a call repeated in a loop, a job that cost far more or did far
less than it usually does. Nothing is dialled and nothing leaves the machine.
  /flightlog              the newest runs and their verdicts
  /flightlog why RUN      one run's report and timeline
  /flightlog jobs         each scheduled job's baseline
  /flightlog ack FINDING  acknowledge a finding; its rule stays quiet for that job
  /flightlog purge confirm  forget every run
The flightlog_runs tool reads the same record. The store is one JSON file under the home."
  "What (help :flightlog) answers while the cell runs.")

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell flightlog
  (:section ("flightlog")
    (:guide "nothing is required: recording starts when the cell starts; dir moves the store, keep_runs bounds it, notice says whether a flagged run is raised to the operator, and the rest are the rules' thresholds; next step: /flightlog lists what has run")
    ("dir" :directory
     :doc "the folder the store lives in; flightlog/ under the home unless answered")
    ("keep_runs" :integer :default 200 :min 10
     :doc "how many runs the store keeps, newest first")
    ("notice" :boolean :default t
     :doc "raise a flagged run as a standing notice naming /flightlog")
    ("swallowed_errors" :integer :default 1 :min 1
     :doc "tool errors in a run that still reported success before it is flagged")
    ("loop_repeats" :integer :default 5 :min 2
     :doc "the same tool with the same arguments this many times in one run is a loop")
    ("cost_spike_pct" :integer :default 300 :min 110
     :doc "tokens this percent of the job's median is a spike")
    ("effort_collapse_pct" :integer :default 25 :min 1
     :doc "tool calls under this percent of the job's median is a collapse")
    ("min_baseline_runs" :integer :default 3 :min 1
     :doc "healthy runs a job needs before its baseline rules speak"))
  (:help :flightlog
   "flightlog: what the last turns did, and which finished green but went wrong - /flightlog"
   +primer+)
  (:command "flightlog" 'run-command
            :description "Flight recorder: recent runs, and why one was flagged"
            :argument-hint "[why RUN|jobs|ack FINDING|purge confirm]")
  (:tool "flightlog_runs" (nlk:contract 'flightlog-query) 'tool-runs)
  (:hook :frame #'observe-frame)
  (:start (lambda ()
            (setf *open-runs* (make-hash-table :test #'equal)
                  *calls* (make-hash-table :test #'equal)
                  *store* (load-store))
            (ensure-directories-exist (store-file))
            (nle:on-stop (lambda ()
                           (when *store* (ignore-errors (save-store *store*)))
                           (setf *store* nil
                                 *open-runs* (make-hash-table :test #'equal)
                                 *calls* (make-hash-table :test #'equal)))))))
