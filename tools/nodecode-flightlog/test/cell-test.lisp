;;;; cell-test.lisp --- lifecycle, the fold of a turn, the rules, /flightlog.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, offline: a start installs
;;;; the one hook, the command, the manual topic and the tool and a stop takes
;;;; them back; a bad section refuses with nothing installed; a store that
;;;; will not parse refuses the start; a turn published as durable frames
;;;; becomes one run with its steps, its tokens and its findings; a repeated
;;;; call is a loop; the baseline rules speak for a job and stay quiet for a
;;;; chat; and /flightlog and the tool answer what was recorded, through the
;;;; same readers a person uses. Every test writes a store of its own under
;;;; the scratch home, and nothing here dials anything.

(in-package #:nodecode.test)

(define-test-slice "flightlog" "FLIGHTLOG-CELL-" :start nodecode-flightlog:start-cell)

(defvar *store-dir* nil
  "The store folder of the test that is running.")

(defun flightlog-test-dir ()
  "A fresh store folder for one test."
  (let ((dir (nlk:home (format nil "flightlog-test-~d-~d/"
                               (get-universal-time) (random 1000000)))))
    (ensure-directories-exist dir)
    (namestring dir)))

(defmacro with-flightlog (&body body)
  "BODY with the cell started on a store folder of its own."
  `(let ((*store-dir* (flightlog-test-dir)))
     (with-cell-stop ((flightlog-start "dir" *store-dir*))
       ,@body)))

(defun flightlog-publish (type &key (session "chat-1") (turn "t1") payload)
  "One durable fact of TYPE, through the :frame hook, as the gateway publishes it."
  (cell-publish "nodecode-flightlog"
                (cell-frame-op type :session session :turn turn :payload payload)
                nil))

(defun flightlog-arguments (form)
  "The arguments JSON an eval call carries for FORM."
  (nlk:encode-json-object (nlk:json-object "form" form)))

(defun flightlog-call (name pairs)
  "The registered tool NAME, called with PAIRS as dispatch calls it: the
contract's record, materialized exactly where the engine materializes it."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is registered" name)
    (funcall (third entry) (nle::tool-body-arguments entry pairs))))

(defun flightlog-findings-of-row (row)
  (mapcar (lambda (item) (getf item :rule)) (nodecode-flightlog::run-findings-of row)))

(defun flightlog-only-row ()
  (let ((rows (nodecode-flightlog::runs-of nodecode-flightlog::*store*)))
    (is (= 1 (length rows)) "one finished turn is one run")
    (first rows)))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "flightlog"
  (:config "dir" (flightlog-test-dir))
  (:hooks :frame)
  (:command "flightlog")
  (:help :flightlog)
  (:running
   (is (nle::find-tool "flightlog_runs") "the tool is registered")
   (is (eq t (nodecode-flightlog::setting :notice)) "a flagged run is raised by default")
   (is (= 5 (nodecode-flightlog::setting :loop-repeats)) "the loop threshold is the original's")
   (is (uiop:directory-exists-p (nodecode-flightlog::store-directory)) "the start makes its folder"))
  (:stopped
   (is (null (nle::find-tool "flightlog_runs")) "stop unregisters the tool")
   (is (null nodecode-flightlog::*store*) "and forgets the store"))
  (:refused
   ("keep_runs" 5)
   ("loop_repeats" 1)
   ("cost_spike_pct" 100)))

;;; --- one turn, folded ------------------------------------------------------------

(deftest flightlog-cell-a-turn-becomes-one-flagged-run ()
  (with-flightlog
    (flightlog-publish "turn.tool_call_started"
                       :payload (nlk:json-object "call-id" "c1" "tool-name" "eval"
                                                  "arguments" (flightlog-arguments "(edit \"a.lisp\" ($ \"x\") ($ \"y\"))")))
    (flightlog-publish "turn.tool_result"
                       :payload (nlk:json-object "call-id" "c1" "tool-name" "eval"
                                                  "result" "ERROR: no such file: a.lisp"
                                                  "failed" t "duration-ms" 12))
    (flightlog-publish "turn.usage"
                       :payload (nlk:json-object "model" "a-model" "total-tokens" 1200))
    (flightlog-publish "turn.assistant_message_completed"
                       :payload (nlk:json-object "message" "All tests pass now. Fixed."))
    (flightlog-publish "turn.completed" :payload (nlk:json-object))
    (let ((row (flightlog-only-row)))
      (is (equal "ok" (nodecode-flightlog::row-string row "outcome")) "a completed turn is ok")
      (is (= 1 (nodecode-flightlog::row-integer row "tool_calls")) "the call is one step")
      (is (= 1 (nodecode-flightlog::row-integer row "tool_errors")) "and its failure is counted")
      (is (= 1200 (nodecode-flightlog::row-integer row "tokens")) "with the round's tokens")
      (is (equal "a-model" (nodecode-flightlog::row-string row "model")) "and the model it ran on")
      (let ((rules (flightlog-findings-of-row row)))
        (is (member "unverified_claim" rules :test #'equal) "a claim no check proved is flagged")
        (is (member "swallowed_errors" rules :test #'equal) "and a failure the answer swallowed")
        (is (equal "unverified_claim" (first rules)) "worst first, as the original ordered them"))
      (is (equal "unverified_claim" (first (flightlog-findings-of-row row))) "the claim leads the report")
      (let ((notice (cell-notice "nodecode-flightlog")))
        (is (stringp (second notice)) "the flagged run is raised to the operator")
        (is (search "Details: /flightlog why" (second notice)) "and says where the timeline is")))))

(deftest flightlog-cell-a-proved-claim-is-not-flagged ()
  (with-flightlog
    (flightlog-publish "turn.tool_call_started"
                       :payload (nlk:json-object "call-id" "c1" "tool-name" "eval"
                                                  "arguments" (flightlog-arguments "(write-file \"a.lisp\" ($ \"x\"))")))
    (flightlog-publish "turn.tool_result"
                       :payload (nlk:json-object "call-id" "c1" "tool-name" "eval" "result" "wrote" "failed" nil))
    (flightlog-publish "turn.tool_call_started"
                       :payload (nlk:json-object "call-id" "c2" "tool-name" "eval"
                                                  "arguments" (flightlog-arguments "(sh \"pytest -q\")")))
    (flightlog-publish "turn.tool_result"
                       :payload (nlk:json-object "call-id" "c2" "tool-name" "eval" "result" "3 passed" "failed" nil))
    (flightlog-publish "turn.assistant_message_completed"
                       :payload (nlk:json-object "message" "Tests pass; the fix works."))
    (flightlog-publish "turn.completed" :payload (nlk:json-object))
    (let ((row (flightlog-only-row)))
      (is (null (flightlog-findings-of-row row)) "a check after the last edit settles the claim")
      (is (search "Looks fine" (nodecode-flightlog::verdict-text row)) "and the run reads as fine")
      (is (null (cell-notice "nodecode-flightlog")) "nothing was raised"))))

(deftest flightlog-cell-a-loop-is-flagged ()
  (with-flightlog
    (dotimes (index 5)
      (flightlog-publish "turn.tool_call_started"
                         :payload (nlk:json-object "call-id" (format nil "c~d" index) "tool-name" "eval"
                                                    "arguments" (flightlog-arguments "(sh \"ls -la\")")))
      (flightlog-publish "turn.tool_result"
                         :payload (nlk:json-object "call-id" (format nil "c~d" index) "tool-name" "eval"
                                                    "result" "total 0" "failed" nil)))
    (flightlog-publish "turn.completed" :payload (nlk:json-object))
    (let ((row (flightlog-only-row)))
      (is (equal '("loop") (flightlog-findings-of-row row)) "five identical calls are one loop")
      (let ((summary (getf (first (nodecode-flightlog::run-findings-of row)) :summary)))
        (is (search "Called eval" summary) "and the finding names what repeated")
        (is (search "5 times with the same arguments" summary) "with the count that made it one")))))

(deftest flightlog-cell-a-silenced-rule-stays-quiet ()
  (with-flightlog
    (dotimes (index 5)
      (flightlog-publish "turn.tool_call_started" :session "cron-7"
                         :payload (nlk:json-object "call-id" (format nil "c~d" index) "tool-name" "eval"
                                                    "arguments" (flightlog-arguments "(sh \"ls -la\")")))
      (flightlog-publish "turn.tool_result" :session "cron-7"
                         :payload (nlk:json-object "call-id" (format nil "c~d" index) "tool-name" "eval"
                                                    "result" "total 0" "failed" nil)))
    (flightlog-publish "turn.completed" :session "cron-7" :payload (nlk:json-object))
    (let* ((row (flightlog-only-row))
           (finding (first (nodecode-flightlog::run-findings-of row))))
      (is (equal "cron-7" (nodecode-flightlog::row-string row "job")) "a cron session is a job")
      (is (member "loop" (flightlog-findings-of-row row) :test #'equal) "the loop is flagged")
      (let ((acked (nodecode-flightlog::ack-finding (getf finding :id))))
        (is (getf acked :acked) "an acknowledgement answers the finding, acked")
        (is (equal "loop" (getf acked :rule)) "with the rule it silences"))
      (let ((silenced (nodecode-flightlog::silenced-rules "cron-7")))
        (is (member "loop" silenced :test #'equal) "the ack silences that rule for the job")))))

;;; --- the rules, one at a time -----------------------------------------------------

(deftest flightlog-cell-baseline-rules-need-a-job-and-a-baseline ()
  (with-flightlog
    (let* ((baseline (list :n 3 :median-tools 10 :median-tokens 10000 :median-duration-ms 1000))
           (job-run (nodecode-flightlog::make-run :id "r1" :session "cron-1" :job "cron-1"
                                                  :tool-calls 1 :tokens 40000
                                                  :events (list (list :seq 1 :kind "model" :name "m" :status "ok"))))
           (chat-run (nodecode-flightlog::make-run :id "r2" :session "chat-1"
                                                   :tool-calls 1 :tokens 40000
                                                   :events (list (list :seq 1 :kind "model" :name "m" :status "ok"))))
           (job-rules (mapcar (lambda (item) (getf item :rule))
                               (nodecode-flightlog::evaluate job-run baseline '())))
           (chat-rules (mapcar (lambda (item) (getf item :rule))
                               (nodecode-flightlog::evaluate chat-run baseline '()))))
      (is (member "cost_spike" job-rules :test #'equal) "four times the median is a spike")
      (is (member "effort_collapse" job-rules :test #'equal) "one call where ten are usual is a collapse")
      (is (null (member "cost_spike" chat-rules :test #'equal)) "a chat gets no baseline rules")
      (is (null (member "effort_collapse" chat-rules :test #'equal)) "none at all"))
    (let ((empty (nodecode-flightlog::make-run :id "r3" :session "cron-1" :job "cron-1")))
      (let* ((thin (list :n 3 :median-tools 10 :median-tokens 100))
             (thin-rules (mapcar (lambda (item) (getf item :rule))
                                  (nodecode-flightlog::evaluate empty thin '())))
             (few (nodecode-flightlog::evaluate empty (list :n 1 :median-tools 10 :median-tokens 100) '())))
        (is (member "empty_work" thin-rules :test #'equal) "no tool call at all is its own finding")
        (is (null few) "and a baseline under min_baseline_runs stays silent")))))

;;; --- the store, /flightlog and the tool --------------------------------------------

(deftest flightlog-cell-a-store-that-will-not-read-refuses ()
  (let ((dir (flightlog-test-dir)))
    (with-open-file (out (merge-pathnames "flightlog.json" dir)
                          :direction :output :if-exists :supersede)
      (write-string "{ not a store" out))
    (let ((refusal (handler-case (progn (flightlog-start "dir" dir) "")
                     (nodecode-flightlog::flightlog-error (condition) (princ-to-string condition)))))
      (is (search "will not read as a store" refusal) "a store that will not parse refuses the start"))
    (is (null (nle::find-tool "flightlog_runs")) "and nothing was installed")))

(deftest flightlog-cell-command-and-tool-read-the-record ()
  (with-flightlog
    (flightlog-publish "turn.tool_call_started"
                       :payload (nlk:json-object "call-id" "c1" "tool-name" "eval"
                                                  "arguments" (flightlog-arguments "(sh \"make test\")")))
    (flightlog-publish "turn.tool_result"
                       :payload (nlk:json-object "call-id" "c1" "tool-name" "eval"
                                                  "result" "12 passed" "failed" nil "duration-ms" 2400))
    (flightlog-publish "turn.assistant_message_completed"
                       :payload (nlk:json-object "message" "The build passes."))
    (flightlog-publish "turn.completed" :payload (nlk:json-object))
    (let* ((row (flightlog-only-row))
           (id (nodecode-flightlog::row-string row "id")))
      (multiple-value-bind (text panel) (cell-entry "nodecode-flightlog" "flightlog" "" "chat-1")
        (is (search "Flightlog · last 24h: 1 run" text) "the overview counts the day")
        (is (search id text) "and lists the run by id")
        (is (search "Looks fine" text) "with its verdict")
        (is panel "the panel offers the runs"))
      (let ((report (cell-entry "nodecode-flightlog" "flightlog" (format nil "why ~a" id) "chat-1")))
        (is (search "What happened" report) "the report says what happened")
        (is (search "(sh \"make test\")" report) "with the step it ran")
        (is (search "2.4 s" report) "and how long the call took"))
      (let ((unknown (cell-entry "nodecode-flightlog" "flightlog" "why nope" "chat-1"))
            (jobs (cell-entry "nodecode-flightlog" "flightlog" "jobs" "chat-1"))
            (listing (flightlog-call "flightlog_runs" (nlk:json-object "run" "" "limit" 3)))
            (one (flightlog-call "flightlog_runs" (nlk:json-object "run" id)))
            (purged (cell-entry "nodecode-flightlog" "flightlog" "purge confirm" "chat-1")))
        (is (search "No run" unknown) "an unknown run is named, not invented")
        (is (search id listing) "the tool reads the same record the command does")
        (is (search "No problems found" one) "and answers one run's report")
        (is (search "No scheduled job has run yet" jobs) "with no job yet, jobs says so")
        (is (search "Purged" purged) "/flightlog purge confirm empties the record"))
      (is (null (nodecode-flightlog::runs-of nodecode-flightlog::*store*)) "and the store is empty"))))
