;;;; cell-test.lisp --- one question one row, the windows, the trim, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test runs in the scratch home the cell lane gives the run, so the
;;;; ledger file is that home's and never the operator's: nothing here dials a
;;;; provider, and the question seam is a stub that answers at once.

(in-package #:nodecode.test)

(define-test-slice "approval-ledger" "APPROVAL-LEDGER-CELL-"
  :start nodecode-approval-ledger:start-cell)

(define-cell-lifecycle-tests "approval-ledger"
  (:hooks 'nle:ask-operator)
  (:help :approval-ledger)
  (:command "approval-log")
  (:running (is (equal nodecode-approval-ledger::+manual+ (nle:help :approval-ledger)))
            (is (= 5000 (nodecode-approval-ledger::setting :keep-rows)) "keep_rows defaults to 5000")
            (is (= 24 (nodecode-approval-ledger::setting :default-hours)) "the default window is a day")
            (is (equal "operator" (nodecode-approval-ledger::setting :surface))))
  (:refused ("keep_rows" 0) ("default_hours" -1) ("max_bytes" 1))
  (:idle approval-ledger:approval-ledger-error
         (approval-ledger:report)
         (approval-ledger:rows)
         (approval-ledger:clear)))

;;; --- fixtures ---------------------------------------------------------------------

(defun ask-as (answer why)
  "A stub for the question seam: it answers ANSWER and WHY at once."
  (lambda (&rest args)
    (declare (ignore args))
    (values answer why)))

(defun ledger-rows ()
  "The rows the ledger holds, oldest first, through the cell's own verb."
  (approval-ledger:rows))

(defmacro with-ledger (&body body)
  "BODY with the cell started on its defaults and an emptied ledger."
  `(with-cell-stop ((approval-ledger-start))
     (approval-ledger:clear)
     ,@body))

;;; --- one question, one row ---------------------------------------------------------

(deftest approval-ledger-cell-an-answered-question-is-one-row ()
  (with-ledger
    (let ((answered (nodecode-approval-ledger::observe-question
                      (ask-as "Allow once" :answered)
                      "The guard stopped a call: recursive delete"
                      :detail "rm -rf build"
                      :choices '("Allow once" "Deny")
                      :seconds 5
                      :session-id "s1")))
      (is (equal "Allow once" answered) "the advice answers what the question answered")
      (let* ((rows (ledger-rows))
             (row (first rows)))
        (is (= 1 (length rows)) "one question, one row")
        (is (equal "approved" (getf row :outcome)))
        (is (equal "The guard stopped a call: recursive delete" (getf row :rule)) "the title is the rule")
        (is (equal "Allow once" (getf row :answer)))
        (is (equal "s1" (getf row :session-id)))
        (is (numberp (getf row :wait-s)) "a person's answer has a wait")
        (is (equal "operator" (getf row :surface)))))))

(deftest approval-ledger-cell-a-denial-and-a-silence-are-not-a-wait ()
  (with-ledger
    (nodecode-approval-ledger::observe-question (ask-as "Deny" :answered)
                                                 "A block pattern stopped a call: rule push")
    (nodecode-approval-ledger::observe-question (ask-as nil :timeout)
                                                 "The guard stopped a call: sudo")
    (nodecode-approval-ledger::observe-question (ask-as nil :nobody)
                                                 "The guard stopped a call: mkfs")
    (let ((rows (ledger-rows)))
      (is (= 3 (length rows)))
      (is (equal "denied" (getf (first rows) :outcome)) "Deny is a denial, and the call did not run")
      (is (equal "unanswered" (getf (second rows) :outcome)) "a timeout is unanswered")
      (is (equal "unanswered" (getf (third rows) :outcome)) "nobody there is unanswered")
      (is (equal "timeout" (getf (second rows) :answer)))
      (is (null (getf (second rows) :wait-s)) "nobody's silence is no wait"))))

(deftest approval-ledger-cell-the-record-verb-writes-the-same-row ()
  (with-ledger
    (is (search "recorded: deploy Deny" (approval-ledger:record "deploy" "Deny" :wait 0.25d0)))
    (let ((row (first (ledger-rows))))
      (is (equal "deploy" (getf row :rule)))
      (is (equal "denied" (getf row :outcome)))
      (is (< (abs (- 0.25d0 (getf row :wait-s))) 1d-6) "the wait is kept to a thousandth"))))

;;; --- the windows and the report -----------------------------------------------------

(deftest approval-ledger-cell-the-report-groups-by-rule-surface-or-day ()
  (with-ledger
    (approval-ledger:clear)
    (let ((now (get-universal-time)))
      (approval-ledger:record "recursive delete" "Allow once" :wait 1.5d0 :ts (- now 90))
      (approval-ledger:record "recursive delete" "Deny" :wait 0.5d0 :ts (- now 30))
      (approval-ledger:record "sudo" "Allow once" :wait 2.0d0 :ts now))
    (let ((text (nodecode-approval-ledger::run-approval-log "all" nil)))
      (is (search "recursive delete" text))
      (is (search "sudo" text))
      (is (search "median wait" text))
      (is (search "total" text)))
    (let ((by-surface (nodecode-approval-ledger::run-approval-log "all surfaces" nil)))
      (is (search "surface" by-surface) "surfaces groups by where it asked")
      (is (search "operator" by-surface)))
    (let ((by-day (nodecode-approval-ledger::run-approval-log "all days" nil)))
      (is (search "day" by-day)))
    (let ((newest (nodecode-approval-ledger::run-approval-log "all recent 2" nil)))
      (is (search "newest 2 of 3" newest))
      (is (< (search "sudo" newest) (search "recursive delete" newest)) "newest leads"))
    (let ((recent (approval-ledger:recent :window "all" :limit 1)))
      (is (search "newest 1 of 3" recent)))))

(deftest approval-ledger-cell-a-window-counts-back-from-now ()
  (with-ledger
    (let ((now (get-universal-time)))
      (approval-ledger:record "old rule" "Deny" :ts (- now 7200))
      (approval-ledger:record "new rule" "Deny" :ts now))
    (is (search "new rule" (nodecode-approval-ledger::run-approval-log "1h" nil)))
    (is (not (search "old rule" (nodecode-approval-ledger::run-approval-log "1h" nil))) "an hour is an hour")
    (is (search "old rule" (nodecode-approval-ledger::run-approval-log "6h" nil)))
    (is (search "old rule" (nodecode-approval-ledger::run-approval-log "all" nil)))
    (is (search "last 1 hour:" (nodecode-approval-ledger::run-approval-log "1h" nil)))))

(deftest approval-ledger-cell-an-unknown-option-says-so-and-clear-empties ()
  (with-ledger
    (is (search "Unknown option: nosuch"
                (nodecode-approval-ledger::run-approval-log "nosuch" nil)))
    (is (search "/approval-log" (nodecode-approval-ledger::run-approval-log "help" nil)))
    (approval-ledger:record "deploy" "Deny")
    (is (search "deleted 1 recorded decision" (approval-ledger:clear)))
    (is (null (ledger-rows)) "the ledger is empty after clear")
    (is (search "none recorded" (approval-ledger:report :window "all")))))

;;; --- the file stays bounded ---------------------------------------------------------

(deftest approval-ledger-cell-the-ledger-keeps-its-newest-rows ()
  (with-cell-stop ((approval-ledger-start "keep_rows" 2 "max_bytes" 64))
    (approval-ledger:clear)
    (let ((now (get-universal-time)))
      (dotimes (index 4)
        (approval-ledger:record (format nil "rule ~d" index) "Deny" :wait 1.0d0
                                 :ts (+ now index))))
    (is (equal '("rule 2" "rule 3") (mapcar (lambda (row) (getf row :rule)) (ledger-rows))) "the newest two are kept")))
