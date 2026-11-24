;;;; cell-test.lisp --- lifecycle, the folding, the windows, the command, the tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a store: the failed-attempt rows the log holds are
;;;; handed to the cell through its own *FAILURE-READER*, in the shape its
;;;; store query answers, and the window the report covers is compared
;;;; against stamps this file writes.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-test-slice "error-ledger" "ERROR-LEDGER-CELL-" :start nodecode-error-ledger:start-cell)

(define-cell-lifecycle-tests "error-ledger"
  (:help :error-ledger)
  (:command "error-ledger")
  (:running (is (nle::find-tool "error_ledger") "start registers the error_ledger tool"))
  (:stopped (is (null (nle::find-tool "error_ledger")) "stop unregisters the tool"))
  (:refused ("recent_rows" 0)))

;;; --- fixtures: rows in the shape the store query answers ------------------------------

(defvar *rows* '()
  "The failed attempts the cell reads, oldest first:
(POSITION AT SESSION TURN ATTEMPT PROVIDER MODEL STATUS REASON DETAIL KIND).")

(defun stub-rows (&key session-id)
  "What the store query answers over *ROWS*: all of them, or one session's."
  (if session-id
      (remove-if-not (lambda (row) (equal session-id (third row))) *rows*)
      *rows*))

(defmacro with-rows ((&rest rows) &body body)
  "Run BODY with the cell reading ROWS instead of the store."
  `(let ((*rows* (list ,@rows))
         (nodecode-error-ledger::*failure-reader* #'stub-rows))
     ,@body))

(defun stamp (seconds-ago)
  "SECONDS-AGO before now, as the UTC stamp the log writes."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (- (get-universal-time) seconds-ago) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.000Z"
            year month date hour minute second)))

(defun row (position &key (at (stamp 60)) (session "s1") (turn nil) (attempt 1)
                      (provider "the-provider") (model "the-model") (status 429)
                      (reason "rate_limit") (detail "slow down") (kind "retry"))
  "One failed attempt as the log holds it. Each POSITION is its own attempt
and its own turn unless TURN says otherwise, so two rows stay two failures."
  (list position at session (or turn (format nil "t~d" position)) attempt
        provider model status reason detail kind))

(defun call-with-ledger (pairs thunk)
  "Run THUNK with the cell started over PAIRS, stopped after, its clear
watermark removed so a report sees the whole log again."
  (let ((stop (apply #'error-ledger-start pairs)))
    (unwind-protect (funcall thunk)
      (funcall stop)
      (setf (nodecode-error-ledger::cleared-position) nil))))

(defun ledger-report (pairs)
  "The report the cell answers over the stub rows under PAIRS."
  (call-with-ledger nil (lambda () (apply #'nodecode-error-ledger:report pairs))))

;;; --- folding: one row per failed attempt ---------------------------------------------

(deftest error-ledger-cell-a-retry-and-a-fallback-are-one-failure ()
  (with-rows ((row 1 :turn "t1" :reason "provider_stream" :detail "timed out" :kind "retry")
              (row 2 :turn "t1" :reason "rate_limit" :detail "429 too many requests" :kind "fallback"))
    (let ((failures (nodecode-error-ledger:failures :window :all)))
      (is (= 1 (length failures)) "the retry and the fallback of one attempt are one failure")
      (is (eq t (getf (first failures) :moved)) "the failure cost the turn its model")
      (is (equal "rate_limit" (getf (first failures) :reason)) "the fallback's classification names the cause")
      (is (equal "429 too many requests" (getf (first failures) :detail)) "the fuller line is the one kept")
      (is (equal "the-provider" (getf (first failures) :provider)) "the provider the attempt targeted"))))

(deftest error-ledger-cell-a-retry-in-place-keeps-its-budget-as-the-reason ()
  (with-rows ((row 1 :provider "a" :model "m1" :reason "provider_request" :status 503))
    (let ((failure (first (nodecode-error-ledger:failures :window :all))))
      (is (equal "provider_request" (getf failure :reason)) "the budget the failure spent is the reason")
      (is (null (getf failure :moved)) "nothing moved")
      (is (= 503 (getf failure :status)) "the status is kept"))))

;;; --- the windows ---------------------------------------------------------------------

(deftest error-ledger-cell-a-window-leaves-older-failures-out ()
  (with-rows ((row 1 :at (stamp (* 10 86400)) :model "stale")
              (row 2 :at (stamp 120) :model "fresh"))
    (let ((week (ledger-report (list :window :day :span "7d")))
          (all (ledger-report (list :window :all))))
      (is (search "last 7 days" week) "the title says the window")
      (is (search "fresh" week) "a failure inside the window is reported")
      (is (null (search "stale" week)) "one outside it is not")
      (is (search "stale" all) "everything recorded still holds it"))))

(deftest error-ledger-cell-session-shows-the-latest-session ()
  (with-rows ((row 1 :at (stamp 600) :session "s1" :model "first-model")
              (row 2 :at (stamp 60) :session "s2" :model "second-model"))
    (let ((text (ledger-report (list :window :session))))
      (is (search "latest session" text) "the title names the window")
      (is (search "second-model" text) "the newest session with a failure is the one shown")
      (is (null (search "first-model" text)) "the older session stays out"))))

;;; --- the tables ----------------------------------------------------------------------

(deftest error-ledger-cell-groups-by-model-with-a-total-row ()
  (call-with-ledger
   nil
   (lambda ()
     (with-rows ((row 1 :model "busy" :reason "rate_limit")
                 (row 2 :model "busy" :reason "rate_limit" :status 529)
                 (row 3 :model "quiet" :reason "timeout" :status nil))
       (let ((text (nodecode-error-ledger:report :window :all)))
         (is (search "Provider errors, all recorded:" text) "the heading names what it read")
         (is (search "busy" text) "the busiest model is there")
         (is (search "top reason" text) "the commonest cause is a column")
         (is (search "rate_limit (2)" text) "with its count")
         (is (search "total" text) "more than one group gets a total row")
         (is (search "One row per failed attempt" text) "and the footer says what a row is"))))))

(deftest error-ledger-cell-recent-lists-the-newest-first ()
  (call-with-ledger
   nil
   (lambda ()
     (with-rows ((row 1 :at (stamp 300) :model "oldest" :reason "timeout")
                 (row 2 :at (stamp 200) :model "middle" :reason "overloaded")
                 (row 3 :at (stamp 100) :model "brand-new" :reason "rate_limit"))
       (let* ((text (nodecode-error-ledger:report :window :all :limit 2))
              (lines (uiop:split-string text :separator '(#\Newline)))
              (first-row (find-if (lambda (line) (search "brand-new" line)) lines)))
         (is (search "newest 2 of 3" text) "the list says how many of how many")
         (is first-row "the newest failure is listed")
         (is (search "ago" first-row) "with how long ago it was"))))))

;;; --- the command --------------------------------------------------------------------

(deftest error-ledger-cell-the-command-takes-the-originals-words ()
  (call-with-ledger
   nil
   (lambda ()
     (with-rows ((row 1 :provider "a-provider" :model "a-model" :reason "rate_limit"))
       (let ((grouped (nodecode-error-ledger::run-slash "7d providers" "s1"))
             (short (nodecode-error-ledger::run-slash "all recent 1" "s1"))
             (unknown (nodecode-error-ledger::run-slash "nonsense" "s1"))
             (help (nodecode-error-ledger::run-slash "help" "s1")))
         (is (search "last 7 days" grouped) "a window word becomes the title")
         (is (search "top reason" grouped) "grouping by provider keeps the cause in view")
         (is (search "newest 1 of 1" short) "recent takes a count")
         (is (search "Unknown option: nonsense" unknown) "an unknown word is refused and the help follows")
         (is (search "/error-ledger" help) "help prints the command's own words"))))))

(deftest error-ledger-cell-clear-hides-what-the-log-holds ()
  (call-with-ledger
   '("recent_rows" 5)
   (lambda ()
     (with-rows ((row 1 :at (stamp 300)) (row 2 :at (stamp 60)))
       (let ((answer (nodecode-error-ledger::run-slash "clear" "s1"))
             (after (nodecode-error-ledger:report :window :all)))
         (is (search "hid 2 recorded error" answer) "clear says how many it hid")
         (is (search "none recorded" after) "and the report is empty afterwards"))))))

;;; --- the tool -----------------------------------------------------------------------

(deftest error-ledger-cell-the-tool-answers-the-same-report ()
  (call-with-ledger
   nil
   (lambda ()
     (with-rows ((row 1 :provider "a-provider" :model "a-model" :reason "overloaded"))
       (let ((grouped (nodecode-error-ledger::read-tool
                       (nlk:json-object "window" "all" "by" "reason")))
             (newest (nodecode-error-ledger::read-tool
                      (nlk:json-object "window" "all" "recent" 5))))
         (is (search "Provider errors, all recorded:" grouped) "the tool answers the report")
         (is (search "top model" grouped) "grouped by cause, it names the model it happened on")
         (is (search "newest 1 of 1" newest) "and it can list the newest instead"))))))
