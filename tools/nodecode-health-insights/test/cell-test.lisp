;;;; cell-test.lisp --- the lifecycle, the reports over a stub, and a real read.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring and its arithmetic: that a
;;;; start installs the tool, /vitals and the manual and a stop takes them all
;;;; back; that a section naming no db refuses; that the weekly report reads
;;;; its two windows and names the direction only past one robust sigma; that a
;;;; severe reading is level 3 and leads; that coverage says which metric is
;;;; stale; and that READ-SOURCE reads a receiver database built here, SELECT
;;;; only, with sleep summed on the morning it ended.

(in-package #:nodecode.test)

(define-test-slice "health-insights" "HEALTH-INSIGHTS-CELL-" :start nodecode-health-insights:start-cell)

(defparameter +health-day+ (nodecode-health-insights::day-from-date "2026-11-07")
  "The reference day every test reads: a Saturday, so its windows are fixed.")

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "health-insights"
  (:config "db" "receiver.sqlite")
  (:help :health-insights)
  (:command "vitals")
  (:running (is (zerop (nodecode-health-insights::setting :utc-offset-hours)) "no offset by default")
            (is (equal "kg" (nodecode-health-insights::setting :weight-unit)) "weight in kg by default")
            (is (= 3 (nodecode-health-insights::setting :gap-days)) "three silent days is stale"))
  (:refused ("weight_unit" "stone") ("gap_days" 0) ("utc_offset_hours" -99)))

(deftest health-insights-cell-a-section-with-no-db-refuses ()
  (is (signals-error nlk:config-refusal (health-insights-start "utc_offset_hours" 1)))
  (is (null nodecode-health-insights::*health-insights*) "and leaves no settings behind"))

;;; --- a stubbed receiver ---------------------------------------------------------

(defun health-instant (day)
  "Noon UTC on DAY, as universal time: one instant a sample and a session share."
  (let ((text (nodecode-health-insights::day-text day)))
    (nodecode-health-insights::parse-timestamp (format nil "~aT12:00:00Z" text))))

(defun health-samples (type from to value)
  "DAYS of TYPE, one a day, FROM through TO; VALUE a number or a function of the day."
  (loop for day from from to to
        for instant = (health-instant day)
        collect (list :type type :start (nodecode-health-insights::day-text day)
                      :ts instant :end nil :end-ts nil
                      :value (coerce (if (functionp value) (funcall value day) value)
                                     'double-float)
                      :unit "")))

(defun health-nights (from to hours)
  "Nights FROM through TO, each HOURS long, ending at 07:00 local on the day."
  (loop for day from from to to
        for wake = (health-instant day)
        collect (cons (- wake (round (* 3600 hours))) wake)))

(defun health-receiver (&key samples sessions sync)
  "A stubbed receiver: what LOAD-SOURCE answers for any path."
  (lambda (db)
    (declare (ignore db))
    (list :samples samples :sessions sessions :sync sync)))

;;; --- the weekly report ----------------------------------------------------------

(deftest health-insights-cell-the-weekly-report-reads-both-windows ()
  (let* ((ref +health-day+)
         (source (list :samples (append (health-samples "resting_heart_rate" (- ref 40) ref 60)
                                        (health-samples "weight" (- ref 34) (- ref 7) 80d0)
                                        (health-samples "weight" (- ref 6) ref 82d0))
                       :sessions (health-nights (- ref 40) ref 7.5d0)
                       :sync nil))
         (text (nodecode-health-insights:weekly-report source ref 0 "kg")))
    (is (search "Weekly health to 2026-11-07" text) "the report names the day it reads to")
    (is (search "Sleep: 7d 7.5 h vs 28d 7.5 h (+0, flat), 7 of 7 days" text) "a flat week says flat")
    (is (search "Resting HR: 7d 60 bpm vs 28d 60 bpm (+0, flat), 7 of 7 days" text))
    (is (search "Weight: 7d 82 kg vs 28d 80 kg (+2, up)" text) "weight two kilos over its baseline is up")
    (is (search "Heart rate: no data" text) "a metric the database never carried says no data")
    (is (search "Notable: Weight up" text) "only the metric that moved is notable")
    (is (search "Informational only, not medical advice." text))))

(deftest health-insights-cell-a-direction-needs-one-robust-sigma ()
  (let* ((ref +health-day+)
         (source (list :samples (append (health-samples "resting_heart_rate" (- ref 34) (- ref 7) 60)
                                        (health-samples "resting_heart_rate" (- ref 6) ref 68))
                       :sessions nil :sync nil))
         (text (nodecode-health-insights:weekly-report source ref 0 "kg")))
    (is (search "Resting HR: 7d 68 bpm vs 28d 60 bpm (+8, up)" text) "a tight baseline calls eight bpm up")
    (is (search "Notable: Resting HR up" text)))
  (let* ((ref +health-day+)
         (source (list :samples (append (health-samples "resting_heart_rate" (- ref 34) (- ref 7)
                                                        (lambda (day) (+ 58 (mod day 5))))
                                        (health-samples "resting_heart_rate" (- ref 6) ref 61))
                       :sessions nil :sync nil))
         (text (nodecode-health-insights:weekly-report source ref 0 "kg")))
    (is (search "flat" text) "a change inside the spread is flat")))

(deftest health-insights-cell-too-few-days-is-not-a-number ()
  (let* ((ref +health-day+)
         (source (list :samples (health-samples "weight" (- ref 2) ref 80d0)
                       :sessions nil :sync nil))
         (text (nodecode-health-insights:weekly-report source ref 0 "kg")))
    (is (search "Weight: not enough data (3 of 7 days)" text) "three days of seven is not a week")))

;;; --- the concerns ---------------------------------------------------------------

(deftest health-insights-cell-a-severe-reading-is-level-three-and-leads ()
  (let* ((ref +health-day+)
         (samples (append (health-samples "blood_pressure_systolic" (1- ref) (1- ref) 185)
                          (health-samples "blood_pressure_diastolic" (1- ref) (1- ref) 125)))
         (findings (nodecode-health-insights::bp-findings (list :samples samples) ref 0)))
    (is (= 3 (getf (first findings) :level)) "185/125 is severe")
    (is (search "185/125" (getf (first findings) :evidence)))
    (is (not (search "bp_severe" (getf (first findings) :evidence))) "the id is not what a person reads"))
  (let* ((ref +health-day+)
         (samples (append (health-samples "blood_pressure_systolic" (1- ref) (1- ref) 118)
                          (health-samples "blood_pressure_diastolic" (1- ref) (1- ref) 75)))
         (findings (nodecode-health-insights::bp-findings (list :samples samples) ref 0)))
    (is (null findings) "118/75 in one reading is no finding at all")))

(deftest health-insights-cell-a-rule-leads-with-its-level ()
  (let* ((ref +health-day+)
         (samples (append (health-samples "blood_pressure_systolic" (- ref 1) ref 185)
                          (health-samples "blood_pressure_diastolic" (- ref 1) ref 125)))
         (text (nodecode-health-insights:concerns-report (list :samples samples) ref 0 3)))
    (is (search "[urgent] Severe blood pressure reading - reading 185/125 on 2026-11-06" text))
    (is (search "seek care now" text) "the advice travels with the finding")
    (is (search "not clinical judgement" text) "and the limits are on every answer")))

(deftest health-insights-cell-a-silent-database-is-a-finding ()
  (let* ((ref +health-day+)
         (source (append (health-samples "weight" (- ref 10) (- ref 8) 80d0) nil))
         (text (nodecode-health-insights:concerns-report
                (list :samples source :sessions nil) ref 0 3)))
    (is (search "[discuss with a clinician] The health data is stale" text) "eight days without a record is stale, and says so first")
    (is (search "does not mean things look fine" text))))

;;; --- coverage and one metric ----------------------------------------------------

(deftest health-insights-cell-coverage-names-what-is-stale ()
  (let* ((ref +health-day+)
         (source (list :samples (health-samples "weight" (- ref 20) (- ref 10) 80d0)
                       :sessions nil :sync "2026-11-06T21:00:00Z"))
         (text (nodecode-health-insights:coverage-report source ref 0 3)))
    (is (search "Weight: 11 days of data, first 2026-10-18, last 2026-10-28" text))
    (is (search "10 days ago" text))
    (is (search "stale" text) "a ten-day-old metric is stale")
    (is (search "Sleep: not tracked" text))
    (is (search "Sync: the last run began 2026-11-06T21:00:00Z" text))))

(deftest health-insights-cell-one-metric-over-its-medians ()
  (let* ((ref +health-day+)
         (source (list :samples (health-samples "resting_heart_rate" (- ref 100) ref 60)
                       :sessions nil :sync nil))
         (text (nodecode-health-insights:daily-report source "resting_heart_rate" ref 0 "kg" 3)))
    (is (search "Resting HR (resting_heart_rate), daily values to 2026-11-07" text))
    (is (search "2026-11-05  60 bpm" text) "the window is the last three days")
    (is (search "30d median 60 bpm (30 days)" text))
    (is (search "90d median 60 bpm (90 days)" text))))

;;; --- the cell's own verbs -------------------------------------------------------

(deftest health-insights-cell-the-tool-and-the-command-answer-from-the-stub ()
  (with-cell-stop ((health-insights-start "db" "receiver.sqlite"))
    (let ((nodecode-health-insights::*source*
            (health-receiver :samples (health-samples "weight" (- +health-day+ 10) +health-day+ 80d0))))
      (let ((answer (nodecode-health-insights::answer-report
                     (nodecode-health-insights::make-health-report
                      :report "coverage" :metric "" :date "2026-11-07"))))
        (is (search "Coverage to 2026-11-07" answer) "the tool reads the section's settings")
        (is (search "Weight: 11 days of data" answer)))
      (is (search "Notable: none"
                  (nodecode-health-insights::run-command "weekly 2026-11-07" "s1")) "/vitals weekly answers the report")
      (is (search "Coverage to 2026-11-07"
                  (nodecode-health-insights::run-command "coverage 2026-11-07" "s1")) "and so does each verb")
      (is (search "name weekly, concerns, coverage or daily"
                  (nodecode-health-insights::run-command "fortnightly" "s1")) "an unknown verb says which it knows"))))

(deftest health-insights-cell-a-missing-database-refuses-in-words ()
  (with-cell-stop ((health-insights-start "db" "no-such-receiver.sqlite"))
    (is (search "no such file"
                (nodecode-health-insights::run-command "weekly" "s1")) "a refused read answers as text, not as a thrown condition")))

;;; --- a real database ------------------------------------------------------------

(deftest health-insights-cell-reads-a-receiver-database ()
  (let* ((path (namestring (nlk:home "health-insights-cell.sqlite")))
         (handle nil))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (setf handle (sqlite:connect path))
           (sqlite:execute-non-query
            handle
            "CREATE TABLE samples (sample_id INTEGER PRIMARY KEY, type_code TEXT, start_time TEXT, end_time TEXT, value REAL, unit TEXT)")
           (sqlite:execute-non-query
            handle
            "CREATE TABLE sleep_sessions (sleep_session_id INTEGER PRIMARY KEY, start_time TEXT, end_time TEXT)")
           (dolist (row '(("weight" "2026-11-05T07:00:00Z" 80.0)
                          ("weight" "2026-11-06T07:00:00Z" 82.0)
                          ("oxygen_saturation" "2026-11-06T03:00:00Z" 0.965)))
             (destructuring-bind (type start value) row
               (sqlite:execute-non-query
                handle
                "INSERT INTO samples (type_code, start_time, end_time, value, unit) VALUES (?, ?, ?, ?, ?)"
                type start start value "")))
           (sqlite:execute-non-query
            handle
            "INSERT INTO sleep_sessions (start_time, end_time) VALUES ('2026-11-05T23:00:00Z', '2026-11-06T07:00:00Z')")
           (let ((source (nodecode-health-insights::read-source path)))
             (is (= 3 (length (getf source :samples))) "every sample the samples table holds")
             (is (= 1 (length (getf source :sessions))) "and the night it holds")
             (is (null (getf source :sync)) "a database with no sync_runs table answers no sync, not an error")
             (let ((series (nodecode-health-insights::daily-means
                            (getf source :samples) "weight" 0)))
               (is (= 2 (length series)))
               (is (= 82d0 (cdr (second series))) "a day's value is that day's weight"))
             (let ((nights (nodecode-health-insights::daily-sleep-hours
                            (getf source :sessions) 0)))
               (is (= 8d0 (cdr (first nights))) "a session is summed on the morning it ended")
               (is (= (nodecode-health-insights::day-from-date "2026-11-06") (car (first nights)))))))
      (when handle (ignore-errors (sqlite:disconnect handle)))
      (when (probe-file path) (delete-file path)))))
