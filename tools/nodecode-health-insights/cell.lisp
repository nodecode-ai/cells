;;;; cell.lisp --- the cell: one section, one tool, /vitals and the manual.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One declaration: the `health_insights' section (the receiver database,
;;;; the offset a day is counted in, the unit weight is shown in, the days
;;;; after which data is stale), the health_insights tool, the /vitals
;;;; command and the manual topic. REPORT.LISP holds the arithmetic; nothing
;;;; here reads a file itself.

(in-package #:nodecode-health-insights)

(defparameter *reports* '("weekly" "concerns" "coverage" "daily")
  "What the tool's report member names.")

(nlk:define-contract health-report
  (:doc "Read the user's Apple Health data out of the receiver database the
health-insights section names: weekly trends, rule-based concerns, and data
coverage. Aggregates only, computed, never estimated: quote what it answers,
never invent a number or a cause.")
  ("report" :text :required t :options *reports*
   :doc "weekly trends, concerns, coverage, or one metric's daily values")
  ("metric" :text :default ""
   :doc "the type code daily reads, as coverage prints it, e.g. resting_heart_rate")
  ("date" :text :default ""
   :doc "the reference day, YYYY-MM-DD; empty is today in the configured offset"))

(defun report-text (report metric date)
  "REPORT read off the configured database, as the text a person reads."
  (let* ((offset (setting :utc-offset-hours))
         (ref-day (or (day-from-date date) (today-day offset)))
         (source (load-source (setting :db))))
    (cond ((string-equal report "weekly")
           (weekly-report source ref-day offset (setting :weight-unit)))
          ((string-equal report "concerns")
           (concerns-report source ref-day offset (setting :gap-days)))
          ((string-equal report "coverage")
           (coverage-report source ref-day offset (setting :gap-days)))
          ((string-equal report "daily")
           (daily-report source metric ref-day offset (setting :weight-unit) 14))
          (t (fail "unknown report ~a: name weekly, concerns, coverage or daily" report)))))

(defun answer-report (report)
  "The model's health_insights call: the report, read now."
  (report-text (health-report-report report)
               (health-report-metric report)
               (health-report-date report)))

(defun run-command (arguments session-id)
  "One /vitals invocation: [weekly|concerns|coverage|daily] [type-code] [date]."
  (declare (ignore session-id))
  (let* ((words (uiop:split-string (or arguments "") :separator '(#\Space #\Tab)))
         (verb (or (first words) "weekly"))
         (rest (rest words))
         (metric (if (string-equal verb "daily") (or (pop rest) "") ""))
         (date (or (find-if #'day-from-date rest) "")))
    (handler-case (report-text verb metric date)
      (health-insights-error (condition)
        (format nil "health-insights: ~a" condition)))))

(defparameter *manual*
  "The health-insights cell: what an Apple Health receiver database says about
the last weeks. It computes; you explain. Informational only, never medical
advice, and never a diagnosis.

- (vitals \"weekly\") or (vitals \"weekly\" \"\" \"2026-11-07\"): one line a
  metric (sleep, resting HR, HRV, weight, SpO2, heart rate, blood pressure),
  the 7-day mean against the 28-day median, and up/down only when the change
  reaches one robust sigma of that baseline. A metric with too little data
  says so; pass that on, never read it as zero.
- (vitals \"concerns\"): the rule findings, worst first, each with its
  evidence and its advice. Levels: [worth watching], [discuss with a
  clinician], [urgent]. Quote only the evidence and advice given; add no
  causes and no treatment. A [urgent] finding warrants prompt medical
  attention, and emergency care if there are symptoms. Data-gap and stale
  findings mean the check saw less than it should: say that first.
- (vitals \"coverage\"): which metrics the database holds, how much of each,
  how old the newest day is, and when the receiver last synced.
- (vitals \"daily\" \"resting_heart_rate\"): one metric's daily values and
  its 30- and 90-day medians.

The database is the file the HealthRelay or Health Bridge receiver writes;
name it once in the health-insights section of the config (the db setting),
and point utc_offset_hours at your own offset so a late-night reading lands
on the day you had it. Everything is read locally with SELECT; nothing is
sent anywhere, and raw values belong in this conversation, not in a channel.")

(nle:define-cell health-insights
  (:section ("health-insights")
    (:guide "db is the receiver's SQLite file; utc_offset_hours is your own offset from UTC; weight_unit is display only")
    ("db" :path :required t
     :doc "the HealthRelay or Health Bridge receiver's SQLite database")
    ("utc_offset_hours" :integer :default 0 :min -12
     :doc "your offset from UTC in hours, so a day is your day")
    ("weight_unit" :choice :options '("kg" "lb") :default "kg"
     :doc "display unit for weight; the database stores kg")
    ("gap_days" :integer :default 3 :min 1
     :doc "days of silence before a metric, or the database, is called stale"))
  (:tool "health_insights" (nlk:contract 'health-report) 'answer-report)
  (:help :health-insights
         "health-insights: weekly trends, concern findings and data coverage from an Apple Health receiver database"
         *manual*)
  (:command "vitals" 'run-command
            :description "Weekly health trends, concerns or coverage from your Apple Health data"
            :argument-hint "[weekly|concerns|coverage|daily] [type_code] [YYYY-MM-DD]")
  (:start (lambda ()
            (unless (and (stringp (setting :db)) (plusp (length (setting :db))))
              (error 'nlk:config-refusal
                     :detail "the health-insights section names no db: point it at the receiver's SQLite file")))))
