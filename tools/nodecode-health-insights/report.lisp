;;;; report.lisp --- the numbers behind every answer: samples in, text out.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original's analysis is deterministic, and so is this: daily means out
;;;; of the receiver's own samples, a 7-day window against a 28-day baseline
;;;; whose spread is the mean absolute deviation, and concern rules whose
;;;; thresholds stand where they are read. Every report is a function of
;;;; samples, sessions and a reference day; READ-SOURCE is the only reader of
;;;; a file, and *SOURCE* replaces it, so a test needs no database.

(in-package #:nodecode-health-insights)

;;; --- time ---------------------------------------------------------------------

(defparameter *day* 86400
  "Seconds in a day: days are counted as integers from 1970-01-01.")

(defun zone-hours (text)
  "The offset TEXT's ISO 8601 suffix names, in hours; 0 for a Z suffix or none."
  (let ((tail (and (> (length text) 19) (subseq text 19))))
    (if (and tail (>= (length tail) 6) (member (char tail 0) '(#\+ #\-)))
        (let ((sign (if (char= (char tail 0) #\-) -1 1)))
          (* sign (+ (parse-integer tail :start 1 :end 3)
                     (/ (parse-integer tail :start 4 :end 6) 60))))
        0)))

(defun parse-timestamp (text)
  "Universal time for an ISO 8601 TEXT (Z, an offset, or none), or NIL."
  (when (and (stringp text) (>= (length text) 16)
             (char= (char text 4) #\-) (char= (char text 7) #\-))
    (handler-case
        (encode-universal-time
         (if (and (> (length text) 18) (digit-char-p (char text 17)))
             (parse-integer text :start 17 :end 19) 0)
         (parse-integer text :start 14 :end 16)
         (parse-integer text :start 11 :end 13)
         (parse-integer text :start 8 :end 10)
         (parse-integer text :start 5 :end 7)
         (parse-integer text :start 0 :end 4)
         (zone-hours text))
      (error () nil))))

(defun local-day (universal offset)
  "The calendar day number UNIVERSAL falls on, OFFSET hours from UTC: the
user's own day, so a late-night reading lands on that day, not the next."
  (floor (+ universal (* 3600 offset)) *day*))

(defun day-text (day)
  "DAY as YYYY-MM-DD."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (* day *day*) 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun day-from-date (text)
  "The day number a YYYY-MM-DD TEXT names, or NIL when it is not one."
  (when (and (stringp text) (>= (length text) 10))
    (handler-case
        (floor (encode-universal-time 0 0 12
                                      (parse-integer text :start 8 :end 10)
                                      (parse-integer text :start 5 :end 7)
                                      (parse-integer text :start 0 :end 4)
                                      0)
               *day*)
      (error () nil))))

(defun today-day (offset)
  "Today's local day number, in the configured offset."
  (local-day (get-universal-time) offset))

;;; --- statistics -----------------------------------------------------------------

(defun mean (values)
  "The arithmetic mean of VALUES, 0 when there are none."
  (if values (/ (reduce #'+ values) (length values)) 0d0))

(defun median (values)
  "The median of VALUES, 0 when there are none: the baseline the report reads."
  (if values
      (let* ((sorted (sort (copy-list values) #'<))
             (count (length sorted))
             (middle (floor count 2)))
        (if (evenp count)
            (/ (+ (nth (1- middle) sorted) (nth middle sorted)) 2)
            (coerce (nth middle sorted) 'double-float)))
      0d0))

(defun mad (values)
  "The mean absolute deviation from the median: a spread a few wild readings cannot move."
  (if values
      (let ((middle (median values)))
        (mean (mapcar (lambda (value) (abs (- value middle))) values)))
      0d0))

(defun robust-sigma (values)
  "MAD read as a standard deviation, so one sigma is a change worth naming."
  (* 1.4826d0 (mad values)))

(defun number-text (value)
  "VALUE to one decimal place, without a trailing .0."
  (let ((text (format nil "~,1f" (coerce value 'double-float))))
    (if (and (> (length text) 2) (string= ".0" text :start2 (- (length text) 2)))
        (subseq text 0 (- (length text) 2))
        text)))

(defun direction-of (recent-mean baseline)
  "up or down when the change from the baseline median reaches one robust
sigma of the baseline, else flat: a small change over a tight baseline counts."
  (if baseline
      (let ((change (- recent-mean (median baseline))))
        (cond ((zerop change) "flat")
              ((>= (abs change) (* 1d0 (robust-sigma baseline)))
               (if (plusp change) "up" "down"))
              (t "flat")))
      "flat"))

;;; --- reading the receiver -------------------------------------------------------

(defvar *source* nil
  "Function of a database path answering (:samples ... :sessions ... :sync),
or NIL to read the SQLite file itself. A test binds a fixture here.")

(defun make-sample (type start end value unit)
  "One sample as the receiver stores it: its type code, both instants and its value."
  (list :type type :start start :ts (parse-timestamp start)
        :end end :end-ts (parse-timestamp end)
        :value (coerce value 'double-float) :unit (or unit "")))

(defun db-rows (db sql)
  "Every row SQL answers from the SQLite database DB, as a list each; a
refusal naming DB when it cannot be opened or read. SELECT only."
  (unless (and (stringp db) (plusp (length db)))
    (fail "no database is configured: name the receiver's SQLite file in the db setting"))
  (unless (probe-file db)
    (fail "cannot read the receiver database ~a: no such file" db))
  (let ((handle (handler-case (sqlite:connect (namestring (pathname db)))
                  (error (condition)
                    (fail "cannot open the receiver database ~a: ~a" db condition)))))
    (unwind-protect
         (handler-case (sqlite:execute-to-list handle sql)
           (error (condition)
             (fail "cannot read the receiver database ~a: ~a" db condition)))
      (ignore-errors (sqlite:disconnect handle)))))

(defun read-source (db)
  "The samples, sleep sessions and newest sync the receiver DB holds."
  (let* ((tables (mapcar #'first (db-rows db "SELECT name FROM sqlite_master WHERE type='table'")))
         (has (lambda (name) (member name tables :test #'string=))))
    (list :samples
          (when (funcall has "samples")
            (loop for (type start end value unit)
                    in (db-rows db "SELECT type_code, start_time, end_time, value, unit FROM samples")
                  when (and (stringp type) (realp value))
                    collect (make-sample type start end value unit)))
          :sessions
          (when (funcall has "sleep_sessions")
            (loop for (start end) in (db-rows db "SELECT start_time, end_time FROM sleep_sessions")
                  for began = (parse-timestamp start)
                  for ended = (parse-timestamp end)
                  when (and began ended (> ended began)) collect (cons began ended)))
          :sync
          (when (funcall has "sync_runs")
            (first (first (db-rows db "SELECT MAX(started_at) FROM sync_runs")))))))

(defun load-source (db)
  "DB read, or *SOURCE*'s fixture when a test binds one."
  (if *source* (funcall *source* db) (read-source db)))

;;; --- series --------------------------------------------------------------------

(defun daily-means (samples type offset)
  "((DAY . MEAN) ...) oldest first: one type's samples averaged per local day."
  (let ((groups (make-hash-table :test #'eql)))
    (dolist (sample samples)
      (when (and (equal type (getf sample :type)) (getf sample :ts))
        (push (getf sample :value) (gethash (local-day (getf sample :ts) offset) groups))))
    (sort (loop for day being the hash-keys of groups using (hash-value values)
                collect (cons day (mean values)))
          #'< :key #'car)))

(defun daily-sleep-hours (sessions offset)
  "((WAKE-DAY . HOURS) ...) oldest first: each session's length summed on the
day it ended, which is the night a person names."
  (let ((groups (make-hash-table :test #'eql)))
    (dolist (session sessions)
      (push (/ (- (cdr session) (car session)) 3600d0)
            (gethash (local-day (cdr session) offset) groups)))
    (sort (loop for day being the hash-keys of groups using (hash-value hours)
                collect (cons day (reduce #'+ hours)))
          #'< :key #'car)))

(defun window-values (series from to)
  "The values of SERIES (a ((DAY . VALUE) ...) list) on days FROM through TO."
  (loop for (day . value) in series when (and (>= day from) (<= day to)) collect value))

(defun window-pairs (series from to)
  "The (DAY . VALUE) pairs of SERIES on days FROM through TO."
  (loop for (day . value) in series when (and (>= day from) (<= day to)) collect (cons day value)))
;;; --- metrics -------------------------------------------------------------------

(defparameter *metrics*
  '(("sleep_analysis" "Sleep" "h")
    ("resting_heart_rate" "Resting HR" "bpm")
    ("heart_rate_variability_sdnn" "HRV" "ms")
    ("weight" "Weight" "kg")
    ("oxygen_saturation" "SpO2" "%")
    ("heart_rate" "Heart rate" "bpm"))
  "What a report names, in the order it names them: the type code the
receiver stores, the label, the unit. Weight is stored in kg.")

(defparameter *recent-days* 7
  "The window a change is read over: the last seven days, ending today.")

(defparameter *baseline-days* 28
  "The window the baseline sits in: the 28 days before the recent one.")

(defparameter *min-recent-days* 5
  "Fewer than this many days in the recent window is not enough to compare.")

(defparameter *min-baseline-days* 5
  "Fewer than this many days in the baseline is no baseline: no direction.")

(defun metric-series (source type offset)
  "TYPE's daily series: sleep summed by wake-up day, everything else averaged."
  (if (equal type "sleep_analysis")
      (daily-sleep-hours (getf source :sessions) offset)
      (daily-means (getf source :samples) type offset)))

(defun shown-value (type value unit weight-unit)
  "(values VALUE UNIT) as a person reads it: SpO2 as a percent, weight in the chosen unit."
  (cond ((equal type "oxygen_saturation") (values (* 100d0 value) "%"))
        ((and (equal type "weight") (equal weight-unit "lb"))
         (values (* 2.2046226218d0 value) "lb"))
        (t (values value unit))))

(defun metric-line (type label unit series ref-day weight-unit)
  "One weekly line: the 7-day mean against the 28-day median, and the direction."
  (let* ((recent-start (- ref-day (1- *recent-days*)))
         (baseline-start (- ref-day (+ *recent-days* *baseline-days* -1)))
         (recent (window-values series recent-start ref-day))
         (baseline (window-values series baseline-start (1- recent-start))))
    (cond ((null series)
           (format nil "~a: no data" label))
          ((< (length recent) *min-recent-days*)
           (format nil "~a: not enough data (~d of ~d days)" label (length recent) *recent-days*))
          ((< (length baseline) *min-baseline-days*)
           (multiple-value-bind (shown shown-unit) (shown-value type (mean recent) unit weight-unit)
             (format nil "~a: 7d ~a ~a, no baseline yet (~d days of the last ~d)"
                     label (number-text shown) shown-unit (length baseline) *baseline-days*)))
          (t
           (multiple-value-bind (recent-shown recent-unit) (shown-value type (mean recent) unit weight-unit)
             (multiple-value-bind (baseline-shown baseline-unit)
                 (shown-value type (median baseline) unit weight-unit)
               (let* ((change (- recent-shown baseline-shown))
                      (tail (if (equal type "weight") ""
                                (format nil ", ~d of ~d days" (length recent) *recent-days*))))
                 (format nil "~a: 7d ~a ~a vs 28d ~a ~a (~a~a, ~a)~a"
                         label (number-text recent-shown) recent-unit
                         (number-text baseline-shown) baseline-unit
                         (if (minusp change) "-" "+") (number-text (abs change))
                         (direction-of (mean recent) baseline) tail))))))))

(defun notable-line (lines)
  "Name every line the windows call up or down, or say none moved."
  (let ((named nil))
    (dolist (line lines)
      (let ((colon (position #\: line)))
        (cond ((null colon))
              ((search ", up)" line) (push (format nil "~a up" (subseq line 0 colon)) named))
              ((search ", down)" line) (push (format nil "~a down" (subseq line 0 colon)) named)))))
    (if named
        (format nil "Notable: ~{~a~^, ~}" (nreverse named))
        "Notable: none")))

(defun weekly-report (source ref-day offset weight-unit)
  "The weekly report: a line a metric, what moved, and the limits of the arithmetic."
  (let* ((lines (loop for (type label unit) in *metrics*
                       collect (metric-line type label unit
                                            (metric-series source type offset) ref-day weight-unit)))
         (bp (bp-line source ref-day offset))
         (all (if bp (append lines (list bp)) lines)))
    (format nil "Weekly health to ~a (weight in ~a)~%~{~a~%~}~a~%Informational only, not medical advice."
            (day-text ref-day) weight-unit all (notable-line all))))

;;; --- blood pressure -------------------------------------------------------------

(defun bp-category (systolic diastolic)
  "The AHA band a reading falls in; the worse of the two numbers wins."
  (cond ((or (>= systolic 180) (>= diastolic 120)) "Severe")
        ((or (>= systolic 140) (>= diastolic 90)) "Stage 2")
        ((or (>= systolic 130) (>= diastolic 80)) "Stage 1")
        ((>= systolic 120) "Elevated")
        (t "Normal")))

(defun bp-readings (source offset)
  "((DAY SYS DIA) ...) oldest first: only samples sharing one start instant
form a reading, and an unpaired value is dropped."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (sample (getf source :samples))
      (when (and (getf sample :ts)
                 (member (getf sample :type)
                         '("blood_pressure_systolic" "blood_pressure_diastolic")
                         :test #'equal))
        (let ((pair (gethash (getf sample :ts) groups)))
          (setf (getf pair (if (string= (getf sample :type) "blood_pressure_systolic")
                               :sys :dia))
                (getf sample :value))
          (setf (gethash (getf sample :ts) groups) pair))))
    (sort (loop for instant being the hash-keys of groups using (hash-value pair)
                when (and (getf pair :sys) (getf pair :dia))
                  collect (list (local-day instant offset) (getf pair :sys) (getf pair :dia)))
          #'< :key #'first)))

(defun bp-line (source ref-day offset)
  "The BP line of the weekly report, or NIL when there are no readings."
  (let* ((readings (bp-readings source offset))
         (recent (remove-if (lambda (reading) (< (first reading) (- ref-day 6))) readings))
         (baseline (remove-if-not (lambda (reading)
                                     (and (>= (first reading) (- ref-day 34))
                                          (< (first reading) (- ref-day 6))))
                                   readings)))
    (when readings
      (let* ((systolic (mean (mapcar #'second recent)))
             (diastolic (mean (mapcar #'third recent)))
             (systolic-base (median (mapcar #'second baseline)))
             (diastolic-base (median (mapcar #'third baseline)))
             (change (- systolic systolic-base)))
        (format nil "BP: 7d ~d/~d mmHg vs 28d ~d/~d mmHg (~a~d, ~a)"
                (round systolic) (round diastolic)
                (round systolic-base) (round diastolic-base)
                (if (minusp change) "-" "+") (round (abs change))
                (direction-of systolic (mapcar #'second baseline)))))))

;;; --- coverage ------------------------------------------------------------------

(defun coverage-line (type label series ref-day gap-days)
  "One metric's line: how much of it the database holds and how old its last day is."
  (declare (ignore type))
  (if (null series)
      (format nil "~a: not tracked" label)
      (let* ((first (caar series))
             (last (car (car (last series))))
             (age (- ref-day last))
             (recent (window-values series (- ref-day 34) ref-day)))
        (format nil "~a: ~d days of data, first ~a, last ~a (~d day~:p ago), ~d of 35 days in the window, ~a"
                label (length series) (day-text first) (day-text last) age
                (length recent) (if (> age gap-days) "stale" "ok")))))

(defun coverage-report (source ref-day offset gap-days)
  "What the database holds, how fresh each metric is, and when it last synced."
  (let ((lines (loop for (type label unit) in *metrics*
                     collect (coverage-line type label (metric-series source type offset)
                                            ref-day gap-days))))
    (format nil "Coverage to ~a~%~{~a~%~}~@[Sync: the last run began ~a~%~]~a"
            (day-text ref-day) lines (getf source :sync)
            "A day without data is a day the phone did not sync, not a zero.")))

;;; --- concerns ------------------------------------------------------------------

(defparameter *level-words*
  '((0 . "all clear") (1 . "worth watching") (2 . "discuss with a clinician") (3 . "urgent"))
  "What a level says, in the words a finding leads with.")

(defparameter *rhr-rise* 8
  "bpm above the baseline median, for three days running, that counts as elevated.")

(defparameter *rhr-high* 100
  "A 7-day resting average above this bpm is worth a clinician.")

(defparameter *sleep-very-low* 5d0
  "A 7-day mean below this many hours of sleep is level 2.")

(defparameter *sleep-low* 6d0
  "A 7-day mean below this many hours, or 1.5 h under the baseline, is level 1.")

(defparameter *spo2-critical* 0.88d0
  "A reading below this fraction of oxygen saturation is level 3.")

(defparameter *spo2-low* 0.94d0
  "A reading below this fraction of oxygen saturation is level 1.")

(defun finding (id level title evidence advice)
  "One rule's verdict: what it read, and what the model may say about it."
  (list :id id :level level :title title :evidence evidence :advice advice))

(defun finding-line (finding)
  "A finding as one line: its level in words, its title, its evidence."
  (format nil "[~a] ~a - ~a"
          (or (cdr (assoc (getf finding :level) *level-words*)) "note")
          (getf finding :title) (getf finding :evidence)))

(defun consecutive-days-p (series from to predicate)
  "T when every day FROM through TO has a value and each one satisfies PREDICATE."
  (loop for day from from to to
        for value = (cdr (assoc day series))
        always (and value (funcall predicate value))))

(defun rhr-findings (source ref-day offset)
  "Resting heart rate: an average above the tachycardia line, or three days above baseline."
  (let* ((series (metric-series source "resting_heart_rate" offset))
         (recent (window-values series (- ref-day 6) ref-day))
         (baseline (window-values series (- ref-day 30) (- ref-day 3)))
         (average (mean recent))
         (high (and (>= (length recent) 4) (> average *rhr-high*)))
         (elevated (and (>= (length baseline) 14)
                         (consecutive-days-p series (- ref-day 2) ref-day
                                             (lambda (value)
                                               (>= value (+ (median baseline) *rhr-rise*)))))))
    (remove nil
            (list (when high
                    (finding "rhr_high_avg" 2 "Elevated 7-day average resting heart rate"
                             (format nil "7-day average ~d bpm at rest" (round average))
                             "Resting heart rate is high. Worth discussing with your clinician."))
                  (when (and elevated (not high))
                    (finding "rhr_elevated" 1 "Resting heart rate above its baseline"
                             (format nil "three days in a row at least ~d bpm above the 30-day median of ~d bpm"
                                     *rhr-rise* (round (median baseline)))
                             "Training, illness or poor sleep can do this. Worth watching."))))))

(defun hrv-findings (source ref-day offset)
  "Heart rate variability: five days running below the baseline median minus two sigmas."
  (let* ((series (metric-series source "heart_rate_variability_sdnn" offset))
         (baseline (window-values series (- ref-day 30) (- ref-day 3))))
    (when (>= (length baseline) 14)
      (let ((floor (- (median baseline) (* 2 (robust-sigma baseline)))))
        (when (consecutive-days-p series (- ref-day 4) ref-day
                                   (lambda (value) (< value floor)))
          (list (finding "hrv_low" 1 "Heart rate variability below its baseline"
                         (format nil "five days in a row below ~a ms, against a median of ~a ms"
                                 (number-text floor) (number-text (median baseline)))
                         "A low run can follow strain, poor sleep or illness. Worth watching.")))))))

(defun sleep-findings (source ref-day offset)
  "Sleep: a 7-day mean under five hours, or under six."
  (let* ((series (metric-series source "sleep_analysis" offset))
         (recent (window-values series (- ref-day 6) ref-day))
         (baseline (window-values series (- ref-day 34) (- ref-day 7)))
         (average (mean recent)))
    (when (>= (length recent) 3)
      (cond ((< average *sleep-very-low*)
             (list (finding "sleep_very_low" 2 "Sleeping under five hours a night"
                            (format nil "7-day average ~a h of sleep" (number-text average))
                            "That is a short week of sleep. Worth discussing with your clinician if it lasts.")))
            ((or (< average *sleep-low*)
                 (and (>= (length baseline) 5)
                      (>= (- (median baseline) average) 1.5d0)))
             (list (finding "sleep_low_avg" 1 "Sleeping under six hours a night"
                            (format nil "7-day average ~a h of sleep" (number-text average))
                            "Short sleep adds up. Worth watching, and worth mentioning to a clinician.")))))))

(defun bp-findings (source ref-day offset)
  "Blood pressure: a severe reading, or a 7-day average in stage 2 or stage 1."
  (let* ((readings (bp-readings source offset))
         (recent (remove-if (lambda (reading) (< (first reading) (- ref-day 1))) readings))
         (seven (remove-if (lambda (reading) (< (first reading) (- ref-day 6))) readings))
         (prior (remove-if-not (lambda (reading)
                                  (and (>= (first reading) (- ref-day 13))
                                       (< (first reading) (- ref-day 6))))
                                readings))
         (severe (find-if (lambda (reading)
                            (string= "Severe" (bp-category (second reading) (third reading))))
                          recent))
         (week (and (>= (length seven) 3)
                    (bp-category (mean (mapcar #'second seven)) (mean (mapcar #'third seven)))))
         (before (and (>= (length prior) 3)
                      (bp-category (mean (mapcar #'second prior)) (mean (mapcar #'third prior))))))
    (remove nil
            (list (when severe
                    (finding "bp_severe" 3 "Severe blood pressure reading"
                             (format nil "reading ~d/~d on ~a"
                                     (round (second severe)) (round (third severe))
                                     (day-text (first severe)))
                             "Sit quietly and recheck after five minutes. If it holds, or with chest pain, breathlessness or vision changes, seek care now."))
                  (when (and week (string= week "Stage 2"))
                    (finding "bp_stage2_avg" 2 "Seven-day blood pressure average in stage 2"
                             (format nil "7-day average ~d/~d mmHg"
                                     (round (mean (mapcar #'second seven)))
                                     (round (mean (mapcar #'third seven))))
                             "Worth discussing with your clinician."))
                  (when (and week (string= week "Stage 1")
                             (member before '("Stage 1" "Stage 2") :test #'string=))
                    (finding "bp_stage1_avg" 1 "Blood pressure average holding in stage 1"
                             (format nil "7-day average ~d/~d mmHg, and ~d/~d mmHg the week before"
                                     (round (mean (mapcar #'second seven)))
                                     (round (mean (mapcar #'third seven)))
                                     (round (mean (mapcar #'second prior)))
                                     (round (mean (mapcar #'third prior))))
                             "Two weeks in the stage 1 band is worth a routine check."))))))

(defun spo2-findings (source ref-day offset)
  "Oxygen saturation: any reading in the last two days under the low line."
  (let ((recent (loop for sample in (getf source :samples)
                       when (and (equal (getf sample :type) "oxygen_saturation")
                                 (getf sample :ts)
                                 (>= (local-day (getf sample :ts) offset) (- ref-day 1)))
                         collect (getf sample :value))))
    (let ((lowest (when recent (apply #'min recent))))
      (when lowest
        (cond ((< lowest *spo2-critical*)
               (list (finding "spo2_critical" 3 "Oxygen saturation below 88%"
                              (format nil "lowest reading ~a %" (number-text (* 100d0 lowest)))
                              "A reading this low, or any breathlessness with it, warrants prompt medical attention.")))
              ((< lowest *spo2-low*)
               (list (finding "spo2_low" 1 "Oxygen saturation below 94%"
                              (format nil "lowest reading ~a %" (number-text (* 100d0 lowest)))
                              "One low reading is common; a run of them is worth mentioning."))))))))

(defun gap-findings (source ref-day offset gap-days)
  "Metrics the database carries but has not received for more than GAP-DAYS."
  (let ((silent (loop for (type label unit) in *metrics*
                      for series = (metric-series source type offset)
                      for last = (and series (car (car (last series))))
                      when (and last (> (- ref-day last) gap-days))
                        collect (format nil "~a (~d days)" label (- ref-day last)))))
    (when silent
      (list (finding "data_gap" 1 "Some metrics have not synced"
                     (format nil "silent for more than ~d days: ~{~a~^, ~}" gap-days silent)
                     "The phone or the receiver may not be syncing. Until it does, the checks miss what it would have seen.")))))

(defun stale-findings (source ref-day offset gap-days)
  "The whole database silent for more than GAP-DAYS is a finding of its own."
  (let* ((instants (delete nil (append (mapcar (lambda (sample) (getf sample :ts))
                                               (getf source :samples))
                                       (mapcar #'cdr (getf source :sessions)))))
         (newest (when instants (apply #'max instants)))
         (age (and newest (- ref-day (local-day newest offset)))))
    (when (and age (> age gap-days))
      (list (finding "store_stale" 2 "The health data is stale"
                     (format nil "the newest record is from ~a, ~d days ago"
                             (day-text (local-day newest offset)) age)
                     "Absence of other findings does not mean things look fine: nothing recent was checked.")))))

(defun concerns-report (source ref-day offset gap-days)
  "Every finding the rules make, worst first; or the line that says none was."
  (let ((findings (sort (append (rhr-findings source ref-day offset)
                                 (hrv-findings source ref-day offset)
                                 (sleep-findings source ref-day offset)
                                 (bp-findings source ref-day offset)
                                 (spo2-findings source ref-day offset)
                                 (gap-findings source ref-day offset gap-days)
                                 (stale-findings source ref-day offset gap-days))
                         #'> :key (lambda (finding) (getf finding :level)))))
    (if findings
        (format nil "Concerns to ~a~%~{~a~%~}Rules are deterministic thresholds, not clinical judgement. Informational only, not medical advice."
                (day-text ref-day)
                (mapcar (lambda (finding)
                          (format nil "~a~%  ~a" (finding-line finding) (getf finding :advice)))
                        findings))
        (format nil "Concerns to ~a: nothing was flagged by the rules, and the data is current. Informational only, not medical advice."
                (day-text ref-day)))))

;;; --- one metric's days ---------------------------------------------------------

(defun daily-report (source type ref-day offset weight-unit days)
  "One metric's daily values over DAYS, with its 30- and 90-day medians."
  (let* ((known (assoc type *metrics* :test #'equal))
         (series (if known
                     (metric-series source type offset)
                     (daily-means (getf source :samples) type offset))))
    (unless series
      (fail "no ~a samples in the database: coverage names what it holds" type))
    (let* ((label (if known (second known) type))
           (unit (if known (third known) ""))
           (window (window-pairs series (- ref-day (1- days)) ref-day))
           (last-30 (window-values series (- ref-day 29) ref-day))
           (last-90 (window-values series (- ref-day 89) ref-day))
           (lines (mapcar (lambda (pair)
                            (multiple-value-bind (shown shown-unit)
                                (shown-value type (cdr pair) unit weight-unit)
                              (format nil "~a  ~a ~a" (day-text (car pair))
                                      (number-text shown) shown-unit)))
                          window)))
      (format nil "~a (~a), daily values to ~a~%~{~a~%~}30d median ~a ~a (~d days)~%90d median ~a ~a (~d days)"
              label type (day-text ref-day) lines
              (number-text (median last-30)) unit (length last-30)
              (number-text (median last-90)) unit (length last-90)))))
