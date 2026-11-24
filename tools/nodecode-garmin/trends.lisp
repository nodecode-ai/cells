;;;; trends.lisp --- deterministic baselines and trend flags, plus the training
;;;; load and the operator's two verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A flag needs BOTH a relative change of at least the threshold and a
;;;; z-score of at least one against the person's own baseline, so ordinary
;;;; day-to-day noise is not reported. The deviation uses a spread of at least
;;;; 2% of the baseline mean, so a very regular baseline cannot hide a real
;;;; change. That is the original's rule, carried over unchanged.

(in-package #:nodecode-garmin)

(defparameter +metrics+
  '(;; key label unit better digits
    ("hrv_ms" "overnight HRV" "ms" :higher 0)
    ("resting_hr_bpm" "resting heart rate" "bpm" :lower 0)
    ("sleep_hours" "sleep" "h" :higher 1)
    ("sleep_score" "sleep score" "" :higher 0)
    ("avg_stress" "average stress" "" :lower 0)
    ("body_battery_high" "Body Battery peak" "" :higher 0)
    ("steps" "steps" "" nil 0)
    ("intensity_minutes" "intensity minutes" "min" nil 0)))

(defparameter +min-baseline-days+ 5)
(defparameter +min-sd-fraction+ 0.02)

(defun mean-of (numbers)
  (when numbers (/ (reduce #'+ numbers) (length numbers))))

(defun sd-of (numbers)
  (when (> (length numbers) 1)
    (let ((mean (mean-of numbers)))
      (sqrt (/ (reduce #'+ (mapcar (lambda (value) (expt (- value mean) 2)) numbers))
               (1- (length numbers)))))))

(defun pct-change (value reference)
  (when (and (realp value) (realp reference) (/= reference 0))
    (/ (* 100.0d0 (- value reference)) (abs reference))))

(defun fmt-value (value unit digits)
  (let ((number (if (zerop digits) (n0 value) (n1 value digits))))
    (if (plusp (length unit)) (format nil "~a ~a" number unit) number)))

(defun verdict-of (change better)
  (cond ((null better) "neutral")
        ((if (eq better :higher) (plusp change) (minusp change)) "favourable")
        (t "unfavourable")))

(defun analyze-metric (key points as-of threshold-z &key (threshold 10.0))
  "KEY's stats and flags over POINTS -- (DAY . VALUE) -- as of AS-OF.
=> (values TEXT FLAGS), FLAGS oldest-verdict-first."
  (destructuring-bind (name label unit better digits) (assoc key +metrics+ :test #'string=)
    (declare (ignore name))
    (let* ((points (remove-if-not (lambda (point) (and (realp (cdr point)) (string<= (car point) as-of)))
                                  points))
           (points (sort (copy-list points) #'string< :key #'car))
           (values (mapcar #'cdr points))
           (days (mapcar #'car points)))
      (when (null values)
        (return-from analyze-metric (values nil nil)))
      (let* ((latest (car (last values)))
             (latest-day (car (last days)))
             (baseline (butlast values))
             (last7 (loop for (day . value) in points
                           when (string> day (iso-add as-of -7)) collect value))
             (last28 (loop for (day . value) in points
                            when (string> day (iso-add as-of -28)) collect value))
             (flags '())
             (stats (list (format nil "~a ~a on ~a" label (fmt-value latest unit digits) latest-day)
                          (format nil "mean ~a" (fmt-value (mean-of values) unit digits)))))
        (when last7
          (setf stats (append stats (list (format nil "7d ~a" (fmt-value (mean-of last7) unit digits))))))
        (when (>= (length baseline) +min-baseline-days+)
          (let* ((base (mean-of baseline))
                 (spread (sd-of baseline))
                 (floor (max (or spread 0) (* (abs base) +min-sd-fraction+)))
                 (change (pct-change latest base))
                 (z (and (plusp floor) (/ (- latest base) floor))))
            (when (and change (or (null spread) (>= (abs change) threshold)))
              (setf stats (append stats (list (format nil "baseline ~a" (fmt-value base unit digits))
                                              (format nil "z ~,1f" z)))))
            (when (and change z (>= (abs change) threshold) (>= (abs z) threshold-z))
              (push (format nil "~a: ~a ~d% ~a baseline (~a vs ~a, z=~,1f)"
                            (verdict-of change better) label (round (abs change))
                            (if (plusp change) "above" "below")
                            (fmt-value latest unit digits) (fmt-value base unit digits) z)
                    flags))))
        (when (and (>= (length last7) 3) (>= (length last28) 14))
          (let* ((week (mean-of last7)) (month (mean-of last28))
                 (change (pct-change week month)))
            (when (and change (>= (abs change) threshold))
              (push (format nil "~a: 7-day ~a average ~d% ~a the 28-day average (~a vs ~a)"
                            (verdict-of change better) label (round (abs change))
                            (if (plusp change) "above" "below")
                            (fmt-value week unit digits) (fmt-value month unit digits))
                    flags))))
        (when (string< latest-day (iso-add as-of -1))
          (setf stats (append stats (list (format nil "stale: latest is from ~a" latest-day)))))
        (values (format nil "~a: ~{~a~^, ~}" label stats) (nreverse flags))))))

(defun series-from-days (rows key)
  "(DAY . VALUE) for every daily summary ROW carrying KEY."
  (loop for row in rows
        for day = (text (at row "calendarDate"))
        for value = (at row key)
        when (and day (realp value)) collect (cons day value)))

(defun trends (call)
  "Deterministic baselines over the last N days: mean, 7-day against
28-day, the z-score of the latest value and plain-language flags. Call this
before concluding anything about recovery, fatigue or progress."
  (answer
    (let* ((days (or (garmin-trends-days call) 28))
           (as-of (parse-day (garmin-trends-as-of call)))
           (threshold (or (garmin-trends-threshold-pct call) 10.0))
           (start (iso-add as-of (- 1 days)))
           (summaries (maybe (lambda () (daily-summaries start as-of))))
           (sleeps (maybe (lambda () (sleep-nights start as-of))))
           (hrvs (maybe (lambda () (hrv-nights start as-of))))
           (series (list (cons "hrv_ms" (loop for row in hrvs
                                                when (realp (at row "lastNightAvg"))
                                                  collect (cons (text (at row "calendarDate"))
                                                                (at row "lastNightAvg"))))
                         (cons "resting_hr_bpm" (series-from-days summaries "restingHeartRate"))
                         (cons "sleep_hours" (loop for row in sleeps
                                                    when (realp (at row "sleepTimeSeconds"))
                                                      collect (cons (text (at row "calendarDate"))
                                                                    (/ (at row "sleepTimeSeconds") 3600.0d0))))
                         (cons "sleep_score" (loop for row in sleeps
                                                    for score = (at (at row "sleepScores" "overall") "value")
                                                    when (realp score) collect (cons (text (at row "calendarDate")) score)))
                         (cons "avg_stress" (series-from-days summaries "averageStressLevel"))
                         (cons "body_battery_high" (series-from-days summaries "bodyBatteryHighestValue"))
                         (cons "steps" (series-from-days summaries "totalSteps"))
                         (cons "intensity_minutes" (series-from-days summaries "moderateIntensityMinutes")))))
      (let ((stats '()) (flags '()))
        (dolist (pair series)
          (multiple-value-bind (text metric-flags)
              (analyze-metric (car pair) (cdr pair) as-of 1.0 :threshold threshold)
            (when text (push text stats))
            (setf flags (append flags metric-flags))))
        (lines (format nil "garmin trends ~a..~a (baseline = earlier days in the window; flagged at >= ~,1f% and |z| >= 1)"
                       start as-of threshold)
               (if flags
                   (format nil "FLAGS~%~{~a~^~%~}" (sort flags #'string<))
                   "no flags: every metric is within its usual range.")
               (format nil "~{~a~^~%~}" (nreverse stats)))))))

(defun training-load (call)
  "Intensity by heart-rate zone over the last N days: minutes per zone, the
share of easy, moderate and hard time, the same per activity type, and the
intensity minutes against the weekly goal."
  (answer
    (let* ((days (or (garmin-load-days call) 7))
           (as-of (parse-day (garmin-load-as-of call)))
           (start (iso-add as-of (- 1 days)))
           (rows (maybe (lambda () (activities-between start as-of :limit 100))))
           (summary (maybe (lambda () (daily-summaries start as-of))))
           (zones (loop for zone from 1 to 5
                        collect (reduce #'+ (loop for row in rows
                                                  for seconds = (at row (format nil "hrTimeInZone_~d" zone))
                                                  when (realp seconds) collect seconds))
                        into totals finally (return totals)))
           (minutes (mapcar (lambda (seconds) (round seconds 60)) zones))
           (zoned (reduce #'+ minutes))
           (easy (+ (nth 0 minutes) (nth 1 minutes)))
           (hard (+ (nth 3 minutes) (nth 4 minutes)))
           (intensity (reduce #'+ (mapcar (lambda (row)
                                            (+ (or (at row "moderateIntensityMinutes") 0)
                                               (* 2 (or (at row "vigorousIntensityMinutes") 0))))
                                          summary))))
      (lines (format nil "garmin training load ~a..~a (~d day~:p): ~d activit~p"
                     start as-of days (length rows) (length rows))
             (format nil "zones: z1 ~d  z2 ~d  z3 ~d  z4 ~d  z5 ~d minutes"
                     (nth 0 minutes) (nth 1 minutes) (nth 2 minutes) (nth 3 minutes) (nth 4 minutes))
             (when (plusp zoned)
               (format nil "easy (z1-2) ~d%  moderate (z3) ~d%  hard (z4-5) ~d%"
                       (round (* 100 easy) zoned) (round (* 100 (nth 2 minutes)) zoned)
                       (round (* 100 hard) zoned)))
             (format nil "intensity minutes ~d~@[ of ~d~@[ (~d% of the goal)~]~]"
                     intensity (and summary (at (car (last summary)) "intensityMinutesGoal"))
                     (let ((goal (and summary (at (car (last summary)) "intensityMinutesGoal"))))
                       (and (realp goal) (plusp goal) (round (* 100 intensity) goal))))
             (when rows
               (format nil "by type: ~{~a~^, ~}"
                       (let ((counts '()))
                         (dolist (row rows)
                           (let ((type (or (text (at row "activityType" "typeKey")) "unknown")))
                             (setf (getf counts type) (1+ (or (getf counts type) 0)))))
                         (loop for (type count) on counts by #'cddr
                               collect (format nil "~a ~d" type count)))))
             (unless rows "no recorded activity with heart rate in this window.")))))

;;; --- the operator's verbs ------------------------------------------------------------

(defun status ()
  "Where the key comes from, which server, and whether it answers. No network:
/garmin check asks the token endpoint instead."
  (multiple-value-bind (key) (values (api-key))
    (let* ((name (or (ignore-errors (setting :api-key-env)) "GARMIN_PERSONAL_API_KEY"))
           (region (string-downcase (princ-to-string (or (ignore-errors (setting :region)) "global"))))
           (where (cond ((null key) (format nil "no key: set api_key, or ~a" name))
                        ((and (stringp (ignore-errors (setting :api-key)))
                              (plusp (length (nlk:trimmed (or (ignore-errors (setting :api-key)) "")))))
                         "key from this cell's api_key setting")
                        (t (format nil "key from ~a" name)))))
      (format nil "garmin: ~a; server ~a (~a); 14 read-only tools, and (help :garmin) explains them"
              where region (base-url)))))

(defun tool-list ()
  "One line naming the tools, for /garmin help."
  (format nil "garmin_today, garmin_sleep, garmin_hrv, garmin_body_battery, garmin_stress, garmin_steps, ~
garmin_heart_rate, garmin_activities, garmin_activity, garmin_activity_fit, garmin_body, garmin_trends, ~
garmin_training_load, garmin_fitness_age"))

(defun run-command (args session-id)
  "ARGS is what was typed after /garmin: status, today, help, nothing."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((zerop (length verb)) (status))
          ((string= verb "status") (status))
          ((string= verb "help")
           (format nil "~a~%~%~a" *primer* (tool-list)))
          ((string= verb "today")
           (handler-case (today (decode-garmin-day (nlk:json-object)))
             (garmin-error (condition) (format nil "ERROR: ~a" condition))))
          (t (format nil "garmin: ~s is not a verb. Try status, today, or help." verb)))))
