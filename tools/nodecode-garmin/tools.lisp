;;;; tools.lisp --- the fourteen read verbs, one Garmin endpoint each, and
;;;; the baselines they are read against.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every verb answers a string: a header line naming the days it covers, then
;;;; one line per day (or per activity, per night). The original answered JSON;
;;;; these are the same numbers as labelled fields, which is what a model reads
;;;; anyway and what the original's own summaries were before they were
;;;; serialized. A failure is GARMIN-ERROR, which ANSWER turns into the one
;;;; line the model sees.

(in-package #:nodecode-garmin)

(defmacro answer (&body body)
  "BODY's value, or its GARMIN-ERROR as the one line the model reads."
  `(handler-case (progn ,@body)
     (garmin-error (condition) (format nil "ERROR: ~a" condition))))

(defun maybe (thunk)
  "THUNK's value, or NIL when Garmin could not answer that part: one missing
endpoint in a joined answer is not the whole answer's failure."
  (handler-case (funcall thunk)
    (garmin-error () nil)))

;;; --- the endpoints, as the original spelled them -------------------------------------

(defun daily-summary (day)
  (garmin-get "/usersummary-service/usersummary/daily"
              :params (list (cons "calendarDate" day))))

(defun daily-summaries (start end)
  (items (garmin-get "/usersummary-service/v2/dailysummaries"
                      :params (list (cons "startDate" start) (cons "endDate" end)))
         "data"))

(defun sleep-nights (start end)
  (items (garmin-get "/sleep-service/sleep/dailySleepSummariesByDate"
                      :params (list (cons "startDate" start) (cons "endDate" end)))))

(defun sleep-night (day)
  (garmin-get "/sleep-service/sleep/dailySleep" :params (list (cons "date" day))))

(defun hrv-nights (start end)
  (items (garmin-get (format nil "/hrv-service/hrv/daily/~a/~a" start end)) "hrvSummaries"))

(defun hrv-night (day)
  (garmin-get (format nil "/hrv-service/hrv/~a" day)))

(defun body-battery-reports (start end)
  (items (garmin-get "/wellness-service/wellness/bodyBattery/reports/daily"
                      :params (list (cons "startDate" start) (cons "endDate" end)))))

(defun stress-days (start end)
  (items (garmin-get (format nil "/usersummary-service/stats/stress/daily/~a/~a" start end))))

(defun steps-days (start end)
  (items (garmin-get (format nil "/usersummary-service/stats/steps/daily/~a/~a" start end))))

(defun heart-rate-day (day)
  (garmin-get "/wellness-service/wellness/dailyHeartRate" :params (list (cons "date" day))))

(defun weights (start end)
  (items (garmin-get (format nil "/weight-service/weight/range/~a/~a" start end)
                      :params (list (cons "includeAll" "true")))
         "dailyWeightSummaries"))

(defun hydration-days (start end)
  (items (garmin-get (format nil "/usersummary-service/stats/hydration/daily/~a/~a" start end))))

(defun activities-between (start end &key (limit 10) activity-type search)
  (let ((page (garmin-get "/activitylist-service/activities/search/activities"
                           :params (list (cons "startDate" start) (cons "endDate" end)
                                         (cons "sortOrder" "desc")
                                         (cons "limit" limit)
                                         (cons "activityType" (text activity-type))
                                         (cons "search" (text search))))))
    (let ((rows (if (vectorp page) (coerce page 'list) nil)))
      (when (and (text activity-type) (null rows))
        ;; Garmin's filter answers 400 or nothing for a type it does not know;
        ;; the original matched among the newest activities instead.
        (setf rows (remove-if-not
                    (lambda (row)
                      (string-equal (or (text (at row "activityType" "typeKey")) "")
                                    (string-downcase (nlk:trimmed activity-type))))
                    (activities-between start end :limit 100))))
      (if (and (text activity-type) rows)
          (remove-if-not
           (lambda (row)
             (string-equal (text (at row "activityType" "typeKey")) (string-downcase (nlk:trimmed activity-type))))
           rows)
          rows))))

(defun activity-detail (activity-id)
  (garmin-get (format nil "/activity-service/activity/~a" activity-id)))

(defun fitness-age-of (day)
  (let ((data (garmin-get (format nil "/fitnessage-service/fitnessage/v2/~a" day))))
    (if (vectorp data) (first (coerce data 'list)) data)))

;;; --- reading one record -------------------------------------------------------------

(defun sleep-line (night)
  (let ((scores (at night "sleepScores")))
    (fields (one "date" (text (at night "calendarDate")))
            (one "score" (n0 (at scores "overall" "value")))
            (one "total" (hmm (at night "sleepTimeSeconds")))
            (one "deep" (hmm (at night "deepSleepSeconds")))
            (one "rem" (hmm (at night "remSleepSeconds")))
            (one "awake" (hmm (at night "awakeSleepSeconds")))
            (one "avg_hr" (n0 (at night "avgHeartRate")))
            (one "spo2" (n0 (at night "averageSpO2Value")))
            (one "stress" (n0 (at night "avgSleepStress"))))))

(defun hrv-line (night)
  (let ((baseline (at night "baseline")))
    (fields (one "date" (text (at night "calendarDate")))
            (one "last_night" (n0 (at night "lastNightAvg")))
            (one "weekly_avg" (n0 (at night "weeklyAvg")))
            (one "status" (text (at night "status")))
            (one "balanced" (when (and (realp (at baseline "balancedLow")) (realp (at baseline "balancedUpper")))
                               (format nil "~a-~a" (n0 (at baseline "balancedLow"))
                                       (n0 (at baseline "balancedUpper")))))
            (one "5min_high" (n0 (at night "lastNight5MinHigh")))
            (one "feedback" (text (at night "feedbackPhrase"))))))

(defun battery-line (report)
  (let ((series (levels (at report "bodyBatteryValuesArray"))))
    (fields (one "date" (text (at report "date")))
            (one "charged" (n0 (at report "charged")))
            (one "drained" (n0 (at report "drained")))
            (when series (one "high" (n0 (reduce #'max series))))
            (when series (one "low" (n0 (reduce #'min series))))
            (when series (one "latest" (n0 (car (last series))))))))

(defun stress-line (day)
  (let ((values (at day "values")))
    (fields (one "date" (text (at day "calendarDate")))
            (one "avg" (n0 (at values "overallStressLevel")))
            (one "high" (hmm (at values "highStressDuration")))
            (one "medium" (hmm (at values "mediumStressDuration")))
            (one "low" (hmm (at values "lowStressDuration")))
            (one "rest" (hmm (at values "restStressDuration"))))))

(defun steps-line (day)
  (fields (one "date" (text (at day "calendarDate")))
          (one "steps" (n0 (at day "totalSteps")))
          (one "goal" (n0 (at day "stepGoal")))
          (one "km" (km (at day "totalDistance")))))

(defun weight-line (summary)
  (let ((latest (at summary "latestWeight")))
    (fields (one "date" (or (text (at summary "summaryDate")) (text (at latest "calendarDate"))))
            (one "kg" (when (realp (at latest "weight")) (n1 (/ (at latest "weight") 1000.0d0) 2)))
            (one "bmi" (n1 (at latest "bmi")))
            (one "body_fat" (n1 (at latest "bodyFat")))
            (one "body_water" (n1 (at latest "bodyWater")))
            (one "muscle_kg" (when (realp (at latest "muscleMass")) (n1 (/ (at latest "muscleMass") 1000.0d0) 2))))))

(defun activity-line (row)
  (fields (one "id" (n0 (at row "activityId")))
          (one "type" (text (at row "activityType" "typeKey")))
          (one "name" (text (at row "activityName")))
          (one "start" (text (at row "startTimeLocal")))
          (one "km" (km (at row "distance")))
          (one "time" (hms (at row "duration")))
          (one "avg_hr" (n0 (at row "averageHR")))
          (one "max_hr" (n0 (at row "maxHR")))
          (one "load" (n0 (at row "activityTrainingLoad")))
          (one "aerobic_te" (n1 (at row "aerobicTrainingEffect")))
          (one "kcal" (n0 (at row "calories")))))

;;; --- the fourteen verbs --------------------------------------------------------------

(defun today (day)
  "Today's, or one day's, overview: steps, heart rate, stress, Body Battery,
last night's sleep and HRV."
  (answer
    (let* ((date (parse-day (garmin-day-date day)))
           (summary (daily-summary date))
           (night (maybe (lambda () (sleep-night date))))
           (hrv (maybe (lambda () (hrv-night date))))
           (battery (at summary "bodyBatteryMostRecentValue")))
      (lines (format nil "garmin ~a~@[ (last sync ~a)~]" date
                     (text (at summary "lastSyncTimestampGMT")))
             (fields (one "steps" (n0 (at summary "totalSteps")))
                     (one "of" (n0 (at summary "dailyStepGoal")))
                     (one "km" (km (at summary "totalDistanceMeters")))
                     (one "resting_hr" (n0 (at summary "restingHeartRate")))
                     (one "7d_resting_hr" (n0 (at summary "lastSevenDaysAvgRestingHeartRate"))))
             (fields (one "stress" (n0 (at summary "averageStressLevel")))
                     (one "body_battery" (n0 battery))
                     (one "at_wake" (n0 (at summary "bodyBatteryAtWakeTime")))
                     (one "intensity_min" (n0 (at summary "moderateIntensityMinutes"))))
             (and night (sleep-line night))
             (and hrv (hrv-line hrv))))))

(defun sleep (day)
  "Sleep per night: score, total, stages, heart rate, SpO2 and sleep stress,
over several nights, or one night in detail."
  (answer
    (let* ((range (window (garmin-window-start-date day) (garmin-window-end-date day)
                           (garmin-window-days day)))
           (start (car range)) (end (cdr range)))
      (if (garmin-window-detail day)
          (let ((night (sleep-night end)))
            (if (or (null night) (null (at night "sleepTimeSeconds")))
                (format nil "garmin: no sleep recorded for the night ending ~a." end)
                (lines (format nil "garmin sleep ~a (one night)" end)
                       (sleep-line night)
                       (fields (one "bedtime" (text (at night "sleepStartTimestampLocal")))
                               (one "wake" (text (at night "sleepEndTimestampLocal")))
                               (one "light" (hmm (at night "lightSleepSeconds")))
                               (one "respiration" (n1 (at night "averageRespirationValue")))
                               (one "lowest_spo2" (n0 (at night "lowestSpO2Value")))
                               (one "awake_count" (n0 (at night "awakeCount")))
                               (one "feedback" (text (at night "sleepScoreFeedback")))))))
          (let* ((nights (remove-if-not (lambda (row) (realp (at row "sleepTimeSeconds")))
                                        (sleep-nights start end)))
                 (totals (mapcar (lambda (row) (at row "sleepTimeSeconds")) nights))
                 (scores (mapcar (lambda (row) (at (at row "sleepScores" "overall") "value")) nights)))
            (lines (format nil "garmin sleep ~a..~a: ~d night~:p" start end (length nights))
                   (loop for night in nights collect (sleep-line night))
                   (when (> (length nights) 1)
                     (fields (one "average_total" (hmm (/ (reduce #'+ totals) (length totals))))
                             (one "average_score" (n0 (/ (reduce #'+ (remove nil scores))
                                                          (max 1 (length (remove nil scores))))))))))))))

(defun hrv (day)
  "Overnight heart-rate variability per night with Garmin's status and the
person's own balanced range; detail reads one night's readings."
  (answer
    (let* ((range (window (garmin-window-start-date day) (garmin-window-end-date day)
                           (garmin-window-days day)))
           (start (car range)) (end (cdr range)))
      (if (garmin-window-detail day)
          (let ((night (hrv-night end)))
            (if (null night)
                (format nil "garmin: no overnight HRV for the night ending ~a." end)
                (lines (format nil "garmin hrv ~a (one night)" end)
                       (hrv-line (at night "hrvSummary"))
                       (fields (one "readings" (length (items night "hrvReadings")))))))
          (let ((nights (hrv-nights start end)))
            (lines (format nil "garmin hrv ~a..~a: ~d night~:p" start end (length nights))
                   (loop for night in nights collect (hrv-line night))
                   "HRV is individual: compare it with the balanced range above, not with other people."))))))

(defun body-battery (day)
  "Body Battery per day: charged, drained, high, low and latest."
  (answer
    (let* ((range (window (garmin-window-start-date day) (garmin-window-end-date day)
                           (garmin-window-days day)))
           (reports (body-battery-reports (car range) (cdr range))))
      (lines (format nil "garmin body battery ~a..~a: ~d day~:p" (car range) (cdr range) (length reports))
             (loop for report in reports collect (battery-line report))))))

(defun stress (day)
  "Garmin's stress score per day with the time spent in each band."
  (answer
    (let* ((range (window (garmin-window-start-date day) (garmin-window-end-date day)
                           (garmin-window-days day)))
           (rows (stress-days (car range) (cdr range))))
      (lines (format nil "garmin stress ~a..~a: ~d day~:p" (car range) (cdr range) (length rows))
             (loop for row in rows collect (stress-line row))))))

(defun steps (day)
  "Daily steps, the step goal and the distance walked."
  (answer
    (let* ((range (window (garmin-window-start-date day) (garmin-window-end-date day)
                           (garmin-window-days day)))
           (rows (steps-days (car range) (cdr range))))
      (lines (format nil "garmin steps ~a..~a: ~d day~:p" (car range) (cdr range) (length rows))
             (loop for row in rows collect (steps-line row))))))

(defun heart-rate (day)
  "One day's resting, minimum and maximum heart rate, optionally the hourly mean."
  (answer
    (let* ((date (parse-day (garmin-hr-date day)))
           (data (heart-rate-day date))
           (hourly (garmin-hr-hourly day))
           (readings (items data "heartRateValues")))
      (lines (format nil "garmin heart rate ~a" date)
             (fields (one "resting" (n0 (at data "restingHeartRate")))
                     (one "7d_resting" (n0 (at data "lastSevenDaysAvgRestingHeartRate")))
                     (one "min" (n0 (at data "minHeartRate")))
                     (one "max" (n0 (at data "maxHeartRate"))))
             (when (and hourly readings)
               (format nil "readings: ~{~a~^, ~}"
                       (loop for reading in (subseq readings 0 (min 24 (length readings)))
                             collect (n0 (if (vectorp reading) (aref reading 1) reading)))))))))

(defun activities (call)
  "The person's recorded activities, newest first: type, distance, time, heart
rate, training effect. Filter by dates, type or a word in the name."
  (answer
    (let* ((range (window (garmin-activities-start-date call) (garmin-activities-end-date call)
                           (garmin-activities-days call) :default-days 30))
           (limit (or (garmin-activities-limit call) 10))
           (rows (activities-between (car range) (cdr range)
                                     :limit limit
                                     :activity-type (garmin-activities-activity-type call)
                                     :search (garmin-activities-search-text call))))
      (lines (format nil "garmin activities ~a..~a: ~d of at most ~d"
                     (car range) (cdr range) (length rows) limit)
             (loop for row in rows collect (activity-line row))
             (when (null rows) "none recorded in this range.")))))

(defun activity (call)
  "One activity in full: pace, heart rate, cadence, power, training effect,
load and the time spent in each heart-rate zone."
  (answer
    (let* ((id (garmin-activity-activity-id call))
           (data (activity-detail id))
           (type (text (at data "activityType" "typeKey"))))
      (when (null data) (fail "garmin: Garmin has no activity ~a." id))
      (lines (fields (one "id" (n0 id))
                     (one "type" type)
                     (one "name" (text (at data "activityName")))
                     (one "start" (text (at data "startTimeLocal"))))
             (fields (one "km" (km (at data "distance")))
                     (one "time" (hms (at data "duration")))
                     (one "moving" (hms (at data "movingDuration")))
                     (one "elevation_m" (n0 (at data "elevationGain"))))
             (fields (one "avg_hr" (n0 (at data "averageHR")))
                     (one "max_hr" (n0 (at data "maxHR")))
                     (one "kcal" (n0 (at data "calories")))
                     (one "avg_power" (n0 (at data "avgPower"))))
             (fields (one "aerobic_te" (n1 (at data "aerobicTrainingEffect")))
                     (one "anaerobic_te" (n1 (at data "anaerobicTrainingEffect")))
                     (one "load" (n0 (at data "activityTrainingLoad")))
                     (one "vo2max" (n1 (at data "vO2MaxValue"))))
             (format nil "zones: ~{~a~^  ~}"
                     (loop for zone from 1 to 5
                           collect (one (format nil "z~d" zone) (hmm (at data (format nil "hrTimeInZone_~d" zone))))))))))

(defun activity-fit (call)
  "One activity's original file (a ZIP, usually a .fit) saved under the home's
nodecode folder, and the path, for lap or second-by-second analysis."
  (answer
    (let* ((id (garmin-activity-fit-activity-id call))
           (folder (ensure-directories-exist (home-path ".nodecode/garmin/activity-files/")))
           (path (merge-pathnames (format nil "~a.zip" id) folder))
           (bytes (download (format nil "/download-service/files/activity/~a" id))))
      (with-open-file (out path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (write-sequence bytes out))
      (format nil "garmin: saved ~d bytes of activity ~a to ~a (a ZIP with the original .fit file)."
              (length bytes) id path))))

(defun body (day)
  "Weight, BMI, body fat, water and muscle mass per day, with logged water intake."
  (answer
    (let* ((range (window (garmin-window-start-date day) (garmin-window-end-date day)
                           (garmin-window-days day) :default-days 30))
           (start (car range)) (end (cdr range))
           (weights (weights start end))
           (water (hydration-days start end)))
      (lines (format nil "garmin body ~a..~a" start end)
             (if weights
                 (loop for summary in weights collect (weight-line summary))
                 "no weight entries in this range (they come from a scale or manual entries).")
             (loop for row in water
                   collect (fields (one "date" (text (at row "calendarDate")))
                                   (one "water_ml" (n0 (at row "valueInML")))
                                   (one "goal_ml" (n0 (at row "goalInML"))) ))))))

(defun fitness-age (day)
  "Garmin's fitness age against the person's own age, and the inputs behind it."
  (answer
    (let* ((date (parse-day (garmin-day-date day)))
           (data (fitness-age-of date))
           (components (at data "components")))
      (when (null data)
        (fail "garmin: Garmin has no fitness age for this account yet (it needs a birth date and some activity with heart rate)."))
      (lines (format nil "garmin fitness age as of ~a" date)
             (if (realp (at data "fitnessAge"))
                 (fields (one "fitness_age" (n1 (at data "fitnessAge")))
                         (one "age" (n0 (at data "chronologicalAge")))
                         (one "previous" (n1 (at data "previousFitnessAge")))
                         (one "achievable" (n1 (at data "achievableFitnessAge"))))
                 (format nil "Garmin returned no fitness age (~a)." (text (at data "invalidReason"))))
             (when (hash-table-p components)
               (loop for key being the hash-keys of components using (hash-value part)
                     collect (fields (one "component" key)
                                     (one "value" (n1 (at part "value")))
                                     (one "target" (n1 (at part "targetValue"))))))))))
