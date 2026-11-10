;;;; health.lisp --- the Apple Health half: samples in, deterministic numbers out.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original imports an Apple Health export.zip into SQLite through a
;;;; background job; this rewrite reads the export.xml the user already
;;;; unzipped, line by line, and keeps one row per metric per day in
;;;; samples.tsv. That is enough for what the trend tool promises: a series,
;;;; a before-and-after around a date or a medication's start, a measurement
;;;; over time, and the latest day against the user's own range. Nothing is
;;;; inferred: every answer says which days it covers and what unit it is in.

(in-package #:nodecode-caduceus)

(defparameter +metric-table+
  '(("StepCount" "steps" :sum "count" nil)
    ("DistanceWalkingRunning" "distance_km" :sum "km" (("mi" . 1.609344)))
    ("DistanceCycling" "distance_cycling_km" :sum "km" (("mi" . 1.609344)))
    ("ActiveEnergyBurned" "active_energy_kcal" :sum "kcal" (("kJ" . 0.2390057)))
    ("BasalEnergyBurned" "basal_energy_kcal" :sum "kcal" (("kJ" . 0.2390057)))
    ("HeartRate" "heart_rate" :mean "count/min" nil)
    ("RestingHeartRate" "resting_heart_rate" :mean "count/min" nil)
    ("HeartRateVariabilitySDNN" "hrv_ms" :mean "ms" nil)
    ("BodyMass" "weight_kg" :mean "kg" (("lb" . 0.45359237)))
    ("BodyFatPercentage" "body_fat_percent" :mean "%" nil)
    ("BodyTemperature" "body_temperature_c" :mean "degC" (("degF" . 0.5555556)))
    ("BloodGlucose" "blood_glucose" :mean "mg/dL" nil)
    ("OxygenSaturation" "oxygen_saturation" :mean "%" nil)
    ("RespiratoryRate" "respiratory_rate" :mean "count/min" nil)
    ("SleepAnalysis" "sleep_hours" :sum "h" nil))
  "The Health record types this cell keeps, as (BARE-TYPE METRIC AGGREGATION
UNIT CONVERSIONS). A type not listed keeps its own snake_case name, averaged.")

(defun snake-case (text)
  "TEXT in snake_case, for a metric name nothing here named."
  (string-downcase
   (with-output-to-string (out)
     (loop for char across (princ-to-string (or text ""))
           for previous = nil then char
           do (cond ((or (char= char #\Space) (char= char #\-) (char= char #\_))
                     (write-char #\_ out))
                    ((and previous (upper-case-p char) (lower-case-p previous))
                     (write-char #\_ out) (write-char char out))
                    (t (write-char char out)))))))

(defun bare-type (type)
  "TYPE without the HK… prefix an export writes."
  (or (loop for prefix in '("HKQuantityTypeIdentifier" "HKCategoryTypeIdentifier"
                            "HKDataTypeTypeIdentifier" "HKDataTypeIdentifier"
                            "HKWorkoutTypeIdentifier")
            when (search prefix type) return (subseq type (length prefix)))
      type))

(defun metric-info (type)
  "TYPE's (METRIC AGGREGATION UNIT CONVERSIONS)."
  (let ((bare (bare-type type)))
    (or (rest (assoc bare +metric-table+ :test #'string=))
        (list (snake-case bare) :mean nil nil))))

;;; --- the XML, one element a line --------------------------------------------------

(defun attribute (line name)
  "LINE's attribute NAME, or NIL. An export writes one element per line, and
this reads the attributes off that line without a full XML tree."
  (let* ((needle (format nil "~a=\"" name))
         (start (search needle line :test #'char-equal)))
    (when start
      (let ((from (+ start (length needle)))
            (to (position #\" line :start (+ start (length needle)))))
        (and to (subseq line from to))))))

(defun iso-seconds (stamp)
  "STAMP, an Apple Health date-time, as universal time, or NIL."
  (when (and (stringp stamp) (>= (length stamp) 19))
    (handler-case
        (encode-universal-time
         (parse-integer stamp :start 17 :end 19)
         (parse-integer stamp :start 14 :end 16)
         (parse-integer stamp :start 11 :end 13)
         (parse-integer stamp :start 8 :end 10)
         (parse-integer stamp :start 5 :end 7)
         (parse-integer stamp :start 0 :end 4) 0)
      (error () nil))))

(defun import-apple-health (path)
  "Read PATH's export.xml and keep one row per metric per day."
  (let ((values (make-hash-table :test #'equal))
        (units (make-hash-table :test #'equal))
        (rules (make-hash-table :test #'equal))
        (records 0))
    (with-open-file (in path :direction :input :external-format :utf-8)
      (loop for line = (read-line in nil nil)
            while line
            when (search "<Record " line)
              do (let ((type (attribute line "type"))
                       (value (attribute line "value"))
                       (unit (attribute line "unit"))
                       (start (attribute line "startDate"))
                       (end (attribute line "endDate")))
                   (when (and type start (>= (length start) 10))
                     (let* ((day (subseq start 0 10))
                            (info (metric-info type))
                            (metric (first info))
                            (aggregation (second info))
                            (fixed-unit (third info))
                            (conversions (fourth info))
                            (number (and value (ignore-errors (read-from-string value))))
                            (hours (and end (string= metric "sleep_hours")
                                        (let ((from (iso-seconds start)) (to (iso-seconds end)))
                                          (and from to (max 0 (/ (- to from) 3600.0))))))
                            (factor (or (cdr (assoc unit conversions :test #'string=)) 1)))
                       (setf (gethash metric units) (or fixed-unit unit metric))
                       (setf (gethash metric rules) aggregation)
                       (cond (hours
                              (incf records)
                              (push hours (gethash (list metric day) values)))
                             ((numberp number)
                              (incf records)
                              (push (* number factor) (gethash (list metric day) values)))))))))
    (let ((rows '()))
      (maphash (lambda (key numbers)
                 (destructuring-bind (metric day) key
                   (let* ((listed (nreverse numbers))
                          (value (if (eq (gethash metric rules) :sum)
                                     (reduce #'+ listed)
                                     (/ (reduce #'+ listed) (length listed)))))
                     (push (list metric day (format nil "~,3f" (float value))
                                 (or (gethash metric units) ""))
                           rows))))
               values)
      (write-rows (vault-file +samples-file+) (sort rows #'string< :key #'second))
      (append-source path "Apple Health export" (length rows))
      (nlk:json-object
       "imported" (file-namestring path)
       "records_read" records
       "metric_days" (length rows)
       "metrics" (coerce (sort (remove-duplicates (mapcar #'first rows) :test #'string=)
                                #'string<)
                         'vector)
       "note" "one row per metric per day: sums for steps, distance, energy and sleep; means for rates, weight and percentages"))))

;;; --- FHIR observations ------------------------------------------------------------

(defun slug (text)
  (let ((words (words-of text)))
    (if words (format nil "~{~a~^-~}" words) "observation")))

(defun import-fhir (path)
  "Read a FHIR Bundle of Observation resources and keep their values as
samples: the lab result half of a health record."
  (let* ((object (nlk:decode-json (uiop:read-file-string path)))
         (entries (nlk:json-array object "entry"))
         (rows '()))
    (when (zerop (length entries))
      (fail "~a holds no FHIR Bundle entries" (file-namestring path)))
    (loop for index from 0 below (length entries)
          for resource = (nlk:json-value (aref entries index) :object "resource")
          when (and resource (equal "Observation" (nlk:json-value resource :string "resourceType")))
            do (let* ((code (or (nlk:json-value (nlk:json-value resource :object "code") :string "text")
                                "observation"))
                      (quantity (nlk:json-value resource :object "valueQuantity"))
                      (value (and quantity (nlk:json-value quantity :number "value")))
                      (unit (or (and quantity (nlk:json-value quantity :string "unit")) ""))
                      (when-text (nlk:json-value resource :string "effectiveDateTime"))
                      (day (and when-text (subseq when-text 0 (min 10 (length when-text))))))
                 (when (and (numberp value) day (iso-day-p day))
                   (push (list (slug code) day (format nil "~,3f" (float value)) unit) rows))))
    (when (null rows)
      (fail "~a holds no Observation with a value and a date" (file-namestring path)))
    (write-rows (vault-file +samples-file+) (sort rows #'string< :key #'second))
    (append-source path "FHIR observations" (length rows))
    (nlk:json-object "imported" (file-namestring path) "observations" (length rows)
                     "metrics" (coerce (sort (remove-duplicates (mapcar #'first rows) :test #'string=)
                                              #'string<)
                                       'vector))))

;;; --- samples --------------------------------------------------------------------

(defun samples ()
  "Every sample row, as (:metric :day :value :unit)."
  (loop for (metric day value unit) in (read-rows (vault-file +samples-file+))
        for number = (ignore-errors (read-from-string value))
        when (numberp number)
          collect (list :metric metric :day day :value (float number) :unit unit)))

(defun sample-metrics ()
  (remove-duplicates (mapcar (lambda (sample) (getf sample :metric)) (samples)) :test #'string=))

(defun in-window-p (day start end)
  (and (or (null start) (string>= day start))
       (or (null end) (string<= day end))))

(defun bucket-day (day bucket)
  "DAY's bucket key: the day itself, its Monday, or its month."
  (cond ((string= bucket "week")
         (let ((number (day-number day)))
           (if number
               (day-from-number (- number (mod (+ number 3) 7)))
               day)))
        ((string= bucket "month") (subseq day 0 7))
        (t day)))

(defun statistics (values)
  "COUNT, MEAN, MIN and MAX of a non-empty list."
  (let ((count (length values)))
    (values count
            (/ (reduce #'+ values) count)
            (reduce #'min values)
            (reduce #'max values))))

(defun percentile (values fraction)
  "The nearest-rank FRACTION percentile of VALUES."
  (let* ((sorted (sort (copy-list values) #'<))
         (position (min (1- (length sorted))
                        (max 0 (floor (* fraction (length sorted)))))))
    (nth position sorted)))

(defun as-double (number)
  (coerce (float number 1.0d0) 'double-float))

(defun series-object (metric start end bucket)
  "METRIC's samples in the window, bucketed, oldest bucket first."
  (let ((buckets (make-hash-table :test #'equal))
        (values '()))
    (dolist (sample (samples))
      (when (and (string= metric (getf sample :metric))
                 (in-window-p (getf sample :day) start end))
        (push (getf sample :value) values)
        (push (getf sample :value) (gethash (bucket-day (getf sample :day) bucket) buckets))))
    (let ((rows '()))
      (maphash (lambda (key listed)
                 (multiple-value-bind (count mean low high) (statistics (nreverse listed))
                   (push (list key count mean low high) rows)))
               buckets)
      (setf rows (sort rows #'string< :key #'first))
      (nlk:json-object
       "metric" metric
       "unit" (or (getf (first (remove-if-not (lambda (sample)
                                                 (string= metric (getf sample :metric)))
                                               (samples)))
                         :unit)
                  "")
       "bucket" bucket
       "start" (or start (and rows (first (first rows))) "")
       "end" (or end (and rows (first (car (last rows)))) "")
       "buckets" (coerce (loop for (key count mean low high) in rows
                                collect (nlk:json-object "bucket" key "count" count
                                                          "mean" (as-double mean)
                                                          "min" (as-double low)
                                                          "max" (as-double high)))
                          'vector)
       "samples" (length values)
       "mean" (if values (as-double (/ (reduce #'+ values) (length values))) :null)
       "note" (if values
                   "a bucket the export held nothing for is absent, not zero; quote the units with the numbers"
                   "no samples for that metric in that window")))))

(defun metric-object (metric rows)
  (let ((days (mapcar (lambda (sample) (getf sample :day)) rows)))
    (nlk:json-object
     "metric" metric
     "unit" (or (getf (first rows) :unit) "")
     "samples" (length rows)
     "days" (length (remove-duplicates days :test #'string=))
     "first_day" (when days (first (sort (copy-list days) #'string<)))
     "last_day" (when days (car (last (sort (copy-list days) #'string<))))
     "aggregation" (if (member metric '("steps" "distance_km" "active_energy_kcal"
                                        "basal_energy_kcal" "sleep_hours")
                               :test #'string=)
                       "sum" "mean"))))

(defun metrics-object ()
  (let ((all (samples))
        (by-metric (make-hash-table :test #'equal)))
    (dolist (sample all)
      (push sample (gethash (getf sample :metric) by-metric)))
    (let ((metrics (sort (loop for key being the hash-keys of by-metric collect key)
                         #'string<)))
      (nlk:json-object
       "metrics" (coerce (loop for metric in metrics
                                collect (metric-object metric (nreverse (gethash metric by-metric))))
                          'vector)
       "count" (length metrics)
       "days" (length (remove-duplicates (mapcar (lambda (sample) (getf sample :day)) all)
                                         :test #'string=))
       "note" (if metrics
                   "one row per metric per day; a missing day is a day the export held nothing for, and is not a zero"
                   "no Apple Health samples are imported yet: import an export.xml with caduceus_capture op=import")))))

(defun days-ago (day count)
  (let ((number (day-number day)))
    (and number (day-from-number (- number count)))))

(defun compare-object (metric pivot before-days after-days)
  "METRIC's mean before and after PIVOT, over the two windows."
  (let* ((pivot-number (day-number pivot))
         (before '()) (after '()))
    (dolist (sample (samples))
      (when (string= metric (getf sample :metric))
        (let ((number (day-number (getf sample :day))))
          (when (and pivot-number number)
            (cond ((and (>= number (- pivot-number before-days)) (< number pivot-number))
                   (push (getf sample :value) before))
                  ((<= pivot-number number (+ pivot-number after-days))
                   (push (getf sample :value) after)))))))
    (let ((mean-before (and before (/ (reduce #'+ before) (length before))))
          (mean-after (and after (/ (reduce #'+ after) (length after)))))
      (nlk:json-object
       "metric" metric
       "pivot" pivot
       "before" (if mean-before
                     (nlk:json-object "days" before-days "n" (length before)
                                      "mean" (as-double mean-before)
                                      "min" (as-double (reduce #'min before))
                                      "max" (as-double (reduce #'max before)))
                     :null)
       "after" (if mean-after
                    (nlk:json-object "days" after-days "n" (length after)
                                     "mean" (as-double mean-after)
                                     "min" (as-double (reduce #'min after))
                                     "max" (as-double (reduce #'max after)))
                    :null)
       "change" (if (and mean-before mean-after)
                    (as-double (- mean-after mean-before))
                    :null)
       "percent_change" (if (and mean-before mean-after (plusp mean-before))
                            (as-double (* 100 (- (/ mean-after mean-before) 1)))
                            :null)
       "note" "a before-and-after around a date is a comparison, not a cause: the answer says nothing about why"
       "unit" (or (getf (first (remove-if-not (lambda (sample)
                                                 (string= metric (getf sample :metric)))
                                               (samples)))
                         :unit)
                  "")))))

(defun check-object (&optional (window 90))
  "Each metric's latest day against the user's own range for it."
  (let ((by-metric (make-hash-table :test #'equal))
        (signals '()))
    (dolist (sample (samples))
      (push sample (gethash (getf sample :metric) by-metric)))
    (maphash
     (lambda (metric rows)
       (let* ((rows (sort rows #'string< :key (lambda (sample) (getf sample :day))))
              (latest (car (last rows)))
              (latest-day (getf latest :day))
              (floor-day (days-ago latest-day window))
              (history (remove-if-not (lambda (sample)
                                        (and (string< (getf sample :day) latest-day)
                                             floor-day
                                             (string>= (getf sample :day) floor-day)))
                                      rows))
              (values (mapcar (lambda (sample) (getf sample :value)) history)))
         (when (>= (length values) 5)
           (let ((low (percentile values 0.05))
                 (high (percentile values 0.95))
                 (value (getf latest :value)))
             (when (or (< value low) (> value high))
               (push (nlk:json-object
                      "metric" metric
                      "day" latest-day
                      "value" (as-double value)
                      "unit" (getf latest :unit)
                      "range_low" (as-double low)
                      "range_high" (as-double high)
                      "history_days" (length values)
                      "signal" (if (< value low) "below your own range" "above your own range"))
                     signals))))))
     by-metric)
    (nlk:json-object
     "signals" (coerce (sort signals #'string< :key (lambda (signal) (nlk:json-value signal :string "metric"))) 'vector)
     "window_days" window
     "note" (if (plusp (length signals))
                 "compared with the user's own 5th to 95th percentile over the prior 90 days, one row a metric; a signal is a number to look at, not a finding"
                 "no metric strays outside the user's own 5th to 95th percentile on its latest day, or not enough history to say"))))
