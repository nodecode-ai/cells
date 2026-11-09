;;;; cell.lisp --- Apple Health: the user's own export, read from disk.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :apple-health) answers the primer below, and every request's help
;;;; section carries one line naming it. Nothing hooks the provider path and
;;;; nothing dials: the only thing this cell reads is one file, the export the
;;;; user unzipped out of the Health app, one XML element at a time.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "applehealth": {"export_path": "~/Health/apple_health_export/export.xml",
;;;;                   "privacy_mode": "structured", "max_records": 25}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Rewritten from davidmosiah's Hermes Agent plugin (see NOTICE): the
;;;; plugin's MCP tools and what they answer - connection status, data
;;;; inventory, a daily and a weekly summary, bounded record and workout
;;;; lists, a privacy audit - with the export parsed here instead of by an
;;;; npm server over stdio.

(in-package #:nodecode-apple-health)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Apple Health is available through the nodecode-apple-health cell: seven tools that read the
Apple Health export on this machine (workouts, steps, heart rate, HRV, sleep, cycle tracking),
and a /health command a person can type. The data comes from a file the user exported, not from
a live iPhone: it is as fresh as the last export they imported, and nothing here touches the
network.
  apple_health_connection_status  {}
      Is the export there and readable? The path, its size and age, the privacy mode, and the
      next step when it is missing. Call this first.
  apple_health_data_inventory  {}
      One scan: how many records of each type, which workout activities, and the first and last
      day covered. Call it before a summary when the export's contents are unknown.
  apple_health_daily_summary  {date}
      One day: steps, distance, active energy, heart rate average/min/max, resting heart rate,
      heart-rate variability, sleep hours and that day's workouts. date is YYYY-MM-DD; leave it
      out for the latest day the export covers.
  apple_health_weekly_summary  {end_date, days}
      The same numbers per day for a run of days (7 by default, 90 at most), plus the totals.
      Leave both arguments out for the last seven days the export covers.
  apple_health_records  {type, start_date, end_date, limit}
      Individual records, bounded: filter by a part of a type name (StepCount, HeartRate,
      SleepAnalysis) and by first and last day. The answer says how many matched in all and
      whether the list was cut.
  apple_health_workouts  {start_date, end_date, limit}
      The workouts, newest first: activity, minutes, distance and energy burned.
  apple_health_privacy_audit  {}
      What the cell reads and what it does not.

A key missing from a day's answer means the export holds no such data for that day (no sleep
records, no workouts) - say so rather than reading it as zero. Nothing here is a medical
reading: it is the user's own data, for reflection and planning, not diagnosis."
  "What (help :apple-health) answers while the cell runs.")

(defparameter +summary-disclosure+
  "Computed from the local Apple Health export on this machine; not live HealthKit, not medical advice."
  "What a summary answer says about where its numbers come from.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (apple-health-error (condition)
       (nle:failure (format nil "ERROR: ~a" (apple-health-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Apple Health: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

;;; --- reading the section --------------------------------------------------------

(defun resolve-export-path (path)
  "PATH as a namestring, or the export.xml inside it when PATH names the
unzipped export folder instead of the file. A :path setting arrives as a
pathname; the cell reads one string everywhere after this."
  (cond
    ((uiop:directory-exists-p path)
     (or (loop for relative in '("export.xml" "apple_health_export/export.xml" "HealthData/export.xml")
               for file = (merge-pathnames relative (uiop:ensure-directory-pathname path))
               when (probe-file file) return (namestring (probe-file file)))
         (nlk:config-error "~a holds no export.xml: point export_path at the export.xml itself (an unzipped Apple Health export keeps it in apple_health_export/)" path)))
    (t (namestring path))))

(defun read-settings (values table)
  "The declared VALUES, with export_path resolved from a folder to its
export.xml when it names the folder."
  (declare (ignore table))
  (let ((path (nonblank (getf values :export-path)))
        (plist (copy-list values)))
    (unless path
      (nlk:config-error "no export_path: set it to the export.xml of your Apple Health export (Health app > profile > Export All Health Data, unzipped), or to the folder that holds it"))
    (setf (getf plist :export-path) (resolve-export-path (nlk:expand-home path)))
    plist))

(defun export-path ()
  "The export the cell reads, or a refusal naming what to set."
  (let ((path (setting :export-path)))
    (cond ((not (and (stringp path) (plusp (length (nlk:trimmed path)))))
           (fail "no export_path is set: set it in the applehealth section of the config"))
          ((not (probe-file path))
           (fail "~a does not exist: unzip the Health export and set export_path to its export.xml, or to the folder that holds it" path))
          (t path))))

(defun privacy-mode ()
  (if (equalp (setting :privacy-mode) "raw") :raw :structured))

;;; --- one XML element at a time -------------------------------------------------

(defparameter +tag-space+ (list #\Space #\Tab #\Newline #\Return #\/ #\>)
  "What ends an element's name: whitespace, the closing slash or the closing bracket.")

(defparameter +whitespace+ (list #\Space #\Tab #\Newline #\Return)
  "What an attribute name is trimmed of.")

(defparameter +chunk-size+ 65536
  "How much of the export is in hand at once. An export of a few hundred
megabytes is read in one pass, and only the element being parsed is held.")

(defun decode-entities (text)
  "TEXT with the five XML entities a Health export writes turned back into
their characters."
  (cond
    ((null (search "&" text)) text)
    (t (with-output-to-string (out)
         (loop with length = (length text) with from = 0
               do (let ((amp (position #\& text :start from)))
                    (cond ((null amp)
                           (write-string (subseq text from) out)
                           (return))
                          (t
                           (write-string (subseq text from amp) out)
                           (let ((semi (position #\; text :start amp)))
                             (cond ((null semi)
                                    (write-string (subseq text amp) out)
                                    (return))
                                   (t
                                    (let ((entity (subseq text (1+ amp) semi)))
                                      (write-string
                                       (cond ((string= entity "amp") "&")
                                             ((string= entity "lt") "<")
                                             ((string= entity "gt") ">")
                                             ((string= entity "quot") "\"")
                                             ((string= entity "apos") "'")
                                             (t (subseq text amp (1+ semi))))
                                       out)
                                      (setf from (1+ semi))))))))))))))

(defun element-name (text from)
  "The name of the element whose \"<\" sits at FROM."
  (let ((end (or (position-if (lambda (character)
                                (member character +tag-space+))
                              text :start (1+ from))
                 (length text))))
    (subseq text (1+ from) end)))

(defun parse-attributes (element)
  "ELEMENT's attributes, as an alist of (lowercase name . value)."
  (let* ((length (length element))
         (from (or (position-if (lambda (character) (member character +tag-space+)) element)
                   length))
         (attributes '()))
    (loop while (< from length)
          do (let ((name-end (position #\= element :start from)))
               (if (null name-end)
                   (return)
                   (let* ((name (string-trim +whitespace+ (subseq element from name-end)))
                          (quote-at (position-if (lambda (character)
                                                   (or (char= character #\") (char= character #\')))
                                                 element :start (1+ name-end))))
                     (if (null quote-at)
                         (return)
                         (let* ((quote-character (char element quote-at))
                                (value-end (position quote-character element :start (1+ quote-at))))
                           (if (null value-end)
                               (return)
                               (progn
                                 (push (cons name
                                             (decode-entities (subseq element (1+ quote-at) value-end)))
                                       attributes)
                                 (setf from (1+ value-end))))))))))
    (nreverse attributes)))

(defun attribute (attributes name)
  "ATTRS's NAME, or an empty string."
  (or (cdr (assoc name attributes :test #'string=)) ""))

(defun consume-elements (text element-fn)
  "Call ELEMENT-FN with the name and the whole text of every complete
element of TEXT. Answer where the next chunk starts: the position of an
element that runs past TEXT's end, or TEXT's length."
  (let ((length (length text)) (from 0))
    (loop
      (let ((open (position #\< text :start from)))
        (cond ((null open) (return length))
              (t (let ((close (position #\> text :start open)))
                   (if (null close)
                       (return open)
                       (progn
                         (funcall element-fn (element-name text open) (subseq text open (1+ close)))
                         (setf from (1+ close)))))))))))

(defun scan-elements (path element-fn)
  "Call ELEMENT-FN with (NAME TEXT) for every complete XML element of PATH,
which is read in chunks so a file larger than memory still parses."
  (with-open-file (in path :element-type 'character :external-format :utf-8)
    (let ((buffer (make-string +chunk-size+))
          (carry ""))
      (loop
        (let ((count (read-sequence buffer in)))
          (let* ((text (if (plusp (length carry))
                           (concatenate 'string carry (subseq buffer 0 count))
                           (subseq buffer 0 count)))
                 (next (consume-elements text element-fn)))
            (setf carry (if (< next (length text)) (subseq text next) "")))
          (when (< count +chunk-size+) (return))))
      (when (plusp (length carry))
        (consume-elements carry element-fn)))))

;;; --- one record, one workout ------------------------------------------------

(defstruct (record (:conc-name rec-))
  (type "") (unit "") (value "") (start "") (end "") (source ""))

(defstruct (workout (:conc-name wo-))
  (activity "") (duration nil) (duration-unit "") (distance nil) (distance-unit "")
  (energy nil) (energy-unit "") (start "") (end "") (source ""))

(defparameter +quantity-prefix+ "HKQuantityTypeIdentifier")
(defparameter +category-prefix+ "HKCategoryTypeIdentifier")
(defparameter +activity-prefix+ "HKWorkoutActivityType")

(defun short-type (type)
  "TYPE without the HealthKit prefix the export writes, so
HKQuantityTypeIdentifierStepCount reads StepCount."
  (let ((type (or type "")))
    (cond ((uiop:string-prefix-p +quantity-prefix+ type) (subseq type (length +quantity-prefix+)))
          ((uiop:string-prefix-p +category-prefix+ type) (subseq type (length +category-prefix+)))
          (t type))))

(defun short-activity (activity)
  "ACTIVITY without its HKWorkoutActivityType prefix."
  (let ((activity (or activity "")))
    (if (uiop:string-prefix-p +activity-prefix+ activity)
        (subseq activity (length +activity-prefix+))
        activity)))

(defun number-of (text)
  "TEXT read as a number, or NIL when it is not one. *READ-EVAL* is off: the
export is a file the user was given, not code."
  (when (and (stringp text) (plusp (length text)))
    (let ((*read-eval* nil))
      (multiple-value-bind (value position) (read-from-string text nil nil)
        (and (numberp value) (= position (length text)) value)))))

(defun day-of (timestamp)
  "The day (YYYY-MM-DD) an Apple timestamp starts on, in the local time the
export writes (offset included in the text), or NIL."
  (when (and (stringp timestamp) (>= (length timestamp) 10))
    (subseq timestamp 0 10)))

(defun parse-int (text start end)
  (when (and (stringp text) (<= end (length text)) (< start end))
    (ignore-errors (parse-integer text :start start :end end))))

(defun offset-of (text)
  "The seconds east of UTC the trailing offset of an Apple timestamp names,
e.g. +0100 reads 3600."
  (let ((at (position-if (lambda (character) (or (char= character #\+) (char= character #\-)))
                         text :start (min 19 (length text)))))
    (if (and at (>= (length text) (+ at 5)))
        (let ((hours (parse-int text (1+ at) (+ at 3)))
              (minutes (parse-int text (+ at 3) (+ at 5))))
          (if (and hours minutes)
              (* (if (char= (char text at) #\-) -1 1) (+ (* 60 hours) minutes) 60)
              0))
        0)))

(defun parse-apple-time (text)
  "The universal time an Apple timestamp names, or NIL. Apple writes a local
wall clock and its offset, e.g. 2026-01-01 08:00:00 +0100."
  (when (and (stringp text) (>= (length text) 19))
    (let ((year (parse-int text 0 4)) (month (parse-int text 5 7)) (date (parse-int text 8 10))
          (hour (parse-int text 11 13)) (minute (parse-int text 14 16)) (second (parse-int text 17 19)))
      (when (and year month date hour minute second)
        (handler-case
            (- (encode-universal-time second minute hour date month year 0) (offset-of text))
          (error () nil))))))

(defun parse-record (element)
  (let ((attributes (parse-attributes element)))
    (make-record :type (attribute attributes "type")
                 :unit (attribute attributes "unit")
                 :value (attribute attributes "value")
                 :start (attribute attributes "startDate")
                 :end (attribute attributes "endDate")
                 :source (attribute attributes "sourceName"))))

(defun parse-workout (element)
  (let ((attributes (parse-attributes element)))
    (make-workout :activity (attribute attributes "workoutActivityType")
                  :duration (number-of (attribute attributes "duration"))
                  :duration-unit (attribute attributes "durationUnit")
                  :distance (number-of (attribute attributes "totalDistance"))
                  :distance-unit (attribute attributes "totalDistanceUnit")
                  :energy (number-of (attribute attributes "totalEnergyBurned"))
                  :energy-unit (attribute attributes "totalEnergyBurnedUnit")
                  :start (attribute attributes "startDate")
                  :end (attribute attributes "endDate")
                  :source (attribute attributes "sourceName"))))

(defun walk-export (path visit)
  "Call VISIT with :RECORD or :WORKOUT and the entry, for every element of
PATH. VISIT answering :STOP ends the walk."
  (catch 'stop
    (scan-elements
     path
     (lambda (name text)
       (cond ((string= name "Record")
              (when (eq :stop (funcall visit :record (parse-record text))) (throw 'stop nil)))
             ((string= name "Workout")
              (when (eq :stop (funcall visit :workout (parse-workout text))) (throw 'stop nil))))))))

;;; --- one day's numbers ---------------------------------------------------------

(defstruct (day (:conc-name day-) (:constructor make-day (date)))
  (date "" :read-only t)
  (steps 0) (distance 0) (energy 0)
  (heart-count 0) (heart-sum 0) (heart-min nil) (heart-max nil)
  (resting-sum 0) (resting-count 0)
  (hrv-sum 0) (hrv-count 0)
  (sleep-seconds 0)
  (workouts '()))

(defun note-heart (day value)
  (incf (day-heart-count day))
  (incf (day-heart-sum day) value)
  (setf (day-heart-min day) (if (day-heart-min day) (min (day-heart-min day) value) value)
        (day-heart-max day) (if (day-heart-max day) (max (day-heart-max day) value) value)))

(defun distance-in-km (value unit)
  "VALUE kilometres: the export writes miles for a user who measures in them."
  (cond ((null value) nil)
        ((string-equal unit "mi") (* value 1.609344))
        (t value)))

(defun note-record (table entry)
  "Fold one record into its day's numbers, when its day is one this call wants."
  (let* ((day (day-of (rec-start entry)))
         (stats (and day (gethash day table))))
    (when stats
      (let ((type (short-type (rec-type entry)))
            (value (number-of (rec-value entry))))
        (cond
              ((string= type "SleepAnalysis")
               ;; A category record: its value is a word, and the night is the
               ;; span between its two timestamps.
               (when (search "Asleep" (rec-value entry) :test #'char-equal)
                 (let ((from (parse-apple-time (rec-start entry)))
                       (to (parse-apple-time (rec-end entry))))
                   (when (and from to (plusp (- to from)))
                     (incf (day-sleep-seconds stats) (- to from))))))
              ((null value))
              ((string= type "StepCount") (incf (day-steps stats) value))
              ((string= type "DistanceWalkingRunning")
               (incf (day-distance stats) (or (distance-in-km value (rec-unit entry)) 0)))
              ((string= type "ActiveEnergyBurned") (incf (day-energy stats) value))
              ((string= type "HeartRate") (note-heart stats value))
              ((string= type "RestingHeartRate")
               (incf (day-resting-sum stats) value)
               (incf (day-resting-count stats)))
              ((string= type "HeartRateVariabilitySDNN")
               (incf (day-hrv-sum stats) value)
               (incf (day-hrv-count stats))))))))

(defun workout-minutes (workout)
  (let ((duration (wo-duration workout)) (unit (wo-duration-unit workout)))
    (cond ((null duration) nil)
          ((string-equal unit "sec") (plain-number (/ (float duration 1.0) 60)))
          ((string-equal unit "hr") (plain-number (* duration 60.0)))
          (t (plain-number (float duration 1.0))))))

(defun note-workout (table entry)
  (let* ((day (day-of (wo-start entry)))
         (stats (and day (gethash day table))))
    (when stats (push entry (day-workouts stats)))))

(defun collect-days (path days)
  "A hash of day string -> DAY for the records and workouts inside DAYS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (day days) (setf (gethash day table) (make-day day)))
    (walk-export path
                 (lambda (kind entry)
                   (if (eq kind :record) (note-record table entry) (note-workout table entry))))
    table))

;;; --- days as dates ---------------------------------------------------------------

(defun day-number (day)
  "The days since 1970-01-01 a YYYY-MM-DD string names, or NIL."
  (when (and (stringp day) (>= (length day) 10))
    (let ((year (parse-int day 0 4)) (month (parse-int day 5 7)) (date (parse-int day 8 10)))
      (when (and year month date)
        (ignore-errors
          (floor (- (encode-universal-time 0 0 12 date month year 0) 2208988800) 86400))))))

(defun day-string (number)
  "The YYYY-MM-DD string NUMBER days after 1970-01-01 names."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (+ 2208988800 (* number 86400) (* 12 3600)) 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun day-window (end days)
  "DAYS day strings, oldest first, the last one END."
  (let ((base (day-number end)))
    (if (null base)
        (list end)
        (loop for offset from (1- days) downto 0 collect (day-string (- base offset))))))

(defun latest-day (path)
  "The last day the export's records cover. The export writes its records in
date order, so the walk ends at the first workout."
  (let ((latest nil))
    (walk-export path
                 (lambda (kind entry)
                   (if (eq kind :record)
                       (let ((day (day-of (rec-start entry))))
                         (when (and day (or (null latest) (string> day latest)))
                           (setf latest day)))
                       :stop)))
    latest))

;;; --- numbers as answers -----------------------------------------------------------

(defun plain-number (value)
  "VALUE as JSON reads it plainly: an integer when it is whole, so a mean of
70 answers 70 rather than 7.0e1."
  (when (numberp value)
    (multiple-value-bind (whole fraction) (round value)
      (if (zerop fraction) whole (coerce value 'single-float)))))

(defun round-1 (value)
  "VALUE to one decimal place, as a JSON number."
  (when (numberp value)
    (plain-number (/ (round (* (coerce value 'single-float) 10)) 10.0))))

(defun average-of (sum count)
  (when (plusp count) (plain-number (float (/ sum count) 1.0))))

(defun file-size (path)
  (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (and in (file-length in))))

(defun utc-string (universal-time)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time universal-time 0)
    (declare (ignore second))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d UTC" year month date hour minute)))

(defun top-counts (table limit)
  "TABLE's entries, most frequent first, as JSON objects, at most LIMIT."
  (let ((pairs '()))
    (maphash (lambda (name count) (push (cons name count) pairs)) table)
    (setf pairs (sort pairs #'> :key #'cdr))
    (loop for (name . count) in (if (> (length pairs) limit) (subseq pairs 0 limit) pairs)
          collect (nlk:json-object "name" name "count" count))))

(defun workout-object (workout)
  (nlk:json-object
   "activity" (short-activity (wo-activity workout))
   "day" (or (day-of (wo-start workout)) "")
   "start" (wo-start workout)
   "end" (wo-end workout)
   :opt "minutes" (round-1 (workout-minutes workout))
   :opt "distance_km" (round-1 (distance-in-km (wo-distance workout) (wo-distance-unit workout)))
   :opt "energy_kcal" (wo-energy workout)))

(defun day-object (stats)
  (nlk:json-object
   "date" (day-date stats)
   "steps" (round (day-steps stats))
   "distance_km" (round-1 (day-distance stats))
   "active_energy_kcal" (round (day-energy stats))
   "heart_rate" (nlk:json-object
                 :opt "average" (average-of (day-heart-sum stats) (day-heart-count stats))
                 :opt "min" (day-heart-min stats)
                 :opt "max" (day-heart-max stats)
                 "count" (day-heart-count stats))
   :opt "resting_heart_rate" (average-of (day-resting-sum stats) (day-resting-count stats))
   :opt "hrv_ms" (average-of (day-hrv-sum stats) (day-hrv-count stats))
   "sleep_hours" (round-1 (/ (day-sleep-seconds stats) 3600.0))
   "workouts" (coerce (mapcar #'workout-object (reverse (day-workouts stats))) 'vector)))

(defun window-totals (stats-list)
  (nlk:json-object
   "days" (length stats-list)
   "steps" (round (reduce #'+ stats-list :key #'day-steps :initial-value 0))
   "distance_km" (round-1 (reduce #'+ stats-list :key #'day-distance :initial-value 0))
   "active_energy_kcal" (round (reduce #'+ stats-list :key #'day-energy :initial-value 0))
   "sleep_hours" (round-1 (/ (reduce #'+ stats-list :key #'day-sleep-seconds :initial-value 0) 3600.0))
   "workouts" (reduce #'+ stats-list
                      :key (lambda (stats) (length (day-workouts stats))) :initial-value 0)
   "days_with_workouts" (count-if (lambda (stats) (plusp (length (day-workouts stats)))) stats-list)))

;;; --- the seven tools ---------------------------------------------------------------

(defun call-limit (requested)
  "The cap one call asks for, else the operator's max_records."
  (if (and (integerp requested) (plusp requested))
      requested
      (setting :max-records)))

(defun connection-status (request)
  "The apple_health_connection_status tool: is the export there, and what next."
  (declare (ignore request))
  (answering
    (let* ((path (setting :export-path))
           (present (and (nonblank path) (probe-file path))))
      (nlk:encode-json-object
       (nlk:json-object
        "ok" (and present t)
        "export_path" (or path "")
        "export_exists" (and present t)
        :opt "size_bytes" (and present (file-size present))
        :opt "modified_utc" (and present (utc-string (file-write-date present)))
        "privacy_mode" (if (eq (privacy-mode) :raw) "raw" "structured")
        "max_records" (setting :max-records)
        "live_healthkit" nil
        "network" nil
        "summary" (if present
                     "The export is there: ask for a daily or weekly summary, or call apple_health_data_inventory first."
                     "No export at export_path yet. On the iPhone: Health app > profile picture > Export All Health Data, then transfer the export to this machine, unzip it and point export_path at apple_health_export/export.xml.")
        "next_steps" (coerce
                      (if present
                          (list "apple_health_data_inventory: what the export holds and which days it covers."
                                "apple_health_daily_summary / apple_health_weekly_summary: the numbers for a day or a week.")
                          (list "Export from the iPhone: Health > profile picture > Export All Health Data."
                                "Unzip the archive on this machine; it holds apple_health_export/export.xml."
                                "Set export_path in the applehealth section of the config to that file, or to the folder holding it."))
                      'vector))))))

(defun data-inventory (request)
  "The apple_health_data_inventory tool: one scan, then what the export holds."
  (declare (ignore request))
  (answering
    (let ((path (export-path))
          (types (make-hash-table :test #'equal))
          (activities (make-hash-table :test #'equal))
          (records 0) (workouts 0) (first-day nil) (last-day nil))
      (walk-export
       path
       (lambda (kind entry)
         (if (eq kind :record)
             (let ((day (day-of (rec-start entry))))
               (incf records)
               (incf (gethash (short-type (rec-type entry)) types 0))
               (when day
                 (when (or (null first-day) (string< day first-day)) (setf first-day day))
                 (when (or (null last-day) (string> day last-day)) (setf last-day day))))
             (progn
               (incf workouts)
               (incf (gethash (short-activity (wo-activity entry)) activities 0))))))
      (nlk:encode-json-object
       (nlk:json-object
        "export_path" path
        "total_records" records
        "distinct_record_types" (hash-table-count types)
        "record_types" (coerce (top-counts types 25) 'vector)
        "total_workouts" workouts
        "workout_activities" (coerce (top-counts activities 25) 'vector)
        "first_day" (or first-day "")
        "last_day" (or last-day "")
        "privacy_mode" (if (eq (privacy-mode) :raw) "raw" "structured")
        "disclosure" +summary-disclosure+)))))

(defun daily-summary (request)
  "The apple_health_daily_summary tool: one day's numbers."
  (answering
    (let* ((path (export-path))
           (day (or (nonblank (daily-request-date request))
                    (latest-day path)
                    (fail "the export holds no dated records: check that export_path names the export.xml of an Apple Health export")))
           (stats (gethash day (collect-days path (list day)))))
      (nlk:encode-json-object
       (nlk:copy-json-object (day-object stats) "disclosure" +summary-disclosure+)))))

(defun weekly-summary (request)
  "The apple_health_weekly_summary tool: a run of days, then their totals."
  (answering
    (let* ((path (export-path))
           (days (max 1 (min 90 (or (weekly-request-days request) 7))))
           (end (or (nonblank (weekly-request-end-date request))
                    (latest-day path)
                    (fail "the export holds no dated records: check that export_path names the export.xml of an Apple Health export")))
           (window (day-window end days))
           (table (collect-days path window))
           (stats-list (loop for day in window collect (gethash day table))))
      (nlk:encode-json-object
       (nlk:json-object
        "start_date" (first window)
        "end_date" (car (last window))
        "days" (coerce (mapcar #'day-object stats-list) 'vector)
        "totals" (window-totals stats-list)
        "disclosure" +summary-disclosure+)))))

(defun in-window-p (day from to)
  (and (nonblank day)
       (or (null from) (string>= day from))
       (or (null to) (string<= day to))))

(defun record-matches-p (entry type from to)
  (and (in-window-p (day-of (rec-start entry)) from to)
       (or (null type)
           (search (string-downcase type) (string-downcase (short-type (rec-type entry)))))))

(defun record-object (entry mode)
  (let ((type (short-type (rec-type entry)))
        (value (or (number-of (rec-value entry)) (rec-value entry))))
    (if (eq mode :raw)
        (nlk:json-object
         "type" type
         "record_type" (rec-type entry)
         "value" value
         "unit" (rec-unit entry)
         "start" (rec-start entry)
         "end" (rec-end entry)
         :opt "source" (nonblank (rec-source entry)))
        (nlk:json-object
         "type" type
         "value" value
         "unit" (rec-unit entry)
         "day" (or (day-of (rec-start entry)) "")))))

(defun list-records (request)
  "The apple_health_records tool: bounded records, the count, the disclosure."
  (answering
    (let* ((path (export-path))
           (type (nonblank (record-query-record-type request)))
           (from (nonblank (record-query-start-date request)))
           (to (nonblank (record-query-end-date request)))
           (limit (call-limit (record-query-limit request)))
           (mode (privacy-mode))
           (matched 0)
           (kept '()))
      (walk-export
       path
       (lambda (kind entry)
         (when (and (eq kind :record) (record-matches-p entry type from to))
           (incf matched)
           (when (< (length kept) limit) (push entry kept)))))
      (setf kept (reverse kept))
      (nlk:encode-json-object
       (nlk:json-object
        "count" (length kept)
        "matched" matched
        "truncated" (and (> matched (length kept)) t)
        :when (> matched (length kept)) "truncated_note"
        (format nil "kept the first ~a of ~a matching records: narrow the dates or the type, or raise limit"
                (length kept) matched)
        "privacy_mode" (if (eq mode :raw) "raw" "structured")
        "records" (coerce (mapcar (lambda (entry) (record-object entry mode)) kept) 'vector)
        "disclosure" (if (eq mode :raw)
                         "raw export attributes: source, full timestamps and the export's own type names"
                         "structured: type, value, unit and the day, without source names"))))))

(defun list-workouts (request)
  "The apple_health_workouts tool: the workouts in the window, newest first."
  (answering
    (let* ((path (export-path))
           (from (nonblank (workout-query-start-date request)))
           (to (nonblank (workout-query-end-date request)))
           (limit (call-limit (workout-query-limit request)))
           (matched '()))
      (walk-export
       path
       (lambda (kind entry)
         (when (and (eq kind :workout) (in-window-p (day-of (wo-start entry)) from to))
           (push entry matched))))
      (setf matched (sort matched #'string> :key #'wo-start))
      (let* ((kept (if (> (length matched) limit) (subseq matched 0 limit) matched)))
        (nlk:encode-json-object
         (nlk:json-object
          "count" (length kept)
          "matched" (length matched)
          "truncated" (and (> (length matched) (length kept)) t)
          :when (> (length matched) (length kept)) "truncated_note"
          (format nil "kept the newest ~a of ~a matching workouts: narrow the dates, or raise limit"
                  (length kept) (length matched))
          "workouts" (coerce (mapcar #'workout-object kept) 'vector)
          "disclosure" +summary-disclosure+))))))

(defun privacy-audit (request)
  "The apple_health_privacy_audit tool: what this cell reads, and what it does not."
  (declare (ignore request))
  (answering
    (let ((path (setting :export-path)))
      (nlk:encode-json-object
       (nlk:json-object
        "export_path" (or path "")
        "export_exists" (and (nonblank path) (probe-file path) t)
        "privacy_mode" (if (eq (privacy-mode) :raw) "raw" "structured")
        "max_records" (setting :max-records)
        "reads" "the Apple Health export file at export_path, one XML element at a time"
        "writes" "nothing: the export is opened read-only and no cache is kept"
        "network" nil
        "live_healthkit" nil
        "notes" (coerce
                 (list "An Apple Health export holds sensitive health data; it stays on this machine."
                       "structured mode answers type, value, unit and day; raw mode adds source names and full timestamps."
                       "Nothing here is a medical device or a diagnosis.")
                 'vector))))))

;;; --- the command a person types -----------------------------------------------------

(defun health-command (args session-id)
  "The /health command: the last DAYS days of the export, as text."
  (declare (ignore session-id))
  (answering
    (let* ((days (max 1 (min 90 (or (and (nonblank args)
                                         (ignore-errors (parse-integer args :junk-allowed t)))
                                    7))))
           (path (export-path))
           (end (or (latest-day path)
                    (fail "the export holds no dated records: check that export_path names the export.xml of an Apple Health export")))
           (window (day-window end days))
           (table (collect-days path window))
           (stats-list (loop for day in window collect (gethash day table))))
      (format nil "Apple Health, ~a to ~a~%~{  ~a~}~%  ~a~%  ~a"
              (first window) end
              (loop for stats in stats-list
                    collect (format nil "~a  steps ~a  sleep ~,1f h  workouts ~a"
                                    (day-date stats)
                                    (round (day-steps stats))
                                    (/ (day-sleep-seconds stats) 3600.0)
                                    (length (day-workouts stats))))
              (window-totals-line stats-list)
              +summary-disclosure+))))

(defun window-totals-line (stats-list)
  (format nil "total  steps ~a  sleep ~,1f h  workouts ~a"
          (round (reduce #'+ stats-list :key #'day-steps :initial-value 0))
          (/ (reduce #'+ stats-list :key #'day-sleep-seconds :initial-value 0) 3600.0)
          (reduce #'+ stats-list
                  :key (lambda (stats) (length (day-workouts stats))) :initial-value 0)))

(defun no-arguments-schema (description)
  "The schema of a tool that takes no argument: an empty object, and the one
line the model reads before calling it."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                     "properties" (nlk:json-object))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell apple-health
  (:section ("applehealth")
    (:guide "export_path is the export.xml of your Apple Health export (iPhone: Health > your picture > Export All Health Data, transferred here and unzipped), or the folder that holds it; privacy_mode decides whether a record list carries source names; max_records caps one list answer; next step: ask what the export covers")
    ("export_path" :path
     :doc "the export.xml of an unzipped Apple Health export, or the folder that holds it")
    ("privacy_mode" :choice :options '("structured" "raw") :default "structured"
     :doc "structured answers type, value, unit and day; raw adds source names and full timestamps")
    ("max_records" :integer :default 25 :min 1
     :doc "the most records or workouts one list call answers; a call may ask for fewer"))
  (:settings #'read-settings)
  (:help :apple-health
         "apple-health: apple_health_daily_summary, apple_health_weekly_summary, apple_health_records and apple_health_workouts read the Apple Health export on this machine"
         *manual*)
  (:tool "apple_health_connection_status"
         (no-arguments-schema "Check that the local Apple Health export is set up and readable, without reading the whole export: the path, whether the file is there, its size and age.")
         #'connection-status)
  (:tool "apple_health_data_inventory"
         (no-arguments-schema "Scan the local Apple Health export once and report what it holds: how many records of each type, which workout activities, and the first and last day it covers. Use it before a summary when you do not know what the export contains.")
         #'data-inventory)
  (:tool "apple_health_daily_summary" (nlk:contract 'daily-request) #'daily-summary)
  (:tool "apple_health_weekly_summary" (nlk:contract 'weekly-request) #'weekly-summary)
  (:tool "apple_health_records" (nlk:contract 'record-query) #'list-records)
  (:tool "apple_health_workouts" (nlk:contract 'workout-query) #'list-workouts)
  (:tool "apple_health_privacy_audit"
         (no-arguments-schema "Report the privacy posture of this cell: which export file it reads, the privacy mode, and the fact that the data never leaves the machine.")
         #'privacy-audit)
  (:command "health" 'health-command
            :description "Your Apple Health numbers for the last days"
            :argument-hint "DAYS"))
