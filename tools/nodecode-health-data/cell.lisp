;;;; cell.lisp --- a local health store, filled from Oura, read by the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :health-data) answers the primer below, and every request's help
;;;; section carries one line naming it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "healthdata": {"oura_token_env": "OURA_TOKEN",
;;;;                  "data_dir": "~/.nodecode/health-data",
;;;;                  "days": 30}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from Infinite-Labs-AI's Hermes Agent plugin (see NOTICE): the
;;;; question the tools answer, the daily metrics they keep, the food log and
;;;; the doctor's rules for reading them. Oura is read over its v2 REST API;
;;;; the Google Health half of the original needs an OAuth application of the
;;;; user's own, and is left out (see NOTICE).

(in-package #:nodecode-health-data)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Health data is kept locally by the nodecode-health-data cell: three tools, which a
person can also call from eval in the health-data package.
  health_query   {query_type, start, end, limit, left, right, shift_days}
      Reads the local store; it never dials a provider. query_type recent answers the
      last limit days (7 by default) in date order, date_range a window, stress_days
      the days with the most physiological stress, coverage how much data there is and
      how fresh, food the meals logged, and correlate pairs two metrics across the
      window (sleep_score, readiness_score, activity_score, steps, stress_high,
      recovery_high, resting_heart_rate, average_hrv, sleep_hours, sleep_efficiency,
      spo2_average, temperature_deviation), optionally with shift_days so that one
      night's sleep is compared with a later day's outcome.
      Start with coverage when a question is about data you have not seen: it says the
      first and last day stored, which metrics each day carries, and whether the store
      is stale (more than 36 hours old).
  health_sync   {start_date, end_date, days}
      Fetches daily sleep, readiness, activity, stress and SpO2, and the long sleep
      period, from Oura into the store. Call it when the user asks for fresh data or
      coverage says the store is stale - not on every question.
  health_log_food   {day, description, calories, items}
      Appends one meal to the store. Calories are optional; never invent them, and say
      how many meals had them.
How to answer: lead with the answer, then the dates and values behind it. Compare
against the user's own baseline, not population norms. Frame cross-domain links as
associations unless the user ran an experiment. Say when data is thin, partial or
missing. Do not diagnose, do not give medical directives, and treat sustained low SpO2,
a sustained unexplained rise in resting heart rate or acute symptoms as worth a doctor.
Physiological stress is not emotional stress; readiness and sleep scores are composites,
so name the metric behind a claim."
  "What (help :health-data) answers while the cell runs.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (health-data-error (condition)
       (nle:failure (format nil "ERROR: ~a" (health-data-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: health data: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun answer-detail (body)
  "Oura's own words for a failed call, clipped, or NIL."
  (let ((text (nlk:trimmed (or body ""))))
    (and (plusp (length text)) (nlk:clip text 200))))

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the bearer token the section names and the store's
directory: read once, here."
  (declare (ignore table))
  (let ((env (nonblank (getf values :oura-token-env)))
        (file (nonblank (getf values :oura-token-file)))
        (dir (nonblank (getf values :data-dir)))
        (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of oura_token_env and oura_token_file, not both"))
    (setf (getf plist :token)
          (cond (env (or (nonblank (nle::credential-env env))
                         (nlk:config-error "oura_token_env names ~a, which is unset or empty" env)))
                (file (or (nonblank (ignore-errors
                                      (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "oura_token_file ~a cannot be read, or is empty" file)))
                (t (nlk:config-error
                    "no Oura token: set oura_token_env to the environment variable holding a personal access token, or oura_token_file to a file holding one"))))
    (setf (getf plist :data-dir)
          (uiop:ensure-directory-pathname
           (nlk:expand-home (or dir "~/.nodecode/health-data"))))
    plist))

;;; --- the store: JSON lines under the cell's directory -----------------------------

(defun daily-path () (merge-pathnames "daily.jsonl" (setting :data-dir)))
(defun food-path () (merge-pathnames "food.jsonl" (setting :data-dir)))

(defun day-of (row) (or (nlk:json-value row :string "day") ""))

(defun store-rows ()
  "Every daily row the store holds, oldest first."
  (sort (copy-list (nlk:read-json-lines (daily-path))) #'string< :key #'day-of))

(defun write-rows (rows)
  "ROWS to the store as one JSON line each, replaced in one rename."
  (let ((path (daily-path)))
    (ensure-directories-exist path)
    (let ((temp (make-pathname :name (format nil "~a.tmp" (pathname-name path))
                                :type (pathname-type path) :defaults path)))
      (with-open-file (out temp :direction :output :if-exists :supersede
                                :if-does-not-exist :create :external-format :utf-8)
        (dolist (row rows) (write-line (nlk:encode-json-object row) out)))
      (nlk:replace-file temp path))))

(defun put (row key value)
  "ROW's KEY set to VALUE when VALUE is worth keeping."
  (when value (setf (gethash key row) value))
  row)

(defun day-cell (rows day)
  "ROWS's row for DAY, made when absent."
  (or (gethash day rows)
      (setf (gethash day rows) (nlk:json-object "day" day))))

;;; --- days ----------------------------------------------------------------------

(defun parse-day (text)
  "TEXT, a YYYY-MM-DD day, as universal time, or NIL."
  (when (and (stringp text) (>= (length text) 10))
    (let ((year (ignore-errors (parse-integer text :start 0 :end 4)))
          (month (ignore-errors (parse-integer text :start 5 :end 7)))
          (day (ignore-errors (parse-integer text :start 8 :end 10))))
      (when (and year month day (<= 1 month 12) (<= 1 day 31))
        (ignore-errors (encode-universal-time 0 0 0 day month year 0))))))

(defun day-text (universal)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun today-text () (day-text (get-universal-time)))

(defun day-shifted (text days)
  "TEXT moved DAYS days later (or earlier, when negative)."
  (day-text (+ (* days 86400) (or (parse-day text) (get-universal-time)))))

(defun checked-day (text field)
  "TEXT as a day, or a refusal naming FIELD."
  (or (and (nonblank text) (parse-day text) text)
      (fail "~a ~s is not a day: pass YYYY-MM-DD, e.g. 2026-01-31" field text)))

;;; --- talking to Oura ---------------------------------------------------------------

(defun oura-get (url)
  "URL's decoded answer, or a refusal carrying Oura's own words."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get url
                    :headers (list (cons "authorization"
                                         (format nil "Bearer ~a" (setting :token)))
                                   (cons "accept" "application/json"))
                    :timeout (setting :timeout)
                    :connect-timeout (setting :timeout))
        (let ((text (if (stringp body) body (nlk:body-text body))))
          (if (and (integerp status) (< status 400))
              (or (ignore-errors (nlk:decode-json text))
                  (fail "GET ~a answered something that is not JSON" url))
              (fail "GET ~a answered HTTP ~a~@[: ~a~]" url status (answer-detail text)))))
    (health-data-error (condition) (error condition))
    (error (condition)
      (fail "GET ~a did not answer: ~a" url (nlk:one-line (princ-to-string condition))))))

(defun collection-items (path start end)
  "Every item PATH answers for START..END, following next_token, a list of
decoded objects."
  (let ((items '()) (token nil) (pages 0))
    (loop
      (incf pages)
      (let* ((url (format nil "~a~a?start_date=~a&end_date=~a~@[&next_token=~a~]"
                           +api-base+ path start end token))
             (body (oura-get url)))
        (setf items (nconc items (coerce (or (nlk:json-array body "data") #()) 'list)))
        (setf token (nlk:json-value body :string "next_token"))
        (when (or (null token) (zerop (length token)) (>= pages 10)) (return))))
    items))

(defparameter +api-base+ "https://api.ouraring.com"
  "Oura's API root; every path in +COLLECTIONS+ hangs off it.")

(defparameter +collections+
  '(("daily_sleep" . "/v2/usercollection/daily_sleep")
    ("daily_readiness" . "/v2/usercollection/daily_readiness")
    ("daily_activity" . "/v2/usercollection/daily_activity")
    ("daily_stress" . "/v2/usercollection/daily_stress")
    ("daily_spo2" . "/v2/usercollection/daily_spo2")
    ("sleep" . "/v2/usercollection/sleep"))
  "What one sync reads, as (NAME . PATH).")

(defun absorb (collection item rows)
  "ITEM of COLLECTION into ROWS' row for its day."
  (let ((day (nlk:json-value item :string "day")))
    (when (and day (plusp (length day)))
      (let ((row (day-cell rows day)))
        (cond
          ((string= collection "daily_sleep")
           (put row "sleep_score" (nlk:json-value item :integer "score")))
          ((string= collection "daily_readiness")
           (put row "readiness_score" (nlk:json-value item :integer "score"))
           (put row "resting_heart_rate"
                (nlk:json-value item :integer "contributors" "resting_heart_rate"))
           (put row "temperature_deviation"
                (nlk:json-value item :number "temperature_deviation")))
          ((string= collection "daily_activity")
           (put row "activity_score" (nlk:json-value item :integer "score"))
           (put row "steps" (nlk:json-value item :integer "steps"))
           (put row "active_calories" (nlk:json-value item :integer "active_calories")))
          ((string= collection "daily_stress")
           (put row "stress_high" (nlk:json-value item :integer "stress_high"))
           (put row "recovery_high" (nlk:json-value item :integer "recovery_high"))
           (put row "stress_summary" (nlk:json-value item :string "day_summary")))
          ((string= collection "daily_spo2")
           (put row "spo2_average"
                (nlk:json-value item :number "spo2_percentage" "average")))
          ((string= collection "sleep")
           ;; A day carries several periods; the long sleep is the night.
           (when (or (null (nlk:json-value item :string "type"))
                     (string= "long_sleep" (nlk:json-value item :string "type")))
             (let ((duration (nlk:json-value item :integer "total_sleep_duration")))
               (when (or duration (null (nlk:json-value row :integer "sleep_duration_seconds")))
                 (put row "sleep_duration_seconds" duration)
                 (put row "sleep_efficiency" (nlk:json-value item :integer "efficiency"))
                 (put row "average_hrv" (nlk:json-value item :number "average_hrv"))
                 (put row "lowest_heart_rate" (nlk:json-value item :integer "lowest_heart_rate"))
                 (put row "bedtime_start" (nlk:json-value item :string "bedtime_start"))))))))
      rows)))

;;; --- health_sync ---------------------------------------------------------------

(defun sync-range (start end &optional days)
  "Oura's collections for START..END (or the section's window) into the
store, answering what was stored: the one body health_sync and /health sync
share."
  (answering
    (let* ((end (or (and (nonblank end) (checked-day end "end_date")) (today-text)))
           (days (if (and (integerp days) (plusp days)) days (setting :days)))
           (start (or (and (nonblank start) (checked-day start "start_date"))
                      (day-shifted end (- (1- days))))))
      (when (string< end start)
        (fail "end_date ~a is before start_date ~a" end start))
      (let ((rows (make-hash-table :test #'equal))
            (counts (nlk:json-object)))
        (loop for (name . path) in +collections+
              for items = (collection-items path start end)
              do (setf (gethash name counts) (length items))
                 (dolist (item items) (absorb name item rows)))
        (dolist (row (store-rows)) (setf (gethash (day-of row) rows) row))
        (let ((merged (sort (loop for row being the hash-values of rows collect row)
                            #'string< :key #'day-of)))
          (write-rows merged)
          (nlk:encode-json-object
           (nlk:json-object
            "ok" t
            "start" start
            "end" end
            "days_synced" (length merged)
            "collections" counts
            "store" (namestring (daily-path)))))))))

(defun sync-now (call)
  "The health_sync tool: Oura's collections for the window the call names."
  (sync-range (sync-request-start-date call) (sync-request-end-date call)
              (sync-request-days call)))

;;; --- health_query --------------------------------------------------------------

(defparameter +metrics+
  '(("sleep_score" . "sleep_score") ("readiness_score" . "readiness_score")
    ("activity_score" . "activity_score") ("steps" . "steps")
    ("stress_high" . "stress_high") ("recovery_high" . "recovery_high")
    ("resting_heart_rate" . "resting_heart_rate") ("average_hrv" . "average_hrv")
    ("sleep_hours" . :hours) ("sleep_efficiency" . "sleep_efficiency")
    ("spo2_average" . "spo2_average") ("temperature_deviation" . "temperature_deviation")
    ("active_calories" . "active_calories"))
  "The metrics a correlate call may name, as (NAME . KEY or :HOURS).")

(defun metric-value (row metric)
  "ROW's METRIC as a number, or NIL when the row does not carry it."
  (let ((key (cdr (assoc metric +metrics+ :test #'string=))))
    (cond ((null key) nil)
          ((eq key :hours)
           (let ((seconds (nlk:json-value row :number "sleep_duration_seconds")))
             (and seconds (/ seconds 3600.0))))
          (t (nlk:json-value row :number key)))))

(defun in-window (rows start end)
  (remove-if-not (lambda (row)
                   (let ((day (day-of row)))
                     (and (or (null start) (not (string< day start)))
                          (or (null end) (not (string< end day))))))
                 rows))

(defun window-of (call)
  "CALL's start and end as days, or (values NIL NIL) for everything."
  (values (and (nonblank (query-request-start call))
               (checked-day (query-request-start call) "start"))
          (and (nonblank (query-request-end call))
               (checked-day (query-request-end call) "end"))))

(defun pearson (pairs)
  "The correlation of PAIRS, a list of (X . Y); NIL below two pairs or when
either side never varies."
  (let ((n (length pairs)))
    (when (>= n 2)
      (let* ((xs (mapcar #'car pairs)) (ys (mapcar #'cdr pairs))
             (xmean (/ (reduce #'+ xs) n)) (ymean (/ (reduce #'+ ys) n))
             (sxy (reduce #'+ (mapcar (lambda (x y) (* (- x xmean) (- y ymean))) xs ys)))
             (sxx (reduce #'+ (mapcar (lambda (x) (expt (- x xmean) 2)) xs)))
             (syy (reduce #'+ (mapcar (lambda (y) (expt (- y ymean) 2)) ys))))
        (unless (or (zerop sxx) (zerop syy))
          (/ sxy (sqrt (* sxx syy))))))))

(defun correlate-answer (call rows)
  (let ((left (nonblank (query-request-left call)))
        (right (nonblank (query-request-right call)))
        (shift (query-request-shift-days call)))
    (unless (and left right)
      (fail "correlate needs both left and right: two metrics to pair"))
    (dolist (metric (list left right))
      (unless (assoc metric +metrics+ :test #'string=)
        (fail "unknown metric ~s; the metrics are ~{~a~^, ~}"
              metric (mapcar #'car +metrics+))))
    (multiple-value-bind (start end) (window-of call)
      (let* ((rows (in-window rows start end))
             (by-day (make-hash-table :test #'equal))
             (pairs '()))
        (dolist (row rows) (setf (gethash (day-of row) by-day) row))
        (dolist (row rows)
          (let ((other (gethash (day-shifted (day-of row) (or shift 0)) by-day)))
            (when other
              (let ((x (metric-value row left)) (y (metric-value other right)))
                (when (and x y) (push (cons x y) pairs))))))
        (let ((ordered (reverse pairs)))
          (nlk:encode-json-object
           (nlk:json-object
            "left" left "right" right "shift_days" (or shift 0)
            "pairs" (coerce (loop for (x . y) in ordered
                                  collect (nlk:json-object "left_value" x
                                                            "right_value" y))
                             'vector)
            "count" (length ordered)
            "correlation" (pearson ordered))))))))

(defun coverage-answer (rows)
  (let ((fields (nlk:json-object))
        (first-day (and rows (day-of (first rows))))
        (last-day (and rows (day-of (car (last rows))))))
    (dolist (field '("sleep_score" "readiness_score" "activity_score" "steps"
                     "stress_high" "resting_heart_rate" "average_hrv"
                     "sleep_duration_seconds" "spo2_average"))
      (let ((present (remove-if-not (lambda (row) (nlk:json-value row :number field)) rows)))
        (when present
          (setf (gethash field fields)
                (nlk:json-object "days" (length present)
                                 "last_day" (day-of (car (last present))))))))
    (let* ((path (daily-path))
           (written (ignore-errors (file-write-date path)))
           (age (and written (/ (- (get-universal-time) written) 3600.0))))
      (nlk:encode-json-object
       (nlk:json-object
        "days" (length rows)
        "first_day" (or first-day "")
        "last_day" (or last-day "")
        "fields" fields
        "last_sync_hours_ago" (and age (/ (round (* age 10)) 10.0))
        "stale" (or (null age) (> age 36)))))))

(defun food-answer (start end)
  (let* ((entries (remove-if-not
                   (lambda (entry)
                     (let ((day (or (nlk:json-value entry :string "day") "")))
                       (and (or (null start) (not (string< day start)))
                            (or (null end) (not (string< end day))))))
                   (nlk:read-json-lines (food-path))))
         (with-calories (remove-if-not (lambda (entry) (nlk:json-value entry :integer "calories"))
                                        entries)))
    (nlk:encode-json-object
     (nlk:json-object
      "count" (length entries)
      "with_calories" (length with-calories)
      "total_calories" (reduce #'+ with-calories :key (lambda (entry)
                                                          (or (nlk:json-value entry :integer "calories") 0))
                                :initial-value 0)
      "entries" (coerce entries 'vector)))))

(defun query-now (call)
  "The health_query tool: one read of the local store."
  (answering
    (let ((type (or (nonblank (query-request-query-type call)) "recent"))
          (rows (store-rows)))
      (cond
        ((string= type "recent")
         (let* ((limit (max 1 (or (query-request-limit call) 7)))
                (shown (if (> (length rows) limit) (last rows limit) rows)))
           (nlk:encode-json-object
            (nlk:json-object "query_type" type "count" (length shown)
                             "days" (coerce shown 'vector)))))
        ((string= type "date_range")
         (multiple-value-bind (start end) (window-of call)
           (unless (or start end) (fail "date_range needs start and end, e.g. 2026-01-01 and 2026-01-31"))
           (let ((shown (in-window rows start end)))
             (nlk:encode-json-object
              (nlk:json-object "query_type" type "start" (or start "") "end" (or end "")
                               "count" (length shown) "days" (coerce shown 'vector))))))
        ((string= type "stress_days")
         (let* ((limit (max 1 (or (query-request-limit call) 10)))
                (scored (remove-if-not (lambda (row) (nlk:json-value row :number "stress_high")) rows))
                (top (subseq (sort (copy-list scored) #'> :key (lambda (row)
                                                                 (nlk:json-value row :number "stress_high")))
                             0 (min limit (length scored)))))
           (nlk:encode-json-object
            (nlk:json-object "query_type" type "count" (length top)
                             "days" (coerce top 'vector)))))
        ((string= type "correlate") (correlate-answer call rows))
        ((string= type "coverage") (coverage-answer rows))
        ((string= type "food")
         (multiple-value-bind (start end) (window-of call)
           (food-answer start end)))
        (t (fail "unknown query_type ~s" type))))))

;;; --- health_log_food ------------------------------------------------------------

(defun log-food (call)
  "The health_log_food tool: one meal appended to the store."
  (answering
    (let* ((day (checked-day (food-entry-day call) "day"))
           (description (nonblank (food-entry-description call)))
           (calories (food-entry-calories call))
           (items (food-entry-items call))
           (entry (nlk:json-object "day" day
                                    "logged_at" (nlk:iso-time (nlk:unix-now))
                                    "description" (or description ""))))
      (unless description (fail "description is empty: say what was eaten"))
      (when calories (setf (gethash "calories" entry) calories))
      (when (and items (plusp (length items)))
        (setf (gethash "items" entry) items))
      (nlk:append-json-line (food-path) entry)
      (let* ((same-day (remove-if-not (lambda (other) (string= day (or (nlk:json-value other :string "day") "")))
                                       (nlk:read-json-lines (food-path))))
             (logged (remove-if-not (lambda (other) (nlk:json-value other :integer "calories"))
                                     same-day)))
        (nlk:encode-json-object
         (nlk:json-object
          "ok" t "day" day "description" description
          "calories" (or calories 0)
          "meals_today" (length same-day)
          "meals_with_calories_today" (length logged)
          "total_calories_today" (reduce #'+ logged :initial-value 0
                                          :key (lambda (other)
                                                 (or (nlk:json-value other :integer "calories") 0)))))))))

;;; --- the /health command ---------------------------------------------------------

(defun coverage-text ()
  "The store in a few lines a person reads."
  (let* ((rows (store-rows))
         (path (daily-path)))
    (if (null rows)
        (format nil "No health data yet. Run /health sync (or ask for it) to fetch ~a days from Oura."
                (setting :days))
        (let ((written (ignore-errors (file-write-date path)))
              (last (car (last rows))))
          (format nil "~a days stored, ~a to ~a.~@[ Synced ~a hour(s) ago.~] Last night: sleep ~@[~a~], readiness ~@[~a~]."
                  (length rows) (day-of (first rows)) (day-of last)
                  (and written (round (- (get-universal-time) written) 3600))
                  (nlk:json-value last :integer "sleep_score")
                  (nlk:json-value last :integer "readiness_score"))))))

(defun health-command (args session-id)
  "The /health command: /health answers the store's coverage, /health sync
fetches the section's window, /health help says what else there is."
  (declare (ignore session-id))
  (let ((words (nlk:split-words (or args ""))))
    (cond
      ((null words) (coverage-text))
      ((member (string-downcase (first words)) '("status" "coverage") :test #'string=)
       (coverage-text))
      ((string= (string-downcase (first words)) "sync")
       (sync-range nil nil (ignore-errors (parse-integer (second words) :junk-allowed t))))
      (t (format nil "~a~%/health sync [days] fetches from Oura; /health answers what is stored."
                 (coverage-text))))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell health-data
  (:section ("healthdata")
    (:guide "a personal access token for Oura (cloud.ouraring.com, Personal Access Tokens) named in oura_token_env or kept in oura_token_file; the store lives under data_dir and starts empty; next step: /health sync, then ask how you slept")
    (:one-of "oura_token_env" "oura_token_file")
    ("oura_token_env" :env
     :doc "the environment variable holding an Oura personal access token (OURA_TOKEN)")
    ("oura_token_file" :path
     :doc "a file holding an Oura personal access token, which the file's own permissions guard")
    ("data_dir" :path
     :doc "where the store keeps its JSON lines; default ~/.nodecode/health-data")
    ("days" :integer :default 30 :min 1
     :doc "how many days one sync fetches when the call names no window")
    ("timeout" :integer :default 30 :min 1
     :doc "how long one Oura request may take, in seconds"))
  (:settings #'read-settings)
  (:help :health-data
         "health-data: health_query reads the local store, health_sync fetches Oura, health_log_food logs a meal"
         *manual*)
  (:tool "health_sync" (nlk:contract 'sync-request) #'sync-now)
  (:tool "health_query" (nlk:contract 'query-request) #'query-now)
  (:tool "health_log_food" (nlk:contract 'food-entry) #'log-food)
  (:command "health" 'health-command
            :description "Health data: /health for what is stored, /health sync [days] to fetch from Oura"))
