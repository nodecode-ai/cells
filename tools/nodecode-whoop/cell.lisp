;;;; cell.lisp --- WHOOP's five tools, over the WHOOP developer API v2.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :whoop) answers the primer below, and every request's help section
;;;; carries one line naming it. Nothing hooks the provider path: WHOOP speaks
;;;; HTTPS, and NLK:HTTP is the one seam.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "whoop": {"client_id": "...", "client_secret_env": "WHOOP_CLIENT_SECRET",
;;;;             "refresh_token_env": "WHOOP_REFRESH_TOKEN"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Rewritten from davidmosiah's whoop-mcp (see NOTICE): the collection
;;;; endpoints, the scored-record reads, the per-metric trend statistic
;;;; (average, min, max, a least-squares slope and the direction it spells)
;;;; and the not-medical-advice posture. The OAuth round trip the connector's
;;;; own CLI ran is one refresh-token exchange here, held in memory.

(in-package #:nodecode-whoop)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "WHOOP is available through the nodecode-whoop cell: five tools, which a person can also
call from eval in the whoop package.
  whoop_daily_summary  {days}
      The headline of the last DAYS days (1 to 30, default 7): the newest scored recovery
      (recovery score, HRV in milliseconds, resting heart rate, SpO2, skin temperature), the
      newest scored sleep (performance, efficiency, consistency, hours in bed and asleep,
      disturbances, respiratory rate), the newest scored cycle (day strain, kilojoules,
      average and max heart rate), the workouts of the window, and the window's counts and
      averages. Answer the person's question with these numbers and their dates first.
  whoop_recovery_trend  {days}
      Recovery over the last DAYS days (2 to 30, default 30): recovery score, HRV and
      resting heart rate, each with avg, min, max, slope (per scored day, oldest first) and
      direction - rising, falling, stable or insufficient_data. Use it for \"is my recovery
      trending up?\" instead of reading the raw records yourself.
  whoop_sleep_trend  {days}
      The same statistic for sleep performance percentage, hours asleep and sleep efficiency
      percentage. Use it for \"is my sleep improving?\".
  whoop_list_workouts  {days, limit}
      The scored workouts of the last DAYS days (default 14), newest first, at most LIMIT
      (default 10): sport id, start and end, strain, kilojoules, average and max heart
      rate, distance and the fraction of the workout recorded.
  whoop_get_profile  {}
      Who the token belongs to: the WHOOP profile, and the body measurements WHOOP holds
      (height, weight, max heart rate).
A day with no scored record is a gap, not a zero: say the data is missing rather than
reporting 0. WHOOP scores a night when the strap syncs, so a recovery or sleep the person
remembers may simply not be scored yet. None of this is medical advice: it supports
reflection and planning, not diagnosis or treatment."
  "What (help :whoop) answers while the cell runs.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (whoop-error (condition)
       (nle:failure (format nil "ERROR: ~a" (whoop-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: WHOOP: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun answer-detail (body)
  "WHOOP's own words for a failed call, clipped, as a string that starts with
 a colon, or the empty string."
  (let ((text (nlk:trimmed (or body ""))))
    (if (plusp (length text))
        (format nil ": ~a" (nlk:clip text 300))
        "")))

;;; --- reading the section --------------------------------------------------------

(defvar *token* nil
  "The access token this process holds: (:access TOKEN :expires UNIVERSAL-TIME),
 or NIL before the first exchange.")

(defvar *refresh-token* nil
  "The refresh token the running cell exchanges and WHOOP may rotate.")

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the two secrets the section names: read once, at
start, and the access token cleared so each start begins with none."
  (declare (ignore table))
  (let* ((client-id (nonblank (getf values :client-id)))
         (secret-env (nonblank (getf values :client-secret-env)))
         (token-env (nonblank (getf values :refresh-token-env)))
         (token-file (nonblank (getf values :refresh-token-file)))
         (api-base (string-right-trim "/" (or (nonblank (getf values :api-base))
                                               "https://api.prod.whoop.com/developer")))
         (token-url (or (nonblank (getf values :token-url))
                        "https://api.prod.whoop.com/oauth/oauth2/token"))
         (plist (copy-list values)))
    (unless client-id
      (nlk:config-error "client_id is empty: it is the client id of your own WHOOP developer app, from developer.whoop.com"))
    (unless secret-env
      (nlk:config-error "client_secret_env is empty: name the environment variable holding your WHOOP developer app's client secret"))
    (when (and token-env token-file)
      (nlk:config-error "set one of refresh_token_env and refresh_token_file, not both"))
    (unless (or token-env token-file)
      (nlk:config-error "no refresh token: set refresh_token_env to the environment variable holding one, or refresh_token_file to a file holding one; the connector's own auth command creates one"))
    (unless (or (uiop:string-prefix-p "http://" api-base)
                (uiop:string-prefix-p "https://" api-base))
      (nlk:config-error "api_base ~s has no http:// or https:// scheme" api-base))
    (setf (getf plist :client-id) client-id
          (getf plist :api-base) api-base
          (getf plist :token-url) token-url
          (getf plist :refresh-token-file) token-file
          (getf plist :client-secret)
          (or (nonblank (nle::credential-env secret-env))
              (nlk:config-error "client_secret_env names ~a, which is unset or empty" secret-env))
          (getf plist :refresh-token)
          (if token-env
              (or (nonblank (nle::credential-env token-env))
                  (nlk:config-error "refresh_token_env names ~a, which is unset or empty" token-env))
              (or (nonblank (ignore-errors
                             (uiop:read-file-string (nlk:expand-home token-file))))
                  (nlk:config-error "refresh_token_file ~a cannot be read, or is empty" token-file))))
    (setf *refresh-token* (getf plist :refresh-token)
          *token* nil)
    plist))

;;; --- the access token -------------------------------------------------------------

(defun url-encode (text)
  "TEXT percent-encoded for a query string or a form body."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets text :external-format :utf-8)
          do (let ((character (code-char octet)))
               (if (or (and (char<= #\a character) (char<= character #\z))
                       (and (char<= #\A character) (char<= character #\Z))
                       (and (char<= #\0 character) (char<= character #\9))
                       (find character "-._~"))
                   (write-char character out)
                   (format out "%~2,'0X" octet))))))

(defun keep-rotated-refresh-token (rotated)
  "WHOOP hands back a new refresh token on most exchanges; keep it, and let a
file the operator named hold the rotation so the next process starts from it."
  (when (and rotated (not (equal rotated *refresh-token*)))
    (setf *refresh-token* rotated)
    (let ((path (nonblank (setting :refresh-token-file))))
      (when path
        (let ((path (nlk:expand-home path)))
          (handler-case
              (progn
                (with-open-file (out path :direction :output :if-exists :supersede
                                          :if-does-not-exist :create :external-format :utf-8)
                  (write-string rotated out)
                  (terpri out))
                (ignore-errors (sb-posix:chmod path #o600)))
            (error (condition)
              (nle:notice (format nil "whoop: the rotated refresh token could not be written to ~a: ~a"
                                  path (nlk:one-line (princ-to-string condition)))
                          :level :warning :key "nodecode-whoop-token"))))))))

(defun refresh-access-token ()
  "One refresh-token exchange against WHOOP's token endpoint => the access token."
  (let* ((body (format nil "grant_type=refresh_token&refresh_token=~a&client_id=~a&client_secret=~a"
                        (url-encode (or *refresh-token* ""))
                        (url-encode (setting :client-id))
                        (url-encode (setting :client-secret))))
         (answer (multiple-value-bind (text status)
                     (exchange :post (setting :token-url) :content body
                               :headers (list (cons "content-type" "application/x-www-form-urlencoded")
                                              (cons "accept" "application/json")))
                   (if (and (integerp status) (< status 400))
                       (or (ignore-errors (nlk:decode-json text))
                           (fail "WHOOP's token endpoint answered something that is not JSON~a"
                                 (answer-detail text)))
                       (fail "WHOOP's token endpoint answered HTTP ~a: the refresh token may have been revoked, so run the connector's auth again~a"
                             status (answer-detail text)))))
         (access (or (nlk:json-value answer :text "access_token")
                     (fail "WHOOP's token endpoint answered no access_token"))))
    (keep-rotated-refresh-token (nlk:json-value answer :text "refresh_token"))
    (setf *token* (list :access access
                        :expires (+ (get-universal-time)
                                    (or (nlk:json-value answer :integer "expires_in") 3600))))
    access))

(defun access-token (&key force)
  "The bearer token, refreshed through the stored refresh token when it is
within two minutes of expiry, or FORCE."
  (let ((cached *token*))
    (if (and (not force) cached
             (> (getf cached :expires) (+ (get-universal-time) 120)))
        (getf cached :access)
        (refresh-access-token))))

;;; --- one exchange ----------------------------------------------------------------

(defun exchange (method url &key headers content)
  "One WHOOP exchange => (values TEXT STATUS). A dial that fails is a refusal."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http method url
                    :headers headers
                    :content content
                    :timeout (setting :timeout)
                    :connect-timeout (setting :timeout))
        (values (nlk:body-text body) status))
    (error (condition)
      (fail "~a did not answer: ~a" url (nlk:one-line (princ-to-string condition))))))

(defun query-string (parameters)
  "PARAMETERS — a list of (NAME . VALUE) — as a query string, empty or starting
with ?."
  (with-output-to-string (out)
    (let ((first t))
      (dolist (pair parameters)
        (let ((value (cdr pair)))
          (when (and value (not (equal value "")))
            (write-string (if first "?" "&") out)
            (setf first nil)
            (format out "~a=~a" (car pair) (url-encode (princ-to-string value)))))))))

(defun api-get (path parameters &key (retry t))
  "PATH's decoded answer, or a refusal carrying WHOOP's own words."
  (let ((url (concatenate 'string (setting :api-base) path (query-string parameters))))
    (multiple-value-bind (text status)
        (exchange :get url
                  :headers (list (cons "authorization" (format nil "Bearer ~a" (access-token)))
                                 (cons "accept" "application/json")))
      (cond ((and (integerp status) (< status 400))
             (or (ignore-errors (nlk:decode-json text)) (nlk:json-object)))
            ((and retry (eql status 401))
             ;; The token in hand was refused: ask for one more and try once.
             (access-token :force t)
             (api-get path parameters :retry nil))
            ((eql status 429)
             (fail "WHOOP rate-limited this call (HTTP 429), so wait a minute before asking again~a"
                   (answer-detail text)))
            (t (fail "WHOOP answered HTTP ~a for ~a~a" status path (answer-detail text)))))))

(defun list-records (path parameters &key (pages 3))
  "PATH's records, up to PAGES pages of 25, following WHOOP's next_token."
  (let ((records '()) (token nil) (done nil))
    (loop repeat pages
          while (not done)
          do (let ((answer (api-get path (append parameters
                                                 (and token (list (cons "nextToken" token)))))))
               (setf records (nconc records (coerce (nlk:json-array answer "records") 'list))
                     token (nlk:json-value answer :string "next_token"))
               (unless token (setf done t))))
    records))

;;; --- reading records ---------------------------------------------------------

(defun iso (universal)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month date hour minute second)))

(defun iso-now () (iso (get-universal-time)))

(defun iso-ago (days) (iso (- (get-universal-time) (* days 86400))))

(defun clamp-days (asked default lowest)
  (let ((days (if (and (integerp asked) (plusp asked)) asked default)))
    (max lowest (min 30 days))))

(defun round-to (value places)
  "VALUE to PLACES decimals, or NIL when VALUE is not a number."
  (when (numberp value)
    (let ((scale (coerce (expt 10 places) 'double-float)))
      (/ (round (* (coerce value 'double-float) scale)) scale))))

(defun milli-hours (milliseconds)
  (and (numberp milliseconds) (round-to (/ milliseconds 3600000.0d0) 2)))

(defun scored (records)
  (remove-if-not (lambda (record)
                   (equal "SCORED" (nlk:json-value record :string "score_state")))
                 records))

(defun nap-p (record)
  (eq t (nlk:json-value record :boolean "nap")))

(defun score-of (record)
  (nlk:json-value record :object "score"))

(defun record-date (record)
  (or (nlk:json-value record :string "created_at")
      (nlk:json-value record :string "start")
      ""))

(defun oldest-first (records)
  (sort (copy-list records) #'string< :key #'record-date))

(defun newest-first (records)
  (sort (copy-list records) #'string> :key #'record-date))

(defun latest (records)
  (first (newest-first records)))

(defun metric (record &rest path)
  (apply #'nlk:json-value (score-of record) :number path))

(defun average-of (records &rest path)
  (let ((values (loop for record in records
                      for value = (apply #'metric record path)
                      when (numberp value) collect value)))
    (and values (round-to (/ (reduce #'+ values) (length values)) 1))))

;;; --- the trend statistic ---------------------------------------------------------

(defun trend-stat (values epsilon)
  "VALUES — numbers or NIL, oldest first — as a JSON object: avg, min, max,
slope and the direction it spells, or insufficient_data below two points.
The slope is a least-squares fit against each value's own place in the window,
so a gap is a gap rather than a point pulled forward."
  (let ((points (loop for value in values
                      for index from 0
                      when (numberp value) collect (cons index value)))
        (n 0))
    (setf n (length points))
    (if (zerop n)
        (nlk:json-object "direction" "insufficient_data" "n_valid" 0)
        (let* ((numbers (mapcar #'cdr points))
               (avg (/ (reduce #'+ numbers) n))
               (low (reduce #'min numbers))
               (high (reduce #'max numbers)))
          (if (< n 2)
              (nlk:json-object "avg" (round-to avg 1) "min" (round-to low 1)
                                "max" (round-to high 1)
                                "direction" "insufficient_data" "n_valid" n)
              (let* ((mean-x (/ (reduce #'+ (mapcar #'car points)) n))
                     (numerator (reduce #'+ (mapcar (lambda (point)
                                                      (* (- (car point) mean-x)
                                                         (- (cdr point) avg)))
                                                    points)))
                     (denominator (reduce #'+ (mapcar (lambda (point)
                                                        (expt (- (car point) mean-x) 2))
                                                      points)))
                     (slope (if (zerop denominator) 0 (/ numerator denominator)))
                     (direction (cond ((> slope epsilon) "rising")
                                      ((< slope (- epsilon)) "falling")
                                      (t "stable"))))
                (nlk:json-object "avg" (round-to avg 1) "min" (round-to low 1)
                                  "max" (round-to high 1)
                                  "slope" (round-to slope 3)
                                  "direction" direction "n_valid" n)))))))

(defun trend-line (label stat &rest unit)
  "One sentence of the trend's diagnostic, or NIL when nothing was scored."
  (let ((avg (nlk:json-value stat :number "avg")))
    (when (numberp avg)
      (format nil "~a averaged ~a~@[ ~a~] over ~a scored day(s) and is ~a."
              label avg (and unit (first unit))
              (nlk:json-value stat :integer "n_valid")
              (nlk:json-value stat :string "direction")))))

;;; --- the views ---------------------------------------------------------------

(defun recovery-view (record)
  (if (null record)
      (nlk:json-object "note" "no scored recovery in the window")
      (let ((score (score-of record)))
        (nlk:json-object
         "date" (record-date record)
         :opt "recovery_score" (nlk:json-value score :number "recovery_score")
         :opt "hrv_rmssd_milli" (nlk:json-value score :number "hrv_rmssd_milli")
         :opt "resting_heart_rate" (nlk:json-value score :number "resting_heart_rate")
         :opt "spo2_percentage" (nlk:json-value score :number "spo2_percentage")
         :opt "skin_temp_celsius" (nlk:json-value score :number "skin_temp_celsius")
         :opt "user_calibrating" (nlk:json-value score :boolean "user_calibrating")))))

(defun sleep-view (record)
  (if (null record)
      (nlk:json-object "note" "no scored sleep in the window")
      (let* ((score (score-of record))
             (stages (nlk:json-value score :object "stage_summary"))
             (in-bed (nlk:json-value stages :number "total_in_bed_time_milli"))
             (awake (nlk:json-value stages :number "total_awake_time_milli")))
        (nlk:json-object
         "date" (record-date record)
         :opt "sleep_performance_percentage" (nlk:json-value score :number "sleep_performance_percentage")
         :opt "sleep_efficiency_percentage" (nlk:json-value score :number "sleep_efficiency_percentage")
         :opt "sleep_consistency_percentage" (nlk:json-value score :number "sleep_consistency_percentage")
         :opt "respiratory_rate" (nlk:json-value score :number "respiratory_rate")
         :opt "hours_in_bed" (milli-hours in-bed)
         :opt "hours_asleep" (and (numberp in-bed) (milli-hours (- in-bed (or awake 0))))
         :opt "disturbance_count" (nlk:json-value stages :integer "disturbance_count")))))

(defun cycle-view (record)
  (if (null record)
      (nlk:json-object "note" "no scored cycle in the window")
      (let ((score (score-of record)))
        (nlk:json-object
         "date" (record-date record)
         :opt "strain" (nlk:json-value score :number "strain")
         :opt "kilojoule" (nlk:json-value score :number "kilojoule")
         :opt "average_heart_rate" (nlk:json-value score :integer "average_heart_rate")
         :opt "max_heart_rate" (nlk:json-value score :integer "max_heart_rate")))))

(defun workout-view (record)
  (let ((score (score-of record)))
    (nlk:json-object
     "date" (record-date record)
     :opt "end" (nlk:json-value record :string "end")
     :opt "sport_id" (nlk:json-value record :any "sport_id")
     :opt "strain" (nlk:json-value score :number "strain")
     :opt "kilojoule" (nlk:json-value score :number "kilojoule")
     :opt "average_heart_rate" (nlk:json-value score :integer "average_heart_rate")
     :opt "max_heart_rate" (nlk:json-value score :integer "max_heart_rate")
     :opt "distance_meter" (nlk:json-value score :number "distance_meter")
     :opt "percent_recorded" (nlk:json-value score :number "percent_recorded"))))

(defun window (days)
  (list (cons "start" (iso-ago days)) (cons "limit" 25)))

;;; --- whoop_daily_summary ---------------------------------------------------------

(defun daily-summary (call)
  "The whoop_daily_summary tool: the headline of the last DAYS days."
  (answering
    (let* ((days (clamp-days (summary-request-days call) 7 1))
           (parameters (window days))
           (recoveries (scored (list-records "/v2/recovery" parameters)))
           (sleeps (scored (remove-if #'nap-p (list-records "/v2/activity/sleep" parameters))))
           (cycles (scored (list-records "/v2/cycle" parameters)))
           (workouts (newest-first (scored (list-records "/v2/activity/workout" parameters)))))
      (nlk:encode-json-object
       (nlk:json-object
        "as_of" (iso-now)
        "window_days" days
        "latest_recovery" (recovery-view (latest recoveries))
        "latest_sleep" (sleep-view (latest sleeps))
        "latest_cycle" (cycle-view (latest cycles))
        "workouts" (map 'vector #'workout-view workouts)
        "window" (nlk:json-object
                  "recoveries" (length recoveries)
                  "sleeps" (length sleeps)
                  "cycles" (length cycles)
                  "workouts" (length workouts)
                  :opt "recovery_score_avg" (average-of recoveries "recovery_score")
                  :opt "sleep_performance_avg" (average-of sleeps "sleep_performance_percentage")
                  :opt "strain_avg" (average-of cycles "strain"))
        "note" "Not medical advice. A day with no scored record is a gap, not a zero.")))))

;;; --- whoop_recovery_trend ---------------------------------------------------------

(defun recovery-trend (call)
  "The whoop_recovery_trend tool: recovery score, HRV and resting heart rate
 over the last DAYS days, each as a fitted trend."
  (answering
    (let* ((days (clamp-days (trend-request-days call) 30 2))
           (recoveries (oldest-first (scored (list-records "/v2/recovery" (window days)))))
           (score (trend-stat (mapcar (lambda (record) (metric record "recovery_score")) recoveries) 0.3))
           (hrv (trend-stat (mapcar (lambda (record) (metric record "hrv_rmssd_milli")) recoveries) 0.2))
           (rhr (trend-stat (mapcar (lambda (record) (metric record "resting_heart_rate")) recoveries) 0.1)))
      (nlk:encode-json-object
       (nlk:json-object
        "kind" "recovery_trend"
        "generated_at" (iso-now)
        "lookback_days" days
        "count" (length recoveries)
        "metrics" (nlk:json-object "recovery_score" score
                                    "hrv_rmssd_milli" hrv
                                    "resting_heart_rate" rhr)
        "summary" (coerce (remove nil (list (trend-line "Recovery score" score)
                                             (trend-line "HRV" hrv "ms")
                                             (trend-line "Resting heart rate" rhr "bpm")))
                           'vector)
        "note" "Slope is per scored day, oldest first. Not medical advice.")))))

;;; --- whoop_sleep_trend ------------------------------------------------------------

(defun sleep-duration-hours (record)
  (let* ((score (score-of record))
         (stages (nlk:json-value score :object "stage_summary"))
         (in-bed (nlk:json-value stages :number "total_in_bed_time_milli"))
         (awake (nlk:json-value stages :number "total_awake_time_milli")))
    (and (numberp in-bed) (/ (- in-bed (or awake 0)) 3600000.0d0))))

(defun sleep-trend (call)
  "The whoop_sleep_trend tool: sleep performance, hours asleep and efficiency
 over the last DAYS days, each as a fitted trend."
  (answering
    (let* ((days (clamp-days (trend-request-days call) 30 2))
           (sleeps (oldest-first (scored (remove-if #'nap-p (list-records "/v2/activity/sleep" (window days))))))
           (performance (trend-stat (mapcar (lambda (record) (metric record "sleep_performance_percentage")) sleeps) 0.3))
           (duration (trend-stat (mapcar #'sleep-duration-hours sleeps) 0.05))
           (efficiency (trend-stat (mapcar (lambda (record) (metric record "sleep_efficiency_percentage")) sleeps) 0.2)))
      (nlk:encode-json-object
       (nlk:json-object
        "kind" "sleep_trend"
        "generated_at" (iso-now)
        "lookback_days" days
        "count" (length sleeps)
        "metrics" (nlk:json-object "sleep_performance_percentage" performance
                                    "sleep_duration_hours" duration
                                    "sleep_efficiency_percentage" efficiency)
        "summary" (coerce (remove nil (list (trend-line "Sleep performance" performance "%")
                                             (trend-line "Sleep duration" duration "hours")
                                             (trend-line "Sleep efficiency" efficiency "%")))
                           'vector)
        "note" "Naps are left out, as the connector left them out. Not medical advice.")))))

;;; --- whoop_list_workouts -----------------------------------------------------------

(defun list-workouts (call)
  "The whoop_list_workouts tool: the scored workouts of the last DAYS days."
  (answering
    (let* ((days (clamp-days (workout-request-days call) 14 1))
           (asked (workout-request-limit call))
           (limit (max 1 (min 25 (if (and (integerp asked) (plusp asked)) asked 10))))
           (workouts (newest-first (scored (list-records "/v2/activity/workout" (window days)))))
           (shown (if (> (length workouts) limit) (subseq workouts 0 limit) workouts))
           (answer (nlk:json-object
                    "count" (length shown)
                    "total_in_window" (length workouts)
                    "window_days" days
                    "workouts" (map 'vector #'workout-view shown))))
      (when (> (length workouts) limit)
        (setf (gethash "truncated" answer) t
              (gethash "truncated_note" answer)
              (format nil "Showing the newest ~a of ~a scored workouts in the window; raise limit (to 25) or shorten days to see a different cut."
                      limit (length workouts))))
      (nlk:encode-json-object answer))))

;;; --- whoop_get_profile ---------------------------------------------------------------

(defun get-profile (call)
  "The whoop_get_profile tool: the profile the token belongs to, and the body
 measurements WHOOP holds."
  (declare (ignore call))
  (answering
    (let ((profile (api-get "/v2/user/profile/basic" '()))
          (body (api-get "/v2/user/measurement/body" '())))
      (nlk:encode-json-object
       (nlk:json-object
        :opt "user_id" (nlk:json-value profile :any "user_id")
        :opt "first_name" (nlk:json-value profile :string "first_name")
        :opt "last_name" (nlk:json-value profile :string "last_name")
        :opt "email" (nlk:json-value profile :string "email")
        :opt "height_meter" (nlk:json-value body :number "height_meter")
        :opt "weight_kilogram" (nlk:json-value body :number "weight_kilogram")
        :opt "max_heart_rate" (nlk:json-value body :integer "max_heart_rate")
        "note" "WHOOP's own body measurements; not medical advice.")))))

;;; --- the declaration ---------------------------------------------------------------

(defparameter +profile-schema+
  (list :description "Who the WHOOP token belongs to: the WHOOP profile, and the body measurements WHOOP holds (height, weight, max heart rate). Read-only, and the call carries nothing."
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:make-json-object))))

(nle:define-cell whoop
  (:section ("whoop")
    (:guide "your own WHOOP developer app (developer.whoop.com) supplies client_id and a client secret; the connector's own auth command turns them into a refresh token; name the secret in an environment variable and the refresh token in one too, or in a file; next step: ask how today's recovery looks")
    (:one-of "refresh_token_env" "refresh_token_file")
    ("client_id" :string :required t
     :doc "the WHOOP developer app's client id, which the app page shows; not a secret")
    ("client_secret_env" :env
     :doc "the environment variable holding the developer app's client secret (WHOOP_CLIENT_SECRET)")
    ("refresh_token_env" :env
     :doc "the environment variable holding the OAuth refresh token the connector's auth command created")
    ("refresh_token_file" :path
     :doc "a file holding the refresh token, which the file's own permissions guard; it is rewritten when WHOOP rotates the token")
    ("api_base" :string :default "https://api.prod.whoop.com/developer"
     :doc "WHOOP's API root")
    ("token_url" :string :default "https://api.prod.whoop.com/oauth/oauth2/token"
     :doc "WHOOP's OAuth token endpoint, where a refresh token becomes an access token")
    ("timeout" :integer :default 20 :min 1
     :doc "how long one WHOOP request may take, in seconds"))
  (:settings #'read-settings)
  (:help :whoop
         "whoop: whoop_daily_summary, whoop_recovery_trend, whoop_sleep_trend, whoop_list_workouts and whoop_get_profile read your own WHOOP recovery, sleep and strain"
         *manual*)
  (:tool "whoop_daily_summary" (nlk:contract 'summary-request) #'daily-summary)
  (:tool "whoop_recovery_trend" (nlk:contract 'trend-request) #'recovery-trend)
  (:tool "whoop_sleep_trend" (nlk:contract 'trend-request) #'sleep-trend)
  (:tool "whoop_list_workouts" (nlk:contract 'workout-request) #'list-workouts)
  (:tool "whoop_get_profile" +profile-schema+ #'get-profile))
