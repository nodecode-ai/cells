;;;; cell.lisp --- Home Assistant's long-term statistics, as two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :hass-statistics) answers the primer below, and every request's
;;;; help section carries one line naming it.
;;;;
;;;; Statistics are not on Home Assistant's REST API: the Recorder answers
;;;; them on the instance's WebSocket API, so the cell dials that with the
;;;; RFC-6455 client the runtime already carries -- one authenticated
;;;; exchange per call, closed after it. No thread, no subscription, no event.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "hass-statistics": {"url": "http://homeassistant.local:8123",
;;;;                        "token_env": "HASS_TOKEN"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from goncami's Hermes Agent plugin (see NOTICE): the two tools and
;;;; their schemas, the Recorder commands, the input validation, the operator
;;;; allow/deny lists and their precedence, the three budgets, and the answer
;;;; shapes.

(in-package #:nodecode-hass-statistics)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Home Assistant's long-term statistics are available through the nodecode-hass-statistics
cell: two read-only tools, which a person can also call from eval in the hass-statistics
package.
  ha_list_statistics  {search}
      The statistic ids the Recorder holds, each with its unit, source and what it has
      (mean, sum). Nothing is written and no service is called. Use search to narrow a
      large install: it matches any part of the id, case-insensitively. Every answer is
      limited by the operator's allow/deny lists, so an id the lists exclude never
      appears; a denylist wins over an allowlist.
  ha_get_statistics  {statistic_ids, start_time, end_time, period, types}
      Aggregated history for one or more ids. Discover the ids with ha_list_statistics
      first; never guess one. Every timestamp needs a timezone offset
      (2026-10-01T00:00:00+02:00), end_time may be left out (it defaults to now) and
      must be later than start_time. period is 5minute, hour, day (the default), week,
      month or year; 5minute reads the Recorder's short-term data, whose retention is
      short. types are mean, min, max, state, sum and change; for energy ask for change,
      which is what an interval used, rather than the cumulative sum. Available fields
      depend on the statistic: a sensor's mean and min/max, a meter's sum and change.
      A query past the row budget is refused before anything is asked, rather than
      truncated: fewer ids, a shorter range or a coarser period answers it.
A denylisted id is refused before the socket opens. These tools read; they change nothing."
  "What (help :hass-statistics) answers while the cell runs.")

;;; --- the Recorder's vocabulary --------------------------------------------------

(defparameter +periods+ '("5minute" "hour" "day" "week" "month" "year")
  "The periods Recorder aggregates by, as it names them.")

(defparameter +types+ '("mean" "min" "max" "state" "sum" "change")
  "The statistic values Recorder keeps for a period.")

(defparameter +period-seconds+
  '(("5minute" . 300) ("hour" . 3600) ("day" . 82800)
    ("week" . 601200) ("month" . 2415600) ("year" . 31532400))
  "How long one period is, in seconds. A conservative floor: a day is 23
hours, a week 167, a month 28 days less an hour, a year 365 less an hour, so
a range's row estimate never undershoots on a daylight-saving change.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (hass-statistics-error (condition)
       (nle:failure (format nil "ERROR: ~a" (hass-statistics-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Home Assistant: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the access token the section names: read once, here."
  (declare (ignore table))
  (let ((url (string-right-trim "/" (or (getf values :url) "")))
        (env (nonblank (getf values :token-env)))
        (file (nonblank (getf values :token-file)))
        (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of token_env and token_file, not both"))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (nlk:config-error "url ~s has no http:// or https:// scheme: set it to the full base URL of your instance, e.g. \"https://ha.example.com\" or \"http://homeassistant.local:8123\"" url))
    (when (or (find #\Space url) (find #\Tab url) (find #\? url) (find #\# url))
      (nlk:config-error "url ~s carries whitespace, a query or a fragment: set it to the instance's base URL alone" url))
    (setf (getf plist :url) url
          (getf plist :token)
          (cond (env (or (nonblank (nle::credential-env env))
                         (nlk:config-error "token_env names ~a, which is unset or empty" env)))
                (file (or (nonblank (ignore-errors
                                     (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "token_file ~a cannot be read, or is empty" file)))
                (t (nlk:config-error "no token: set token_env to the environment variable holding a long-lived access token, or token_file to a file holding one"))))
    plist))

;;; --- instants --------------------------------------------------------------------

(defparameter +instant-pattern+
  "\\A(\\d{4})-(\\d{2})-(\\d{2})[Tt ](\\d{2}):(\\d{2})(?::(\\d{2}))?(?:\\.\\d+)?([Zz]|[+-]\\d{2}:?\\d{2})\\z"
  "An ISO-8601 instant with a mandatory timezone: Python's fromisoformat reads
the same shape, and the timezone is what the original plugin insisted on.")

(defun civil-days (year month day)
  "Days from 1970-01-01 to YEAR-MONTH-DAY, proleptic Gregorian."
  (let* ((shifted (if (<= month 2) (1- year) year))
         (era (floor (if (>= shifted 0) shifted (- shifted 399)) 400))
         (year-of-era (- shifted (* era 400)))
         (day-of-year (+ (floor (* 153 (+ month (if (> month 2) -3 9))) 5) (1- day)))
         (day-of-era (+ (* year-of-era 365) (floor year-of-era 4)
                        (- (floor year-of-era 100)) day-of-year)))
    (- (+ (* era 146097) day-of-era) 719468)))

(defun offset-seconds (zone)
  "ZONE -- Z, +HH:MM, +HHMM or their negative -- as seconds east of UTC."
  (if (find (char zone 0) "Zz")
      0
      (let* ((sign (if (char= (char zone 0) #\-) -1 1))
             (digits (remove #\: (subseq zone 1))))
        (* sign (+ (* 3600 (parse-integer digits :end 2))
                   (* 60 (parse-integer digits :start 2)))))))

(defun parse-instant (text parameter)
  "TEXT as seconds since the epoch, or a refusal naming PARAMETER."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (unless (and text (plusp (length text)))
      (fail "~a is required: an ISO-8601 instant with a timezone offset, e.g. 2026-10-01T00:00:00+02:00"
            parameter))
    (let ((parts (multiple-value-list (ppcre:scan-to-strings +instant-pattern+ text))))
      (unless (first parts)
        (fail "~a must be an ISO-8601 instant with a timezone offset, e.g. 2026-10-01T00:00:00+02:00, not ~s"
              parameter (nlk:clip text 60)))
      (let* ((fields (coerce (second parts) 'list))
             (numbers (mapcar (lambda (field) (and field (parse-integer field)))
                              (subseq fields 0 6)))
             (zone (seventh fields)))
        (destructuring-bind (year month day hour minute &optional (second 0)) numbers
          (unless (and (<= 1 month 12) (<= 1 day 31) (< hour 24) (< minute 60) (<= 0 (or second 0) 60))
            (fail "~a is not a real date and time: ~s" parameter (nlk:clip text 60)))
          (- (+ (* (civil-days year month day) 86400)
                (* hour 3600) (* minute 60) (or second 0))
             (offset-seconds zone)))))))

;;; --- the operator's statistic filters --------------------------------------------

(defun glob-regex (pattern)
  "PATTERN as an anchored regular expression: * is any run, ? one character."
  (with-output-to-string (out)
    (write-string "\\A" out)
    (loop for character across pattern
          do (case character
               (#\* (write-string ".*" out))
               (#\? (write-char #\. out))
               (t (when (find character ".+()[]{}^$|\\") (write-char #\\ out))
                  (write-char character out))))
    (write-string "\\z" out)))

(defun statistic-matches-p (statistic-id pattern)
  "Whether STATISTIC-ID matches PATTERN: exact, a glob (* and ?), or a family
prefix, where `.` and `_` are interchangeable at the boundary so
office_thermostat_* covers office_thermostat.0_temperature. A bare pattern with
no trailing separator is exact-or-glob only, so climate.office does not swallow
climate.office_zombie."
  (and (stringp pattern)
       (plusp (length pattern))
       (or (string= statistic-id pattern)
           (and (ignore-errors (ppcre:scan (glob-regex pattern) statistic-id)) t)
           (let ((last (char pattern (1- (length pattern)))))
             (when (find last "*._")
               (let ((base (string-right-trim "*._" pattern)))
                 (and (plusp (length base))
                      (uiop:string-prefix-p base statistic-id)
                      (or (= (length statistic-id) (length base))
                          (find (char statistic-id (length base)) "._")))))))))

(defun matched-pattern (statistic-id patterns)
  "The first pattern of PATTERNS that STATISTIC-ID matches, or NIL."
  (find-if (lambda (pattern) (statistic-matches-p statistic-id pattern)) patterns))

(defun exclusion (statistic-id)
  "NIL when the operator's lists let STATISTIC-ID be read, else why not: the
denylist is a final veto, and an allowlist limits everything."
  (let ((allow (setting :entity-allowlist))
        (deny (setting :entity-denylist))
        (id (string-downcase statistic-id)))
    (let ((hit (matched-pattern id deny)))
      (cond (hit (format nil "~a is excluded by the operator's denylist (pattern ~s)" statistic-id hit))
            ((and allow (not (matched-pattern id allow)))
             (format nil "~a is not in the operator's allowlist" statistic-id))
            (t nil)))))

;;; --- budgets ----------------------------------------------------------------------

(defun check-ids-budget (ids)
  "The most distinct ids one query may name."
  (when (> (length ids) (setting :max-ids))
    (fail "too many statistic ids (~a): this instance allows ~a per query. Split the request: ask for an id or two, or narrow the list with ha_list_statistics first."
          (length ids) (setting :max-ids))))

(defparameter +epoch-offset+ 2208988800
  "Seconds from 1900-01-01, where GET-UNIVERSAL-TIME counts, to 1970-01-01,
where the ids' timestamps count.")

(defun epoch-now ()
  "Now, on the same count as PARSE-INSTANT's answers."
  (- (get-universal-time) +epoch-offset+))

(defun check-query-budget (ids start end period)
  "A conservative row estimate before anything is asked: the period, the range
and the id count. Refusing here costs the instance no work."
  (let* ((seconds (cdr (assoc period +period-seconds+ :test #'string=)))
         (duration (max 0 (- (or end (epoch-now)) start)))
         (estimated (* (length ids) (+ (ceiling duration seconds) 1)))
         (budget (setting :max-rows)))
    (when (> estimated budget)
      (fail "this query would return about ~:d rows, past the ~:d-row budget. Use fewer ids, a shorter range or a coarser period (month or year)."
            estimated budget))))

(defun check-rows-budget (count)
  "What the instance actually answered, across every series."
  (let ((budget (setting :max-rows)))
    (when (> count budget)
      (fail "the answer holds ~:d rows, past the ~:d-row budget; narrow the search, use fewer ids or split the time range. No partial history is returned."
            count budget))))

(defun bounded-answer (answer)
  "ANSWER as JSON, refused when its bytes pass the output budget."
  (let* ((text (nlk:encode-json-object answer))
         (budget (setting :max-bytes)))
    (when (> (length text) budget)
      (fail "the answer is ~:d bytes, past the ~:d-byte output budget; narrow the search, use fewer ids or split the time range."
            (length text) budget))
    text))

;;; --- the socket -------------------------------------------------------------------

(defvar *wire-maker* nil
  "NIL for the real websocket, or a function of one URL answering a wire plist
(:send :recv :close) -- the seam every test drives.")

(defun now ()
  (/ (get-internal-real-time) (float internal-time-units-per-second)))

(defun socket-url (url)
  "URL as the WebSocket endpoint an instance answers the Recorder commands on."
  (concatenate 'string
               (cond ((uiop:string-prefix-p "https://" url) (concatenate 'string "wss://" (subseq url 8)))
                     ((uiop:string-prefix-p "http://" url) (concatenate 'string "ws://" (subseq url 7)))
                     (t (fail "url ~s has no http:// or https:// scheme" url)))
               "/api/websocket"))

(defun websocket-wire (url)
  "A wire over websocket-driver, the runtime's RFC-6455 client: frames arrive
on the driver's reader thread and wait here for the caller's deadline."
  (let* ((ws (make-instance 'websocket-driver.ws.client:client :url url))
         (lock (sb-thread:make-mutex :name "hass-statistics"))
         (wait (sb-thread:make-waitqueue :name "hass-statistics"))
         (incoming '())
         (closed nil))
    (websocket-driver:on :message ws
                         (lambda (message)
                           (sb-thread:with-mutex (lock)
                             (setf incoming (nconc incoming (list message)))
                             (sb-thread:condition-broadcast wait))))
    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (sb-thread:with-mutex (lock)
                             (setf closed t)
                             (sb-thread:condition-broadcast wait))))
    (websocket-driver:start-connection ws)
    (list :send (lambda (text) (websocket-driver:send ws text))
          :recv (lambda (seconds)
                  (sb-thread:with-mutex (lock)
                    (loop unless (or incoming closed)
                          do (unless (sb-thread:condition-wait wait lock :timeout seconds)
                               (return)))
                    (or (pop incoming) (and closed :closed))))
          :close (lambda () (ignore-errors (nlk:sever-websocket ws))))))

(defun open-wire (url)
  (if *wire-maker* (funcall *wire-maker* url) (websocket-wire url)))

(defun frame-text (frame)
  "FRAME -- a text string, octets, or a value a test's wire already decoded --
as the JSON it is."
  (cond ((hash-table-p frame) frame)
        ((stringp frame) (nlk:decode-json frame))
        ((vectorp frame) (nlk:decode-json (sb-ext:octets-to-string frame :external-format :utf-8)))
        (t (nlk:decode-json (princ-to-string frame)))))

(defun next-message (wire deadline what)
  "The next frame from WIRE before DEADLINE, decoded, or a refusal naming WHAT
the exchange was waiting for. A frame that is not JSON is a refusal too."
  (let ((raw (funcall (getf wire :recv) (max 0.1 (- deadline (now))))))
    (cond ((null raw)
           (fail "Home Assistant sent nothing while waiting for ~a: is the instance awake and reachable within ~a seconds?"
                 what (setting :timeout)))
          ((eq raw :closed)
           (fail "Home Assistant closed the connection while waiting for ~a" what))
          (t (or (ignore-errors (frame-text raw))
                 (fail "Home Assistant sent something that is not JSON while waiting for ~a" what))))))

(defun send-message (wire object)
  (funcall (getf wire :send) (nlk:encode-json-object object)))

(defun exchange (message)
  "MESSAGE's result from the instance, over one authenticated WebSocket
exchange: greeting, auth, the command, its result, the close. The token goes
to the configured instance alone, and no error text quotes it."
  (let* ((deadline (+ (now) (setting :timeout)))
         (wire (open-wire (socket-url (setting :url)))))
    (unwind-protect
         (progn
           (let ((greeting (next-message wire deadline "the instance's greeting")))
             (unless (and (hash-table-p greeting)
                          (equal "auth_required" (nlk:json-value greeting :string "type")))
               (fail "the WebSocket did not greet with auth_required: check that url is the Home Assistant instance itself")))
           (send-message wire (nlk:json-object "type" "auth"
                                               "access_token" (setting :token)))
           (let ((auth (next-message wire deadline "the token's answer")))
             (unless (and (hash-table-p auth)
                          (equal "auth_ok" (nlk:json-value auth :string "type")))
               (fail "Home Assistant refused the access token: create a long-lived access token in the instance's profile, and name it in token_env or token_file")))
           (setf (gethash "id" message) 1)
           (send-message wire message)
           (let ((response (next-message wire deadline "the Recorder's answer")))
             (unless (and (hash-table-p response)
                          (eql 1 (nlk:json-value response :integer "id")))
               (fail "the instance answered something other than this command's result"))
             (unless (equal "result" (nlk:json-value response :string "type"))
               (fail "the instance answered something other than a command result"))
             (unless (nlk:json-value response :boolean "success")
               (let ((code (nlk:json-value response :string "error" "code")))
                 (fail "~a"
                       (if (equal code "unknown_command")
                           "the instance has no Recorder statistics command (unknown_command): statistics need Home Assistant 2022.11 or newer, with the Recorder integration enabled"
                           "the Recorder rejected the statistics request"))))
             (nlk:json-value response :any "result")))
      (ignore-errors (funcall (getf wire :close))))))

;;; --- ha_list_statistics -------------------------------------------------------------

(defun statistic-id (entry)
  "ENTRY's statistic id, or the empty string."
  (or (nlk:json-value entry :string "statistic_id") ""))

(defun list-statistics (call)
  "The ha_list_statistics tool: the statistic ids the Recorder holds, filtered
by the operator's lists and the search, and bounded."
  (answering
    (let* ((result (exchange (nlk:json-object "type" "recorder/list_statistic_ids")))
           (entries (if (vectorp result) result #())))
      (unless (or (vectorp result) (null result))
        (fail "the Recorder answered the statistic list in an unexpected shape"))
      (let* ((kept (remove-if (lambda (entry) (exclusion (statistic-id entry))) entries))
             (search (string-downcase (nlk:trimmed (or (statistics-list-search-text call) ""))))
             (matched (if (plusp (length search))
                          (remove-if-not (lambda (entry)
                                           (search search (string-downcase (statistic-id entry))))
                                         kept)
                          kept)))
        (check-rows-budget (length matched))
        (bounded-answer
         (nlk:json-object "count" (length matched)
                          "statistics" (coerce matched 'vector)))))))

;;; --- ha_get_statistics --------------------------------------------------------------

(defparameter +id-pattern+ "\\A[^\\s\\x00-\\x1f]+\\z"
  "A statistic id: at least one character that is not whitespace or a control
character. Home Assistant's own ids are entity ids and \"domain:name\" external
ids, and both are checked against the same operator patterns.")

(defun checked-ids (ids)
  "IDS as a list of distinct statistic ids, each permitted, or a refusal."
  (let ((ids (coerce (or ids #()) 'list)))
    (when (null ids)
      (return-from checked-ids (fail "statistic_ids is required: the ids to query, e.g. [\"sensor.office_temperature\"]. Discover them with ha_list_statistics.")))
    (dolist (id ids)
      (unless (and (stringp id) (plusp (length id)) (<= (length id) 255)
                   (ppcre:scan +id-pattern+ id))
        (fail "invalid statistic_id ~s: an id is a non-empty string with no whitespace, at most 255 characters" id))
      (let ((why (exclusion id)))
        (when why
          (fail "~a. Use ha_list_statistics to find an id the operator permits." why))))
    (remove-duplicates ids :test #'string= :from-end t)))

(defun checked-types (types)
  "TYPES as a list of distinct statistic values, all six when absent."
  (let ((types (if (or (null types) (and (vectorp types) (zerop (length types))))
                   (coerce +types+ 'vector)
                   types)))
    (let ((types (coerce types 'list)))
      (dolist (type types)
        (unless (and (stringp type) (member type +types+ :test #'string=))
          (fail "invalid statistic type ~s: use any of ~{~a~^, ~}" type +types+)))
      (remove-duplicates types :test #'string= :from-end t))))

(defun checked-period (period)
  "PERIOD as a name in +PERIODS+, or a refusal."
  (let ((period (or (nonblank period) "day")))
    (unless (member period +periods+ :test #'string=)
      (fail "invalid period ~s: use any of ~{~a~^, ~}" period +periods+))
    period))

(defun get-statistics (call)
  "The ha_get_statistics tool: aggregated history for one or more statistic
ids over a time range, bounded before and after the answer."
  (answering
    (let* ((ids (checked-ids (statistics-get-statistic-ids call)))
           (start (parse-instant (statistics-get-start-time call) "start_time"))
           (end-text (nonblank (statistics-get-end-time call)))
           (end (and end-text (parse-instant end-text "end_time")))
           (period (checked-period (statistics-get-period call)))
           (types (checked-types (statistics-get-types call))))
      (when (and end (<= end start))
        (fail "end_time must be later than start_time"))
      (check-ids-budget ids)
      (check-query-budget ids start end period)
      (let* ((message (nlk:json-object "type" "recorder/statistics_during_period"
                                        "start_time" (nlk:trimmed (statistics-get-start-time call))
                                        "statistic_ids" (coerce ids 'vector)
                                        "period" period
                                        "types" (coerce types 'vector)))
             (result (exchange (if end-text
                                   (nlk:copy-json-object message "end_time" (nlk:trimmed end-text))
                                   message))))
        (unless (hash-table-p result)
          (fail "the Recorder answered the statistics in an unexpected shape"))
        (let ((rows 0))
          (loop for series being the hash-values of result
                do (unless (vectorp series)
                     (fail "the Recorder answered a series in an unexpected shape"))
                   (incf rows (length series)))
          (check-rows-budget rows))
        (bounded-answer (nlk:json-object "result" result))))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell hass-statistics
  (:section ("hass-statistics")
    (:guide "a long-lived access token from your instance (Profile, Security, Long-Lived Access Tokens) named in token_env or kept in token_file; url is the instance's base URL, scheme included; the statistic lists and the budgets are optional; next step: ask what the office thermometer read last week")
    (:one-of "token_env" "token_file")
    ("url" :string :default "http://homeassistant.local:8123"
     :doc "the instance's base URL, http:// or https:// included")
    ("token_env" :env
     :doc "the environment variable holding a long-lived access token (HASS_TOKEN)")
    ("token_file" :path
     :doc "a file holding a long-lived access token, which the file's own permissions guard")
    ("entity_allowlist" :list
     :doc "statistic ids or patterns (sensor.office_*, energy:*) the two tools are limited to; empty, every statistic the token can read")
    ("entity_denylist" :list
     :doc "statistic ids or patterns the two tools always exclude; it wins over the allowlist, and a denied id is refused before the socket opens")
    ("max_ids" :integer :default 100 :min 1
     :doc "the most distinct statistic ids one query may name")
    ("max_rows" :integer :default 10000 :min 1
     :doc "the row budget of one answer: estimated before the query, and checked again on what came back")
    ("max_bytes" :integer :default 1048576 :min 1
     :doc "the output budget of one answer, in bytes")
    ("timeout" :integer :default 30 :min 1
     :doc "how long one exchange with the instance may take, in seconds, the connect included"))
  (:settings #'read-settings)
  (:help :hass-statistics
         "hass-statistics: ha_list_statistics and ha_get_statistics read Home Assistant's long-term statistics"
         *manual*)
  (:tool "ha_list_statistics" (nlk:contract 'statistics-list) #'list-statistics)
  (:tool "ha_get_statistics" (nlk:contract 'statistics-get) #'get-statistics))
