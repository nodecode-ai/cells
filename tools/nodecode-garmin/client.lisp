;;;; client.lisp --- Garmin's Personal API in one place: the key, the token,
;;;; one GET, the day arithmetic and the number formatting the verbs share.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The key is exchanged once for a short-lived bearer token, which is kept
;;;; in memory under the key it came from, so a rotated key re-exchanges by
;;;; itself. Nothing is written to disk here. The key reaches this cell two
;;;; ways, as the original let it: the api_key member, or the environment
;;;; variable api_key_env names (GARMIN_PERSONAL_API_KEY by default).

(in-package #:nodecode-garmin)

(defparameter +key-page+ "https://connect.garmin.com/app/settings/personalApiToken"
  "Where a person makes a Personal API key. It is shown once and expires.")

(defparameter +token-path+ "/personalapitoken-service/personal-api-key/token")

(defparameter +bases+ '(("global" . "https://personal.connectapi.garmin.com")
                         ("china" . "https://personal.connectapi.garmin.cn"))
  "The two hosts the original contacted, and the only two this cell does.")

(defparameter +token-seconds+ 1800
  "How long an exchanged token is reused. Garmin's tokens live a day; half an
hour is well inside it and needs no clock arithmetic on a JWT.")

;;; --- the section's values ---------------------------------------------------------

(defun environment-value (name)
  "The environment variable NAME, or NIL. A test stubs this name."
  (uiop:getenv name))

(defun api-key ()
  "The key in the section, else the one api_key_env names, or NIL."
  (let ((inline (ignore-errors (setting :api-key)))
        (name (let ((named (ignore-errors (setting :api-key-env))))
                (if (and (stringp named) (plusp (length (nlk:trimmed named))))
                    (nlk:trimmed named)
                    "GARMIN_PERSONAL_API_KEY"))))
    (or (and (stringp inline) (plusp (length (nlk:trimmed inline))) (nlk:trimmed inline))
        (let ((value (ignore-errors (environment-value name))))
          (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))))

(defun base-url ()
  "The host the section's region names, Garmin's global one when it names none."
  (let ((region (string-downcase (princ-to-string (or (ignore-errors (setting :region)) "global")))))
    (or (cdr (assoc region +bases+ :test #'string=))
        (cdr (assoc "global" +bases+ :test #'string=)))))

(defun check-config ()
  "Refuse, naming the setting, when no key is reachable: a cell that cannot
read anything is worth refusing at start, where the panel and the notice say
what to do, rather than on the first tool call."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (format nil "garmin: no Garmin Personal API key. Make one at ~a, paste it in this cell's api_key setting, or set the environment variable ~a, then (restart-cells \"nodecode-garmin\")."
                            +key-page+
                            (or (ignore-errors (setting :api-key-env)) "GARMIN_PERSONAL_API_KEY")))))

;;; --- one exchange -----------------------------------------------------------------

(defun http (method url &rest arguments)
  "The cell's one network seam -- NLK:HTTP. A test stubs this name."
  (apply #'nlk:http method url arguments))

(defun timeout ()
  (let ((seconds (ignore-errors (setting :timeout-seconds))))
    (if (and (integerp seconds) (plusp seconds)) seconds 30)))

(defvar *token* nil
  "The last exchange as (KEY TOKEN EXPIRES-AT), so a rotated key re-exchanges
and two profiles never see each other's data.")

(defun exchange (key)
  "POST the key for a bearer token and keep it."
  (multiple-value-bind (body status)
      (http :post (concatenate 'string (base-url) +token-path+)
            :headers (list (cons "accept" "*/*")
                           (cons "authorization" (format nil "ApiKey ~a" key)))
            :timeout (timeout))
    (cond ((member status '(401 403))
           (setf *token* nil)
           (fail "garmin: Garmin rejected the Personal API key (HTTP ~a). It expires after 30 to 365 days: make a new one at ~a, paste it in this cell's api_key setting, then (restart-cells \"nodecode-garmin\")."
                 status +key-page+))
          ((not (and (integerp status) (<= 200 status 299)))
           (fail "garmin: Garmin's token endpoint answered HTTP ~a." status))
          (t (let ((value (and body (string-trim (list #\Space #\Tab #\Newline #\Return (code-char 34)) body))))
               (when (or (null value) (zerop (length value)))
                 (fail "garmin: Garmin's token endpoint answered an empty token."))
               (setf *token* (list key value (+ (get-universal-time) +token-seconds+)))
               value)))))

(defun token (&key force)
  "The bearer token for the key in force, exchanged when none is live."
  (let ((key (or (api-key)
                  (fail "garmin: no Garmin Personal API key: set this cell's api_key, or the environment variable ~a."
                        (or (ignore-errors (setting :api-key-env)) "GARMIN_PERSONAL_API_KEY")))))
    (if (and (not force) *token* (equal (first *token*) key) (< (get-universal-time) (third *token*)))
        (second *token*)
        (exchange key))))

(defun url-encode (text)
  "TEXT with everything but the unreserved characters percent-encoded. The
query values here are dates, activity types and search words."
  (with-output-to-string (out)
    (loop for character across text
          do (if (or (alphanumericp character) (find character "-._~" :test #'char=))
                 (write-char character out)
                 (format out "%~2,'0X" (char-code character))))))

(defun get-url (path params)
  "BASE + PATH, with PARAMS -- an alist of (NAME . VALUE) -- as its query."
  (let ((url (concatenate 'string (base-url) path))
        (pairs (remove-if (lambda (pair) (null (cdr pair))) params)))
    (if (null pairs)
        url
        (format nil "~a?~{~a~^&~}"
                url
                (loop for (name . value) in pairs
                      collect (format nil "~a=~a" name (url-encode (princ-to-string value))))))))

(defun garmin-get (path &key params)
  "GET PATH as decoded JSON: an EQUAL hash table, a vector, or NIL for 204.
A 401 is a revoked token, so it re-exchanges once and asks again."
  (let ((url (get-url path params)))
    (flet ((ask (bearer)
             (http :get url
                   :headers (list (cons "accept" "application/json")
                                  (cons "authorization" (format nil "Bearer ~a" bearer)))
                   :timeout (timeout))))
      (multiple-value-bind (body status) (ask (token))
        (when (eql status 401)
          (multiple-value-setq (body status) (ask (token :force t))))
        (cond ((eql status 204) nil)
              ((and (integerp status) (<= 200 status 299))
               (if (or (null body) (zerop (length (nlk:trimmed body))))
                   nil
                   (handler-case (nlk:decode-json body)
                     (error ()
                       (fail "garmin: Garmin answered something that is not JSON for ~a." path)))))
              ((eql status 401)
               (setf *token* nil)
               (fail "garmin: Garmin rejected the key (HTTP 401). Make a new one at ~a and paste it in this cell's api_key setting."
                     +key-page+))
              ((eql status 403)
               (fail "garmin: Garmin refused access (HTTP 403) to ~a: it may belong to another account." path))
              ((eql status 404)
               (fail "garmin: Garmin has no such item (HTTP 404) at ~a." path))
              ((eql status 429)
               (fail "garmin: Garmin is rate-limiting (HTTP 429). Wait a minute and ask again."))
              (t (fail "garmin: Garmin answered HTTP ~a for ~a." status path)))))))

(defun download (path)
  "PATH as the octets Garmin sends (an activity's ZIP)."
  (multiple-value-bind (body status)
      (http :get (get-url path nil)
            :headers (list (cons "accept" "application/x-zip-compressed")
                           (cons "authorization" (format nil "Bearer ~a" (token))))
            :binary t :timeout (timeout))
    (unless (and (integerp status) (<= 200 status 299))
      (fail "garmin: Garmin answered HTTP ~a for the activity file." status))
    (unless (and (vectorp body) (plusp (length body)))
      (fail "garmin: Garmin answered an empty activity file."))
    body))

;;; --- days -------------------------------------------------------------------------

(defun today-iso ()
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun iso-day-p (text)
  "TEXT is a calendar day this cell can ask Garmin for: YYYY-MM-DD, real."
  (and (stringp text)
       (= 10 (length text))
       (loop for index from 0 below 10
             always (let ((character (char text index)))
                      (if (member index '(4 7))
                          (char= character #\-)
                          (and (digit-char-p character) t))))
       (ignore-errors (encode-universal-time 0 0 12 (parse-integer text :start 8)
                                             (parse-integer text :start 5 :end 7)
                                             (parse-integer text :end 4))
                      t)))

(defun iso-noon (text)
  (encode-universal-time 0 0 12 (parse-integer text :start 8)
                         (parse-integer text :start 5 :end 7)
                         (parse-integer text :end 4)))

(defun iso-add (day days)
  "DAY, an ISO date, moved DAYS forward (or back, when DAYS is negative)."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (+ (iso-noon day) (* days 86400)))
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun day-between (earlier later)
  "Whole days from EARLIER to LATER, both ISO dates."
  (round (- (iso-noon later) (iso-noon earlier)) 86400))

(defun parse-day (text &optional (default (today-iso)))
  "TEXT as a day: today, yesterday, an ISO date, or empty for DEFAULT."
  (let ((text (and text (string-downcase (nlk:trimmed text)))))
    (cond ((or (null text) (zerop (length text))) default)
          ((string= text "today") (today-iso))
          ((string= text "yesterday") (iso-add (today-iso) -1))
          ((iso-day-p text) text)
          (t (fail "garmin: ~s is not a day: write YYYY-MM-DD, today, or yesterday." text)))))

(defparameter +max-days+ 93
  "One ask spans at most a quarter, as the original's did: a longer range is
several asks and reads slow.")

(defun window (start end days &key (default-days 1) (max-days +max-days+))
  "The (START . END) days a verb asks for, from a start and an end, or a
number of days ending today. Nothing given reads today."
  (let* ((today (today-iso))
         (end (if (and end (plusp (length (nlk:trimmed end)))) (parse-day end today) today))
         (start (cond ((and start (plusp (length (nlk:trimmed start)))) (parse-day start end))
                      ((and days (plusp days)) (iso-add end (- 1 days)))
                      (t (iso-add end (- 1 default-days))))))
    (when (minusp (day-between start end))
      (fail "garmin: start_date ~a is after end_date ~a." start end))
    (when (> (1+ (day-between start end)) max-days)
      (fail "garmin: ask at most ~a days at once (this range is ~a)."
            max-days (1+ (day-between start end))))
    (cons start end)))

;;; --- reading a response ------------------------------------------------------------

(defun at (object &rest path)
  "The value at PATH in decoded JSON OBJECT, or NIL. The walk stops at the
first level that is not an object, so a missing branch reads as NIL."
  (let ((value object))
    (dolist (key path value)
      (setf value (and (hash-table-p value) (gethash key value))))))

(defun items (object &rest path)
  "The elements at PATH as a list, or NIL when there are none."
  (let ((value (apply #'at object path)))
    (cond ((vectorp value) (coerce value 'list))
          ((listp value) value)
          (t nil))))

(defun text (value)
  "VALUE as a trimmed string, or NIL when it is empty or not a string."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

;;; --- writing a line ----------------------------------------------------------------

(defun n1 (value &optional (digits 1))
  "VALUE as a number rounded to DIGITS places, or NIL."
  (when (realp value)
    (if (zerop digits)
        (format nil "~d" (round value))
        (format nil "~,vf" digits (coerce value 'double-float)))))

(defun n0 (value) (n1 value 0))

(defun hmm (seconds)
  "SECONDS as 7h12, or NIL."
  (when (and (realp seconds) (plusp seconds))
    (multiple-value-bind (hours minutes) (floor seconds 3600)
      (format nil "~dh~2,'0d" hours (floor minutes 60)))))

(defun hms (seconds)
  "SECONDS as 1:02:03 (or 4:05 under an hour), or NIL."
  (when (and (realp seconds) (plusp seconds))
    (multiple-value-bind (hours rest) (floor seconds 3600)
      (multiple-value-bind (minutes remainder) (floor rest 60)
        (if (plusp hours)
            (format nil "~d:~2,'0d:~2,'0d" hours minutes (floor remainder))
            (format nil "~d:~2,'0d" minutes (floor remainder)))))))

(defun km (meters)
  "METERS as kilometres with two places, or NIL."
  (when (and (realp meters) (plusp meters))
    (n1 (/ meters 1000.0d0) 2)))

(defun one (label value)
  "LABEL and VALUE as one field, or NIL when VALUE is NIL."
  (when value (format nil "~a ~a" label value)))

(defun fields (&rest parts)
  "The parts that are not NIL, one line."
  (format nil "~{~a~^  ~}" (remove nil parts)))

(defun lines (&rest parts)
  "The parts that are not NIL, one per line."
  (format nil "~{~a~^~%~}" (remove nil parts)))

(defun levels (series)
  "SERIES -- Garmin's [[time, level], ...] -- as the levels alone."
  (loop for pair across (or (and (vectorp series) series) #())
        when (and (vectorp pair) (>= (length pair) 2) (realp (aref pair 1)))
          collect (aref pair 1)))
