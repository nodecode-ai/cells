;;;; calendar.lisp --- Google Calendar v3: one grant, one token, the calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A refresh grant becomes an access token at oauth2.googleapis.com and that
;;;; token rides every later request to www.googleapis.com; those two hosts are
;;;; the only ones a call leaves for. The token lives in memory for the process,
;;;; its expiry beside it; a 401 refreshes it once and retries. Nothing is
;;;; written to disk and no token is ever printed.

(in-package #:nodecode-booking)

(defparameter *token-url* "https://oauth2.googleapis.com/token"
  "The refresh grant's endpoint: one of the only two hosts a call reaches.")

(defparameter *api-base* "https://www.googleapis.com/calendar/v3"
  "Google Calendar v3, as the original plugin's calls wrote it.")

(defparameter *max-window-days* 14
  "The widest window list-availability answers: FreeBusy over more is a guess.")

(defparameter *max-slots* 60
  "How many slots one availability answer carries.")

(defvar *token* nil
  "The cached (ACCESS-TOKEN . EXPIRY-SECONDS), or NIL: memory only, per process.")

;;; --- reading JSON the lenient way ---------------------------------------------

(defun json-text (text)
  "TEXT decoded, or NIL when it is empty or not JSON."
  (and (stringp text) (plusp (length (nlk:trimmed text)))
       (handler-case (nlk:decode-json text) (error () nil))))

(defun text-field (object name)
  "NAME's value in OBJECT when it is a string."
  (let ((value (and (hash-table-p object) (gethash name object))))
    (and (stringp value) value)))

(defun number-field (object name)
  "NAME's value in OBJECT when it is a number."
  (let ((value (and (hash-table-p object) (gethash name object))))
    (typecase value
      (integer value)
      ((or single-float double-float rational) (round value))
      (t nil))))

(defun items-field (object name)
  "NAME's value in OBJECT as a list: a JSON array reads as a vector or a list."
  (let ((value (and (hash-table-p object) (gethash name object))))
    (typecase value
      (vector (coerce value 'list))
      (list value)
      (t nil))))

(defun first-table-value (table)
  "The first value of TABLE, whichever key it is under: FreeBusy answers by
calendar id, and the caller may have named the calendar by a synonym."
  (when (hash-table-p table)
    (loop for value being the hash-values of table return value)))

(defun nonblank (text)
  "TEXT trimmed when it says something, else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun percent-encode (text)
  "TEXT with all but RFC3986's unreserved characters escaped: a calendar id
and an event id are path segments, and both carry ; or @."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets (or text "") :external-format :utf-8)
          for char = (code-char byte)
          do (if (or (alphanumericp char) (member char '(#\- #\_ #\. #\~)))
                 (write-char char out)
                 (format out "%~2,'0X" byte)))))

;;; --- one exchange -----------------------------------------------------------------

(defun exchange (method url &key headers content)
  "METHOD URL => (values BODY STATUS); a transport failure is a BOOKING-ERROR
naming its type, never a traceback."
  (handler-case
      (nlk:http method url :headers headers :content content
                :timeout (setting :timeout))
    (error (condition)
      (fail "Google could not be reached (~a): ~a" (type-of condition) url))))

;;; --- the token -----------------------------------------------------------------

(defun grant-body ()
  "The refresh grant, form encoded."
  (format nil "grant_type=refresh_token&client_id=~a&client_secret=~a&refresh_token=~a"
          (percent-encode (setting :client-id))
          (percent-encode (setting :client-secret))
          (percent-encode (setting :refresh-token))))

(defun access-token (&optional renew)
  "The access token: the cached one, or a fresh one from the refresh grant.
RENEW drops the cache first, which a 401 does once."
  (let ((now (now-seconds)))
    (when (or renew (null *token*) (<= (cdr *token*) (- now 60)))
      (multiple-value-bind (body status)
          (exchange "POST" *token-url*
                    :headers '(("content-type" . "application/x-www-form-urlencoded")
                               ("accept" . "application/json"))
                    :content (grant-body))
        (let ((data (json-text body))
              (token nil))
          (setf token (text-field data "access_token"))
          (unless (and token (integerp status) (< status 300))
            (fail "Google refused the sign-in grant~@[: ~a~]: ~a"
                  (and status (format nil "HTTP ~a" status))
                  (or (text-field data "error_description") (text-field data "error")
                      (string-trim '(#\Space #\Newline #\Return)
                                   (subseq (or body "") 0 (min 200 (length (or body ""))))))))
          (setf *token* (cons token (+ now (or (number-field data "expires_in") 3000)))))))
    (car *token*)))

;;; --- the calls -----------------------------------------------------------------

(defun call (method path &key json (retried nil))
  "One Calendar API request => the decoded body, or NIL for an empty one.
PATH is what follows the version, already encoded. A 401 renews the token once."
  (let* ((url (concatenate 'string *api-base* path))
         (headers (list (cons "authorization" (format nil "Bearer ~a" (access-token)))
                        (cons "accept" "application/json")
                        (cons "content-type" "application/json"))))
    (multiple-value-bind (body status)
        (exchange method url :headers headers
                  :content (and json (nlk:encode-json-object json)))
      (cond
        ((eql status 401)
         (if retried
             (fail "Google refused the access token; the sign-in grant may have been revoked")
             (call method path :json json :retried t)))
        ((not (and (integerp status) (< status 300)))
         (let* ((data (json-text body))
                (error (and (hash-table-p data) (gethash "error" data)))
                (message (if (hash-table-p error)
                             (text-field error "message")
                             (text-field data "error_description"))))
           (fail "Google Calendar answered HTTP ~a~@[: ~a~]" status message)))
        (t (json-text body))))))

;;; --- what the tools ask for -------------------------------------------------------

(defun calendar-path (calendar)
  "CALENDAR as a path segment."
  (concatenate 'string "/calendars/" (percent-encode calendar)))

(defun free-busy (start end calendar)
  "The busy intervals FreeBusy reports in START..END, as (START . END) seconds."
  (let* ((data (call "POST" "/freeBusy"
                      :json (nlk:json-object
                             "timeMin" (format-time start 0)
                             "timeMax" (format-time end 0)
                             "items" (vector (nlk:json-object "id" calendar)))))
         (calendars (and (hash-table-p data) (gethash "calendars" data)))
         (entry (and (hash-table-p calendars)
                     (or (gethash calendar calendars) (first-table-value calendars)))))
    (when (or (null entry) (items-field entry "errors"))
      (fail "Google could not return availability for that calendar"))
    (loop for busy in (items-field entry "busy")
          collect (cons (parse-time (or (text-field busy "start") ""))
                        (parse-time (or (text-field busy "end") ""))))))

(defun list-events (calendar start count)
  "The first COUNT events on CALENDAR from START, as Google answers them."
  (items-field (call "GET" (format nil "~a/events?singleEvents=true&orderBy=startTime&maxResults=~d&timeMin=~a"
                                    (calendar-path calendar) count
                                    (percent-encode (format-time start 0))))
               "items"))

(defun get-event (calendar event-id)
  "One event, or a refusal saying Google has no such id."
  (let ((event (call "GET" (format nil "~a/events/~a" (calendar-path calendar) (percent-encode event-id)))))
    (unless (hash-table-p event)
      (fail "Google has no event ~a on that calendar" event-id))
    event))

(defun insert-event (calendar body)
  "BODY as a new event; attendees are told, as the original plugin did."
  (call "POST" (concatenate 'string (calendar-path calendar) "/events?sendUpdates=all") :json body))

(defun patch-event (calendar event-id body)
  "BODY merged into one event; attendees are told."
  (call "PATCH" (format nil "~a/events/~a?sendUpdates=all" (calendar-path calendar) (percent-encode event-id))
        :json body))

(defun delete-event (calendar event-id)
  "One event deleted; attendees are told."
  (call "DELETE" (format nil "~a/events/~a?sendUpdates=all" (calendar-path calendar) (percent-encode event-id))))
