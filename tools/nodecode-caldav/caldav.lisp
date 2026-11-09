;;;; caldav.lisp --- one CalDAV REPORT, two tools, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's client and service, in Lisp: one calendar-query REPORT
;;;; with Depth 1 and HTTP Basic, its answer read as a multistatus of
;;;; VEVENTs, and the two tools the plugin gave its harness -- list_events
;;;; and read_event, the same names and the same arguments. Nothing here
;;;; writes a file, starts a program or remembers a uid: a call reads one
;;;; collection and answers. The one network seam is HTTP, which a test
;;;; stubs.

(in-package #:nodecode-caldav)

(defparameter +max-body+ 2000000
  "How large an answer may be before it is refused, as the original refused it.")

(defparameter +user-agent+ "nodecode-caldav"
  "What the cell calls itself on the wire; its own name, not another product's.")

(defparameter +detail-chars+ 300
  "How much of a server's own sentence a refusal quotes.")

;;; --- the boundary: what a model call carries ---------------------------------------
;;; The original's two schemas, member for member.

(nlk:define-contract list-events-request
  (:doc "List the events of one CalDAV calendar collection in one window,
read-only. With no arguments the window is the next days the section's days
names, start included and end excluded. Each row is one iCalendar VEVENT.")
  ("start" :text
   :doc "the window's start as YYYYMMDDTHHMMSSZ in UTC; pass it only together with end")
  ("end" :text
   :doc "the window's end as YYYYMMDDTHHMMSSZ in UTC, excluded; pass it only together with start"))

(nlk:define-contract read-event-request
  (:doc "Read one event of the collection by its uid, read-only. A CalDAV text
match is a substring match, so the exact uid is picked out of the answer and a
uid that merely contains it is not returned. No event with that uid is not an
empty success. This does not create, edit or delete anything.")
  ("uid" :text :required t
   :doc "the event's uid, as list_events answered it; this is not a URL path"))

;;; --- the one seam ---------------------------------------------------------------

(defun http (method url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY STATUS)."
  (nlk:http method url :headers headers :content content :timeout (or timeout 20)))

(defun basic-token (user password)
  "USER and PASSWORD as one HTTP Basic credential, the way the original sent them."
  (cl-base64:usb8-array-to-base64-string
   (sb-ext:string-to-octets (format nil "~a:~a" user password) :external-format :utf-8)))

(defun report-headers (user password)
  "What every request carries: the credential, Depth 1, and the cell's own name."
  (list (cons "authorization" (format nil "Basic ~a" (basic-token user password)))
        (cons "depth" "1")
        (cons "content-type" "application/xml; charset=utf-8")
        (cons "user-agent" +user-agent+)))

(defun xml-escape (text)
  "TEXT with the three characters an XML body may not carry bare escaped."
  (with-output-to-string (out)
    (loop for character across text
          do (case character
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (t (write-char character out))))))

(defun list-body (start end)
  "The calendar-query REPORT that asks for the VEVENTs of one window, the
body the original sent."
  (format nil "<?xml version=\"1.0\" encoding=\"utf-8\"?>~
<C:calendar-query xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:caldav\">~
<D:prop><C:calendar-data/></D:prop>~
<C:filter><C:comp-filter name=\"VCALENDAR\"><C:comp-filter name=\"VEVENT\">~
<C:time-range start=\"~a\" end=\"~a\"/>~
</C:comp-filter></C:comp-filter></C:filter>~
</C:calendar-query>" start end))

(defun read-body (uid)
  "The calendar-query REPORT that asks for the events whose uid matches UID,
as the original filtered it: a property filter, not a path."
  (format nil "<?xml version=\"1.0\" encoding=\"utf-8\"?>~
<C:calendar-query xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:caldav\">~
<D:prop><C:calendar-data/></D:prop>~
<C:filter><C:comp-filter name=\"VCALENDAR\"><C:comp-filter name=\"VEVENT\">~
<C:prop-filter name=\"UID\"><C:text-match collation=\"i;octet\">~a</C:text-match></C:prop-filter>~
</C:comp-filter></C:comp-filter></C:filter>~
</C:calendar-query>" (xml-escape uid)))

(defun short-text (text)
  "TEXT as one line of at most +DETAIL-CHARS+, or NIL when it says nothing."
  (when (stringp text)
    (let ((trimmed (nlk:trimmed (substitute #\Space #\Return
                                             (substitute #\Space #\Newline text)))))
      (when (plusp (length trimmed))
        (if (> (length trimmed) +detail-chars+)
            (concatenate 'string (subseq trimmed 0 +detail-chars+) " ...")
            trimmed)))))

;;; --- the answer, read as the original read it ----------------------------------------

(defun multistatus-events (payload)
  "The event rows PAYLOAD's multistatus carries, one row per uid: the root
must be a multistatus and every inner status 200, as the original required."
  (let ((root (scan-xml payload)))
    (unless (string= (xml-node-local root) "multistatus")
      (calendar-failed "not_multistatus" "The answer's root element is not a multistatus."))
    (let ((components '()))
      (dolist (response (xml-children root))
        (when (string= (xml-node-local response) "response")
          (let ((statuses (collect-texts response "status")))
            (unless statuses
              (calendar-failed "inner_status" "One response in the answer carried no status."))
            (dolist (status statuses)
              (unless (eql 200 (status-code status))
                (calendar-failed "inner_status"
                                 (format nil "One response answered ~a." (nlk:trimmed status))))))
          (dolist (data (collect-texts response "calendar-data"))
            (setf components (nconc components (components-of data))))))
      (group-events (checked-components components)))))

(defun parse-report (payload status)
  "PAYLOAD read as the answer to one REPORT: 207 whose root is a multistatus,
or a refusal naming the status, as HTTP 200 with a calendar in it was a
failure in the original too."
  (unless (eql status 207)
    (calendar-failed (format nil "http_~d" status)
                     (let ((said (short-text payload)))
                       (and said (format nil "The server said: ~a" said)))))
  (unless (stringp payload)
    (calendar-failed "not_xml" "The answer was not text."))
  (when (> (length payload) +max-body+)
    (calendar-failed "too_large" (format nil "The answer was over ~d characters." +max-body+)))
  (multistatus-events payload))

(defun exchange (body)
  "One REPORT to the section's collection, its rows answered; a failure is
CALDAV-ERROR naming which part of the exchange failed."
  (let* ((settings (running-settings))
         (timeout (getf settings :timeout-seconds))
         (url (getf settings :url)))
    (handler-case
        (multiple-value-bind (payload status)
            (http "REPORT" url
                  :headers (report-headers (getf settings :user) (getf settings :password))
                  :content body :timeout timeout)
          (parse-report payload status))
      (caldav-error (condition) (error condition))
      (error (condition)
        (if (typep condition 'sb-sys:deadline-timeout)
            (calendar-failed "timeout"
                             (format nil "No data came from the calendar within ~a seconds." timeout))
            (calendar-failed "network" (short-text (princ-to-string condition))))))))

;;; --- the calendar's own clock -------------------------------------------------------

(defun format-stamp (universal-time)
  "UNIVERSAL-TIME as one YYYYMMDDTHHMMSSZ stamp in UTC, the form the original sent."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ"
            year month day hour minute second)))

(defun days-in-month (year month)
  "How many days MONTH of YEAR has, a leap year counted as RFC 5545 counts it."
  (cond ((member month '(4 6 9 11)) 30)
        ((= month 2) (if (and (zerop (mod year 4))
                               (or (plusp (mod year 100)) (zerop (mod year 400))))
                         29
                         28))
        (t 31)))

(defun stamp-value (value)
  "VALUE as one YYYYMMDDTHHMMSSZ stamp, or NIL when it is not one: an
impossible day or hour is refused rather than turned into another time."
  (let ((text (nonblank value)))
    (when (and text
               (= 16 (length text))
               (char= (char text 8) #\T)
               (char= (char text 15) #\Z)
               (loop for index from 0 below 16
                     always (or (member index '(8 15))
                                (digit-char-p (char text index)))))
      (let ((year (parse-integer text :start 0 :end 4))
            (month (parse-integer text :start 4 :end 6))
            (day (parse-integer text :start 6 :end 8))
            (hour (parse-integer text :start 9 :end 11))
            (minute (parse-integer text :start 11 :end 13))
            (second (parse-integer text :start 13 :end 15)))
        (and (<= 1 month 12)
             (<= 1 day (days-in-month year month))
             (<= 0 hour 23)
             (<= 0 minute 59)
             (<= 0 second 59)
             text)))))

(defun window-stamps (&optional (days (setting :days)))
  "The window a call with no arguments reads: now, truncated to a second,
through DAYS days later, start included and end excluded."
  (let ((now (get-universal-time)))
    (values (format-stamp now)
            (format-stamp (+ now (* 86400 (max 1 (or days 7))))))))

(defun day-window (offset)
  "The UTC day OFFSET days from today, as (values START END)."
  (let* ((now (+ (get-universal-time) (* 86400 offset)))
         (midnight (- now (mod now 86400))))
    (values (format-stamp midnight) (format-stamp (+ midnight 86400)))))

;;; --- the answer as lines a model reads --------------------------------------------

(defun render-row (row index)
  "One event as the block a model reads: what it is, when, where, and its uid."
  (let ((end (getf row :end))
        (tzid (getf row :tzid)))
    (with-output-to-string (out)
      (format out "[~d] ~a~%" index (or (nonblank (getf row :summary)) "(no summary)"))
      (format out "    start ~a~a~a~a~%"
              (getf row :start)
              (if end (format nil "  end ~a" end) "")
              (if tzid (format nil "  tzid ~a" tzid) "")
              (if (getf row :repeats) "  repeats" ""))
      (format out "    uid ~a" (getf row :uid)))))

(defun render-events (rows start end)
  "ROWS as one block an event, under the window they answer; a list longer
than the section's max_events is cut and says so, as the original cut it."
  (if (null rows)
      (format nil "No events in ~a to ~a." start end)
      (let* ((limit (max 1 (or (setting :max-events) 50)))
             (kept (if (> (length rows) limit) (subseq rows 0 limit) rows))
             (blocks (loop for row in kept
                           for index from 1
                           collect (render-row row index))))
        (with-output-to-string (out)
          (format out "~d event~:p from ~a to ~a, times as the calendar holds them and not converted.~%~%"
                  (length rows) start end)
          (format out "~{~a~^~%~%~}" blocks)
          (when (> (length rows) limit)
            (format out "~%~%This is not the whole list: ~d events in the window, the first ~d are shown."
                    (length rows) limit))))))

(defun list-window (start end)
  "The events of one window, as the text a model reads."
  (render-events (exchange (list-body start end)) start end))

;;; --- the two tools ----------------------------------------------------------------

(defun list-events (request)
  "The list_events tool: one window of the collection, one block an event.
Start and end arrive together or not at all, as the original required."
  (let* ((raw-start (list-events-request-start request))
         (raw-end (list-events-request-end request))
         (start (stamp-value raw-start))
         (end (stamp-value raw-end)))
    (cond
      ((or raw-start raw-end)
       (unless (and start end)
         (calendar-failed "bad_args"
                          "start and end are both YYYYMMDDTHHMMSSZ in UTC, and are passed together."))
       (unless (string< start end)
         (calendar-failed "bad_args" "start must come before end."))
       (list-window start end))
      (t (multiple-value-bind (start end) (window-stamps)
           (list-window start end))))))

(defun read-event (request)
  "The read_event tool: one event by uid, and only the exact uid: a CalDAV
text match is a substring match, so a uid that merely contains it is dropped."
  (let* ((uid (nonblank (read-event-request-uid request)))
         (rows (progn
                 (unless (and uid (<= (length uid) 1024))
                   (calendar-failed "bad_args" "uid is one event's uid, at most 1024 characters."))
                 (exchange (read-body uid))))
         (matched (remove-if-not (lambda (row) (equal uid (getf row :uid))) rows)))
    (cond
      ((null matched)
       (calendar-failed "not_found"
                        (if rows
                            (format nil "No event with that uid. The collection returned ~d other uid(s), and none was chosen."
                                    (length rows))
                            "No event with that uid.")))
      ((> (length matched) 1)
       (calendar-failed "ambiguous" "More than one event has that uid, so none was chosen."))
      (t (format nil "1 event with uid ~a.~%~%~a" uid (render-row (first matched) 1))))))

;;; --- the command ------------------------------------------------------------------

(defun run-slash (args session-id)
  "One /caldav invocation: the calendar's next days, or the window ARGS names."
  (declare (ignore session-id))
  (let ((argument (string-downcase (or (nlk:trimmed (or args "")) ""))))
    (multiple-value-bind (start end)
        (cond ((zerop (length argument)) (window-stamps))
              ((every #'digit-char-p argument) (window-stamps (parse-integer argument)))
              ((string= argument "today") (day-window 0))
              ((string= argument "tomorrow") (day-window 1))
              ((string= argument "week") (window-stamps 7))
              (t (values nil nil)))
      (if start
          (list-window start end)
          (format nil "/caldav lists the calendar's next days. /caldav alone is the section's days, ~
/caldav 3 is three days, and /caldav today, /caldav tomorrow and /caldav week are the days they name. ~
The model's tools are list_events and read_event.")))))

;;; --- what a model and an operator are told --------------------------------------------

(defparameter *primer*
  "The calendar is readable through the nodecode-caldav cell: two model tools,
list_events and read_event, and the same calls in the CALDAV package from eval.
It reads one CalDAV collection the operator runs, with the operator's own
credential, and changes nothing.
  (caldav:list-events)                          the next days the section's days names
  (caldav:list-events \"20261108T000000Z\" \"20261115T000000Z\")   one window
  (caldav:read-event \"uid-from-a-list\")          one event by uid
A row is one iCalendar VEVENT: its summary, its start and end as the calendar
writes them, a TZID when the value carries one, and its uid. Times are not
converted, an all-day date stays a date, and a recurring event is one row
whose start is the master's, with repeats said: repeats are not expanded.
A missing DTEND is an end that is not stated, never an invented one.
Doctrine: ask for the window you mean, and pass a uid from a list to read_event
rather than guessing one. A refused read says which part failed - http_401 for a
refused credential, timeout, not_multistatus for a server that does not speak
CalDAV, not_found for a uid the collection does not hold."
  "What (help :caldav) answers while the cell runs.")
