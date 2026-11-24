;;;; ical.lisp --- the iCalendar (RFC 5545) reading and writing this needs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One VEVENT per resource, with SUMMARY, DTSTART, DTEND, LOCATION,
;;;; DESCRIPTION, UID, plus ORGANIZER / ATTENDEE / TRANSP for meetings: what
;;;; Yandex Calendar answers and what this cell writes back.
;;;;
;;;; Everything the model does not understand is kept and re-emitted: a
;;;; property this file does not model (RRULE, VALARM, SEQUENCE, X-) sits in
;;;; the event's raw lines, the VTIMEZONE blocks of the document ride along
;;;; so its TZID references still resolve, and a DTSTART/DTEND the caller did
;;;; not change goes back exactly as it was read, in its own zone. A time the
;;;; caller does change is written as the UTC instant it denotes.
;;;;
;;;; Times are MOMENTs: a civil date and time, ALL-DAY for a date, and OFFSET
;;;; the minutes east of UTC, NIL for a floating local time. Nothing here
;;;; reads a time zone database: an offset the value carries is used, a TZID
;;;; is remembered (DT-ZONES) and kept on the line, and a time that must be
;;;; reformatted is written in UTC.

(in-package #:nodecode-yandex-calendar)

(defparameter *transp-busy* "OPAQUE")
(defparameter *transp-free* "TRANSPARENT")

;;; --- a moment ----------------------------------------------------------------

(defstruct (moment (:constructor %make-moment)
                    (:conc-name moment-))
  "One civil time: YEAR MONTH DAY HOUR MINUTE SECOND, OFFSET minutes east of UTC
(NIL floating), and ALL-DAY for a value that is a date."
  (year 1970 :type integer)
  (month 1 :type integer)
  (day 1 :type integer)
  (hour 0 :type integer)
  (minute 0 :type integer)
  (second 0 :type integer)
  (offset nil)
  (all-day nil))

(defun make-moment (&key (year 1970) (month 1) (day 1) (hour 0) (minute 0) (second 0)
                          offset (all-day nil))
  "A MOMENT; ALL-DAY when one is made from a date."
  (%make-moment :year year :month month :day day :hour hour :minute minute
                 :second second :offset offset :all-day all-day))

(defun days-from-civil (year month day)
  "The day number of YEAR-MONTH-DAY, 1970-01-01 being 0."
  (let* ((year (if (<= month 2) (1- year) year))
         (era (floor year 400))
         (yoe (- year (* era 400)))
         (mp (+ month (if (> month 2) -3 9)))
         (doy (+ (floor (+ (* 153 mp) 2) 5) (1- day)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (- (+ (* era 146097) doe) 719468)))

(defun civil-from-days (days)
  "The (values YEAR MONTH DAY) of DAYS, 1970-01-01 being 0."
  (let* ((days (+ days 719468))
         (era (floor days 146097))
         (doe (- days (* era 146097)))
         (yoe (floor (- doe (floor doe 1460) (- (floor doe 36524)) (floor doe 146096)) 365))
         (year (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (day (- doy (floor (+ (* 153 mp) 2) 5) -1))
         (month (+ mp (if (< mp 10) 3 -9))))
    (values (+ year (if (<= month 2) 1 0)) month day)))

(defun moment-epoch (moment)
  "The UTC second MOMENT denotes; a floating one is read as UTC."
  (+ (* 86400 (days-from-civil (moment-year moment) (moment-month moment) (moment-day moment)))
     (* 3600 (moment-hour moment))
     (* 60 (moment-minute moment))
     (moment-second moment)
     (- (* 60 (or (moment-offset moment) 0)))))

(defun epoch-moment (seconds)
  "The UTC MOMENT at SECONDS."
  (multiple-value-bind (days rest) (floor seconds 86400)
    (multiple-value-bind (year month day) (civil-from-days days)
      (make-moment :year year :month month :day day
                    :hour (floor rest 3600)
                    :minute (mod (floor rest 60) 60)
                    :second (mod rest 60)
                    :offset 0))))

(defun moment< (a b)
  "Whether A is earlier than B."
  (< (moment-epoch a) (moment-epoch b)))

(defun moment-iso (moment)
  "MOMENT as ISO 8601: a date for an all-day value, else the time with its offset."
  (multiple-value-bind (year month day hour minute second offset)
      (values (moment-year moment) (moment-month moment) (moment-day moment)
              (moment-hour moment) (moment-minute moment) (moment-second moment)
              (moment-offset moment))
    (if (moment-all-day moment)
        (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)
        (let ((offset (or offset 0)))
          (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~a~2,'0d:~2,'0d"
                  year month day hour minute second
                  (if (minusp offset) #\- #\+)
                  (abs (floor offset 60)) (abs (mod offset 60)))))))

(defun moment-utc-stamp (moment)
  "MOMENT as the UTC instant a CalDAV time range and DTSTAMP are written in."
  (let ((utc (epoch-moment (moment-epoch moment))))
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ"
            (moment-year utc) (moment-month utc) (moment-day utc)
            (moment-hour utc) (moment-minute utc) (moment-second utc))))

(defun now-moment ()
  "The current UTC MOMENT."
  ;; Common Lisp counts seconds from 1900, MOMENTs from 1970.
  (epoch-moment (- (get-universal-time) 2208988800)))

(defun iso-number (text start end whole)
  "The integer TEXT's START..END spells, or CALDAV-ERROR naming WHOLE."
  (or (ignore-errors (parse-integer text :start start :end end))
      (caldav-fail "Invalid date/time ~s: ~s is not a number." whole (subseq text start end))))

(defun iso-zone-offset (zone whole)
  "ZONE (Z, +HH:MM, -HHMM or empty) as minutes east of UTC, NIL for none."
  (cond ((zerop (length zone)) nil)
        ((member (char zone 0) '(#\Z #\z)) 0)
        ((member (char zone 0) '(#\+ #\-))
         (let* ((sign (if (char= (char zone 0) #\-) -1 1))
                (body (remove #\: (subseq zone 1))))
           (unless (<= 2 (length body) 4)
             (caldav-fail "Invalid date/time ~s: ~s is not a time zone offset." whole zone))
           (* sign (+ (* 60 (iso-number body 0 2 whole))
                      (if (> (length body) 2) (iso-number body 2 4 whole) 0)))))
        (t (caldav-fail "Invalid date/time ~s: ~s is not a time zone offset." whole zone))))

(defun iso-clock (clock whole)
  "CLOCK (HH:MM, HH:MM:SS or HHMMSS) as (values HOUR MINUTE SECOND)."
  (let* ((digits (remove #\: clock))
         (length (length digits)))
    (unless (member length '(4 6))
      (caldav-fail "Invalid date/time ~s: ~s is not a clock time." whole clock))
    (values (iso-number digits 0 2 whole)
            (iso-number digits 2 4 whole)
            (if (= length 6) (iso-number digits 4 6 whole) 0))))

(defun parse-iso-moment (text &key (what "date/time"))
  "TEXT, an ISO 8601 date or date-time, as a MOMENT; CALDAV-ERROR for anything else.
A date is an all-day value; a date-time without an offset is a floating one,
which every caller here reads as UTC."
  (let ((text (string-trim '(#\Space #\Tab) (or text ""))))
    (unless (and (>= (length text) 10)
                 (char= #\- (char text 4))
                 (char= #\- (char text 7)))
      (caldav-fail "Invalid ~a ~s: expected YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS." what text))
    (let ((year (iso-number text 0 4 text))
          (month (iso-number text 5 7 text))
          (day (iso-number text 8 10 text))
          (rest (subseq text 10)))
      (unless (and (<= 1 month 12) (<= 1 day 31))
        (caldav-fail "Invalid ~a ~s: ~d-~d is not a calendar date." what text month day))
      (cond
        ((zerop (length rest))
         (make-moment :year year :month month :day day :all-day t))
        ((not (member (char rest 0) '(#\T #\Tab #\Space)))
         (caldav-fail "Invalid ~a ~s: expected a date or a date-time." what text))
        (t
         (let* ((time (subseq rest 1))
                (zone-at (or (position-if (lambda (char) (member char '(#\Z #\z #\+ #\-))) time)
                             (length time)))
                (clock (subseq time 0 zone-at))
                (zone (subseq time zone-at)))
           (multiple-value-bind (hour minute second) (iso-clock clock text)
             (unless (and (<= 0 hour 23) (<= 0 minute 59) (<= 0 second 59))
               (caldav-fail "Invalid ~a ~s: ~d:~d:~d is not a clock time."
                            what text hour minute second))
             (make-moment :year year :month month :day day :hour hour :minute minute
                          :second second :offset (iso-zone-offset zone text)))))))))

;;; --- a meeting participant -----------------------------------------------------

(defstruct (attendee (:constructor make-attendee (&key (email "") (name "") (role "")
                                                       (partstat "") rsvp))
                     (:conc-name attendee-))
  "One ORGANIZER or ATTENDEE: its address, display name, role and reply."
  (email "" :type string)
  (name "" :type string)
  (role "" :type string)
  (partstat "" :type string)
  (rsvp nil))

(defun parse-cal-address (value params)
  "An ORGANIZER / ATTENDEE value (mailto:x@y) with its PARAMS as an ATTENDEE."
  (let ((email (string-trim '(#\Space #\Tab) value)))
    (when (and (>= (length email) 7) (string-equal "mailto:" email :end2 7))
      (setf email (subseq email 7)))
    (make-attendee :email email
                   :name (or (cdr (assoc "CN" params :test #'string=)) "")
                   :role (or (cdr (assoc "ROLE" params :test #'string=)) "")
                   :partstat (or (cdr (assoc "PARTSTAT" params :test #'string=)) "")
                   :rsvp (let ((raw (cdr (assoc "RSVP" params :test #'string=))))
                           (cond ((null raw) nil)
                                 ((string-equal raw "TRUE") t)
                                 ((string-equal raw "FALSE") :false))))))

;;; --- the event ------------------------------------------------------------------

(defstruct (ical-event (:constructor make-ical-event) (:conc-name event-))
  "One VEVENT, with what its resource said beside it."
  (uid "" :type string)
  (summary "" :type string)
  (start nil)
  (end nil)
  (location "" :type string)
  (description "" :type string)
  (all-day nil)
  (organizer nil)
  (attendees '() :type list)
  (transp "" :type string)
  (href "" :type string)
  (etag "" :type string)
  ;; content lines this file does not model, in the order read, and re-emitted
  (raw-props '() :type list)
  ;; ("DTSTART" . the line as read); dropped when the value it describes changes
  (raw-dt-lines (make-hash-table :test #'equal) :type hash-table)
  ;; ("DTSTART" . TZID) — the zone the event is anchored to
  (dt-zones (make-hash-table :test #'equal) :type hash-table)
  ;; the document's VTIMEZONE blocks, verbatim, as lists of lines
  (timezones '() :type list))

(defun set-event-start (event moment)
  "Set EVENT's start, forgetting a remembered DTSTART line it no longer matches."
  (unless (equalp moment (event-start event))
    (remhash "DTSTART" (event-raw-dt-lines event)))
  (setf (event-start event) moment))

(defun set-event-end (event moment)
  "Set EVENT's end, forgetting a remembered DTEND line it no longer matches."
  (unless (equalp moment (event-end event))
    (remhash "DTEND" (event-raw-dt-lines event)))
  (setf (event-end event) moment))

(defun set-event-all-day (event flag)
  "Set EVENT's all-day flag, forgetting the lines whose form it decides."
  (unless (eql (not (null flag)) (not (null (event-all-day event))))
    (remhash "DTSTART" (event-raw-dt-lines event))
    (remhash "DTEND" (event-raw-dt-lines event)))
  (setf (event-all-day event) (not (null flag))))

;;; --- reading ---------------------------------------------------------------------

(defun ical-lines (text)
  "TEXT as lines, every CR, LF and CRLF ending being one break."
  (let ((lines '())
        (start 0)
        (length (length text)))
    (loop for i from 0 below length
          do (when (member (char text i) '(#\Return #\Newline))
               (push (subseq text start i) lines)
               (when (and (char= (char text i) #\Return)
                          (< (1+ i) length)
                          (char= (char text (1+ i)) #\Newline))
                 (incf i))
               (setf start (1+ i))))
    (when (<= start length)
      (push (subseq text start) lines))
    (nreverse lines)))

(defun ical-unfold (text)
  "TEXT's content lines, folded lines joined to the line they continue."
  (let ((lines '())
        (current nil))
    (dolist (line (ical-lines text))
      (cond ((and (plusp (length line)) (member (char line 0) '(#\Space #\Tab)))
             (when current
               (setf current (concatenate 'string current (subseq line 1)))))
            (t (when current (push current lines))
               (setf current line))))
    (when current (push current lines))
    (nreverse lines)))

(defun split-unquoted (text character)
  "TEXT split at every CHARACTER that is not inside double quotes."
  (let ((pieces '())
        (chunk (make-string-output-stream))
        (quoted nil))
    (loop for char across text
          do (cond ((char= char #\") (setf quoted (not quoted)) (write-char char chunk))
                   ((and (char= char character) (not quoted))
                    (push (get-output-stream-string chunk) pieces))
                   (t (write-char char chunk))))
    (push (get-output-stream-string chunk) pieces)
    (nreverse pieces)))

(defun split-property (line)
  "LINE as (values NAME PARAMS VALUE); PARAMS is an alist of upper-case keys."
  (let* ((head (car (split-unquoted line #\:)))
         (value (subseq line (min (length line) (1+ (length head)))))
         (pieces (split-unquoted head #\;))
         (params '()))
    (dolist (piece (rest pieces))
      (let ((equals (position #\= piece)))
        (when equals
          (push (cons (string-upcase (subseq piece 0 equals))
                      (string-trim "\"" (subseq piece (1+ equals))))
                params))))
    (values (string-upcase (first pieces)) (nreverse params) value)))

(defun ical-unescape (value)
  "VALUE with the RFC 5545 TEXT escapes undone."
  (if (null (position #\\ value))
      value
      (with-output-to-string (out)
        (loop with i = 0 with n = (length value)
              while (< i n)
              do (let ((char (char value i)))
                   (if (and (char= char #\\) (< (1+ i) n))
                       (let ((next (char value (1+ i))))
                         (write-char (case next
                                       ((#\n #\N) #\Newline)
                                       (t next))
                                     out)
                         (incf i 2))
                       (progn (write-char char out) (incf i))))))))

(defun parse-dt-value (value params)
  "A DTSTART/DTEND value as (values MOMENT ALL-DAY-P), or NIL for one unreadable."
  (let* ((value (string-trim '(#\Space #\Tab) value))
         (date-param (or (cdr (assoc "VALUE" params :test #'string=)) ""))
         (all-day (or (string-equal date-param "DATE")
                      (and (= 8 (length value)) (null (position #\T value))))))
    (flet ((number (start end)
             (ignore-errors (parse-integer value :start start :end end))))
      (if all-day
          (let ((year (number 0 4)) (month (number 4 6)) (day (number 6 8)))
            (if (and year month day)
                (values (make-moment :year year :month month :day day :all-day t) t)
                (values nil t)))
          (let* ((utc (and (plusp (length value)) (char= #\Z (char value (1- (length value))))))
                 (digits (if utc (subseq value 0 (1- (length value))) value)))
            (if (and (= 15 (length digits)) (char= #\T (char digits 8)))
                (let ((year (number 0 4)) (month (number 4 6)) (day (number 6 8))
                      (hour (number 9 11)) (minute (number 11 13)) (second (number 13 15)))
                  (if (and year month day hour minute second)
                      (values (make-moment :year year :month month :day day
                                            :hour hour :minute minute :second second
                                            :offset (and utc 0))
                              nil)
                      (values nil nil)))
                (values nil nil)))))))

(defun timezone-key (block)
  "The TZID BLOCK defines, or the whole block when it names none."
  (loop for line in (rest block)
        do (multiple-value-bind (name params value) (split-property line)
             (declare (ignore params))
             (cond ((string= name "TZID") (return (string-trim '(#\Space #\Tab) value)))
                   ((string= name "BEGIN") (return (format nil "~{~a~^~%~}" block)))))
        finally (return (format nil "~{~a~^~%~}" block))))

(defun parse-timezones (lines)
  "Every closed VTIMEZONE in LINES, verbatim, at most one per TZID."
  (let ((blocks '())
        (seen '())
        (current nil))
    (dolist (line lines)
      (let ((upper (string-upcase line)))
        (cond ((and (plusp (length upper)) (uiop:string-prefix-p "BEGIN:VTIMEZONE" upper))
               (setf current (list line)))
              ((null current) nil)
              ((or (uiop:string-prefix-p "BEGIN:VEVENT" upper)
                   (uiop:string-prefix-p "END:VCALENDAR" upper))
               (setf current nil))
              (t (push line current)
                 (when (uiop:string-prefix-p "END:VTIMEZONE" upper)
                   (let* ((block (nreverse current))
                          (key (timezone-key block)))
                     (unless (member key seen :test #'string=)
                       (push key seen)
                       (push block blocks)))
                   (setf current nil))))))
    (nreverse blocks)))

(defun apply-property (event line)
  "Fold one VEVENT content LINE into EVENT, keeping what this file does not model."
  (multiple-value-bind (name params value) (split-property line)
    (cond
      ((string= name "UID") (setf (event-uid event) (ical-unescape value)))
      ((string= name "SUMMARY") (setf (event-summary event) (ical-unescape value)))
      ((string= name "LOCATION") (setf (event-location event) (ical-unescape value)))
      ((string= name "DESCRIPTION") (setf (event-description event) (ical-unescape value)))
      ((string= name "DTSTART")
       (multiple-value-bind (moment all-day) (parse-dt-value value params)
         (setf (event-start event) moment)
         (setf (event-all-day event) all-day)
         (setf (gethash "DTSTART" (event-raw-dt-lines event)) line)
         (remember-zone event "DTSTART" params)))
      ((string= name "DTEND")
       (multiple-value-bind (moment all-day) (parse-dt-value value params)
         (setf (event-end event) moment)
         (setf (event-all-day event) (or (event-all-day event) all-day))
         (setf (gethash "DTEND" (event-raw-dt-lines event)) line)
         (remember-zone event "DTEND" params)))
      ((string= name "TRANSP") (setf (event-transp event) (string-upcase (string-trim " " value))))
      ((string= name "ORGANIZER") (setf (event-organizer event) (parse-cal-address value params)))
      ((string= name "ATTENDEE")
       (push (parse-cal-address value params) (event-attendees event)))
      (t (push line (event-raw-props event))))))

(defun remember-zone (event property params)
  "Note the TZID the time in PROPERTY was written in."
  (let ((tzid (string-trim " " (or (cdr (assoc "TZID" params :test #'string=)) ""))))
    (when (plusp (length tzid))
      (setf (gethash property (event-dt-zones event)) tzid))))

(defun parse-vevents (lines)
  "Every VEVENT in LINES, in document order."
  (let ((events '())
        (current nil)
        (nested '()))
    (dolist (line lines)
      (let ((upper (string-upcase line)))
        (cond
          ((null current)
           (when (uiop:string-prefix-p "BEGIN:VEVENT" upper)
             (setf current (make-ical-event))))
          (nested
           (push line (event-raw-props current))
           (when (and (uiop:string-prefix-p "END:" upper)
                      (string= (string-trim " " (subseq upper 4)) (first nested)))
             (pop nested)))
          ((uiop:string-prefix-p "BEGIN:" upper)
           (push (string-trim " " (subseq upper 6)) nested)
           (push line (event-raw-props current)))
          ((uiop:string-prefix-p "END:VEVENT" upper)
           (push current events)
           (setf current nil))
          ((plusp (length line))
           (apply-property current line)))))
    (nreverse events)))

(defun parse-ical (text)
  "Every VEVENT in TEXT, each carrying the document's VTIMEZONE blocks."
  (let* ((lines (ical-unfold text))
         (timezones (parse-timezones lines))
         (events (parse-vevents lines)))
    (dolist (event events)
      (setf (event-timezones event) timezones
            (event-raw-props event) (nreverse (event-raw-props event))
            (event-attendees event) (nreverse (event-attendees event))))
    events))

;;; --- writing -----------------------------------------------------------------------

(defun ical-escape (value)
  "VALUE as one TEXT value: backslash, newline, comma and semicolon escaped."
  (with-output-to-string (out)
    (let ((text (substitute #\Newline #\Return value)))
      (loop for char across text
            do (case char
                 (#\\ (write-string "\\\\" out))
                 (#\Newline (write-string "\\n" out))
                 (#\, (write-string "\\," out))
                 (#\; (write-string "\\;" out))
                 (t (write-char char out)))))))

(defun param-value (value)
  "VALUE as an iCalendar parameter, quoted when it holds a delimiter."
  (if (some (lambda (char) (member char '(#\, #\; #\: #\"))) value)
      (format nil "\"~a\"" (remove #\" value))
      value))

(defun format-cal-address (property attendee)
  "PROPERTY as one ORGANIZER / ATTENDEE content line for ATTENDEE."
  (let ((parts (list property)))
    (unless (zerop (length (attendee-name attendee)))
      (push (format nil "CN=~a" (param-value (attendee-name attendee))) parts))
    (unless (zerop (length (attendee-role attendee)))
      (push (format nil "ROLE=~a" (param-value (attendee-role attendee))) parts))
    (unless (zerop (length (attendee-partstat attendee)))
      (push (format nil "PARTSTAT=~a" (param-value (attendee-partstat attendee))) parts))
    (when (attendee-rsvp attendee)
      (push (format nil "RSVP=~a" (if (eq :false (attendee-rsvp attendee)) "FALSE" "TRUE")) parts))
    (let ((address (attendee-email attendee)))
      (unless (position #\: address)
        (setf address (concatenate 'string "mailto:" address)))
      (format nil "~a:~a" (format nil "~{~a~^;~}" (nreverse parts)) address))))

(defun char-octets (char)
  "How many UTF-8 octets CHAR takes."
  (let ((code (char-code char)))
    (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4))))

(defun fold-line (line)
  "LINE folded to at most 75 octets a line, a continuation prefixed with one space.
The folding is what RFC 5545 asks for: a long DESCRIPTION or a list of
attendees is not one 400-character line on the wire."
  (when (or (position #\Return line) (position #\Newline line))
    (caldav-fail "iCalendar content lines must not contain a line break."))
  (if (<= (reduce #'+ line :key #'char-octets :initial-value 0) 75)
      line
      (with-output-to-string (out)
        (let ((size 0))
          (loop for char across line
                for width = (char-octets char)
                do (when (> (+ size width) (if (zerop size) 75 74))
                     (write-char #\Return out)
                     (write-char #\Newline out)
                     (write-char #\Space out)
                     (setf size 1))
                   (write-char char out)
                   (incf size width))))))

(defun format-dt (property moment all-day)
  "PROPERTY as a DTSTART / DTEND line for MOMENT."
  (cond
    ((or all-day (moment-all-day moment))
     (format nil "~a;VALUE=DATE:~4,'0d~2,'0d~2,'0d" property
             (moment-year moment) (moment-month moment) (moment-day moment)))
    ((null (moment-offset moment))
     (format nil "~a:~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d" property
             (moment-year moment) (moment-month moment) (moment-day moment)
             (moment-hour moment) (moment-minute moment) (moment-second moment)))
    (t (format nil "~a:~a" property (moment-utc-stamp moment)))))

(defun event-lines (event dtstamp)
  "EVENT's VEVENT body: what this file models, then what it kept verbatim."
  (let ((lines '())
        (has-dtstamp (some (lambda (line) (uiop:string-prefix-p "DTSTAMP" (string-upcase line)))
                            (event-raw-props event))))
    (when (and dtstamp (not has-dtstamp))
      (push (format nil "DTSTAMP:~a" (moment-utc-stamp dtstamp)) lines))
    (when (plusp (length (event-summary event)))
      (push (format nil "SUMMARY:~a" (ical-escape (event-summary event))) lines))
    (when (plusp (length (event-transp event)))
      (push (format nil "TRANSP:~a" (event-transp event)) lines))
    (when (plusp (length (event-location event)))
      (push (format nil "LOCATION:~a" (ical-escape (event-location event))) lines))
    (when (plusp (length (event-description event)))
      (push (format nil "DESCRIPTION:~a" (ical-escape (event-description event))) lines))
    (dolist (property '("DTSTART" "DTEND"))
      (let* ((remembered (gethash property (event-raw-dt-lines event)))
             (moment (if (string= property "DTSTART") (event-start event) (event-end event))))
        (cond (remembered (push remembered lines))
              (moment (push (format-dt property moment (event-all-day event)) lines)))))
    (when (event-organizer event)
      (push (format-cal-address "ORGANIZER" (event-organizer event)) lines))
    (dolist (attendee (event-attendees event))
      (push (format-cal-address "ATTENDEE" attendee) lines))
    (dolist (line (event-raw-props event))
      (push line lines))
    (nreverse lines)))

(defun build-ical (event &key (prodid "-//nodecode-yandex-calendar//EN") (dtstamp (now-moment)))
  "EVENT as one VCALENDAR document, CRLF-terminated, its zones defined first."
  (let ((lines (list "BEGIN:VCALENDAR"
                      "VERSION:2.0"
                      (format nil "PRODID:~a" prodid))))
    (dolist (block (event-timezones event))
      (setf lines (append lines block)))
    (setf lines (append lines
                         (list "BEGIN:VEVENT"
                               (format nil "UID:~a" (ical-escape (event-uid event))))
                         (event-lines event dtstamp)
                         (list "END:VEVENT" "END:VCALENDAR")))
    (with-output-to-string (out)
      (dolist (line lines)
        (write-string (fold-line line) out)
        (write-char #\Return out)
        (write-char #\Newline out)))))
