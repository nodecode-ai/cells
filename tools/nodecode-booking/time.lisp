;;;; time.lisp --- RFC3339 in, UTC seconds inside, RFC3339 out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Google Calendar speaks RFC3339: a civil date and time, and the offset it
;;;; was written at. Everything inside this cell is an integer count of seconds
;;;; since 1970-01-01T00:00:00Z, and the offset travels beside it as its own
;;;; integer, so no slot is ever compared across two zone readings. The civil
;;;; conversions are Hinnant's days-from-civil and civil-from-days: no host
;;;; zone, no time zone database, the same answer on every machine.

(in-package #:nodecode-booking)

(defparameter +seconds-per-day+ 86400
  "One civil day in seconds.")

(defparameter +unix-epoch+ 2208988800
  "Seconds from 1970-01-01T00:00:00Z to the CL universal time 1900-01-01T00:00:00Z.")

(defun now-seconds ()
  "The current instant, as seconds since 1970-01-01T00:00:00Z."
  (- (get-universal-time) +unix-epoch+))

(defun days-from-civil (year month day)
  "Days since 1970-01-01 of a proleptic Gregorian YEAR-MONTH-DAY."
  (let* ((y (if (<= month 2) (1- year) year))
         (era (floor (if (>= y 0) y (- y 399)) 400))
         (yoe (- y (* era 400)))
         (doy (+ (floor (+ (* 153 (+ month (if (> month 2) -3 9))) 2) 5) (1- day)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (- (+ (* era 146097) doe) 719468)))

(defun civil-from-days (days)
  "The YYYY-MM-DD of DAYS since 1970-01-01. => (values year month day)."
  (let* ((z (+ days 719468))
         (era (floor (if (>= z 0) z (- z 146096)) 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (- doe (floor doe 1460) (- (floor doe 36524)) (floor doe 146096)) 365))
         (y (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)) (floor yoe 146096))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (day (+ (- doy (floor (+ (* 153 mp) 2) 5)) 1))
         (month (+ mp (if (< mp 10) 3 -9))))
    (values (if (<= month 2) (1+ y) y) month day)))

(defun digits-at (text start end)
  "The integer TEXT's START..END spells, or NIL when those are not all digits."
  (when (and (<= 0 start) (<= end (length text)) (< start end))
    (let ((digits (loop for index from start below end collect (char text index))))
      (when (every #'digit-char-p digits)
        (parse-integer (coerce digits 'string))))))

(defun char-at (text index)
  "TEXT's character at INDEX, or NIL past its end."
  (and (<= 0 index) (< index (length text)) (char text index)))

(defun parse-offset (text)
  "The seconds east of UTC an offset string names: Z, +HH:MM, -HHMM, +HH."
  (let* ((word (string-upcase text))
         (length (length word)))
    (cond
      ((or (string= word "Z") (string= word "UTC") (string= word "+00:00")) 0)
      ((and (<= 2 length) (member (char word 0) '(#\+ #\-)))
       (let* ((sign (if (char= (char word 0) #\+) 1 -1))
              (hours (digits-at word 1 3))
              (minutes (cond ((and (< 4 length) (char= (char word 3) #\:)) (digits-at word 4 6))
                             ((= 5 length) (digits-at word 3 5))
                             (t 0))))
         (unless (and hours minutes (<= hours 23) (<= minutes 59))
           (fail "~s is not a UTC offset: write -06:00, +05:30 or Z" text))
         (* sign (+ (* hours 3600) (* minutes 60)))))
      (t (fail "~s is not a UTC offset: write -06:00, +05:30 or Z" text)))))

(defun parse-time (text &optional default-offset)
  "TEXT read as an instant. => (values seconds offset)
An RFC3339 time carries its own offset; a bare YYYY-MM-DDTHH:MM[:SS] is read
at DEFAULT-OFFSET (seconds east of UTC, default 0)."
  (let* ((text (nlk:trimmed (or text "")))
         (length (length text))
         (year (digits-at text 0 4))
         (dash1 (char-at text 4))
         (month (and (char= (or dash1 #\Space) #\-) (digits-at text 5 7)))
         (dash2 (char-at text 7))
         (day (and (char= (or dash2 #\Space) #\-) (digits-at text 8 10)))
         (space (char-at text 10))
         (hour (and space (member space '(#\T #\t #\Space)) (digits-at text 11 13)))
         (colon (char-at text 13))
         (minute (and (char= (or colon #\Space) #\:) (digits-at text 14 16)))
         (has-seconds (and (< 16 length) (char= (char text 16) #\:)))
         (second (if has-seconds (digits-at text 17 19) 0))
         (zone (and (< (if has-seconds 19 16) length) (subseq text (if has-seconds 19 16)))))
    (unless (and year month day hour minute second
                 (<= 1 month 12) (<= 1 day 31) (<= 0 hour 23) (< minute 60) (< second 61))
      (fail "~s is not a time: write 2026-11-17T09:00, or a full RFC3339 time such as 2026-11-17T09:00:00-06:00" text))
    (let* ((civil (+ (* (days-from-civil year month day) +seconds-per-day+)
                     (* hour 3600) (* minute 60) second))
           (offset (if zone (parse-offset zone) (or default-offset 0))))
      (values (- civil offset) offset))))

(defun format-offset (offset)
  "OFFSET seconds east of UTC, as +HH:MM."
  (multiple-value-bind (sign magnitude) (if (minusp offset) (values "-" (- offset)) (values "+" offset))
    (format nil "~a~2,'0d:~2,'0d" sign (floor magnitude 3600) (floor (mod magnitude 3600) 60))))

(defun format-time (seconds offset)
  "SECONDS since the epoch, written as an RFC3339 date and time at OFFSET."
  (multiple-value-bind (days within) (floor (+ seconds offset) +seconds-per-day+)
    (multiple-value-bind (year month day) (civil-from-days days)
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~a"
              year month day (floor within 3600) (floor (mod within 3600) 60) (mod within 60)
              (format-offset offset)))))

(defun machine-offset (seconds)
  "The offset seconds east of UTC this machine's own zone was at SECONDS."
  (let ((hours (nth-value 8 (decode-universal-time (+ seconds +unix-epoch+)))))
    (round (* -3600 hours))))

(defun free-slots (start end busy duration)
  "The START..END windows at least DURATION long, cut back to back from START.
BUSY is a list of (START . END) seconds, in any order."
  (let ((cursor start)
        (slots '()))
    (dolist (interval (append (sort (copy-list busy) #'< :key #'car) (list (cons end end))))
      (let ((busy-start (max start (car interval))))
        (loop while (<= (+ cursor duration) (min busy-start end))
              do (push (cons cursor (+ cursor duration)) slots)
                 (incf cursor duration))
        (setf cursor (max cursor (min (cdr interval) end)))))
    (nreverse slots)))
