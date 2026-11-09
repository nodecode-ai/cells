;;;; cell.lisp --- what a cron expression fires, in words and in dates.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cron line is short and easy to get wrong, for people and for models:
;;;; 0 9 1 * 1 is not "the first Monday", * 9 * * * runs sixty times, */7
;;;; does not mean every seven minutes across the hour, and @daily is not a
;;;; five-field expression. This cell reads the expression out in words,
;;;; field by field, with the runs it fires next in a zone the operator names.
;;;;
;;;; Nothing leaves the machine: no request, no subprocess, nothing written.
;;;; The only file read is a zone of the machine's own tz database, and only
;;;; when a timezone is named -- the same bytes TZ=Europe/Berlin reads.

(defpackage #:nodecode-crontab
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:crontab-error #:crontab-error-detail #:*crontab*))

(in-package #:nodecode-crontab)

(nlk:define-peripheral crontab :not-running t)

;;; --- what the fields are -----------------------------------------------------------

(defparameter +field-names+
  #("minute" "hour" "day of month" "month" "day of week" "second" "year")
  "One name per field, in the order a cron expression writes them.")

(defparameter +weekdays+
  #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
  "Cron's weekday names, 0 first (which is why 7 is Sunday too).")

(defparameter +weekday-labels+
  #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
  "The three-letter label a listed run carries.")

(defparameter +months+
  #("January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))

(defparameter +month-names+
  '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12)))

(defparameter +weekday-names+
  '(("sun" . 0) ("mon" . 1) ("tue" . 2) ("wed" . 3) ("thu" . 4) ("fri" . 5) ("sat" . 6)))

(defparameter +macros+
  '(("@yearly" . "0 0 1 1 *") ("@annually" . "0 0 1 1 *") ("@monthly" . "0 0 1 * *")
    ("@weekly" . "0 0 * * 0") ("@daily" . "0 0 * * *") ("@midnight" . "0 0 * * *")
    ("@hourly" . "0 * * * *"))
  "The shortcuts a cron reader knows, each as the five fields it stands for.")

(defparameter +max-length+ 200
  "The longest expression explained here; a cron line is far shorter.")

(defparameter +max-runs+ 20
  "The most next runs a reading lists.")

(defparameter +search-days+ 3660
  "How far ahead a next run is looked for: ten years, past every leap-year shape.")

(defparameter +day-seconds+ 86400)

(defvar *now* nil
  "NIL reads the machine's clock; a universal time pins it, which is how the
tests read a fixed instant.")

(defun clock-now ()
  "The universal time this cell reads as now."
  (or *now* (get-universal-time)))

;;; --- one field of an expression ----------------------------------------------------

(defun field-low (index)
  "The first value field INDEX takes."
  (case index ((0 1 4 5) 0) ((2 3) 1) (6 1970)))

(defun field-high (index)
  "The last value field INDEX takes, 7 in day of week because 7 is Sunday."
  (case index (0 59) (1 23) (2 31) (3 12) (4 7) (5 59) (6 2199)))

(defun field-width (index)
  (1+ (- (field-high index) (field-low index))))

(defun field-slot (index value)
  "VALUE's place in field INDEX's mask."
  (- value (field-low index)))

(defstruct (field (:constructor %make-field (text index mask)))
  "One written field: its TEXT, which field it is, and what it comes to."
  (text "" :read-only t)
  (index 0 :read-only t)
  (mask (make-array 1 :element-type 'bit) :read-only t)
  (values nil)
  (any-p nil)
  (last-p nil)
  (nth nil))

(defun make-field (text index)
  "A FIELD for TEXT at INDEX, its mask sized for the field."
  (%make-field text index (make-array (field-width index) :element-type 'bit
                                                      :initial-element 0)))

(defun field-name (index)
  (aref +field-names+ index))

(defun add-value (field value)
  "FIELD fires at VALUE: into the mask, and into the values a reading lists."
  (when (and (= 4 (field-index field)) (= 7 value))
    (setf value 0))
  (setf (aref (field-mask field) (field-slot (field-index field) value)) 1)
  (pushnew value (field-values field))
  field)

(defun field-fires-p (field value)
  "Whether FIELD lets VALUE through."
  (let ((mask (field-mask field))
        (slot (field-slot (field-index field) value)))
    (and mask (< slot (length mask)) (= 1 (aref mask slot)))))

(defun parse-value (text index)
  "TEXT as a value of field INDEX -- a number, or a month or weekday name --
or NIL."
  (cond ((and (plusp (length text)) (every #'digit-char-p text))
         (parse-integer text))
        (t (let ((names (case index (3 +month-names+) (4 +weekday-names+))))
             (and names
                  (cdr (assoc (subseq (string-downcase text) 0 (min 3 (length text)))
                              names :test #'string=)))))))

(defun split-text (text character)
  "TEXT split at every CHARACTER, empty pieces dropped."
  (let ((pieces '()) (start 0))
    (loop for index from 0 to (length text)
          when (or (= index (length text)) (char= character (char text index)))
            do (when (> index start) (push (subseq text start index) pieces))
               (setf start (1+ index)))
    (nreverse pieces)))

(defun field-bounds (field body)
  "The first and last value BODY names in FIELD, and whether it named a range."
  (let* ((index (field-index field))
         (dash (position #\- body)))
    (cond ((and dash (plusp dash) (< dash (1- (length body))))
           (let ((first (parse-value (subseq body 0 dash) index))
                 (last (parse-value (subseq body (1+ dash)) index)))
             (unless (and first last)
               (fail "~s is not a range in the ~a field; write 9-17, or mon-fri in day of week"
                     body (field-name index)))
             (unless (and (<= (field-low index) first (field-high index))
                          (<= (field-low index) last (field-high index)))
               (fail "~s is out of range in the ~a field, which runs ~d to ~d"
                     body (field-name index) (field-low index) (field-high index)))
             (values first last t)))
          (t (let ((value (parse-value body index)))
               (unless value
                 (fail "~s is not a value in the ~a field; it takes ~d to ~d, a name, a range with -, a step with /, and a list with ,"
                       body (field-name index) (field-low index) (field-high index)))
               (unless (<= (field-low index) value (field-high index))
                 (fail "~s is out of range in the ~a field, which runs ~d to ~d"
                       body (field-name index) (field-low index) (field-high index)))
               (values value value nil))))))

(defun expand-piece (field piece)
  "Add one comma-separated PIECE of FIELD's text to what the field fires on."
  (let* ((index (field-index field))
         (slash (position #\/ piece))
         (body (if slash (subseq piece 0 slash) piece))
         (step (if slash (parse-value (subseq piece (1+ slash)) index) 1)))
    (unless (and (integerp step) (plusp step))
      (fail "~s is not a step in the ~a field; a step is a number after a /, as in */15"
            piece (field-name index)))
    (cond ((string= body "*")
           (loop for value from (field-low index) to (field-high index) by step
                 do (add-value field value)))
          (t (multiple-value-bind (first last range-p) (field-bounds field body)
               (cond ((and (not range-p) (not slash)) (add-value field first))
                     (t (loop for value from first to last by step
                              do (add-value field value)))))))))

(defun parse-nth-weekday (field text)
  "FIELD's day of week from an nth-weekday TEXT: 5#3 is the third Friday, L5 and 5L the last."
  (let* ((hash (position #\# text))
         (day (if hash
                  (parse-value (subseq text 0 hash) 4)
                  (parse-value (remove-if (lambda (char) (char-equal char #\l)) text) 4)))
         (nth (if hash (parse-value (subseq text (1+ hash)) 4) :last)))
    (unless (and day (or (eq nth :last) (and (integerp nth) (<= 1 nth 5))))
      (fail "~s is not an nth weekday; write 5#3 for the third Friday, or L5 for the last"
            text))
    (setf (field-nth field) (list (cons day (list nth))))))

(defun parse-field (text index)
  "FIELD for TEXT at INDEX, or a refusal naming the field."
  (let ((field (make-field text index)))
    (cond ((string= text "*") (setf (field-any-p field) t) (expand-piece field "*"))
          ((and (= index 2) (string-equal text "l")) (setf (field-last-p field) t))
          ((and (= index 4) (find #\# text)) (parse-nth-weekday field text))
          ((and (= index 4) (find-if (lambda (char) (char-equal char #\l)) text))
           (parse-nth-weekday field text))
          (t (dolist (piece (split-text text #\,)) (expand-piece field piece))))
    (setf (field-values field) (sort (field-values field) #'<))
    field))

;;; --- the expression itself ---------------------------------------------------------

(defstruct (expression (:constructor %make-expression))
  "One cron expression: what was written, what a reader reads, and its fields."
  (raw "" :read-only t)
  (expr "" :read-only t)
  (fields #() :read-only t)
  (either-day nil :read-only t))

(defun field-at (expression index)
  (aref (expression-fields expression) index))

(defun expression-field (expression index)
  (field-text (field-at expression index)))

(defun field-count (expression)
  (length (expression-fields expression)))

(defun words-of (text)
  "TEXT split at runs of whitespace."
  (let ((words '()) (start nil))
    (loop for index from 0 to (length text)
          for char = (and (< index (length text)) (char text index))
          do (cond ((or (null char) (member char '(#\Space #\Tab)))
                    (when start (push (subseq text start index) words) (setf start nil)))
                   ((null start) (setf start index))))
    (nreverse words)))

(defun unquote (text)
  "TEXT without the one layer of quotes an expression may be written in."
  (let ((text (nlk:trimmed (or text ""))))
    (if (and (>= (length text) 2)
             (member (char text 0) (list #\" #\' #\`))
             (char= (char text 0) (char text (1- (length text)))))
        (nlk:trimmed (subseq text 1 (1- (length text))))
        text)))

(defun parse (raw)
  "RAW as an EXPRESSION, or a refusal in words the model can repair from."
  (let ((text (unquote raw)))
    (when (zerop (length text))
      (fail "no cron expression given: write five fields, as in \"0 9 * * 1-5\""))
    (when (> (length text) +max-length+)
      (fail "that is ~d characters; a cron expression is far shorter (at most ~d here)"
            (length text) +max-length+))
    (let* ((lowered (string-downcase text))
           (expr (if (char= #\@ (char lowered 0))
                     (cond ((string= lowered "@reboot")
                            (fail "@reboot means when the machine starts, which is not a schedule; write its five fields instead"))
                           (t (or (cdr (assoc lowered +macros+ :test #'string=))
                                  (fail "unknown shortcut ~a; the known ones are ~a"
                                        text (join-text (mapcar #'car +macros+))))))
                     (format nil "~{~a~^ ~}" (words-of text))))
           (pieces (words-of expr)))
      (unless (<= 5 (length pieces) 7)
        (fail "a cron expression has 5 fields (minute hour day-of-month month day-of-week), optionally a 6th (second) and 7th (year); this has ~d~@[, and a value with spaces in it goes in quotes: \"0 9 * * 1-5\"~]"
              (length pieces) (< (length pieces) 5)))
      (let ((fields (loop for piece in pieces for index from 0
                          collect (parse-field piece index))))
        (%make-expression
         :raw text :expr expr :fields (coerce fields 'vector)
         :either-day (and (not (field-any-p (nth 2 fields)))
                           (not (field-any-p (nth 4 fields)))))))))

;;; --- the words --------------------------------------------------------------------

(defun runs-of (values)
  "VALUES as (first . last) runs of consecutive values."
  (let ((runs '()))
    (dolist (value (sort (copy-list values) #'<))
      (if (and runs (= value (1+ (cdar runs))))
          (setf (cdar runs) value)
          (push (cons value value) runs)))
    (nreverse runs)))

(defun join-text (items &optional (word "and"))
  "ITEMS joined with commas and WORD before the last."
  (let ((items (mapcar #'princ-to-string items)))
    (cond ((null items) "")
          ((null (rest items)) (first items))
          (t (format nil "~{~a~^, ~} ~a ~a" (butlast items) word (car (last items)))))))

(defun numbers-text (values)
  "VALUES as '1-5, 10 and 15': runs of three or more said as ranges."
  (join-text
   (loop for (first . last) in (runs-of values)
         if (>= (- last first) 2) collect (format nil "~d-~d" first last)
         else append (loop for value from first to last collect value))))

(defun named-text (values names offset)
  "VALUES as month or weekday names, 'Monday through Friday'."
  (join-text
   (loop for (first . last) in (runs-of values)
         if (>= (- last first) 2)
           collect (format nil "~a through ~a" (aref names (- first offset))
                           (aref names (- last offset)))
         else append (loop for value from first to last
                           collect (aref names (- value offset))))))

(defun clock-text (hour minute)
  "HOUR and MINUTE as a clock time the operator reads."
  (format nil "~2,'0d:~2,'0d" hour minute))

(defun hours-text (hours)
  "HOURS as the spans they cover, '09:00-09:59 and 17:00-17:59'."
  (join-text
   (loop for (first . last) in (runs-of hours)
         collect (format nil "~2,'0d:00-~2,'0d:59" first last))))

(defun step-of (values high)
  "The step when VALUES is every n-th value from its first to the end of the field."
  (when (>= (length values) 2)
    (let ((step (- (second values) (first values))))
      (when (and (plusp step)
                 (equal values (loop for value from (first values) to high by step
                                     collect value))
                 (< (- (first values) step) (first values)))
        step))))

(defun time-phrase (expression)
  "The 'when in the hour' part of the sentence."
  (let* ((minutes (field-values (field-at expression 0)))
         (hours (field-values (field-at expression 1)))
         (any-minute (field-any-p (field-at expression 0)))
         (any-hour (field-any-p (field-at expression 1))))
    (cond ((and any-minute any-hour) "Every minute")
          (any-minute (format nil "Every minute during ~a" (hours-text hours)))
          (any-hour (let ((step (step-of minutes 59)))
                      (cond ((= 1 (length minutes))
                             (format nil "At minute ~d of every hour" (first minutes)))
                            ((and step (zerop (first minutes)))
                             (format nil "Every ~d minutes" step))
                            (step (format nil "Every ~d minutes from minute ~d of every hour"
                                          step (first minutes)))
                            (t (format nil "At minutes ~a of every hour"
                                       (numbers-text minutes))))))
          ((<= (* (length minutes) (length hours)) 8)
           (format nil "At ~a"
                   (join-text (loop for hour in hours
                                    append (loop for minute in minutes
                                                 collect (clock-text hour minute))))))
          (t (let* ((step (step-of minutes 59))
                    (each (cond ((and step (zerop (first minutes)))
                                 (format nil "Every ~d minutes" step))
                                ((= 1 (length minutes))
                                 (format nil "At minute ~d" (first minutes)))
                                (t (format nil "At minutes ~a" (numbers-text minutes))))))
               (format nil "~a during ~a (first ~a, last ~a)" each (hours-text hours)
                       (clock-text (first hours) (first minutes))
                       (clock-text (car (last hours)) (car (last minutes)))))))))

(defun day-of-month-phrase (expression)
  "The 'which days of the month' part."
  (let* ((field (field-at expression 2))
         (numbers (field-values field))
         (parts (append (when numbers
                          (list (if (= 1 (length numbers))
                                    (format nil "day ~d" (first numbers))
                                    (format nil "days ~a" (numbers-text numbers)))))
                        (when (field-last-p field) (list "the last day")))))
    (format nil "~a of the month" (join-text parts))))

(defun day-of-week-phrase (expression)
  "The 'which weekdays' part, nth weekdays included."
  (let* ((field (field-at expression 4))
         (nth (field-nth field)))
    (if nth
        (format nil "~a of the month"
                (join-text
                 (loop for (day . nths) in (sort (copy-list nth) #'< :key #'car)
                       collect (format nil "the ~a ~a"
                                       (join-text (mapcar (lambda (n)
                                                            (if (eq n :last) "last"
                                                                (aref #("first" "second" "third" "fourth" "fifth") (1- n))))
                                                          nths))
                                       (aref +weekdays+ day)))))
        (named-text (field-values field) +weekdays+ 0))))

(defun day-phrase (expression)
  "The 'which days' tail of the sentence, months and years included."
  (let* ((any-dom (field-any-p (field-at expression 2)))
         (any-dow (field-any-p (field-at expression 4)))
         (day (cond ((and any-dom any-dow) "")
                    (any-dow (format nil "on ~a" (day-of-month-phrase expression)))
                    (any-dom (format nil "on ~a" (day-of-week-phrase expression)))
                    (t (format nil "on ~a or on ~a" (day-of-month-phrase expression)
                               (day-of-week-phrase expression))))))
    (unless (field-any-p (field-at expression 3))
      (setf day (format nil "~@[~a ~]in ~a" (and (plusp (length day)) day)
                         (named-text (field-values (field-at expression 3)) +months+ 1))))
    (when (and (> (field-count expression) 6) (not (field-any-p (field-at expression 6))))
      (let ((years (field-values (field-at expression 6))))
        (setf day (format nil "~@[~a ~]~a" (and (plusp (length day)) day)
                           (if (= 1 (length years))
                               (format nil "in the year ~d" (first years))
                               (format nil "in the years ~a" (numbers-text years)))))))
    day))

(defun summary (expression)
  "One sentence for what the expression fires."
  (let ((time (time-phrase expression))
        (day (day-phrase expression)))
    (when (and (field-any-p (field-at expression 2))
               (field-any-p (field-at expression 4))
               (not (field-any-p (field-at expression 1)))
               (<= 3 (length time))
               (string= "At " (subseq time 0 3)))
      (setf day (format nil "every day~@[ ~a~]" (and (plusp (length day)) day))))
    (format nil "~a~@[ ~a~]." time (and (plusp (length day)) day))))

(defun field-meaning (expression index)
  "What one field of the table says, in words."
  (let ((field (field-at expression index)))
    (case index
      (0 (let ((values (field-values field)))
           (cond ((field-any-p field) "any minute")
                 ((= 1 (length values)) (format nil "at minute ~d of the hour" (first values)))
                 (t (format nil "minutes ~a" (numbers-text values))))))
      (1 (if (field-any-p field) "any hour" (hours-text (field-values field))))
      (2 (if (field-any-p field)
             "any day"
             (join-text (append (when (field-values field)
                                  (list (numbers-text (field-values field))))
                                (when (field-last-p field) (list "last day"))))))
      (3 (if (field-any-p field) "any month"
             (named-text (field-values field) +months+ 1)))
      (4 (cond ((field-nth field) (day-of-week-phrase expression))
               ((field-any-p field) "any weekday")
               (t (named-text (field-values field) +weekdays+ 0))))
      (5 (if (field-any-p field) "any second"
             (format nil "at second ~a" (numbers-text (field-values field)))))
      (6 (if (field-any-p field) "any year"
             (numbers-text (field-values field)))))))

;;; --- days --------------------------------------------------------------------------

(defun leap-year-p (year)
  (and (zerop (mod year 4))
       (or (plusp (mod year 100)) (zerop (mod year 400)))))

(defun month-length (month year)
  "How many days MONTH of YEAR has."
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))
    (t 0)))

(defun nth-weekday-p (field day weekday month year)
  "Whether DAY is an nth or last WEEKDAY its FIELD asks for."
  (loop for (wanted . nths) in (field-nth field)
        thereis (and (= wanted weekday)
                     (loop for nth in nths
                           thereis (if (eq nth :last)
                                       (> (+ day 7) (month-length month year))
                                       (and (>= day (1+ (* 7 (1- nth))))
                                            (<= day (* 7 nth))))))))

(defun day-matches-p (expression day month weekday year)
  "Whether a DAY of MONTH that is WEEKDAY, in MONTH of YEAR, is a day this expression fires on."
  (let ((dom (field-at expression 2))
        (dow (field-at expression 4)))
    (and (field-fires-p (field-at expression 3) month)
         (let ((dom-ok (or (and (field-last-p dom) (= day (month-length month year)))
                           (field-fires-p dom day)))
               (dow-ok (or (field-fires-p dow weekday)
                           (nth-weekday-p dow day weekday month year))))
           (cond ((field-any-p dom) dow-ok)
                 ((field-any-p dow) dom-ok)
                 (t (or dom-ok dow-ok)))))))

;;; --- the clock a run is read in ----------------------------------------------------

(defstruct (zone (:constructor make-zone (name transitions offsets types)))
  "A zone of the tz database: its NAME, the TRANSITIONS (universal, ascending),
OFFSETS the type each transition switches to, and TYPES each type's seconds east of UTC."
  (name nil :read-only t)
  (transitions #() :read-only t)
  (offsets #() :read-only t)
  (types #() :read-only t))

(defparameter +zoneinfo+ "/usr/share/zoneinfo"
  "Where a zone name is looked up unless the section says otherwise.")

(defun read-octets (path)
  "Every byte of PATH."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence data in)
      data)))

(defun be-unsigned (data start size)
  "The big-endian unsigned integer of SIZE bytes at START in DATA."
  (let ((value 0))
    (dotimes (index size)
      (setf value (+ (ash value 8) (aref data (+ start index)))))
    value))

(defun be-signed (data start size)
  "The big-endian two's-complement integer of SIZE bytes at START in DATA."
  (let ((value (be-unsigned data start size)))
    (if (logbitp (1- (* 8 size)) value)
        (- value (ash 1 (* 8 size)))
        value)))

(defun tzif-counts (data at)
  "The six counts of the TZif header at AT: how many UT/local indicators,
standard/wall indicators, leap seconds, transitions, types and abbreviation bytes."
  (values (be-unsigned data (+ at 20) 4)
          (be-unsigned data (+ at 24) 4)
          (be-unsigned data (+ at 28) 4)
          (be-unsigned data (+ at 32) 4)
          (be-unsigned data (+ at 36) 4)
          (be-unsigned data (+ at 40) 4)))

(defun tzif-block-length (data at time-size)
  "How many bytes the block under the header at AT takes, in TIME-SIZE-byte times."
  (multiple-value-bind (utcnt stdcnt leapcnt timecnt typecnt charcnt) (tzif-counts data at)
    (+ (* timecnt time-size) timecnt (* typecnt 6) charcnt
       (* leapcnt (+ time-size 4)) utcnt stdcnt)))

(defun parse-tzif-block (data at time-size)
  "The transitions, the type each switches to, and each type's offset, from the
block under the header at AT."
  (multiple-value-bind (utcnt stdcnt leapcnt timecnt typecnt charcnt) (tzif-counts data at)
    (declare (ignore utcnt stdcnt leapcnt charcnt))
    (let ((cursor (+ at 44))
          (transitions (make-array timecnt))
          (offsets (make-array timecnt))
          (types (make-array typecnt)))
      (dotimes (index timecnt)
        (setf (aref transitions index) (be-signed data cursor time-size))
        (incf cursor time-size))
      (dotimes (index timecnt)
        (setf (aref offsets index) (aref data cursor))
        (incf cursor))
      (dotimes (index typecnt)
        (setf (aref types index) (be-signed data cursor 4))
        (incf cursor 6))
      (values transitions offsets types))))

(defun read-zone (path)
  "PATH as a ZONE, or a refusal."
  (let* ((data (read-octets path))
         (name (file-namestring path)))
    (unless (and (>= (length data) 44)
                 (equalp (subseq data 0 4) #(84 90 105 102)))
      (fail "~a is not a zone file of the tz database" name))
    (multiple-value-bind (transitions offsets types)
        (if (member (aref data 4) '(0 48))
            (parse-tzif-block data 0 4)
            (let ((second (+ 44 (tzif-block-length data 0 4))))
              (when (> (+ second 44) (length data))
                (fail "~a ends before the 64-bit block it announces" name))
              (parse-tzif-block data second 8)))
      (make-zone name transitions offsets types))))

(defun zone-offset (zone universal)
  "ZONE's seconds east of UTC at universal time UNIVERSAL."
  (let ((transitions (zone-transitions zone))
        (offsets (zone-offsets zone))
        (types (zone-types zone)))
    (cond ((zerop (length types)) 0)
          ((zerop (length transitions)) (aref types 0))
          (t (let ((index (1- (length transitions))))
               (loop while (and (>= index 0) (> (aref transitions index) universal))
                     do (decf index))
               (if (minusp index)
                   (aref types 0)
                   (aref types (aref offsets index))))))))

(defun machine-offset (universal)
  "This machine's seconds east of UTC at UNIVERSAL, daylight saving included."
  (multiple-value-bind (second minute hour day month year weekday daylight-p zone)
      (decode-universal-time universal)
    (declare (ignore second minute hour day month year weekday))
    (* 3600 (- (if daylight-p 1 0) zone))))

(defun offset-label (seconds)
  "SECONDS east of UTC as UTC+05:30."
  (multiple-value-bind (hours rest) (floor (abs seconds) 3600)
    (format nil "UTC~a~2,'0d:~2,'0d" (if (minusp seconds) "-" "+") hours (floor rest 60))))

(defstruct (clock (:constructor make-clock (zone label)))
  "The zone a reading is made in: a ZONE, or NIL for this machine's own, and the LABEL the reading names it by."
  (zone nil :read-only t)
  (label "" :read-only t))

(defun clock-offset (clock universal)
  (if (clock-zone clock) (zone-offset (clock-zone clock) universal) (machine-offset universal)))

(defun wall-of (clock universal)
  "UNIVERSAL as the wall-clock seconds CLOCK reads, as if it were UTC."
  (+ universal (clock-offset clock universal)))

(defun wall-parts (wall)
  "The (values DAY MONTH YEAR WEEKDAY) of WALL seconds read as if UTC, WEEKDAY in cron's numbering."
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time wall 0)
    (declare (ignore second minute hour))
    (values day month year (mod (1+ weekday) 7))))

(defun wall-time-parts (wall)
  "The (values SECOND MINUTE HOUR) of WALL seconds read as if UTC."
  (multiple-value-bind (second minute hour) (decode-universal-time wall 0)
    (values second minute hour)))

(defun wall-candidates (clock wall)
  "The universal times the wall clock WALL names in CLOCK, ascending; none when
the zone skips that wall time, as a spring-forward gap does."
  (let* ((before (clock-offset clock (- wall +day-seconds+)))
         (after (clock-offset clock (+ wall +day-seconds+)))
         (candidates (remove-duplicates (list (- wall before) (- wall after)))))
    (sort (remove-if-not (lambda (universal)
                            (= (clock-offset clock universal) (- wall universal)))
                          candidates)
          #'<)))

(defun candidate-seconds (expression)
  "The seconds of a matching minute this expression fires at: its own field's,
or the minute's first when it writes five fields."
  (if (> (field-count expression) 5)
      (let ((field (field-at expression 5)))
        (if (field-any-p field)
            (loop for second from 0 below 60 collect second)
            (field-values field)))
      (list 0)))

(defun next-runs (expression clock count &key (now (clock-now)))
  "The COUNT runs after NOW, each as (wall universal offset), read in CLOCK."
  (let* ((start (wall-of clock now))
         (day-start (- start (mod start +day-seconds+)))
         (runs '()))
    (dotimes (step +search-days+)
      (let ((wall-day (+ day-start (* step +day-seconds+))))
        (multiple-value-bind (day month year weekday) (wall-parts wall-day)
          (when (day-matches-p expression day month weekday year)
            (dolist (hour (field-values (field-at expression 1)))
              (dolist (minute (field-values (field-at expression 0)))
                (dolist (second (candidate-seconds expression))
                  (let ((wall (+ wall-day (* hour 3600) (* minute 60) second)))
                    (dolist (universal (wall-candidates clock wall))
                      (when (and (> universal now) (< (length runs) count))
                        (push (list wall universal (- wall universal)) runs)))))))))
        (when (>= (length runs) count) (return))))
    (nreverse runs)))

(defun span-text (seconds)
  "SECONDS as the two largest units: '1 day 14 hours'."
  (let ((whole (abs (floor seconds))))
    (if (< whole 60)
        "less than a minute"
        (let ((days (floor whole 86400))
              (hours (floor (mod whole 86400) 3600))
              (minutes (floor (mod whole 3600) 60)))
          (format nil "~{~a~^ ~}"
                  (remove nil
                          (list (and (plusp days) (format nil "~d day~p" days days))
                                (and (or (plusp days) (plusp hours))
                                     (format nil "~d hour~p" hours hours))
                                (and (or (plusp days) (plusp hours) (plusp minutes))
                                     (format nil "~d minute~p" minutes minutes)))))))))

;;; --- what the notes point out -------------------------------------------------------

(defun seven-in-weekday-field-p (text)
  "Whether TEXT writes 7 as a weekday -- the same day as 0."
  (member "7" (loop for piece in (split-text text #\,)
                     append (split-text piece #\-))
          :test #'string=))

(defun notes (expression)
  "The surprises worth saying about this expression."
  (let ((out '())
        (minutes (field-values (field-at expression 0)))
        (hours (field-values (field-at expression 1)))
        (days (field-values (field-at expression 2)))
        (dom (field-at expression 2)))
    (when (expression-either-day expression)
      (push (if (equal days (loop for day from 1 to 31 collect day))
                "day of month covers every day, so day of week changes nothing: this runs every day"
                "day of month and day of week are both set, so a day that matches EITHER one runs, not only days that match both")
            out))
    (when (and (field-any-p (field-at expression 0))
               (not (field-any-p (field-at expression 1)))
               hours)
      (push (format nil "the minute field is *, so this runs every minute of those hours (~d times a day); for once at ~a write 0 ~a ..."
                    (* 60 (length hours)) (clock-text (first hours) 0)
                    (expression-field expression 1))
            out))
    (let ((step (step-of minutes 59)))
      (when (and step (zerop (first minutes)) (plusp (mod 60 step)))
        (push (format nil "*/~d in the minute field starts again every hour: after :~2,'0d the next run is :00, ~d minutes later, not ~d"
                      step (car (last minutes)) (- 60 (car (last minutes))) step)
              out)))
    (let ((step (step-of hours 23)))
      (when (and step (zerop (first hours)) (plusp (mod 24 step)))
        (push (format nil "*/~d in the hour field starts again every day: after ~a the next run is 00:00, ~d hours later, not ~d"
                      step (clock-text (car (last hours)) 0) (- 24 (car (last hours))) step)
              out)))
    (when (and days (not (field-last-p dom)) (>= (first days) 29)
               (not (expression-either-day expression)))
      (push (format nil "day ~a is missing from some months, and those months are skipped (nothing runs on their last day instead); L means the last day of every month"
                    (numbers-text days))
            out))
    (when (seven-in-weekday-field-p (expression-field expression 4))
      (push "7 in day of week means Sunday, the same as 0" out))
    (when (> (field-count expression) 5)
      (push (if (> (field-count expression) 6)
                "the 6th field is seconds and the 7th is the year"
                "the 6th field is seconds")
            out))
    (nreverse out)))

;;; --- the reading -------------------------------------------------------------------

(defun reading-text (expression clock count &key (now (clock-now)))
  "EXPRESSION as the block the model reads: the sentence, the fields, the runs and the notes."
  (with-output-to-string (out)
    (format out "Cron expression ~s, read the way a scheduler reads it~%" (expression-raw expression))
    (format out "  ~a~%" (summary expression))
    (unless (string= (expression-expr expression) (expression-raw expression))
      (format out "  same as ~a~%" (expression-expr expression)))
    (let ((label-width (loop for index below (field-count expression)
                             maximize (length (aref +field-names+ index))))
          (field-width (loop for piece in (words-of (expression-expr expression))
                             maximize (length piece))))
      (loop for index below (field-count expression)
            for piece in (words-of (expression-expr expression))
            do (format out "  ~vA  ~vA  ~a~%"
                       label-width (aref +field-names+ index)
                       field-width piece
                       (field-meaning expression index))))
    (let ((runs (next-runs expression clock count :now now)))
      (if (null runs)
          (format out "Next runs: none. No date matches this expression, so it never runs~%")
          (progn
            (format out "Next ~d run~:p, in ~a~%" (length runs) (clock-label clock))
            (dolist (run runs)
              (format out "  ~a~%" (run-text run now))))))
    (let ((notes (notes expression)))
      (when notes
        (format out "Notes~%")
        (dolist (note notes) (format out "  - ~a~%" note))))))

(defun run-text (run now)
  "One listed run: its date, its offset and how far off it is."
  (destructuring-bind (wall universal offset) run
    (multiple-value-bind (day month year weekday) (wall-parts wall)
      (multiple-value-bind (second minute hour) (wall-time-parts wall)
        (format nil "~a ~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d~a ~a  in ~a"
                (aref +weekday-labels+ weekday) year month day hour minute
                (if (plusp second) (format nil ":~2,'0d" second) "")
                (offset-label offset)
                (span-text (- universal now)))))))

(defun zone-directory (dir)
  "DIR as the tz database's root, or the machine's own when it is empty."
  (if (and dir (plusp (length dir))) dir +zoneinfo+))

(defun zone-name-p (name)
  "Whether NAME is shaped like an IANA zone name, and cannot walk out of the database."
  (and (stringp name)
       (plusp (length name))
       (<= (length name) 64)
       (not (search ".." name))
       (not (char= #\/ (char name 0)))
       (not (char= #\/ (char name (1- (length name)))))
       (every (lambda (char) (or (alphanumericp char) (find char "/_-+"))) name)))

(defun named-zone (name dir)
  "The ZONE NAME names under the tz database at DIR, or a refusal."
  (unless (zone-name-p name)
    (fail "~s is not a zone name; they look like Europe/Berlin or Asia/Tokyo, and an empty timezone reads this machine's own"
          name))
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname (zone-directory dir)))))
    (unless (probe-file path)
      (fail "there is no zone ~s in the tz database at ~a; an empty timezone reads this machine's own"
            name (zone-directory dir)))
    (read-zone path)))

(defun clock-for (name dir source now)
  "The CLOCK the reading is made in: the zone NAME under DIR when one is named,
this machine's own when it is not."
  (let ((name (nlk:trimmed (or name ""))))
    (if (zerop (length name))
        (make-clock nil (format nil "UTC~a (this machine's own zone; no timezone is set)"
                                (offset-label (machine-offset now))))
        (make-clock (named-zone name dir) (format nil "~a (~a)" name source)))))

(declaim (ftype (function (keyword) t) setting))

(defun reading-for (text &key zone source now)
  "The reading of TEXT in ZONE (else the timezone setting), or a refusal: the
one body the tool and /crontab both answer from."
  (let* ((now (or now (clock-now)))
         (expression (parse text))
         (clock (clock-for (or zone (setting :timezone)) (setting :zoneinfo-dir)
                            (or source "the crontab timezone setting") now)))
    (reading-text expression clock (setting :runs) :now now)))

;;; --- the tool the model calls -------------------------------------------------------

(nlk:define-contract crontab-request
  (:doc "Explain a cron expression: what it fires in plain words, field by field, and the runs it fires next in a zone. Use it instead of working a schedule out in your head -- 0 9 1 * 1 is not the first Monday, * 9 * * * runs sixty times, and */7 restarts at :00.")
  ("expression" :text :required t
                :doc "the cron expression: five fields (minute hour day-of-month month day-of-week), or a shortcut such as @daily")
  ("timezone" :text
              :doc "an IANA zone name to read the runs in, such as Asia/Tokyo; omitted, the cell's timezone setting, else this machine's own zone"))

(defun explain-cron (request)
  "The explain_cron tool. A refusal is answered, never signalled."
  (handler-case
      (let ((asked (nlk:trimmed (or (crontab-request-timezone request) ""))))
        (if (plusp (length asked))
            (reading-for (crontab-request-expression request)
                         :zone asked :source "the zone this call names")
            (reading-for (crontab-request-expression request))))
    (crontab-error (condition)
      (nle:failure (format nil "ERROR: ~a" (crontab-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- /crontab ----------------------------------------------------------------------

(defparameter +examples+ '("0 9 * * 1-5" "*/15 9-17 * * *" "0 9 1 * 1" "@daily")
  "The expressions /crontab shows when it is typed alone.")

(defun examples-text ()
  "What /crontab answers with no expression: the way in, and four readings of it."
  (with-output-to-string (out)
    (format out "Explain a cron expression: /crontab \"0 9 * * 1-5\"~%")
    (format out "An expression after /crontab is read in the crontab timezone setting; the explain_cron tool takes a timezone of its own.~%")
    (dolist (example +examples+)
      (handler-case (format out "  ~a  =>  ~a~%" example (summary (parse example)))
        (crontab-error () nil)))))

(defun run-crontab (args session-id)
  "/crontab [EXPRESSION] => what it fires, field by field, and its next runs."
  (declare (ignore session-id))
  (let ((asked (nlk:trimmed (or args ""))))
    (handler-case
        (if (zerop (length asked))
            (examples-text)
            (reading-for asked))
      (crontab-error (condition) (crontab-error-detail condition))
      (error (condition) (format nil "~a" condition)))))

;;; --- what the manual says ------------------------------------------------------------

(defparameter +manual+
  "A cron expression is short and easy to misread, so this cell reads one out:
what it fires in a sentence, each field on its own row, the runs it fires next
in a zone, and the notes people usually need -- that 0 9 1 * 1 runs on the 1st
and on every Monday (a day matching either day field runs), that * 9 * * * is
every minute of that hour, that */7 restarts at :00, that 30 and 31 skip the
months without them, and that 7 in day of week is Sunday.

The explain_cron tool takes the expression and an optional timezone; /crontab
takes the expression alone, and with nothing after it shows examples. Runs are
read in the crontab cell's timezone setting, or in this machine's own zone when
that is empty; a name such as Asia/Tokyo is looked up in the tz database at
zoneinfo_dir. Nothing is scheduled here: to actually run something on a
schedule, use the cron cell."
  "What (help :crontab) answers while the cell runs.")

;;; --- the cell -----------------------------------------------------------------------

(defun checked-settings (values table)
  "The section's values with the timezone and the run count checked, so a
mistake is a refusal at start rather than a wrong reading later."
  (declare (ignore table))
  (let ((zone (nlk:trimmed (or (getf values :timezone) "")))
        (dir (zone-directory (nlk:trimmed (or (getf values :zoneinfo-dir) ""))))
        (runs (getf values :runs)))
    (setf (getf values :timezone) zone
          (getf values :zoneinfo-dir) dir)
    (when (plusp (length zone))
      (unless (zone-name-p zone)
        (nlk:config-error "crontab.timezone is ~s; a zone name looks like Asia/Tokyo, and an empty timezone reads this machine's own"
                          zone))
      (let ((path (merge-pathnames zone (uiop:ensure-directory-pathname dir))))
        (unless (probe-file path)
          (nlk:config-error "crontab.timezone is ~s and the tz database at ~a has no such zone; name an IANA zone, or leave timezone empty for this machine's own"
                            zone dir))
        (handler-case (read-zone path)
          (error (condition)
            (nlk:config-error "crontab.timezone is ~s and its zone file could not be read: ~a"
                              zone condition)))))
    (unless (and (integerp runs) (<= 1 runs +max-runs+))
      (nlk:config-error "crontab.runs is ~s; a reading lists 1 to ~d next runs" runs +max-runs+))
    values))

(nle:define-cell crontab
  (:section ("crontab")
    (:guide "nothing here is required: an empty timezone reads this machine's own zone; name an IANA zone such as Asia/Tokyo to read runs there; runs is how many next runs a reading lists")
    ("timezone" :string :default ""
                :doc "an IANA zone name such as Asia/Tokyo; empty reads this machine's own zone")
    ("zoneinfo_dir" :string :default "/usr/share/zoneinfo"
                     :doc "where this machine's tz database lives, the folder zone names are looked up under")
    ("runs" :integer :default 5 :min 1
            :doc "how many next runs a reading lists, 1 to 20"))
  (:settings #'checked-settings)
  (:tool "explain_cron" (nlk:contract 'crontab-request) 'explain-cron)
  (:command "crontab" 'run-crontab
            :description "Explain a cron expression, with its next runs"
            :argument-hint "EXPRESSION")
  (:help :crontab
         "crontab: what a cron expression fires, field by field, with its next runs in a zone"
         +manual+))
