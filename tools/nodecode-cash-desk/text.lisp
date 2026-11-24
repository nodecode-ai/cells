;;;; text.lisp --- the ledger's arithmetic in text: money, dates, CSV.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Money never passes through a float: an amount is parsed into whole
;;;; cents (an integer) and printed back from cents, rounded half-up at the
;;;; third decimal. A date is a day number (days since 1970-01-01), so
;;;; "days overdue" is one subtraction and no clock or time zone is asked.
;;;; CSV is read by the one small parser here, without a dependency.

(in-package #:nodecode-cash-desk)

;;; --- small strings -------------------------------------------------------------

(defun trim-text (text)
  "TEXT without the whitespace around it; \"\" for NIL."
  (string-trim '(#\Space #\Tab #\Newline #\Return #\No-Break_Space) (or text "")))

(defun collapse-space (text)
  "TEXT with every run of whitespace as one space, trimmed."
  (let ((out (make-string-output-stream)) (space nil))
    (loop for char across (trim-text text)
          do (if (member char '(#\Space #\Tab #\Newline #\Return #\No-Break_Space))
                 (setf space t)
                 (progn (when (and space (plusp (file-position out))) (write-char #\Space out))
                        (setf space nil)
                        (write-char char out))))
    (get-output-stream-string out)))

(defun split-text (text separator)
  "TEXT split on SEPARATOR, empty fields kept."
  (let ((fields '()) (start 0))
    (loop for at = (position separator text :start start)
          do (push (subseq text start at) fields)
             (if at (setf start (1+ at)) (return (nreverse fields))))))

(defun split-words (text)
  "TEXT's fields on whitespace, empties dropped."
  (remove-if #'zerop
             (loop for field in (uiop:split-string (or text "")
                                                   :separator '(#\Space #\Tab #\Newline #\Return))
                   collect (trim-text field))
             :key #'length))

(defun join-text (separator strings)
  "STRINGS joined with SEPARATOR."
  (with-output-to-string (out)
    (loop for string in strings
          for first = t then nil
          do (unless first (write-string separator out))
             (write-string string out))))

(defun string-replace (old new text)
  "TEXT with every OLD replaced by NEW."
  (if (or (zerop (length old)) (null (search old text)))
      text
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search old text :start2 start)
              do (if at
                     (progn (write-string text out :start start :end at)
                            (write-string new out)
                            (setf start (+ at (length old))))
                     (progn (write-string text out :start start)
                            (return)))))))

(defun prefix-p (prefix text)
  "Whether TEXT starts with PREFIX."
  (and (<= (length prefix) (length text)) (string= prefix text :end2 (length prefix))))

;;; --- money ---------------------------------------------------------------------

(defparameter +currency-codes+
  '("EUR" "USD" "GBP" "CZK" "PLN" "CHF" "SEK" "NOK" "DKK" "CAD" "AUD" "JPY"
    "NZD" "HUF" "RON" "BGN" "TRY" "ZAR" "SGD" "HKD" "MXN" "BRL" "INR" "CNY")
  "The currencies a file may name; an amount is never converted.")

(defparameter +currency-symbols+
  '(("€" . "EUR") ("£" . "GBP") ("$" . "USD") ("Kč" . "CZK") ("zł" . "PLN"))
  "Symbols read as their code before the amount is parsed.")

(defun space-char-p (char)
  (member (char-code char) '(32 9 10 13 160 8201 8202 8239) :test #'=))

(defun normalise-spaces (text)
  "TEXT with every kind of space as a plain one."
  (map 'string (lambda (char) (if (space-char-p char) #\Space char)) (or text "")))

(defun strip-currency (text)
  "TEXT without its currency, and the code it named (or NIL). A text naming
two different currencies is refused."
  (let ((text (normalise-spaces text)) (found '()))
    (loop for (symbol . code) in +currency-symbols+
          do (loop for at = (search symbol text)
                   while at
                   do (pushnew code found :test #'string=)
                      (setf text (concatenate 'string (subseq text 0 at) " "
                                              code " " (subseq text (+ at (length symbol)))))))
    (let ((up (string-upcase text)))
      (loop for code in +currency-codes+
            do (let ((at (search code up)))
                 (when (and at
                            (or (zerop at) (not (alpha-char-p (char up (1- at)))))
                            (or (= (+ at 3) (length up)) (not (alpha-char-p (char up (+ at 3))))))
                   (pushnew code found :test #'string=)
                   (setf text (concatenate 'string (subseq text 0 at) " " (subseq text (+ at 3)))
                         up (string-upcase text)))))
      (when (> (length found) 1)
        (fail "~s names more than one currency: ~{~a~^, ~}" text found))
      (values text (first found)))))

(defun digits-only (text)
  "TEXT without separators, or NIL when a letter it does not know is in it."
  (remove-if (lambda (char) (member char '(#\Space #\' #\, #\.))) text))

(defun parse-amount (text &key (lang "en") (strict t) (what "an amount"))
  "TEXT as whole cents, and the currency it named. STRICT refuses a third
non-zero decimal instead of rounding it, which is what an import wants."
  (multiple-value-bind (body currency) (strip-currency text)
    (let ((body (trim-text (normalise-spaces body)))
          (negative nil))
      (when (zerop (length body))
        (fail "~a is missing" what))
      (when (and (char= (char body 0) #\() (char= (char body (1- (length body))) #\)))
        (setf negative t
              body (subseq body 1 (1- (length body)))))
      (let ((minus (count #\- body)))
        (when (oddp minus) (setf negative (not negative)))
        (setf body (remove #\- body)))
      (loop for char across body
            unless (or (digit-char-p char) (member char (list #\. #\, (code-char 39) #\Space)))
              do (fail "~s is not a number" text))
      (let* ((clean (remove-if (lambda (c) (or (eql 39 (char-code c)) (char= c #\Space))) body))
             (dots (count #\. clean))
             (commas (count #\, clean))
             (decimal nil))
        (cond
          ;; both separators: the last one is the decimal point
          ((and (plusp dots) (plusp commas))
           (setf decimal (if (> (position #\. clean) (position #\, clean)) #\. #\,))
           (setf clean (remove (if (char= decimal #\.) #\, #\.) clean)))
          ;; one kind, repeated: thousands
          ((> dots 1) (setf decimal nil))
          ((> commas 1) (setf decimal nil))
          ;; one separator: thousands only when it is the other language's
          ;; thousands mark and the tail is a three-digit group
          ((= dots 1)
           (let ((head (subseq clean 0 (position #\. clean)))
                 (tail (subseq clean (1+ (position #\. clean)))))
             (setf decimal (if (and (= 3 (length tail)) (<= 1 (length head) 3)
                                     (not (prefix-p "0" head)) (string= lang "cs"))
                               nil #\.))))
          ((= commas 1)
           (let ((head (subseq clean 0 (position #\, clean)))
                 (tail (subseq clean (1+ (position #\, clean)))))
             (setf decimal (if (and (= 3 (length tail)) (<= 1 (length head) 3)
                                     (not (prefix-p "0" head)) (string= lang "en"))
                               nil #\,)))))
        (let* ((normalised
                 (cond ((null decimal) (remove-if (lambda (c) (member c '(#\. #\,))) clean))
                       (t (let ((parts (split-text clean decimal)))
                            (if (= 2 (length parts))
                                (concatenate 'string (first parts) "." (second parts))
                                (fail "~s is not a number" text))))))
               (point (position #\. normalised))
               (whole (if point (subseq normalised 0 point) normalised))
               (fraction (if point (subseq normalised (1+ point)) "")))
          (when (zerop (length whole)) (setf whole "0"))
          (when (plusp (length (remove-if #'digit-char-p whole)))
            (fail "~s is not a number" text))
          (when (> (length fraction) 2)
            (let ((extra (subseq fraction 2)))
              (when (and strict (not (every (lambda (c) (char= c (code-char 48))) extra)))
                (fail "~s carries more than two decimals" text))
              (when (and (not strict) (plusp (length extra)) (digit-char-p (char extra 0))
                         (>= (digit-char-p (char extra 0)) 5))
                (setf fraction (format nil "~2,'0d"
                                       (1+ (parse-integer (subseq fraction 0 2))))))))
          (let* ((cent (if (plusp (length fraction))
                           (parse-integer (subseq (concatenate 'string fraction "00") 0 2))
                           0))
                 (value (+ (* (parse-integer whole) 100) cent)))
            (values (if negative (- value) value) currency)))))))

(defun format-cents (cents currency &key (lang "en"))
  "CENTS as a person reads it: EUR 1,250.00 in English, 1 250,00 Kč in Czech."
  (let* ((negative (minusp cents))
         (value (abs cents))
         (whole (floor value 100))
         (fraction (mod value 100))
         (digits (princ-to-string whole))
         (grouped (with-output-to-string (out)
                    (loop for index from 0 below (length digits)
                          do (when (and (plusp index) (zerop (mod (- (length digits) index) 3)))
                               (write-char (if (string= lang "cs") #\Space #\,) out))
                             (write-char (char digits index) out))))
         (number (format nil "~a~a~a~2,'0d"
                         (if negative "-" "") grouped
                         (if (string= lang "cs") "," ".") fraction))
         (currency (or currency "")))
    (cond ((string= lang "cs")
           (format nil "~a~@[ ~a~]" number
                   (cond ((zerop (length currency)) nil)
                         ((string= currency "CZK") "Kč")
                         (t currency))))
          (t (format nil "~@[~a ~]~a" (and (plusp (length currency)) currency) number)))))

(defun cents-of (record key)
  "RECORD's KEY as cents, 0 when absent."
  (or (getf record key) 0))

;;; --- dates ---------------------------------------------------------------------

(defun days-from-civil (year month day)
  "The day number of YEAR-MONTH-DAY, days since 1970-01-01."
  (let* ((year (if (<= month 2) (1- year) year))
         (era (floor year 400))
         (yoe (- year (* era 400)))
         (doy (+ (floor (+ (* 153 (+ month (if (> month 2) -3 9))) 2) 5) (1- day)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (- (+ (* era 146097) doe) 719468)))

(defun civil-from-days (days)
  "The (VALUES YEAR MONTH DAY) DAYS names."
  (let* ((z (+ days 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (+ (- doe (floor doe 1460)) (floor doe 36524) (- (floor doe 146096))) 365))
         (year (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (day (+ (- doy (floor (+ (* 153 mp) 2) 5)) 1))
         (month (+ mp (if (< mp 10) 3 -9))))
    (values (if (<= month 2) (1+ year) year) month day)))

(defparameter +months+
  '(("january" . 1) ("february" . 2) ("march" . 3) ("april" . 4) ("may" . 5)
    ("june" . 6) ("july" . 7) ("august" . 8) ("september" . 9) ("october" . 10)
    ("november" . 11) ("december" . 12)
    ("ledna" . 1) ("února" . 2) ("brezna" . 3) ("dubna" . 4) ("května" . 5)
    ("června" . 6) ("července" . 7) ("srpna" . 8) ("září" . 9) ("října" . 10)
    ("listopadu" . 11) ("prosince" . 12))
  "Month names an export may spell, English and Czech.")

(defun month-number (word)
  (cdr (assoc (string-downcase (trim-text word)) +months+ :test #'string=)))

(defun valid-date-p (year month day)
  (and (integerp year) (integerp month) (integerp day)
       (<= 1 month 12) (<= 1 day 31) (<= 1 year 9999)
       (<= day (if (member month '(1 3 5 7 8 10 12)) 31
                   (if (= month 2)
                       (if (or (and (zerop (mod year 4)) (not (zerop (mod year 100))))
                               (zerop (mod year 400)))
                           29 28)
                       30)))))

(defun number-text (text)
  "TEXT as an integer, or NIL."
  (let ((trimmed (trim-text text)))
    (and (plusp (length trimmed))
         (every #'digit-char-p trimmed)
         (parse-integer trimmed))))

(defun parse-date (text &key (order "auto") (lang "en"))
  "TEXT as a day number, or NIL. ORDER is auto, dmy or mdy; AUTO reads a
dotted date day-first and a slashed one by whichever number is above 12."
  (let* ((text (trim-text (normalise-spaces text))))
    (when (zerop (length text)) (return-from parse-date nil))
    ;; an ISO stamp keeps its date and drops its time
    (when (> (length text) 10)
      (let ((space (position-if (lambda (c) (member c '(#\T #\Space))) text :start 10)))
        (when space (setf text (subseq text 0 space)))))
    (labels ((build (y m d)
               (when (valid-date-p y m d) (days-from-civil y m d)))
             (numbered (parts projection)
               (let ((a (number-text (first parts))) (b (number-text (second parts)))
                     (c (number-text (third parts))))
                 (when (and a b c)
                   (ecase projection
                     (:ymd (build a b c))
                     (:dmy (build c b a))
                     (:mdy (build c a b)))))))
      (cond
        ;; ISO
        ((and (>= (length text) 10)
              (digit-char-p (char text 0)) (digit-char-p (char text 1))
              (digit-char-p (char text 2)) (digit-char-p (char text 3))
              (char= (char text 4) #\-) (char= (char text 7) #\-))
         (build (parse-integer text :start 0 :end 4)
                (parse-integer text :start 5 :end 7)
                (parse-integer text :start 8 :end 10)))
        ;; dotted: always day first
        ((find #\. text)
         (let ((parts (remove-if #'zerop (split-text text #\.) :key #'length)))
           (when (= 3 (length parts))
             (let ((d (number-text (first parts))) (m (number-text (second parts)))
                   (y (number-text (third parts))))
               (when (and y (< y 100)) (setf y (+ y 2000)))
               (build y m d)))))
        ;; slashed or dashed
        ((or (find #\/ text) (find #\- text))
         (let* ((separator (if (find #\/ text) #\/ #\-))
                (parts (split-text text separator)))
           (when (= 3 (length parts))
             (let ((a (number-text (first parts))) (b (number-text (second parts)))
                   (c (number-text (third parts))))
               (when (and a b c)
                 (cond ((string= order "dmy") (build c b a))
                       ((string= order "mdy") (build c a b))
                       ((> a 12) (build c b a))
                       ((> b 12) (build c a b))
                       ((string= lang "cs") (build c b a))
                       (t (build c a b))))))))
        (t
         ;; 5 November 2026, or November 5, 2026
         (let* ((words (mapcar (lambda (w) (string-right-trim ",." w)) (split-words text)))
                (month-at (position-if #'month-number words)))
           (cond
             ((null month-at) nil)
             ((= month-at 0)
              (let ((month (month-number (first words)))
                    (day (number-text (second words)))
                    (year (number-text (third words))))
                (build year month day)))
             (t
              (let ((day (number-text (first words)))
                    (month (month-number (second words)))
                    (year (number-text (third words))))
                (build year month day))))))))))

(defun iso-date (days)
  "DAYS as YYYY-MM-DD."
  (multiple-value-bind (year month day) (civil-from-days days)
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun days-today ()
  "Today's day number, off the local clock."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (days-from-civil year month day)))

(defun date-text-p (text)
  "Whether TEXT reads as a date at all, under any order."
  (and (parse-date text :order "dmy")
       (or (find #\. text) (find #\/ text) (find #\- text)
           (some #'month-number (split-words text)))))

;;; --- CSV -----------------------------------------------------------------------

(defun detect-delimiter (line)
  "The delimiter LINE carries most of: comma, semicolon or tab."
  (let ((comma (count #\, line)) (semicolon (count #\; line)) (tab (count #\Tab line)))
    (cond ((and (>= semicolon comma) (>= semicolon tab)) #\;)
          ((>= tab comma) #\Tab)
          (t #\,))))

(defun parse-delimited (text delimiter)
  "TEXT as rows of fields, quotes and doubled quotes honoured."
  (let ((rows '()) (fields '()) (field (make-string-output-stream))
        (quoted nil) (index 0) (length (length text)))
    (labels ((end-field () (push (get-output-stream-string field) fields))
             (end-row ()
               (end-field)
               (push (nreverse fields) rows)
               (setf fields '())))
      (loop while (< index length)
            for char = (char text index)
            do (cond
                 (quoted
                  (cond ((char= char #\") 
                         (if (and (< (1+ index) length) (char= (char text (1+ index)) #\"))
                             (progn (write-char #\" field) (incf index))
                             (setf quoted nil)))
                        (t (write-char char field))))
                 ((char= char #\")
                  (setf quoted t))
                 ((char= char delimiter) (end-field))
                 ((char= char #\Newline) (end-row))
                 ((char= char #\Return) nil)
                 (t (write-char char field)))
               (incf index))
      (when (or fields (plusp (file-position field)))
        (end-row)))
    (nreverse rows)))

(defun read-table (text required-groups)
  "TEXT as (VALUES HEADER ROWS): the first row inside the first 40 that
carries every group of aliases in REQUIRED-GROUPS, then the rows after it.
Refuses when no header is found."
  (let ((lines (parse-delimited (string-left-trim '(#\Return #\Newline #\Space) text)
                                 (detect-delimiter
                                  (or (first (remove-if (lambda (l) (zerop (length (trim-text l))))
                                                        (split-text text #\Newline)))
                                      ""))))
        (found nil))
    (loop for row in (subseq lines 0 (min 40 (length lines)))
          for header = (mapcar #'header-key row)
          when (every (lambda (group) (find-column header group)) required-groups)
            do (setf found row)
               (return))
    (unless found
      (fail "no header row found in the first 40 lines: one carrying ~{«~{~a~^|~}»~^ and ~}"
            required-groups))
    (let ((header (mapcar #'header-key found))
          (rest (rest (member found lines :test #'equal))))
      (values header (remove-if (lambda (row) (every (lambda (cell) (zerop (length (trim-text cell)))) row))
                                rest)))))

(defun header-key (cell)
  "CELL as a column name: lowercased, trimmed, underscores read as spaces."
  (let ((text (string-downcase (collapse-space (string-left-trim '(#\Return) cell)))))
    (string-right-trim ":" (substitute #\Space #\_ text))))

(defun find-column (header aliases)
  "The index of the first of ALIASES HEADER carries, or NIL."
  (loop for alias in aliases
        for at = (position alias header :test #'string=)
        when at return at))

(defun cell (row at)
  "ROW's field AT, or \"\"."
  (if (and at (< at (length row))) (trim-text (nth at row)) ""))

(defun required-column (header aliases what)
  "The index of the ALIASES column, or a refusal naming WHAT."
  (or (find-column header aliases)
      (fail "the ~a column is missing: one of ~{~a~^, ~}" what aliases)))
