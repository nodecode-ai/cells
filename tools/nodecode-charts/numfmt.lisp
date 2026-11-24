;;;; numfmt.lisp --- reading numbers the way people type them, and writing them back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's numfmt.py. Reading accepts what turns up in
;;;; Japanese spreadsheets and statistics tables: full-width digits, thousands
;;;; separators, the 万/億/兆/千 multipliers, a leading triangle for a
;;;; negative, accounting parentheses, and the "no value" marks a statistics
;;;; table uses. Writing uses the same conventions in reverse, so a label the
;;;; renderer drew can always be read back by PARSE-NUMBER - which is what the
;;;; verifier relies on.

(in-package #:nodecode-charts)

;;; --- conditions -----------------------------------------------------------------

(define-condition chart-error (error)
  ((code :initarg :code :initform "bad_args" :reader chart-error-code)
   (message :initarg :message :initform "" :reader chart-error-message)
   (where :initarg :where :initform "" :reader chart-error-where)
   (fix :initarg :fix :initform "" :reader chart-error-fix))
  (:report (lambda (condition stream)
             (format stream "~a: ~a~@[ (~a)~]~@[ Fix: ~a~]"
                     (chart-error-code condition) (chart-error-message condition)
                     (let ((where (chart-error-where condition)))
                       (and (plusp (length where)) where))
                     (let ((fix (chart-error-fix condition)))
                       (and (plusp (length fix)) fix))))))

(defun refuse-chart (code message &key where fix)
  "Signal the one refusal the tools report, with the place at fault and the
next step, so a model can repair the call instead of guessing."
  (error 'chart-error :code code :message message :where (or where "") :fix (or fix "")))

(define-condition not-a-number (error)
  ((text :initarg :text :initform "" :reader not-a-number-text))
  (:report (lambda (condition stream)
             (format stream "~s is not a number" (not-a-number-text condition)))))

;;; --- reading --------------------------------------------------------------------

(defparameter +missing-marks+
  '("" "-" "--" "−" "–" "—" "―" "…" "..." "‥" "x" "X" "*" "**" "***"
    "na" "n/a" "nan" "null" "none" "#n/a" "#value!" "#div/0!")
  "The marks a statistics table uses for \"no value\". A cell holding one is
drawn as no bar, never as zero.")

(defparameter +multipliers+ '(("兆" . 1d12) ("億" . 1d8) ("万" . 1d4) ("千" . 1d3))
  "The Japanese multipliers, largest first, as the original's table has them.")

(defun normalize-text (text)
  "TEXT with the full-width forms folded to ASCII and the dash family to '-':
what a spreadsheet's copy of a number looks like before it is read."
  (let ((out (make-string-output-stream)))
    (loop for character across (string text)
          for code = (char-code character)
          do (cond ((<= #xFF10 code #xFF19) (write-char (code-char (+ (- code #xFF10) (char-code #\0))) out))
                   ((<= #x30 (char-code character) #x39) (write-char character out))
                   ((member character '(#\．) :test #'char=) (write-char #\. out))
                   ((member character '(#\，) :test #'char=) (write-char #\, out))
                   ((member character '(#\％) :test #'char=) (write-char #\% out))
                   ((member character '(#\＋) :test #'char=) (write-char #\+ out))
                   ((member character '(#\− #\– #\— #\―) :test #'char=) (write-char #\- out))
                   ((char= character #\　) (write-char #\Space out))
                   (t (write-char character out))))
    (string-trim '(#\Space #\Tab #\Return #\Newline) (get-output-stream-string out))))

(defun numeric-literal (text)
  "TEXT as a number when it is one plain literal (digits, one point, an
exponent, an optional sign), else NIL."
  (when (and (plusp (length text))
             (loop for character across text
                   always (or (digit-char-p character)
                              (find character "+-.eE" :test #'char=))))
    (handler-case
        (multiple-value-bind (value position)
            (let ((*read-default-float-format* 'double-float))
              (read-from-string text))
          (when (and (realp value) (= position (length text)))
            (coerce value 'double-float)))
      (error () nil))))

(defun parse-with-multipliers (text)
  "TEXT as a number, reading 1億2000万 and 3.5万; NIL when it is not one."
  (when (plusp (length text))
    (let ((rest text) (total 0d0) (matched nil) (sign 1d0))
      (when (find (char rest 0) "+-" :test #'char=)
        (when (char= (char rest 0) #\-) (setf sign -1d0))
        (setf rest (subseq rest 1)))
      (dolist (entry +multipliers+)
        (let ((at (position (char (car entry) 0) rest)))
          (when at
            (let ((head (subseq rest 0 at)))
              (unless (numeric-literal head) (return-from parse-with-multipliers nil))
              (incf total (* (numeric-literal head) (cdr entry)))
              (setf rest (subseq rest (1+ at))
                    matched t)))))
      (cond ((zerop (length rest)) (and matched (* sign total)))
            ((numeric-literal rest) (* sign (+ total (numeric-literal rest))))
            (t nil)))))

(defun parse-number (value &optional unit)
  "VALUE as (VALUES NUMBER PERCENT-P). NUMBER is NIL for the marks a table
uses for no value. UNIT is stripped from the end first, so 1,200万円 with
unit 万円 is 1200 and with unit 円 is 12000000. Signals NOT-A-NUMBER when the
text is present but is not a number."
  (cond ((null value) (values nil nil))
        ((numberp value) (values (coerce value 'double-float) nil))
        ((not (stringp value)) (error 'not-a-number :text (princ-to-string value)))
        (t
         (let ((text (normalize-text value)))
           (when (member (string-downcase text) +missing-marks+ :test #'string=)
             (return-from parse-number (values nil nil)))
           (setf text (remove-if (lambda (c) (find c ", _　" :test #'char=)) text))
           (let ((u (and unit (normalize-text unit))))
             (when (and u (plusp (length u)) (> (length text) (length u))
                        (string-suffix-p u text))
               (setf text (subseq text 0 (- (length text) (length u))))))
           (let ((negative nil))
             (when (and (plusp (length text)) (find (char text 0) "△▲" :test #'char=))
               (setf negative t text (subseq text 1)))
             (when (and (> (length text) 1) (char= (char text 0) #\() (char= (char text (1- (length text))) #\)))
               (setf negative t text (subseq text 1 (1- (length text)))))
             (let ((percent nil))
               (when (and (plusp (length text)) (char= (char text (1- (length text))) #\%))
                 (setf percent t text (subseq text 0 (1- (length text)))))
               (let ((total (or (parse-with-multipliers text)
                                (let ((trimmed (string-right-trim "人人円件個所台度年月日月分秒%" text)))
                                  (and (plusp (length trimmed))
                                       (not (string= trimmed text))
                                       (parse-with-multipliers trimmed))))))
                 (unless total
                   (error 'not-a-number :text value))
                 (values (if negative (- total) total) percent))))))))

(defun string-suffix-p (suffix text)
  (let ((n (length suffix)))
    (and (<= n (length text)) (string= suffix text :start2 (- (length text) n)))))

(defun string-prefix-p (prefix text)
  (let ((n (length prefix)))
    (and (<= n (length text)) (string= prefix text :end2 n))))

;;; --- writing --------------------------------------------------------------------

(defun round-half-up (number &optional (decimals 0))
  "Round the way a report does (12.5 -> 13), not the way banker's rounding does."
  (let* ((scale (expt 10 decimals))
         (x (* (abs (coerce number 'double-float)) scale))
         (whole (floor (+ x 0.5d0))))
    (let ((rounded (coerce (/ whole scale) 'double-float)))
      (if (minusp number) (- rounded) rounded))))

(defun group-number (number &optional (decimals 0))
  "NUMBER with thousands separators and DECIMALS places, '-' for a minus."
  (let* ((rounded (round-half-up number decimals))
         (text (format nil (format nil "~~,~df" decimals) (abs rounded)))
         (dot (position #\. text))
         (whole (if dot (subseq text 0 dot) text))
         (fraction (if dot (subseq text dot) ""))
         (out (make-string-output-stream)))
    (loop for character across whole
          for index from 0
          do (when (and (plusp index) (zerop (mod (- (length whole) index) 3)))
               (write-char #\, out))
             (write-char character out))
    (write-string fraction out)
    (concatenate 'string (if (and (minusp rounded) (not (zerop rounded))) "-" "") (get-output-stream-string out))))

(defun unit-has-multiplier-p (unit)
  (some (lambda (c) (find c "千万億兆" :test #'char=)) (string unit)))

(defun choose-style (values unit number-format locale)
  "\"plain\", \"ja\" (万/億) or \"en\" (K/M) - the original's own choice of
what a chart's numbers are written as."
  (cond ((string-equal number-format "plain") "plain")
        ((string-equal number-format "compact") (if (string-equal locale "ja") "ja" "en"))
        (t (let ((peak (loop for v in values maximize (abs (coerce v 'double-float)))))
             (cond ((or (unit-has-multiplier-p unit) (string= (string-trim " " unit) "%") (< peak 1d5))
                    "plain")
                   ((string-equal locale "ja") "ja")
                   (t "en"))))))

(defun decimals-for (values &optional (cap 2))
  "The fewest decimals that write every value as it was given, at most CAP."
  (let ((best 0))
    (dolist (v values best)
      (let ((found nil))
        (loop for d from 0 to cap
              do (when (< (abs (- (round-half-up v d) (coerce v 'double-float)))
                          (* 1d-9 (max 1d0 (abs (coerce v 'double-float)))))
                   (setf best (max best d) found t)
                   (return)))
        (unless found (setf best cap))))))

(defun scale-for (peak style)
  "The multiplier one chart's numbers share, and its mark."
  (if (string-equal style "ja")
      (cond ((>= peak 1d12) (values 1d12 "兆"))
            ((>= peak 1d8) (values 1d8 "億"))
            ((>= peak 1d4) (values 1d4 "万"))
            (t (values 1d0 "")))
      (cond ((>= peak 1d9) (values 1d9 "B"))
            ((>= peak 1d6) (values 1d6 "M"))
            ((>= peak 1d3) (values 1d3 "K"))
            (t (values 1d0 "")))))

(defun format-value (value style &optional (decimals 0))
  "One data label in STYLE; a compact style keeps three significant digits."
  (if (string-equal style "plain")
      (group-number value decimals)
      (multiple-value-bind (scale mark) (scale-for (abs (coerce value 'double-float)) style)
        (if (= scale 1d0)
            (group-number value decimals)
            (let* ((scaled (/ (coerce value 'double-float) scale))
                   (d (cond ((>= (abs scaled) 100) 0) ((>= (abs scaled) 10) 1) (t 2)))
                   (text (string-right-trim "." (string-right-trim "0" (group-number scaled d)))))
              (if (string= text "0") "0" (concatenate 'string text mark)))))))

(defun format-values (values style &optional (decimals 0))
  "Data labels for one chart, with one shared scale so the numbers line up."
  (if (string-equal style "plain")
      (mapcar (lambda (v) (group-number v decimals)) values)
      (let ((peak (loop for v in values maximize (abs (coerce v 'double-float)))))
        (multiple-value-bind (scale mark) (scale-for peak style)
          (if (= scale 1d0)
              (mapcar (lambda (v) (group-number v decimals)) values)
              (let* ((top (/ peak scale))
                     (d (cond ((>= top 100) 0) ((>= top 10) 1) (t 2))))
                (mapcar (lambda (v)
                          (let ((text (string-right-trim "." (string-right-trim "0" (group-number (/ v scale) d)))))
                            (if (string= text "0") "0" (concatenate 'string text mark))))
                        values)))))))

(defun parse-label (label)
  "Read back a label this module wrote, K/M/B included; NIL when it cannot."
  (handler-case
      (let ((text (remove #\% (remove #\, (normalize-text label)))))
        (loop for (mark . factor) in '(("B" . 1d9) ("M" . 1d6) ("K" . 1d3))
              when (string-suffix-p mark text)
                do (let ((head (numeric-literal (subseq text 0 (- (length text) (length mark))))))
                     (return-from parse-label (and head (* head factor)))))
        (multiple-value-bind (number percent) (parse-number text)
          (declare (ignore percent))
          number))
    (error () nil)))
