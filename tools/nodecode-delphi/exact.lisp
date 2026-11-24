;;;; exact.lisp --- decimal text in, arithmetic, decimal text out; and fees.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original did this with Python's Decimal and Fraction: a user
;;;; probability is decimal text from 0 to 1 with at most 18 fraction digits
;;;; (no exponent, no sign, no spaces), an edge is their exact difference,
;;;; and the difference is written back as plain decimal text, never a float.
;;;; In Lisp a rational IS exact, so 0.62 - 0.45 is 17/100 and the text it
;;;; answers is "0.17". Nothing here touches the network.

(in-package #:nodecode-delphi)

(defparameter +probability-digits+ 18
  "How many fraction digits a probability or a share amount may carry.")

(defun digits-only-p (text &key (allow-empty nil) (leading-zero t))
  "Whether TEXT is ASCII digits: one or more, or none when ALLOW-EMPTY;
with LEADING-ZERO, a multi-digit run may not start with 0."
  (and (stringp text)
       (or allow-empty (plusp (length text)))
       (every (lambda (character) (char<= #\0 character #\9)) text)
       (or (not leading-zero) (<= (length text) 1) (not (char= #\0 (char text 0))))))

(defun decimal-fraction (text)
  "TEXT, \"0.62\" or \"1\" or \"0.000\", as an exact rational, or NIL when it
is not plain decimal text of at most 18 fraction digits with no sign."
  (when (stringp text)
    (let* ((dot (position #\. text))
           (whole (if dot (subseq text 0 dot) text))
           (fraction (if dot (subseq text (1+ dot)) "")))
      (when (and (digits-only-p whole)
                 (or (null dot) (digits-only-p fraction :allow-empty t))
                 (<= (length fraction) +probability-digits+)
                 (or (null dot) (plusp (length fraction))))
        (let ((units (parse-integer (concatenate 'string whole fraction))))
          (/ units (expt 10 (length fraction))))))))

(defun probability (text what)
  "TEXT as a probability in [0, 1], or a refusal naming WHAT."
  (let ((value (decimal-fraction text)))
    (unless value
      (fail "~a must be decimal text from 0 to 1 with at most ~d fraction digits, and no sign or exponent; got ~s"
            what +probability-digits+ text))
    (unless (<= 0 value 1)
      (fail "~a must be from 0 to 1; got ~s" what text))
    value))

(defun tens-powers (denominator)
  "DENOMINATOR as (values TWOS FIVES REST): how many 2s and 5s it carries,
and what is left once they are taken out."
  (let ((twos 0) (fives 0))
    (loop while (zerop (mod denominator 2))
          do (setf denominator (/ denominator 2)) (incf twos))
    (loop while (zerop (mod denominator 5))
          do (setf denominator (/ denominator 5)) (incf fives))
    (values twos fives denominator)))

(defun decimal-text (value &key (places +probability-digits+) (ceiling nil))
  "VALUE, a rational, as plain decimal text: exact when its denominator is
only 2s and 5s, else cut (or rounded up with CEILING) to PLACES digits."
  (when (zerop value) (return-from decimal-text "0"))
  (let* ((sign (if (minusp value) "-" ""))
         (magnitude (abs value))
         (denominator (denominator magnitude))
         (numerator (numerator magnitude)))
    (multiple-value-bind (twos fives rest) (tens-powers denominator)
      (if (= rest 1)
          (let* ((scale (max twos fives))
                 (scaled (* numerator
                             (expt 2 (- scale twos))
                             (expt 5 (- scale fives))))
                 (digits (princ-to-string scaled)))
            (if (zerop scale)
                (concatenate 'string sign digits)
                (let* ((padded (concatenate 'string (make-string (max 0 (- (1+ scale) (length digits)))
                                                                 :initial-element #\0)
                                             digits))
                       (whole (subseq padded 0 (- (length padded) scale)))
                       (fraction (string-right-trim "0" (subseq padded (- (length padded) scale)))))
                  (format nil "~a~a~@[.~a~]" sign whole (and (plusp (length fraction)) fraction)))))
          (let* ((scale places)
                 (scaled (* magnitude (expt 10 scale)))
                 (whole (floor scaled))
                 (up (and ceiling (/= scaled whole))))
            (when up (incf whole))
            (let* ((digits (princ-to-string whole))
                   (padded (concatenate 'string (make-string (max 0 (- (1+ scale) (length digits)))
                                                             :initial-element #\0)
                                        digits))
                   (text (format nil "~a~a.~a" sign
                                 (subseq padded 0 (- (length padded) scale))
                                 (string-right-trim "0" (subseq padded (- (length padded) scale))))))
              (if (or (string= text "-0.") (string= text "0.") (string= text "-0"))
                  "0"
                  (string-right-trim "." text))))))))

(defun fee-percent (raw)
  "The trading fee RAW, an integer string where 10^18 is 100%, as percent text."
  (when (and (stringp raw) (plusp (length raw)) (every #'digit-char-p raw))
    (decimal-text (/ (* (parse-integer raw) 100) (expt 10 18)))))

(defun shares-decimal (raw)
  "A raw share count, 10^18 units to a share, as decimal text."
  (when (and (stringp raw) (plusp (length raw)) (every #'digit-char-p raw))
    (decimal-text (/ (parse-integer raw) (expt 10 18)))))

(defun share-fraction (text)
  "TEXT as a share amount: decimal text greater than 0 with at most 18
fraction digits, as an exact rational."
  (let ((value (decimal-fraction text)))
    (unless (and value (plusp value))
      (fail "shares must be decimal text greater than 0 with at most ~d fraction digits; got ~s"
            +probability-digits+ text))
    value))

(defun raw-shares (value)
  "VALUE, shares as a rational, in raw units of 10^18 to a share."
  (* value (expt 10 +probability-digits+)))

(defun ratio-text (value &key (places +probability-digits+) ceiling)
  "VALUE, a rational, as plain decimal text cut (or rounded up with CEILING)
to PLACES; => (values TEXT TRUNCATED-P), TRUNCATED-P when the text is not
the whole value."
  (let ((text (decimal-text value :places places :ceiling ceiling)))
    (values text (not (eql (decimal-fraction text) value)))))

(defun human-amount (raw decimals)
  "RAW collateral units shown in units of 10^DECIMALS, as almost-plain
decimal text, or NIL when that is not a plain decimal."
  (and (integerp raw) (integerp decimals) (<= 0 decimals 30)
       (let ((text (decimal-text (/ raw (expt 10 decimals)))))
         (and (decimal-fraction text) text))))

