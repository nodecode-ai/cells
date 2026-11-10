;;;; vat.lisp --- the money and the rules, with nothing of the cell in them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin's two handlers (__init__.py: _handle_vat_calculate
;;;; and _handle_zatca_validate) were arithmetic and regular expressions over
;;;; the call's own fields, and so are these: no network, no file, no
;;;; environment, no clock. Every amount is carried as whole halalas (SAR
;;;; cents), so no binary float decides a total, and a half halala rounds up
;;;; the way an invoice is written - the original rounded a Python float with
;;;; round(), which rounds half to even, and the two differ only at an exact
;;;; half halala.

(in-package #:nodecode-zatca)

;;; --- money ------------------------------------------------------------------

(defun halalas (amount)
  "AMOUNT, a JSON number in SAR, as whole halalas, rounded half up."
  (floor (+ (* (rational amount) 100) 1/2)))

(defun money (halalas)
  "HALALAS written the way money is: 2,875.00."
  (multiple-value-bind (whole part) (floor (abs halalas) 100)
    (format nil "~@[~a~]~:d.~2,'0d" (and (minusp halalas) "-") whole part)))

(defun money-sar (halalas)
  "HALALAS with the currency it is in: SAR 2,875.00."
  (format nil "SAR ~a" (money halalas)))

(defun vat-breakdown (amount rate-percent inclusive-p)
  "AMOUNT in halalas split at RATE-PERCENT: (VALUES TAXABLE VAT TOTAL), all
halalas, with the tax rounded half up. INCLUSIVE-P says AMOUNT already
carries the tax."
  (if inclusive-p
      (let* ((total amount)
             (vat (floor (+ (/ (* total rate-percent) (+ 100 rate-percent)) 1/2))))
        (values (- total vat) vat total))
      (let* ((taxable amount)
             (vat (floor (+ (/ (* taxable rate-percent) 100) 1/2))))
        (values taxable vat (+ taxable vat)))))

(defun vat-answer (amount inclusive-p rate-percent)
  "The ksa_vat_calculate answer for AMOUNT, in SAR, at RATE-PERCENT."
  (let ((given (halalas amount)))
    (multiple-value-bind (taxable vat total) (vat-breakdown given rate-percent inclusive-p)
      (format nil "zatca: ~d% VAT ~a ~a~%  taxable amount  ~a~%  VAT             ~a~%  total           ~a"
              rate-percent (if inclusive-p "inside" "on") (money-sar given)
              (money-sar taxable) (money-sar vat) (money-sar total)))))

;;; --- the rules ----------------------------------------------------------------

(defparameter +month-days+ #(31 28 31 30 31 30 31 31 30 31 30 31)
  "Days in each month of a common year; February gains one in a leap year.")

(defun all-digits-p (text)
  "Whether every character of TEXT is a decimal digit, and TEXT is not empty."
  (and (plusp (length text)) (every (lambda (char) (and (digit-char-p char) t)) text)))

(defun valid-uuid-p (text)
  "Whether TEXT is a version 4 UUID: 8-4-4-4-12 hex digits, a 4 at the front
of the third group and 8, 9, a or b at the front of the fourth (BR-KSA-03)."
  (and (stringp text)
       (= (length text) 36)
       (loop for char across text
             for index from 0
             always (if (member index '(8 13 18 23))
                        (char= char #\-)
                        (and (digit-char-p char 16) t)))
       (char-equal (char text 14) #\4)
       (and (find (char text 19) "89abAB") t)))

(defun valid-seller-vat-p (text)
  "Whether TEXT is a Saudi VAT registration number: 15 digits, 3 at both ends
(BR-KSA-06)."
  (and (stringp text)
       (= (length text) 15)
       (all-digits-p text)
       (char= (char text 0) #\3)
       (char= (char text 14) #\3)))

(defun valid-date-p (text)
  "Whether TEXT is a calendar date written YYYY-MM-DD (BR-KSA-05)."
  (and (stringp text)
       (= (length text) 10)
       (char= (char text 4) #\-)
       (char= (char text 7) #\-)
       (all-digits-p (concatenate 'string (subseq text 0 4) (subseq text 5 7) (subseq text 8 10)))
       (let* ((year (parse-integer text :start 0 :end 4))
              (month (parse-integer text :start 5 :end 7))
              (day (parse-integer text :start 8 :end 10))
              (leap (and (zerop (mod year 4))
                         (or (plusp (mod year 100)) (zerop (mod year 400)))))
              (limit (and (<= 1 month 12)
                          (if (= month 2) (if leap 29 28) (aref +month-days+ (1- month))))))
         (and (<= 1 year) limit (<= 1 day limit)))))

(defun valid-time-p (text)
  "Whether TEXT is a clock time written HH:MM:SS (BR-KSA-25)."
  (and (stringp text)
       (= (length text) 8)
       (char= (char text 2) #\:)
       (char= (char text 5) #\:)
       (all-digits-p (concatenate 'string (subseq text 0 2) (subseq text 3 5) (subseq text 6 8)))
       (<= 0 (parse-integer text :start 0 :end 2) 23)
       (<= 0 (parse-integer text :start 3 :end 5) 59)
       (<= 0 (parse-integer text :start 6 :end 8) 59)))

(defun check-invoice (&key uuid seller-vat icv issue-date issue-time
                        taxable vat total (rate-percent 15) (tolerance 5))
  "The ZATCA Phase 2 field rules over one invoice - TAXABLE, VAT and TOTAL in
SAR - answered as (VALUES FAILURES CHECKS-RUN): each failure one line naming
its rule, and CHECKS-RUN counting only the rules a field was given for."
  ;; The original ran four checks and answered `4 - the number of errors',
  ;; which goes negative once a fifth rule fails, and left two of the rules it
  ;; described (the date and the time) unrun. Here every rule that runs is
  ;; counted once, and a field left out of the call is not a rule at all.
  (let ((failures '()) (checks 0))
    (labels ((rule (code ok text)
               (incf checks)
               (unless ok (push (format nil "~a: ~a" code text) failures))))
      (rule "BR-KSA-03" (valid-uuid-p uuid)
            (format nil "uuid ~a is not a version 4 UUID" uuid))
      (rule "BR-KSA-06" (valid-seller-vat-p seller-vat)
            (format nil "seller VAT number ~a is not 15 digits starting and ending with 3" seller-vat))
      (rule "BR-KSA-14" (and (integerp icv) (>= icv 1))
            (format nil "invoice counter value ~a is not an integer of at least 1" icv))
      (when (plusp (length (or issue-date "")))
        (rule "BR-KSA-05" (valid-date-p issue-date)
              (format nil "issue date ~a is not a calendar date written YYYY-MM-DD" issue-date)))
      (when (plusp (length (or issue-time "")))
        (rule "BR-KSA-25" (valid-time-p issue-time)
              (format nil "issue time ~a is not a clock time written HH:MM:SS" issue-time)))
      (let* ((taxable-h (halalas taxable))
             (vat-h (halalas vat))
             (total-h (halalas total))
             (expected-vat (floor (+ (/ (* taxable-h rate-percent) 100) 1/2)))
             (expected-total (+ taxable-h vat-h)))
        (rule "BR-KSA-52" (<= (abs (- vat-h expected-vat)) tolerance)
              (format nil "VAT ~a is not ~d% of the taxable amount ~a (~a)"
                      (money-sar vat-h) rate-percent (money-sar taxable-h) (money-sar expected-vat)))
        (rule "BR-KSA-53" (<= (abs (- total-h expected-total)) tolerance)
              (format nil "total ~a is not the taxable amount ~a plus VAT ~a (~a)"
                      (money-sar total-h) (money-sar taxable-h) (money-sar vat-h)
                      (money-sar expected-total)))))
    (values (nreverse failures) checks)))

(defun report-text (uuid failures checks)
  "One invoice's checks, as the text the model and the operator both read."
  (if (null failures)
      (format nil "zatca: ~d ZATCA Phase 2 checks on invoice ~a: all passed" checks uuid)
      (format nil "zatca: ~d of ~d ZATCA Phase 2 checks failed on invoice ~a:~{~%  ~a~}"
              (length failures) checks uuid failures)))

;;; --- reading what the operator typed --------------------------------------------

(defun words (text)
  "TEXT split on runs of spaces and tabs."
  (let ((out '()) (start nil))
    (loop for index from 0 to (length text)
          for char = (and (< index (length text)) (char text index))
          do (if (and char (not (member char '(#\Space #\Tab))))
                 (unless start (setf start index))
                 (when start (push (subseq text start index) out) (setf start nil))))
    (nreverse out)))

(defun parse-amount (text)
  "TEXT read as a number of SAR - commas between thousands allowed - or NIL."
  (handler-case
      (let* ((*read-eval* nil)
             (*package* (find-package :keyword))
             (clean (remove #\, text)))
        (multiple-value-bind (value end) (read-from-string clean nil nil)
          (and (rationalp value) (= end (length clean)) value)))
    (error () nil)))
