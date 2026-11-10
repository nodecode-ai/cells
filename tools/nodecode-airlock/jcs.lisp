;;;; jcs.lisp --- RFC 8785 canonical JSON, the exact bytes a receipt's hash and
;;;; signature cover.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Airlock signs `canonical(receipt without "hash" and "sig_ed25519")': the
;;;; members sorted, no whitespace, strings escaped minimally, numbers as
;;;; ECMAScript writes them (RFC 8785 sections 3.2.2.2 and 3.2.2.3). The
;;;; plugin this cell rewrites got that from the `jcs' package; here it is
;;;; written out over Nodecode's own decoded JSON (objects as EQUAL hash
;;;; tables, arrays as vectors, T / NIL / :NULL for true / false / null), so a
;;;; receipt is checked against the same bytes its author signed.

(in-package #:nodecode-airlock)

;;; --- strings ---------------------------------------------------------------

(defun jcs-write-string (string out)
  "STRING as a canonical JSON string: the short escapes for the characters
JSON names, \\u00xx for the rest of C0, and nothing else escaped."
  (write-char #\" out)
  (loop for char across string
        for code = (char-code char)
        do (case char
             (#\" (write-string "\\\"" out))
             (#\\ (write-string "\\\\" out))
             (#\Backspace (write-string "\\b" out))
             (#\Tab (write-string "\\t" out))
             (#\Newline (write-string "\\n" out))
             (#\Page (write-string "\\f" out))
             (#\Return (write-string "\\r" out))
             (t (if (< code #x20)
                    (format out "\\u~4,'0x" code)
                    (write-char char out)))))
  (write-char #\" out))

(defun jcs-code-units (string)
  "STRING's UTF-16 code units, which is what RFC 8785 sorts member names by."
  (let ((units (make-array 8 :adjustable t :fill-pointer 0)))
    (loop for char across string
          for code = (char-code char)
          do (if (< code #x10000)
                 (vector-push-extend code units)
                 (let ((rest (- code #x10000)))
                   (vector-push-extend (+ #xd800 (ash rest -10)) units)
                   (vector-push-extend (+ #xdc00 (logand rest #x3ff)) units))))
    units))

(defun jcs-name-lessp (a b)
  "Whether member name A sorts before B by UTF-16 code units."
  (let ((ua (jcs-code-units a))
        (ub (jcs-code-units b)))
    (dotimes (index (min (length ua) (length ub)) (< (length ua) (length ub)))
      (cond ((< (aref ua index) (aref ub index)) (return-from jcs-name-lessp t))
            ((> (aref ua index) (aref ub index)) (return-from jcs-name-lessp nil))))))

;;; --- numbers ---------------------------------------------------------------

(defun split-decimal (text)
  "TEXT, a decimal SBCL printed for a float, as (values NEGATIVE DIGITS EXPONENT):
DIGITS the significant digits with no leading or trailing zero, EXPONENT the
power of ten DIGITS as an integer is multiplied by."
  (let* ((negative (and (plusp (length text)) (char= #\- (char text 0))))
         (body (if negative (subseq text 1) text))
         (marker (position-if (lambda (char) (member char '(#\e #\E #\d #\D))) body))
         (mantissa (if marker (subseq body 0 marker) body))
         (power (if marker (parse-integer body :start (1+ marker)) 0))
         (point (or (position #\. mantissa) (length mantissa)))
         (digits (concatenate 'string (subseq mantissa 0 point)
                               (if (< point (length mantissa)) (subseq mantissa (1+ point)) "")))
         (exponent (- power (- (length mantissa) point 1))))
    ;; Leading zeros move nothing; each trailing zero is one power of ten.
    (let ((from (or (position-if (lambda (c) (not (char= c #\0))) digits) (length digits))))
      (setf digits (subseq digits from))
      (loop while (and (plusp (length digits)) (char= #\0 (char digits (1- (length digits)))))
            do (setf digits (subseq digits 0 (1- (length digits))))
               (incf exponent)))
    (values negative (if (plusp (length digits)) digits "0") exponent)))

(defun es-float (number)
  "NUMBER as ECMAScript's Number::toString writes it: the shortest decimal
that reads back as the same float, in fixed form for the exponents it keeps
fixed, exponential otherwise (RFC 8785 section 3.2.2.3)."
  (multiple-value-bind (negative digits exponent)
      (split-decimal (let ((*read-default-float-format* 'double-float))
                       (princ-to-string number)))
    (if (equal digits "0")
        "0"
        (let* ((count (length digits))
               (point (+ exponent count))
               (text (cond ((and (<= count point) (<= point 21))
                            (concatenate 'string digits (make-string (- point count)
                                                                     :initial-element #\0)))
                           ((and (< 0 point) (<= point 21))
                            (concatenate 'string (subseq digits 0 point) "."
                                         (subseq digits point)))
                           ((and (< -6 point) (<= point 0))
                            (concatenate 'string "0." (make-string (- point) :initial-element #\0)
                                         digits))
                           (t (format nil "~a~@[.~a~]e~a~d"
                                      (subseq digits 0 1)
                                      (and (< 1 count) (subseq digits 1))
                                      (if (minusp (1- point)) #\- #\+)
                                      (abs (1- point)))))))
          (if negative (concatenate 'string "-" text) text)))))

;;; --- the value walk --------------------------------------------------------

(defun jcs-write (value out)
  "VALUE as canonical JSON on OUT."
  (cond ((eq value t) (write-string "true" out))
        ((null value) (write-string "false" out))
        ((eq value :null) (write-string "null" out))
        ((stringp value) (jcs-write-string value out))
        ((integerp value) (format out "~d" value))
        ((floatp value) (write-string (es-float value) out))
        ((hash-table-p value)
         (let ((names (sort (loop for name being the hash-keys of value collect name)
                            #'jcs-name-lessp)))
           (write-char #\{ out)
           (loop for name in names
                 for first = t then nil
                 do (unless first (write-char #\, out))
                    (unless (stringp name)
                      (fail "the receipt carries a member name that is not a string: ~s" name))
                    (jcs-write-string name out)
                    (write-char #\: out)
                    (jcs-write (gethash name value) out))
           (write-char #\} out)))
        ((vectorp value)
         (write-char #\[ out)
         (loop for index from 0 below (length value)
               do (when (plusp index) (write-char #\, out))
                  (jcs-write (aref value index) out))
         (write-char #\] out))
        (t (fail "the receipt carries a JSON value this reader does not know: ~s" value))))

(defun jcs-canonical (value)
  "VALUE as the RFC 8785 canonical JSON octets."
  (sb-ext:string-to-octets (with-output-to-string (out) (jcs-write value out))
                            :external-format :utf-8))
