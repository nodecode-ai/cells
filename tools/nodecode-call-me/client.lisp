;;;; client.lisp --- numbers, names, and where the session file lives.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A /call-me number is 10 digits and is the allowlist: a number that is
;;;; not in the section's list is never texted and never rung, so an agent
;;;; talked into reaching a stranger is refused here, before any request.
;;;; WIRE.LISP is the requests themselves and the session file they share.

(in-package #:nodecode-call-me)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix nil))

(defparameter +api+ "https://callmemcp.com/aiphone"
  "The /call-me gateway: the one host a request leaves for.")

(defparameter +user-agent+ "nodecode-call-me/0.1.0"
  "What the service is told is calling.")

(defparameter +max-text+ 2000
  "The service's own ceiling on a text, and on the text a call speaks.")

(defparameter +max-wait+ 300
  "The longest a call may hold the caller open, in seconds.")

(defparameter +min-wait+ 30
  "The shortest wait the service accepts for a call, in seconds.")

;;; --- numbers -------------------------------------------------------------------

(defun digits (value)
  "VALUE as a bare 10-digit number: a leading country code 1 and every
separator dropped, as the original's DIGITS did."
  (let ((raw (remove-if-not #'digit-char-p (princ-to-string (or value "")))))
    (if (and (= (length raw) 11) (char= (char raw 0) #\1))
        (subseq raw 1)
        raw)))

(defun display (number)
  "NUMBER the way a person reads it back: 555-123-4567."
  (let ((n (digits number)))
    (if (= (length n) 10)
        (format nil "~a-~a-~a" (subseq n 0 3) (subseq n 3 6) (subseq n 6))
        n)))

(defun parse-numbers (raw)
  "RAW -- the section's list of numbers -- as 10-digit strings, in order, once each."
  (let ((seen '()))
    (dolist (item (if (listp raw) raw (list raw)) (nreverse seen))
      (let ((n (digits item)))
        (when (and (= (length n) 10) (not (member n seen :test #'string=)))
          (push n seen))))))

(defun parse-names (raw)
  "RAW -- \"5551234567=Sam,5559876543=Alex\" -- as an alist of (NUMBER . NAME)."
  (let ((names '()))
    (dolist (item (uiop:split-string (or raw "") :separator '(#\, #\;)) (nreverse names))
      (let* ((cut (or (position #\= item) (position #\: item)))
             (number (digits (if cut (subseq item 0 cut) item)))
             (name (and cut (string-trim '(#\space #\tab) (subseq item (1+ cut))))))
        (when (and (= (length number) 10) name (plusp (length name)))
          (push (cons number (subseq name 0 (min 40 (length name)))) names))))))

(defun resolve-recipient (who numbers names)
  "WHO -- a number, or one of NAMES -- as an allowed number, or NIL.
An empty WHO answers the only allowed number, and nobody when there are several."
  (let ((text (string-trim '(#\space #\tab) (princ-to-string (or who "")))))
    (cond ((zerop (length text)) (and (= 1 (length numbers)) (first numbers)))
          ((member (digits text) numbers :test #'string=) (digits text))
          (t (let ((wanted (string-downcase text)))
               (loop for (number . name) in names
                     when (string= (string-downcase name) wanted) return number))))))

(defun name-of (number)
  "What the operator calls NUMBER: its name, else the number in words."
  (or (cdr (assoc number (setting :names) :test #'string=))
      (display number)))

;;; --- the session file ------------------------------------------------------------

(defun nonblank (value)
  (let ((text (and (stringp value) (string-trim '(#\space #\tab #\newline) value))))
    (and text (plusp (length text)) text)))

(defun state-file ()
  "Where the session token and the agent's number are kept, under the nodecode home."
  (nlk:home "call-me/session.json"))
