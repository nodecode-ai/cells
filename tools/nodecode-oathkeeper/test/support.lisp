;;;; support.lisp --- the slice, a scratch oath, and the helpers every test reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under an
;;;; OATHKEEPER- prefix and RUN-OATHKEEPER-TESTS runs exactly that slice. Every
;;;; test writes its own oath into the run's scratch home, so nothing here reads
;;;; or writes ~/.nodecode and no test sees another's rules.

(in-package #:nodecode.test)

(define-test-slice "oathkeeper" "OATHKEEPER-"
  :start nodecode-oathkeeper:start-cell
  :defaults (("oath_file"
              (namestring (merge-pathnames "oath.txt"
                                            (nlk:home "oathkeeper/"))))))

(defun oathkeeper-dir ()
  "The scratch folder the cell owns in this run's home."
  (nlk:home "oathkeeper/"))

(defun oathkeeper-file ()
  "The scratch oath file the slice's default section names."
  (namestring (merge-pathnames "oath.txt" (oathkeeper-dir))))

(defun oathkeeper-write-oath (text)
  "TEXT as the scratch oath, and a ledger and snapshot cleared beside it."
  (let ((path (oathkeeper-file)))
    (ensure-directories-exist path)
    (nlk:write-file-atomically path text)
    (dolist (name '("ledger.jsonl" "snapshot.jsonl"))
      (ignore-errors (delete-file (merge-pathnames name (oathkeeper-dir)))))
    path))

(defmacro with-oath ((text) &body body)
  "BODY with TEXT as the oath and the cell started against it."
  `(let ((path (oathkeeper-write-oath ,text)))
     (with-cell-stop ((oathkeeper-start "oath_file" path))
       ,@body)))

(defmacro with-no-oath (&body body)
  "BODY with the cell started and no oath file at all: the inert state."
  `(let ((path (oathkeeper-write-oath "")))
     (ignore-errors (delete-file path))
     (with-cell-stop ((oathkeeper-start "oath_file" path))
       ,@body)))

(defun oathkeeper-strings (pairs)
  "The strings object a call carries, from PAIRS of name and text."
  (let ((object (nlk:make-json-object)))
    (loop for (name text) on pairs by #'cddr
          do (setf (gethash name object) text))
    object))

(defun oathkeeper-verdict (form &rest strings)
  "The installed hook's answer for FORM: :RAN, or the refusal the model reads."
  (funcall (cell-hook "nodecode-oathkeeper" :tool)
           (list :name "eval"
                 :arguments (apply #'nlk:make-json-object "form" form
                                   (when strings
                                     (list "strings" (oathkeeper-strings strings))))
                 :call-id "call-1")
           (lambda (op) (declare (ignore op)) :ran)))

(defun oathkeeper-events ()
  "Every event name on the ledger, oldest first."
  (mapcar (lambda (row) (nodecode-oathkeeper::row-field row "event"))
          (nodecode-oathkeeper::read-ledger (nodecode-oathkeeper::data-dir))))

(defun oathkeeper-actions (form &optional strings)
  "Every action the gate would read from FORM, with the strings object STRINGS."
  (nodecode-oathkeeper::extract-actions
   "eval" (apply #'nlk:make-json-object "form" form
                  (when strings (list "strings" strings)))))

(defun oathkeeper-scratch (name)
  "A path in this run's scratch home, with its parents made."
  (let ((path (namestring (nlk:home name))))
    (ensure-directories-exist path)
    path))
