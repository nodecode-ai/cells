;;;; support.lisp --- read-only guard test runner and the helpers its tests share.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; READONLY-GUARD- name prefix, so RUN-READONLY-GUARD-TESTS runs exactly
;;;; this slice. Every one is offline: a call is a form string handed to the
;;;; installed :TOOL hook, and the plans directory is a scratch one.

(in-package #:nodecode.test)

(define-test-slice "readonly-guard" "READONLY-GUARD" :start nodecode-readonly-guard:start-cell)

(defun readonly-guard-arguments (form strings)
  "An eval call's arguments object for FORM, with the STRINGS a heredoc carries."
  (let ((object (nlk:json-object "form" form)))
    (when strings
      (let ((carried (nlk:make-json-object)))
        (loop for (name text) on strings by #'cddr
              do (setf (gethash name carried) text))
        (setf (gethash "strings" object) carried)))
    object))

(defun readonly-guard-verdict (form &optional strings)
  "The installed :TOOL hook's answer for an eval call of FORM: :ALLOWED, or
 the refusal text it answered instead."
  (funcall (cell-hook "nodecode-readonly-guard" :tool)
           (list :name "eval"
                 :arguments (readonly-guard-arguments form strings)
                 :call-id "readonly-guard-call")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun readonly-guard-tool-verdict (name)
  "The hook's answer for a call of the tool NAME, which carries no form."
  (funcall (cell-hook "nodecode-readonly-guard" :tool)
           (list :name name :arguments (nlk:make-json-object) :call-id "readonly-guard-call")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun readonly-guard-command (&optional (args ""))
  "Run /readonly ARGS in the session the hook reads outside a turn."
  (nodecode-readonly-guard::run-readonly args ""))

(defun readonly-guard-note (session-id)
  "The standing harness section text the cell supplies for SESSION-ID, or NIL."
  (let ((sections (funcall (cell-hook "nodecode-readonly-guard" 'nle::read-harness-sections)
                            (lambda (id) (declare (ignore id)) (list (cons "index" "nothing here")))
                            session-id)))
    (cdr (assoc "readonly-guard" sections :test #'equal))))

(defun readonly-guard-scratch ()
  "A fresh directory for a plan file."
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "readonly-guard-~d/" (random 1000000))
                    (uiop:temporary-directory))))
