;;;; support.lisp --- plan mode test runner and the helpers its tests share.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Plan mode's tests register into the shared nodecode.test registry under a
;;;; PLAN-MODE- name prefix, so RUN-PLAN-MODE-TESTS runs exactly that slice
;;;; and never the core suite. Every test here is offline: the plan is a file
;;;; under a scratch directory, and the approval question is a stub, never an
;;;; operator and never a network call.

(in-package #:nodecode.test)

(define-test-slice "plan-mode" "PLAN-MODE" :start nodecode-plan-mode:start-cell)

(defun plan-mode-scratch ()
  "A fresh plans directory under the temporary directory."
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "plan-mode-~d/" (random 1000000))
                    (uiop:temporary-directory))))

(defun plan-mode-scratch-text ()
  "The same directory, as the section carries a directory: a namestring."
  (uiop:native-namestring (plan-mode-scratch)))

(defun plan-mode-plan (dir &optional (text "# Plan\n\nDo the thing.\n"))
  "Write one plan file into DIR and answer its native path."
  (ensure-directories-exist dir)
  (let ((file (merge-pathnames "2026-01-02_030405-plan.md" dir)))
    (with-open-file (out file :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-string text out))
    (uiop:native-namestring file)))

(defun plan-mode-verdict (form &optional strings)
  "The installed :TOOL hook's answer for an eval call of FORM: :ALLOWED, or
 the refusal text it answered instead."
  (funcall (cell-hook "nodecode-plan-mode" :tool)
           (list :name "eval"
                 :arguments (if strings
                                (let ((object (nlk:json-object "form" form)))
                                  (setf (gethash "strings" object)
                                        (let ((carried (nlk:make-json-object)))
                                          (loop for (name text) on strings by #'cddr
                                                do (setf (gethash name carried) text))
                                          carried))
                                  object)
                                (nlk:json-object "form" form))
                 :call-id "plan-mode-call")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun plan-mode-tool-call (action &rest pairs)
  "Call the plan_mode tool the way a provider's call reaches it."
  (let* ((object (apply #'nlk:make-json-object (append (list "action" action) pairs)))
         (record (nlk:contract-decode (nlk:contract 'nodecode-plan-mode::plan-action)
                                       object)))
    (funcall (third (nle::find-tool "plan_mode")) record)))

(defun plan-mode-command (args)
  "Run /plan ARGS: what the registered command's own function answers."
  (nodecode-plan-mode::run-plan args "s1"))

(defun call-with-plan-mode-operator (stub thunk)
  "THUNK with the core's ASK-OPERATOR answered by STUB, put back after."
  (let* ((symbol (intern "ASK-OPERATOR" '#:nodecode.evolved))
         (saved (and (fboundp symbol) (fdefinition symbol))))
    (setf (fdefinition symbol) stub)
    (unwind-protect (funcall thunk)
      (if saved (setf (fdefinition symbol) saved) (fmakunbound symbol)))))

(defun plan-mode-section (session-id)
  "The standing harness section the cell supplies for SESSION-ID: an alist of
 (NAME . TEXT), the next reader's own entries beneath its own."
  (funcall (cell-hook "nodecode-plan-mode" 'nle::read-harness-sections)
           (lambda (id) (declare (ignore id)) (list (cons "index" "nothing here")))
           session-id))
