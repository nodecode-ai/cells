;;;; support.lisp --- the foundry test slice.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Tests register into the shared NODECODE.TEST registry under a FOUNDRY-
;;;; name prefix; RUN-FOUNDRY-TESTS runs exactly that slice. Every test that
;;;; writes does it under *TEST-ROOT*, a fresh directory of its own, so
;;;; nothing here reads or writes this home's foundry/generated.

(in-package #:nodecode.test)

(define-test-slice "foundry" "FOUNDRY-" :start nodecode-foundry:start-cell)

(defmacro with-foundry-root ((var) &body body)
  "Run BODY with VAR a fresh generated root, removed on the way out."
  `(let ((,var (merge-pathnames (format nil "foundry-~d-~d/"
                                        (sb-posix:getpid) (random 1000000))
                                 (uiop:temporary-directory))))
     (let ((nodecode-foundry::*test-root* ,var))
       (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree ,var :validate t)))
         (ensure-directories-exist ,var)
         ,@body))))

(defun foundry-refusal (thunk)
  "The foundry condition THUNK signals, as text, or NIL when it answers."
  (handler-case (progn (funcall thunk) nil)
    (nodecode-foundry:foundry-error (condition) (princ-to-string condition))))

(defun foundry-request-record (&rest pairs)
  "A FOUNDRY-REQUEST record built the way a tool call decodes into one."
  (nlk:contract-decode (nlk:contract 'nodecode-foundry::foundry-request)
                        (apply #'nlk:make-json-object pairs)))

(defun foundry-path-record (path)
  (nlk:contract-decode (nlk:contract 'nodecode-foundry::foundry-path)
                        (nlk:json-object "path" path)))
