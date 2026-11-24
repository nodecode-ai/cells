;;;; package.lisp --- the DEVSENTRY package: three read verbs and one refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; DEVSENTRY is a nickname so an eval form reads
;;;; (devsentry:inspect ...). SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; the other files read the
;;;; section through it.

(defpackage #:nodecode-devsentry
  (:use #:cl)
  (:nicknames #:devsentry)
  (:export #:devsentry-refusal #:devsentry-refusal-code #:devsentry-refusal-detail))

(in-package #:nodecode-devsentry)

(nlk:define-peripheral devsentry :not-running t)

;;; --- the one refusal ------------------------------------------------------------

(define-condition devsentry-refusal (error)
  ((code :initarg :code :reader devsentry-refusal-code)
   (detail :initarg :detail :reader devsentry-refusal-detail))
  (:documentation "One refusal a tool call answers as JSON: the code a caller
reads and the sentence a person reads.")
  (:report (lambda (condition stream)
             (write-string (devsentry-refusal-detail condition) stream))))

(defun refuse (code format-control &rest arguments)
  "Signal a DEVSENTRY-REFUSAL of CODE, its detail ARGUMENTS formatted."
  (error 'devsentry-refusal :code code
                            :detail (apply #'format nil format-control arguments)))
