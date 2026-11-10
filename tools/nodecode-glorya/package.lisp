;;;; package.lisp --- the NODECODE-GLORYA package: seven tools, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it. The tools are what the model calls; a person can call
;;;; the same functions from eval, or type /glorya.

(defpackage #:nodecode-glorya
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:glorya-create
           #:glorya-job
           #:glorya-answer
           #:glorya-refine
           #:glorya-publish
           #:glorya-attach
           #:glorya-jobs
           #:glorya-command
           #:glorya-error
           #:glorya-error-detail))

(in-package #:nodecode-glorya)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and GLORYA-ERROR, which a call that cannot go on signals,
;;; come from here: every refusal starts as one of these, and a tool answers
;;; it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral glorya :not-running t)
