;;;; package.lisp --- the NODECODE-PHONE-CALL package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it. SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; safety.lisp, brief.lisp,
;;;; wire.lisp, store.lisp and tools.lisp read the section through it.
;;;; PHONE-CALL-ERROR comes from NLK:DEFINE-PERIPHERAL: what a tool signals
;;;; when its call cannot be made, and the model reads as "ERROR: ...", and
;;;; what a tool might answer when the cell is not running at all.

(defpackage #:nodecode-phone-call
  (:use #:cl)
  (:export #:phone-call-error #:phone-call-error-detail))

(in-package #:nodecode-phone-call)

(nlk:define-peripheral phone-call :not-running t)
