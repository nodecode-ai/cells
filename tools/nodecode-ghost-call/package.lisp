;;;; package.lisp --- the NODECODE-GHOST-CALL package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it. SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; wire.lisp, hints.lisp,
;;;; session.lisp, summary.lisp and tools.lisp read the section through it.
;;;; FAIL and GHOST-CALL-ERROR come from NLK:DEFINE-PERIPHERAL: what a tool
;;;; signals when its call cannot be made, and the model reads as "ERROR: ...".

(defpackage #:nodecode-ghost-call
  (:use #:cl)
  (:export #:ghost-call-error #:ghost-call-error-detail))

(in-package #:nodecode-ghost-call)

(nlk:define-peripheral ghost-call :not-running t)
