;;;; package.lisp --- the NODECODE-FORESEE package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; FORESEE is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp
;;;; and tools.lisp read the section through it. FAIL and FORESEE-ERROR come
;;;; from NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be
;;;; made.

(defpackage #:nodecode-foresee
  (:use #:cl)
  (:nicknames #:foresee)
  (:export #:foresee-error #:foresee-error-detail))

(in-package #:nodecode-foresee)

(nlk:define-peripheral foresee :not-running t)
