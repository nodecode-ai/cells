;;;; package.lisp --- the NODECODE-BOXCOMPUTE package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; BOXCOMPUTE is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it, and tools.lisp's functions are
;;;; the functions the declaration registers. FAIL and BOXCOMPUTE-ERROR come
;;;; from NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be
;;;; made.

(defpackage #:nodecode-boxcompute
  (:use #:cl)
  (:nicknames #:boxcompute)
  (:export #:boxcompute-error #:boxcompute-error-detail
           #:tools #:schema))

(in-package #:nodecode-boxcompute)

(nlk:define-peripheral boxcompute :not-running t)
