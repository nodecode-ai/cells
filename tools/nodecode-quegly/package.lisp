;;;; package.lisp --- the NODECODE-QUEGLY package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; QUEGLY is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it. FAIL and QUEGLY-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-quegly
  (:use #:cl)
  (:nicknames #:quegly)
  (:export #:quegly-error #:quegly-error-detail))

(in-package #:nodecode-quegly)

(nlk:define-peripheral quegly :not-running t)
