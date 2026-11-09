;;;; package.lisp --- the NODECODE-SCALEKIT package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; SCALEKIT is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it. FAIL and SCALEKIT-ERROR come
;;;; from NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be
;;;; made.

(defpackage #:nodecode-scalekit
  (:use #:cl)
  (:nicknames #:scalekit)
  (:export #:scalekit-error #:scalekit-error-detail))

(in-package #:nodecode-scalekit)

(nlk:define-peripheral scalekit :not-running t)
