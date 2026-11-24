;;;; package.lisp --- the NODECODE-BASEKIT package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; BASEKIT is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp,
;;;; chain.lisp and tools.lisp read the section through it. FAIL and
;;;; BASEKIT-ERROR come from NLK:DEFINE-PERIPHERAL: what a tool signals when
;;;; a read cannot be made.

(defpackage #:nodecode-basekit
  (:use #:cl)
  (:nicknames #:basekit)
  (:export #:basekit-error #:basekit-error-detail))

(in-package #:nodecode-basekit)

(nlk:define-peripheral basekit :not-running t)
