;;;; package.lisp --- the NODECODE-MAQAMI-TRAVEL package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; MAQAMI is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp
;;;; reads the section through it. FAIL and MAQAMI-TRAVEL-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-maqami-travel
  (:use #:cl)
  (:nicknames #:maqami))

(in-package #:nodecode-maqami-travel)

(nlk:define-peripheral maqami-travel :not-running t)
