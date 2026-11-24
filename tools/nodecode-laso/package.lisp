;;;; package.lisp --- the NODECODE-LASO package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; LASO is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp; FAIL and LASO-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool answers when its call cannot be made.

(defpackage #:nodecode-laso
  (:use #:cl)
  (:nicknames #:laso)
  (:export #:laso-error #:laso-error-detail))

(in-package #:nodecode-laso)

(nlk:define-peripheral laso :not-running t)
