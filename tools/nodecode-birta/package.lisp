;;;; package.lisp --- the NODECODE-BIRTA package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; BIRTA is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; mcp.lisp and
;;;; tools.lisp read the section through it. FAIL and BIRTA-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-birta
  (:use #:cl)
  (:nicknames #:birta)
  (:export #:birta-error #:birta-error-detail))

(in-package #:nodecode-birta)

(nlk:define-peripheral birta :not-running t)
