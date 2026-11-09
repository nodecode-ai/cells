;;;; package.lisp --- the NODECODE-TAVILY package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; TAVILY is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it. FAIL and TAVILY-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-tavily
  (:use #:cl)
  (:nicknames #:tavily)
  (:export #:tavily-error #:tavily-error-detail))

(in-package #:nodecode-tavily)

(nlk:define-peripheral tavily :not-running t)
