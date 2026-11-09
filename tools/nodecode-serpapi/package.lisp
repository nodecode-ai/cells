;;;; package.lisp --- the NODECODE-SERPAPI package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; SERPAPI is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it. FAIL and SERPAPI-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-serpapi
  (:use #:cl)
  (:nicknames #:serpapi)
  (:export #:serpapi-error #:serpapi-error-detail))

(in-package #:nodecode-serpapi)

(nlk:define-peripheral serpapi :not-running t)
