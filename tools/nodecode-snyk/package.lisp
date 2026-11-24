;;;; package.lisp --- the NODECODE-SNYK package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; SNYK is a nickname so an eval form reads
;;;; (snyk:...). SETTING comes from the NLE:DEFINE-CELL declaration in
;;;; cell.lisp, which loads last; cli.lisp and tools.lisp read the section
;;;; through it. FAIL and SNYK-ERROR come from NLK:DEFINE-PERIPHERAL, and
;;;; are what a tool says when its call cannot be built at all.

(defpackage #:nodecode-snyk
  (:use #:cl)
  (:nicknames #:snyk)
  (:export #:snyk-error))

(in-package #:nodecode-snyk)

(nlk:define-peripheral snyk :not-running t)
