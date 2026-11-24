;;;; package.lisp --- the NODECODE-POSTKITTY package: the platform table and the five tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; POSTKITTY is a nickname. FAIL and
;;;; POSTKITTY-ERROR come from NLK:DEFINE-PERIPHERAL: what a tool signals when
;;;; the cell is not running.

(defpackage #:nodecode-postkitty
  (:use #:cl)
  (:nicknames #:postkitty)
  (:export #:postkitty-error #:postkitty-error-detail
           #:run-command))

(in-package #:nodecode-postkitty)

(nlk:define-peripheral postkitty :not-running t)

(declaim (ftype (function (symbol) t) setting))
