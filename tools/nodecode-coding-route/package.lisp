;;;; package.lisp --- the NODECODE-CODING-ROUTE package: two lanes, two verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; CODING-ROUTE is a nickname, so an eval form reads
;;;; (coding-route:route ...). SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last. FAIL and CODING-ROUTE-ERROR
;;;; come from NLK:DEFINE-PERIPHERAL: what a verb signals when the cell is not
;;;; running or the section cannot route.

(defpackage #:nodecode-coding-route
  (:use #:cl)
  (:nicknames #:coding-route)
  (:export
   ;; the two verbs, each answering a string
   #:route #:result
   ;; the one condition
   #:coding-route-error #:coding-route-error-detail))

(in-package #:nodecode-coding-route)

(nlk:define-peripheral coding-route :not-running t)
