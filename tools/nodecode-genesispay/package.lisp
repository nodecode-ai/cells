;;;; package.lisp --- the NODECODE-GENESISPAY package, and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; GENESISPAY is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; every file
;;;; before it reads the section through SETTING. FAIL and GENESISPAY-ERROR
;;;; come from NLK:DEFINE-PERIPHERAL: what a tool signals when its call
;;;; cannot be made or the catalog refuses it.

(defpackage #:nodecode-genesispay
  (:use #:cl)
  (:nicknames #:genesispay)
  (:export #:genesispay-error #:genesispay-error-detail))

(in-package #:nodecode-genesispay)

(nlk:define-peripheral genesispay :not-running t)
