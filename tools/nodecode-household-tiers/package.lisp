;;;; package.lisp --- the NODECODE-HOUSEHOLD-TIERS package: one verb and the settings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed HOUSEHOLD-TIERS so an eval form
;;;; reads (household-tiers:summary ...). The condition, FAIL, the settings
;;;; variable and SETTING come from the NLK:DEFINE-PERIPHERAL and
;;;; NLE:DEFINE-CELL declarations in cell.lisp.

(defpackage #:nodecode-household-tiers
  (:use #:cl)
  (:nicknames #:household-tiers)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:summary
   ;; the one condition
   #:household-tiers-error))

(in-package #:nodecode-household-tiers)

(nlk:define-peripheral household-tiers :not-running t)
