;;;; package.lisp --- the NODECODE-SLUICE package: a stage, a review, an apply.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; SLUICE is the nickname the command, the help
;;;; topic and an eval form use. FAIL and SLUICE-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL; DEFINE-VERB is its refusal while the cell is not
;;;; running, which every verb below opens with.

(defpackage #:nodecode-sluice
  (:use #:cl)
  (:nicknames #:sluice)
  (:export #:sluice-error))

(in-package #:nodecode-sluice)

(nlk:define-peripheral sluice :not-running t)
