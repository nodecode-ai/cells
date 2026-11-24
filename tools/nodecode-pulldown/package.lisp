;;;; package.lisp --- the NODECODE-PULLDOWN package: one verb and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed PULLDOWN so an eval form reads
;;;; (pulldown:fetch "https://..."). It is meant to be used qualified only.

(defpackage #:nodecode-pulldown
  (:use #:cl)
  (:nicknames #:pulldown)
  (:export #:fetch #:pulldown-error))

(in-package #:nodecode-pulldown)

;;; PULLDOWN-ERROR is the one condition every refusal signals, FAIL signals it
;;; with a detail; NOT-RUNNING gives this package *PULLDOWN*, RUNNING-SETTINGS
;;; -- the refusal a verb opens with while the cell is not started -- and
;;; DEFINE-VERB. SETTING comes from the NLE:DEFINE-CELL declaration in
;;; cell.lisp, which loads last.
(nlk:define-peripheral pulldown :not-running t)
