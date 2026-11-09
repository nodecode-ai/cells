;;;; package.lisp --- the NODECODE-DEFUDDLE package: one verb and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed DEFUDDLE so an eval form reads
;;;; (defuddle:extract "https://..."): not WEB, which the shipped websearch
;;;; cell's package answers to. It is meant to be used qualified only.

(defpackage #:nodecode-defuddle
  (:use #:cl)
  (:nicknames #:defuddle)
  (:export #:extract #:defuddle-error))

(in-package #:nodecode-defuddle)

;;; DEFUDDLE-ERROR is the one condition every refusal signals, FAIL signals it
;;; with a detail; NOT-RUNNING gives this package *DEFUDDLE*, RUNNING-SETTINGS
;;; -- the refusal a verb opens with while the cell is not started -- and
;;; DEFINE-VERB. SETTING comes from the NLE:DEFINE-CELL declaration in
;;; cell.lisp, which loads last.
(nlk:define-peripheral defuddle :not-running t)
