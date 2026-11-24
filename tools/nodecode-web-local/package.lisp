;;;; package.lisp --- the NODECODE-WEB-LOCAL package: one verb and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed WEB-LOCAL so an eval form reads
;;;; (web-local:extract "https://..."): not WEB, which the shipped websearch
;;;; cell's package answers to. It is meant to be used qualified only.

(defpackage #:nodecode-web-local
  (:use #:cl)
  (:nicknames #:web-local)
  (:export #:extract #:extract-many #:web-local-error))

(in-package #:nodecode-web-local)

;;; WEB-LOCAL-ERROR is the one condition every refusal signals, FAIL signals it
;;; with a detail; NOT-RUNNING gives this package *WEB-LOCAL*, RUNNING-SETTINGS
;;; -- the refusal a verb opens with while the cell is not started -- and
;;; DEFINE-VERB. SETTING comes from the NLE:DEFINE-CELL declaration in
;;; cell.lisp, which loads last.
(nlk:define-peripheral web-local :not-running t)
