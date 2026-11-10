;;;; package.lisp --- the AIRLOCK package: the verbs, the condition, the settings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed AIRLOCK so an eval form reads
;;;; (airlock:run "uname -a"). It is meant to be used qualified only.

(defpackage #:nodecode-airlock
  (:use #:cl)
  (:nicknames #:airlock)
  (:export #:run #:health #:receipt #:last-receipt #:verify-text
           #:receipt-id #:receipt-verdict
           #:airlock-error))

(in-package #:nodecode-airlock)

;;; FAIL, AIRLOCK-ERROR, the settings variable, RUNNING-SETTINGS and
;;; DEFINE-VERB come from here; SETTING comes from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last, and the files between read the
;;; section through it.
(nlk:define-peripheral airlock :not-running t)
