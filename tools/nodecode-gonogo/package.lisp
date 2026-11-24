;;;; package.lisp --- the NODECODE-GONOGO package: the decision verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed GONOGO so an eval form reads
;;;; (gonogo:decide :passed 47 :total 50). Every refusal the model sees is
;;;; GONOGO-ERROR, which the eval snippet renders as "ERROR: GONOGO-ERROR:
;;;; <detail>"; the verdicts themselves are strings, never conditions.

(defpackage #:nodecode-gonogo
  (:use #:cl)
  (:nicknames #:gonogo)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:decide #:report #:compare
   ;; the one condition
   #:gonogo-error))

(in-package #:nodecode-gonogo)

;;; Gives this package GONOGO-ERROR, FAIL, *GONOGO*, RUNNING-SETTINGS and
;;; DEFINE-VERB; SETTING comes from the NLE:DEFINE-CELL declaration in
;;; cell.lisp, which loads last.
(nlk:define-peripheral gonogo :not-running t)
