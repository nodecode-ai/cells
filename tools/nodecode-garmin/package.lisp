;;;; package.lisp --- the GARMIN package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed GARMIN so an eval form reads
;;;; (garmin:today). Every verb answers a string; every refusal is
;;;; GARMIN-ERROR, which a tool call shows the model as its own line.

(defpackage #:nodecode-garmin
  (:use #:cl)
  (:nicknames #:garmin)
  ;; SLEEP is a CL function (of seconds); the verb of that name is the
  ;; model's, so the package shadows the CL one, as perplexity shadows SEARCH.
  (:shadow #:sleep)
  (:export
   ;; the fourteen model verbs, every one a string
   #:today #:sleep #:hrv #:body-battery #:stress #:steps #:heart-rate
   #:activities #:activity #:activity-fit #:body #:trends #:training-load
   #:fitness-age
   ;; the operator's verb
   #:status #:run-command
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:garmin-error))

(in-package #:nodecode-garmin)

;;; FAIL, GARMIN-ERROR, *GARMIN*, RUNNING-SETTINGS and DEFINE-VERB come from
;;; here; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp,
;;; which loads last, so the files before it read the section through it.
(nlk:define-peripheral garmin :not-running t)
