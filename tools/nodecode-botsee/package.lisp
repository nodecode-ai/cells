;;;; package.lisp --- the BOTSEE package: two verbs, the condition, the primer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed BOTSEE so an eval form reads
;;;; (botsee:tools) and (botsee:call "run_analysis" "{...}"): the same
;;;; calls the two tools make, answering the same text, for the operator who
;;;; would rather type one.

(defpackage #:nodecode-botsee
  (:use #:cl)
  (:nicknames #:botsee)
  (:export
   ;; the verbs, answering strings
   #:tools
   #:call
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:botsee-error))

(in-package #:nodecode-botsee)

(nlk:define-peripheral botsee :not-running t)
