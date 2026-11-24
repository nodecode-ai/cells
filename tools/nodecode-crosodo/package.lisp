;;;; package.lisp --- the CROSODO package: six verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CROSODO so an eval form reads the same call the
;;;; model makes: (crosodo:lookup-state-law "VA") answers the text the
;;;; lookup_state_law tool answers.

(defpackage #:nodecode-crosodo
  (:use #:cl)
  (:nicknames #:crosodo)
  (:export
   ;; the verbs, every one a string
   #:lookup-state-law #:lookup-county #:products #:start-checkout #:verify-certification
   #:call-tool
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:crosodo-error))

(in-package #:nodecode-crosodo)

(nlk:define-peripheral crosodo :not-running t)
