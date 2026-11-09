;;;; package.lisp --- the TAM package: two verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed TAM so an eval form reads (tam:recall "..."):
;;;; the same call the tools make, answering the same text, for the operator
;;;; who would rather type one.

(defpackage #:nodecode-tam
  (:use #:cl)
  (:nicknames #:tam)
  (:export
   ;; the verbs, answering a string as a model reads it
   #:recall
   #:save
   #:status
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:tam-error))

(in-package #:nodecode-tam)

(nlk:define-peripheral tam :not-running t)
