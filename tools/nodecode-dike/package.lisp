;;;; package.lisp --- the NODECODE-DIKE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed DIKE so an eval form reads (dike:check-citations ...).
;;;; Every failure the model sees is DIKE-ERROR, which the eval snippet renders as
;;;; "ERROR: DIKE-ERROR: <detail>".

(defpackage #:nodecode-dike
  (:use #:cl)
  (:nicknames #:dike)
  (:export
   ;; the verbs
   #:check-citations #:corpus
   ;; the one condition
   #:dike-error))

(in-package #:nodecode-dike)

(nlk:define-peripheral dike :not-running t)
