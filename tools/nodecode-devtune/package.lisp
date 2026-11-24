;;;; package.lisp --- the DEVTUNE package: one verb, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed DEVTUNE so an eval form reads
;;;; (devtune:report :visibility-summary): the same call the tool makes,
;;;; answering the same text, for the operator who would rather type one.

(defpackage #:nodecode-devtune
  (:use #:cl)
  (:nicknames #:devtune)
  (:export
   ;; the verb, answering a string
   #:report
   ;; the report names, for a caller deciding what to ask for
   #:reports
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:devtune-error))

(in-package #:nodecode-devtune)

(nlk:define-peripheral devtune :not-running t)
