;;;; package.lisp --- the CLEF package: four verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CLEF so an eval form reads (clef:transpose "C G Am" 2):
;;;; the same call the tool makes, answering the same text, for the operator
;;;; who would rather type one.

(defpackage #:nodecode-clef
  (:use #:cl)
  (:nicknames #:clef)
  (:export
   ;; the four verbs, each answering a string
   #:transpose
   #:spell-chord
   #:tab-notes
   #:guides
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:clef-error))

(in-package #:nodecode-clef)

(nlk:define-peripheral clef :not-running t)
