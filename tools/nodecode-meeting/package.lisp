;;;; package.lisp --- the MEETING package: four verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MEETING so an eval form reads
;;;; (meeting:transcript :lines 30): the same four calls the tools make,
;;;; answering the same text, for the operator who would rather type one.

(defpackage #:nodecode-meeting
  (:use #:cl)
  (:nicknames #:meeting)
  (:export
   ;; the four verbs, each answering a string
   #:status #:transcript #:note-line #:save
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:meeting-error))

(in-package #:nodecode-meeting)

(nlk:define-peripheral meeting :not-running t)
