;;;; package.lisp --- the LOVEDONE package: one verb, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed LOVEDONE so an eval form reads
;;;; (lovedone:call "list_loved_ones"): the same call the tools make, for the
;;;; operator who would rather type one line than ask the model.

(defpackage #:nodecode-lovedone
  (:use #:cl)
  (:nicknames #:lovedone)
  (:export
   ;; the verb, answering the server's own text
   #:call
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:lovedone-error))

(in-package #:nodecode-lovedone)

(nlk:define-peripheral lovedone :not-running t)
