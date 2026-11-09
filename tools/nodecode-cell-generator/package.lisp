;;;; package.lisp --- the CELL-GENERATOR package: three tools, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CELL-GENERATOR. The three verbs are the ones the
;;;; model calls; every refusal is CELL-GENERATOR-ERROR, which the tool lane
;;;; hands the model as the reason, and (:idle ...) in the tests reads one.

(defpackage #:nodecode-cell-generator
  (:use #:cl)
  (:nicknames #:cell-generator)
  (:export
   ;; the three model-facing verbs
   #:scaffold #:add-tool #:add-command
   ;; data, so a layer can reword it
   #:*markers*
   ;; the one condition
   #:cell-generator-error))

(in-package #:nodecode-cell-generator)

(nlk:define-peripheral cell-generator :not-running t)
