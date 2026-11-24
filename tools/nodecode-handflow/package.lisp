;;;; package.lisp --- the HANDFLOW package: the plan, and the verbs over it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed HANDLOW so an eval form reads (handflow:plan-text).
;;;; Every exported verb answers a STRING and every refusal is HANDFLOW-ERROR,
;;;; which the eval snippet renders as "ERROR: HANDFLOW-ERROR: <detail>".

(defpackage #:nodecode-handflow
  (:use #:cl)
  (:nicknames #:handflow)
  (:export
   ;; verbs, every one a string
   #:plan-text #:plans-text
   ;; the one condition
   #:handflow-error))
