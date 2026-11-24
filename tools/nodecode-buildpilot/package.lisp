;;;; package.lisp --- the BUILDPILOT package: the store, the verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed BUILDPILOT so an eval form reads
;;;; (buildpilot:projects). Every exported verb answers a STRING and every
;;;; refusal is BUILDPILOT-ERROR, which the eval snippet renders as
;;;; "ERROR: BUILDPILOT-ERROR: <detail>".

(defpackage #:nodecode-buildpilot
  (:use #:cl)
  (:nicknames #:buildpilot)
  (:export
   ;; verbs, every one a string
   #:projects #:add-project #:add-task #:update-task #:checkpoint #:context
   ;; the one seam a test may replace, and the words
   #:*state-file-override* #:*primer*
   ;; the one condition
   #:buildpilot-error))

(in-package #:nodecode-buildpilot)

(nlk:define-peripheral buildpilot :not-running t)
