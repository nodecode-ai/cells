;;;; package.lisp --- the WITNESS package: vocabulary, settings, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed WITNESS so an eval form reads (witness:report-text),
;;;; and used qualified: its verbs are the cell's own words, not Lisp's.
;;;;
;;;; A refusal is WITNESS-ERROR (NLK:DEFINE-PERIPHERAL), whose report the
;;;; model reads; RUNNING-SETTINGS answers "not running" before anything else.

(defpackage #:nodecode-witness
  (:use #:cl)
  (:nicknames #:witness)
  (:export
   ;; the condition
   #:witness-error #:witness-error-detail
   ;; grading, as functions an eval form may call
   #:grade-skill #:trail-for #:constraints-for #:grade-named
   #:report-text #:show-text #:verify-text #:trace-text
   #:trust-named #:release-named
   ;; what the cell declares
   #:witness-verb #:run-slash #:watch-keeps #:trace-call
   ;; read by the tests, and by anyone looking at the cell's own files
   #:witness-directory #:records #:ledger-verify #:usage-stats))

(in-package #:nodecode-witness)

(nlk:define-peripheral witness :not-running t)
