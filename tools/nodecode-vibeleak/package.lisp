;;;; package.lisp --- the VIBELEAK package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, nicknamed VIBELEAK so an eval form
;;;; reads (vibeleak:review "."). The tool bodies are the exported verbs; the
;;;; local review and the API client are usable from eval on the same values
;;;; the model is shown.

(defpackage #:nodecode-vibeleak
  (:use #:cl)
  (:nicknames #:vibeleak))

(in-package #:nodecode-vibeleak)

;;; The condition every refusal is, and FAIL which signals it; with
;;; :NOT-RUNNING, *VIBELEAK*, SETTING and the not-running refusal too. It
;;; comes before any file that refuses.
(nlk:define-peripheral vibeleak :not-running t)

(export '(start-cell stop-cell review assess-php compare-findings
          normalize-domain vibeleak-error vibeleak-error-detail
          finding finding-rule finding-title finding-severity
          finding-file finding-line finding-remediation
          review-root review-findings review-coverage review-php))
