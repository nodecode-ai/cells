;;;; package.lisp --- one package, named after the system.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; NLK:DEFINE-PERIPHERAL declares EDINET-ERROR, FAIL, *EDINET* and
;;;; RUNNING-SETTINGS here, before the files that reach for them load: every
;;;; verb this cell answers signals EDINET-ERROR while the cell is stopped, and
;;;; that is the refusal the model reads instead of a stack trace.

(defpackage #:nodecode-edinet
  (:nicknames #:edinet)
  (:use #:cl)
  (:export #:edinet-error
           #:search-filings
           #:financials
           #:save-document
           #:run-command))

(in-package #:nodecode-edinet)

(nlk:define-peripheral edinet :not-running t)
