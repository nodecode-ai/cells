;;;; package.lisp --- the NODECODE-OPENALEX package, and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; OPENALEX is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; every file
;;;; before it reads the section through SETTING. FAIL and OPENALEX-ERROR come
;;;; from NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be
;;;; made.

(defpackage #:nodecode-openalex
  (:use #:cl)
  (:nicknames #:openalex)
  (:export #:openalex-error #:openalex-error-detail))

(in-package #:nodecode-openalex)

(nlk:define-peripheral openalex :not-running t)

(define-condition openalex-not-found (openalex-error) ()
  (:documentation "A 404 from the API: the identifier named no record. A child of
OPENALEX-ERROR, so a caller that turns every failure into one line still catches
it; OPENALEX-GET catches this one first, to list the identifiers that were missing."))
