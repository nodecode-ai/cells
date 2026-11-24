;;;; package.lisp --- the FRIHET package: the checks, and the one call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed FRIHET so an eval form reads
;;;; (frihet:doctor) or (frihet:call-operation "list_invoices"): the same
;;;; calls /frihet makes, answering the same text.

(defpackage #:nodecode-frihet
  (:use #:cl)
  (:nicknames #:frihet)
  (:export
   ;; the checks and the one verb, each answering a string
   #:status #:setup #:doctor #:operations #:call-operation
   ;; the classifier, so a layer can read what the gate decides
   #:classify-operation
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:frihet-error))

(in-package #:nodecode-frihet)

(nlk:define-peripheral frihet :not-running t)
