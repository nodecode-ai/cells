;;;; package.lisp --- the PARCEL package: one refusal, its code, the client.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed PARCEL so an eval form reads (parcel:budget).
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package its condition PARCEL-ERROR,
;;;; FAIL, the settings variable *PARCEL* and RUNNING-SETTINGS: every
;;;; refusal names what to fix, and a verb called while the cell is stopped
;;;; says so.

(defpackage #:nodecode-parcel
  (:use #:cl)
  (:nicknames #:parcel)
  (:export
   ;; the refusal every verb signals, and the code it carries
   #:parcel-error
   #:parcel-failure
   #:parcel-failure-code
   ;; data, so a layer can reword it
   #:*primer*
   #:*slash-help*
   ;; one tracking number in the spelling two sources share
   #:normalize-tracking))

(in-package #:nodecode-parcel)

(nlk:define-peripheral parcel :not-running t)

(nlk:define-error parcel-failure (parcel-error)
  ((code :initform "error"))
  (:report "~a" detail)
  (:documentation "A refusal that also carries a CODE, so a caller can tell a
spent rate limit from a rejected key without reading the sentence."))

(defun fail-as (code format-control &rest arguments)
  "Signal PARCEL-FAILURE of CODE with a formatted detail."
  (error 'parcel-failure :code code :detail (apply #'format nil format-control arguments)))
