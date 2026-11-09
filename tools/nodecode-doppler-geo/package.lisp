;;;; package.lisp --- the DOPPLER-GEO package: the two verbs and the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed DOPPLER-GEO so an eval form reads
;;;; (doppler-geo:detect "rec.wav"): the same two calls the tools make, and
;;;; the same text they answer, for the operator who would rather type one.
;;;;
;;;; Every verb answers a STRING; every refusal is DOPPLER-GEO-ERROR, which
;;;; the eval snippet and a tool result both render as "ERROR: ...: <detail>".

(defpackage #:nodecode-doppler-geo
  (:use #:cl)
  (:nicknames #:doppler-geo)
  (:export
   ;; the two verbs, each answering its report as text
   #:detect #:locate
   ;; the pieces a layer may call on their own
   #:detect-events #:read-wav #:write-wav #:read-tracks
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:doppler-geo-error))

(in-package #:nodecode-doppler-geo)

(defun as-double (x)
  "X as a double-float."
  (float x 1d0))

(nlk:define-peripheral doppler-geo :not-running t)
