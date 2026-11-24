;;;; package.lisp --- the NODECODE-DEADBOLT package: a gate on one new text file.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed DEADBOLT so an eval form reads
;;;; (deadbolt:prepare "report.md" "..."). Every refusal the model sees is
;;;; DEADBOLT-ERROR, which the eval snippet renders as
;;;; "ERROR: DEADBOLT-ERROR: <detail>"; the detail never carries the
;;;; admission credential.

(defpackage #:nodecode-deadbolt
  (:use #:cl)
  (:nicknames #:deadbolt)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:prepare #:commit #:pending
   ;; the one condition
   #:deadbolt-error))

(in-package #:nodecode-deadbolt)

(nlk:define-peripheral deadbolt :not-running t)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; REVIEW.LISP reads the section through it.
