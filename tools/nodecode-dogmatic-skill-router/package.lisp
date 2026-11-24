;;;; package.lisp --- the NODECODE-DOGMATIC-SKILL-ROUTER package: the verbs a model
;;;; or the operator calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed DOGMATIC-SKILL-ROUTER so an eval form
;;;; reads (dogmatic-skill-router:route "..."). Every failure the model sees is
;;;; DOGMATIC-SKILL-ROUTER-ERROR; its detail never carries the key.

(defpackage #:nodecode-dogmatic-skill-router
  (:use #:cl)
  (:nicknames #:dogmatic-skill-router)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:route #:skills
   ;; the one condition
   #:dogmatic-skill-router-error))

(in-package #:nodecode-dogmatic-skill-router)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the files between read the section through it.
(nlk:define-peripheral dogmatic-skill-router :not-running t :filter redact)

(defun redact (text)
  "TEXT with every key this cell would send replaced by [redacted]."
  (uiop:frob-substrings text (ignore-errors (keys-in-hand)) "[redacted]"))
