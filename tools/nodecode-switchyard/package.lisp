;;;; package.lisp --- the NODECODE-SWITCHYARD package: the model-facing verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed SWITCHYARD so an eval form reads
;;;; (switchyard:route ...). Every failure the model sees is SWITCHYARD-ERROR,
;;;; which the eval snippet renders as "ERROR: SWITCHYARD-ERROR: <detail>";
;;;; the detail never carries the key.

(defpackage #:nodecode-switchyard
  (:use #:cl)
  (:nicknames #:switchyard)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:route #:skills
   ;; the one condition
   #:switchyard-error))

(in-package #:nodecode-switchyard)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; route.lisp reads the section through it.
(nlk:define-peripheral switchyard :not-running t :filter redact)

(defun redact (text)
  "TEXT with every key this cell would send replaced by [redacted]."
  (uiop:frob-substrings text (ignore-errors (api-keys)) "[redacted]"))
