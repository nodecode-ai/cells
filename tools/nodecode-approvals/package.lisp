;;;; package.lisp --- the NODECODE-APPROVALS package: the model-facing verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed APPROVALS so an eval form reads
;;;; (approvals:review "rm -rf build"). Every failure the model sees is
;;;; APPROVALS-ERROR, which the eval snippet renders as
;;;; "ERROR: APPROVALS-ERROR: <detail>"; the detail never carries the key.

(defpackage #:nodecode-approvals
  (:use #:cl)
  (:nicknames #:approvals)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:review #:decide #:decisions
   ;; the one condition
   #:approvals-error))

(in-package #:nodecode-approvals)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; judge.lisp reads the section through it.
(nlk:define-peripheral approvals :not-running t :filter redact)

(defparameter +provider+ "typesafe"
  "The provider id: what auth.json keeps a key under (api_keys.typesafe).")

(declaim (ftype (function (&optional t) t) api-keys))

(defun redact (text)
  "TEXT with every key this cell would send replaced by [redacted]."
  (uiop:frob-substrings text (ignore-errors (api-keys)) "[redacted]"))
