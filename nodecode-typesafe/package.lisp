;;;; package.lisp --- the NODECODE-TYPESAFE package: the model-facing verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed TYPESAFE so an eval form reads
;;;; (typesafe:judge ...). Every failure the model sees is TYPESAFE-ERROR,
;;;; which the eval snippet renders as "ERROR: TYPESAFE-ERROR: <detail>"; the
;;;; detail never carries the key.

(defpackage #:nodecode-typesafe
  (:use #:cl)
  (:nicknames #:typesafe)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:judge #:models
   ;; the one condition
   #:typesafe-error))

(in-package #:nodecode-typesafe)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; judge.lisp reads the section through it.
(nlk:define-peripheral typesafe :not-running t :filter redact)

(defparameter +provider+ "typesafe"
  "The provider id: what auth.json keeps a key under (api_keys.typesafe).")

(declaim (ftype (function (&optional t) t) api-keys))

(defun redact (text)
  "TEXT with every key this cell would send replaced by [redacted]."
  (uiop:frob-substrings text (ignore-errors (api-keys)) "[redacted]"))
