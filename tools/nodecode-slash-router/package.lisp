;;;; package.lisp --- the NODECODE-SLASH-ROUTER package: the model-facing verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed SLASH-ROUTER so an eval form reads
;;;; (slash-router:route "/modles"). Every failure the model sees is
;;;; SLASH-ROUTER-ERROR, which the eval snippet renders as
;;;; "ERROR: SLASH-ROUTER-ERROR: <detail>"; the detail never carries the key.

(defpackage #:nodecode-slash-router
  (:use #:cl)
  (:nicknames #:slash-router)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:route #:remember #:forget #:status
   ;; the one condition
   #:slash-router-error))

(in-package #:nodecode-slash-router)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the verbs below read the section through it.
(nlk:define-peripheral slash-router :not-running t :filter redact)

(defparameter +provider+ "typesafe"
  "The provider id: what auth.json keeps a key under (api_keys.typesafe).")

(declaim (ftype (function (&optional t) t) api-keys))

(defun redact (text)
  "TEXT with every key this cell would send replaced by [redacted]."
  (uiop:frob-substrings text (ignore-errors (api-keys)) "[redacted]"))
