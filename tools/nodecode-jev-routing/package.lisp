;;;; package.lisp --- the NODECODE-JEV-ROUTING package: the model-facing verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed JEV so an eval form reads
;;;; (jev-routing:route "...").

(defpackage #:nodecode-jev-routing
  (:use #:cl)
  (:nicknames #:jev-routing)
  (:export
   ;; the vocabulary the manual in cell.lisp teaches
   #:route #:judge #:spend #:record
   ;; the one condition, and the name it is declared under
   #:jev-error #:jev-routing-error))

(in-package #:nodecode-jev-routing)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; every file between reads the section through it.
(nlk:define-peripheral jev-routing :not-running t :filter redact)

(deftype jev-error ()
  "Every refusal the model sees: a missing key, no routing table, a bad answer."
  'jev-routing-error)

(defparameter +provider+ "typesafe"
  "The provider id: what auth.json keeps a key under (api_keys.typesafe).")

(defparameter +last+ nil
  "The last routing pass: (:prompt ... :analysis ... :decision ...), for /jev-routing.")

(declaim (ftype (function (&optional t) t) api-keys))

(defun redact (text)
  "TEXT with every key this cell would send replaced by [redacted]."
  (uiop:frob-substrings text (ignore-errors (api-keys)) "[redacted]"))
