;;;; package.lisp --- the NODECODE-JEV package: the model-facing verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed JEV so an eval form reads (jev:ask ...).
;;;; Every failure the model sees is JEV-ERROR, which the eval snippet renders
;;;; as "ERROR: JEV-ERROR: <detail>"; the detail never carries a key.

(defpackage #:nodecode-jev
  (:use #:cl)
  (:nicknames #:jev)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:ask #:triage #:next-step #:relevance #:presets #:status
   ;; the one condition
   #:jev-error))

(in-package #:nodecode-jev)

;;; *JEV*, SETTING, RUNNING-SETTINGS, FAIL and DEFINE-VERB come from here and
;;; from the NLE:DEFINE-CELL declaration in cell.lisp, which loads last. Keys
;;; are cleaned out of every refusal: a token must never reach the model.
(nlk:define-peripheral jev :not-running t :filter redact)

(declaim (ftype (function () list) all-api-keys))

(defun redact (text)
  "TEXT with every key this cell would send replaced by [redacted]."
  (uiop:frob-substrings text (ignore-errors (all-api-keys)) "[redacted]"))
