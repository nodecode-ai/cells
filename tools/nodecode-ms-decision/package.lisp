;;;; package.lisp --- the NODECODE-MS-DECISION package: the decision verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed MS-DECISION so an eval form reads
;;;; (ms-decision:noul ...). Every failure the model sees is
;;;; MS-DECISION-ERROR, which the eval snippet renders as
;;;; "ERROR: MS-DECISION-ERROR: <detail>"; the detail never carries the key.

(defpackage #:nodecode-ms-decision
  (:use #:cl)
  (:nicknames #:ms-decision)
  (:export
   ;; the model-facing verbs, callable through eval as (ms-decision:noul ...)
   #:noul #:choice #:score #:decide
   ;; the one condition
   #:ms-decision-error))

(in-package #:nodecode-ms-decision)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; decision.lisp reads the section through it.
(nlk:define-peripheral ms-decision :not-running t :filter redact)

(defparameter +provider+ "ms-decision"
  "The provider id: what auth.json keeps a key under (api_keys.ms-decision).")

(defun redact (text)
  "TEXT with the Foundry key this cell would send replaced by [redacted]."
  (let ((key (ignore-errors (api-key))))
    (if key (uiop:frob-substrings text key "[redacted]") text)))
