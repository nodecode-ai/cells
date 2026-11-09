;;;; package.lisp --- the NODECODE-RECEIPTS package: the model-facing verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed RECEIPTS so an eval form reads
;;;; (receipts:receipts-get ...). Every refusal the model sees is
;;;; RECEIPTS-ERROR, whose detail names the thing that was wrong.

(defpackage #:nodecode-receipts
  (:use #:cl)
  (:nicknames #:receipts)
  (:export
   ;; the four verbs, called by the tools of the same name
   #:receipts-begin #:receipts-check #:receipts-get #:receipts-export
   ;; and the slash command
   #:run-slash
   ;; the one condition
   #:receipts-error))

(in-package #:nodecode-receipts)

(nlk:define-peripheral receipts :not-running t)

;;; The contract markers below are fixed by the original plugin's schema; the
;;; section's own knobs are the four in cell.lisp.
(defparameter +schema-version+ 1
  "The spec schema this cell reads.")

(defparameter +max-checks+ 20
  "The most checks one spec may carry.")

(defparameter +max-expectation+ 4096
  "The longest text a `contains' may require.")

(defparameter +max-command+ 1024
  "The longest command a test check may declare.")

(defparameter +max-path+ 4096
  "The longest path a spec may name.")

(defparameter +max-url+ 2048
  "The longest url a website check may name.")

(defparameter +max-json-keys+ 20
  "The most json_keys a file check may require.")

(defparameter +output-keep+ (* 8 1024)
  "How much command output an observation keeps; the rest is said to be cut.")

(defparameter +website-deadline+ 5
  "Seconds one website GET may take, whole.")

(defparameter +disclosures+
  (list "As-of snapshot of explicit checks, not ongoing certification."
        "Not a security sandbox or a signed attestation."
        "A test check runs the declared command here, bounded, in its workdir."
        "No telemetry: the receipts live under the nodecode home until you delete them.")
  "What every sealed receipt says about itself.")
