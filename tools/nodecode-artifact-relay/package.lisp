;;;; package.lisp --- the ARTIFACT-RELAY package and its one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A package named after the system, which is how the folder loader finds
;;;; START-CELL in it. NLK:DEFINE-PERIPHERAL gives it the condition
;;;; ARTIFACT-RELAY-ERROR, FAIL, the settings variable *ARTIFACT-RELAY*,
;;;; RUNNING-SETTINGS, and DEFINE-VERB, whose functions refuse while the
;;;; cell is not running.

(defpackage #:nodecode-artifact-relay
  (:use #:cl)
  (:export #:artifact-relay-error))

(in-package #:nodecode-artifact-relay)

(nlk:define-peripheral artifact-relay :not-running t)

(defparameter +control-plane+ "https://relay.lok-labs.com"
  "The hosted control plane the original defaulted to: where /artifact-relay
setup asks for a device code, and where a finished sign-in learns which
service the new token belongs to.")

(defparameter +token-variable+ "ARTIFACT_RELAY_API_TOKEN"
  "The variable the original read its sole secret from, and the default of
the section's token_env.")

(defparameter +max-content-bytes+ (* 5 1024 1024)
  "One artifact's source may not pass five mebibytes -- the relay's own cap.")

(defparameter +max-title+ 200
  "A title's ceiling: the relay's form takes a short line here.")

(defparameter +max-summary+ 1000
  "A summary's ceiling.")

(defparameter +max-expires-days+ 3650
  "The longest life a published artifact may be given, in days.")

(defparameter +formats+ '("markdown" "html")
  "What a publish may say its content is.")

(defparameter +min-id-length+ 22
  "The shortest artifact id the relay mints, and the longest:")
(defparameter +max-id-length+ 64
  "see +MIN-ID-LENGTH+.")
