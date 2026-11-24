;;;; package.lisp --- the ARTIFACTS package, its condition and its constants.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A package named after the system, which is how the folder loader finds
;;;; START-CELL in it. NLK:DEFINE-PERIPHERAL gives it the condition
;;;; ARTIFACTS-ERROR, FAIL, the settings variable *ARTIFACTS* and
;;;; RUNNING-SETTINGS.

(defpackage #:nodecode-artifacts
  (:use #:cl)
  (:export #:artifacts-error))

(in-package #:nodecode-artifacts)

(nlk:define-peripheral artifacts :not-running t)

(defparameter +kinds+ '("html" "markdown" "svg" "png" "webp")
  "What an artifact may be: the service's own five formats.")

(defparameter +text-kinds+ '("html" "markdown" "svg")
  "The kinds the service can hand back as text, so a read may show a preview.")

(defparameter +kinds-by-extension+
  '(("html" . "html") ("htm" . "html")
    ("md" . "markdown") ("markdown" . "markdown")
    ("svg" . "svg") ("png" . "png") ("webp" . "webp"))
  "A file's kind, read off its extension as the original read it.")

(defparameter +media-types+
  '(("html" . "text/html; charset=utf-8")
    ("markdown" . "text/markdown; charset=utf-8")
    ("svg" . "image/svg+xml")
    ("png" . "image/png")
    ("webp" . "image/webp"))
  "The content type each kind travels as.")

(defparameter +max-upload-bytes+ (* 10 1024 1024)
  "One artifact's bytes: the service's default ceiling, checked before the wire.")

(defparameter +preview-chars+ 4000
  "How much of a text artifact artifact_get shows.")

(defparameter +max-hours+ 8760
  "The longest life artifact_share gives a guest link: the service's own cap.")

(defparameter +default-token-variable+ "HERMES_ARTIFACTS_TOKEN"
  "The variable the original read its machine credential from, and the
default of the section's token_env.")
