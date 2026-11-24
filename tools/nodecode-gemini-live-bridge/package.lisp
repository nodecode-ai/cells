;;;; package.lisp --- the NODECODE-GEMINI-LIVE-BRIDGE package: one Live conversation and its catalog.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it. The folder is a Nodecode rewrite of cruzlxyz's
;;;; gemini-live-bridge (see NOTICE): the original's desktop minibar, its
;;;; list-models filter and its ephemeral-token dial, reached here as tools.
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives the condition GEMINI-LIVE-BRIDGE-ERROR and FAIL,
;;;; which every verb signals through; :NOT-RUNNING says the tools cannot
;;;; answer before the cell starts.

(defpackage #:nodecode-gemini-live-bridge
  (:use #:cl)
  (:export #:gemini-live-bridge-error))

(in-package #:nodecode-gemini-live-bridge)

(nlk:define-peripheral gemini-live-bridge :not-running t)
