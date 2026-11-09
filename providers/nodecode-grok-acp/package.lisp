;;;; package.lisp --- the NODECODE-GROK-ACP package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-grok-acp
  (:use #:cl)
  (:export #:start-cell #:call-grok-streaming))

(in-package #:nodecode-grok-acp)

(nlk:define-peripheral grok-acp :not-running t)

(defparameter +provider+ "grok-acp"
  "The provider id and the lane's name: what /models lists models under and
what a turn's frozen config names.")

(defparameter +protocol-version+ 1
  "The Agent Client Protocol version this client speaks, the one grok's
`agent stdio' answers.")

(defparameter +tool-open+ "<tool_call>"
  "The tag the prompt asks for a tool call in, as the plugin this rewrites
asks: the CLI has no client-supplied tool list on a session.")

(defparameter +tool-close+ "</tool_call>"
  "The tag that closes one.")

(defparameter +default-models+
  (vector "grok-4.7" "grok-4.6" "grok-4.5")
  "The roster a grok 1.0.41 account advertised to the original; `session/new'
lists what an account actually has, and the section may name others.")
