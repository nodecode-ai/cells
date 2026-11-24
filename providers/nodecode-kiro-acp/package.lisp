;;;; package.lisp --- the NODECODE-KIRO-ACP package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-kiro-acp
  (:use #:cl)
  (:export #:start-cell #:call-kiro-streaming))

(in-package #:nodecode-kiro-acp)

(nlk:define-peripheral kiro-acp :not-running t)

(defparameter +provider+ "kiro-acp"
  "The provider id and the lane's name: what /models lists models under and
what a turn's frozen config names.")

(defparameter +protocol-version+ 1
  "The Agent Client Protocol version this client speaks, the one
`kiro-cli acp' answers (verified by the original against Kiro CLI 2.22.0).")

(defparameter +tool-open+ "<tool_call>"
  "The tag the prompt asks for a tool call in, as the plugin this rewrites
asks: the CLI has no client-supplied tool list on a session.")

(defparameter +tool-close+ "</tool_call>"
  "The tag that closes one.")

(defparameter +default-models+ (vector "default")
  "What /models offers before the section names the account's own models:
`default' is the agent's session model, which the original selected by
sending no model at all. What a Kiro account has is what its CLI lists.")
