;;;; package.lisp --- the NODECODE-NGROK-AI-GATEWAY package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-ngrok-ai-gateway
  (:use #:cl))

(in-package #:nodecode-ngrok-ai-gateway)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral ngrok-ai-gateway :not-running t)

(defparameter +provider+ "ngrok"
  "The provider id: what /connect saves a key under, what a round's frozen
config names, and what a turn is named `ngrok/<model>' after. The original
module's profile name, so `--provider ngrok' keeps meaning the gateway.")
