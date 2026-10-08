;;;; package.lisp --- the NODECODE-CLOUDFLARE-AI-GATEWAY package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-cloudflare-ai-gateway
  (:use #:cl))

(in-package #:nodecode-cloudflare-ai-gateway)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral cloudflare-ai-gateway :not-running t)

(defparameter +provider+ "cloudflare-ai-gateway"
  "The provider id: what /connect saves a token under, what /models lists
models under, and what a turn's frozen config names.")
