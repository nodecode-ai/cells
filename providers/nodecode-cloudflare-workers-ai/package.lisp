;;;; package.lisp --- the NODECODE-CLOUDFLARE-WORKERS-AI package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-cloudflare-workers-ai
  (:use #:cl))

(in-package #:nodecode-cloudflare-workers-ai)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral cloudflare-workers-ai :not-running t)

(defparameter +provider+ "cloudflare-workers-ai"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names. It is the id Nodecode's
own privacy list already carries for Cloudflare's Workers AI endpoint.")
