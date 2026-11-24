;;;; package.lisp --- the NODECODE-APINEX package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-apinex
  (:use #:cl))

(in-package #:nodecode-apinex)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral apinex :not-running t)

(defparameter +providers+ '("apinex" "apinex-openai")
  "The two provider ids the original registers, one per wire: Claude over the
Messages API, everything else over the OpenAI-compatible one.")

(defparameter +anthropic-provider+ "apinex"
  "The provider id that carries the Claude family over /v1/messages.")

(defparameter +openai-provider+ "apinex-openai"
  "The provider id that carries the rest over /v1/chat/completions.")

(defparameter +env+ '("APINEX_API_KEY")
  "The environment variables an APInex key is read from, in order; both
providers take the one key.")

(defparameter +key-page+ "https://apinex.bond/register"
  "Where a key is made.")
