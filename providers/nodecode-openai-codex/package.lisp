;;;; package.lisp --- the NODECODE-OPENAI-CODEX package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-openai-codex
  (:use #:cl))

(in-package #:nodecode-openai-codex)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp and signin.lisp read the section through it.
(nlk:define-peripheral openai-codex :not-running t)

(defparameter +provider+ "openai-codex"
  "The provider id: what /models lists the Codex models under, what a turn's
frozen config names, and the key the sign-in is kept under in auth.json's
oauth_tokens.")

(defparameter +lane+ "openai-codex"
  "The lane this cell registers: the Responses fold under a name and a family
of its own, so the Codex backend's request is shaped here and no
openai-family credential hook mistakes it for an OpenAI API round.")

(defparameter +key+ "nodecode-openai-codex"
  "The key this cell's notices stand under: the folder's name.")
