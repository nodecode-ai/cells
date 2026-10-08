;;;; package.lisp --- the NODECODE-ZAI-CODING-PLAN package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-zai-coding-plan
  (:use #:cl))

(in-package #:nodecode-zai-coding-plan)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral zai-coding-plan :not-running t)

(defparameter +provider+ "zai-coding-plan"
  "The provider id: what /connect saves a key under, what /models lists
models under, what a turn's frozen config names, and the auth.json
oauth_tokens entry the sign-in writes.")
