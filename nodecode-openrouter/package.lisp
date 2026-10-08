;;;; package.lisp --- the NODECODE-OPENROUTER package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-openrouter
  (:use #:cl))

(in-package #:nodecode-openrouter)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral openrouter :not-running t)

(defparameter +provider+ "openrouter"
  "The provider id: what /connect saves a key under, what the sign-in saves
the key it obtains under, what /models lists models under, and what a turn's
frozen config names.")
