;;;; package.lisp --- the NODECODE-LIGHTNING-AI package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it. The short name is the word every contribution is filed
;;;; under: the slash command, the manual topic and the folder's own key.

(defpackage #:nodecode-lightning-ai
  (:use #:cl))

(in-package #:nodecode-lightning-ai)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral lightning-ai :not-running t)

(defparameter +provider+ "lightningai"
  "The provider id: what /connect saves a key under, what /models lists models
under, and what a turn's frozen config names. The original's own spelling, so
a key already saved under it answers.")
