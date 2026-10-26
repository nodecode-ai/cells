;;;; package.lisp --- the NODECODE-OPENCODE-GO package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-opencode-go
  (:use #:cl))

(in-package #:nodecode-opencode-go)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral opencode-go :not-running t)

(defparameter +provider+ "opencode-go"
  "The provider id: models.dev's, what /connect saves a key under, what
/models lists models under, and what a turn's frozen config names.")
