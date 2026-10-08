;;;; package.lisp --- the NODECODE-GITHUB-COPILOT package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-github-copilot
  (:use #:cl))

(in-package #:nodecode-github-copilot)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral github-copilot :not-running t)

(defparameter +provider+ "github-copilot"
  "The provider id: what /connect saves a key under, what /models lists
models under, what auth.json keeps the sign-in under, and the slash command.")

(defparameter +key+ "nodecode-github-copilot"
  "The key the cell's notices stand under: the folder's name.")
