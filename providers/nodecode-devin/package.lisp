;;;; package.lisp --- the NODECODE-DEVIN package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-devin
  (:use #:cl))

(in-package #:nodecode-devin)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral devin :not-running t)

(defparameter +provider+ "devin"
  "The provider id: what /models lists models under, what auth.json keeps the
sign-in under, the slash command, and the name of the lane the cell registers.")

(defparameter +key+ "nodecode-devin"
  "The key the cell's notices stand under: the folder's name.")
