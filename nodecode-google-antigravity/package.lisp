;;;; package.lisp --- the NODECODE-GOOGLE-ANTIGRAVITY package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-google-antigravity
  (:use #:cl))

(in-package #:nodecode-google-antigravity)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral google-antigravity :not-running t)

(defparameter +provider+ "google-antigravity"
  "The provider id: what /models lists models under, what auth.json keeps the
sign-in under, the slash command, and the name of the lane the cell registers.")

(defparameter +key+ "nodecode-google-antigravity"
  "The key the cell's notices stand under: the folder's name.")
