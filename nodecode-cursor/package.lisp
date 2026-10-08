;;;; package.lisp --- the NODECODE-CURSOR package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-cursor
  (:use #:cl))

(in-package #:nodecode-cursor)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral cursor :not-running t)

(defparameter +provider+ "cursor"
  "The provider id: what /models lists models under, what auth.json keeps the
sign-in under, the slash command, and the name of the lane the cell registers.")

(defparameter +key+ "nodecode-cursor"
  "The key the cell's notices stand under: the folder's name.")
