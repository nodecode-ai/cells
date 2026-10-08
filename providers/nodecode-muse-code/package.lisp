;;;; package.lisp --- the NODECODE-MUSE-CODE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-muse-code
  (:use #:cl))

(in-package #:nodecode-muse-code)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral muse-code :not-running t)

(defparameter +provider+ "muse-code"
  "The provider id: what the sign-in stores its token under, what /models
lists models under, and what a turn's frozen config names.")

(defparameter +key+ "nodecode-muse-code"
  "The key the cell files its hooks and its notices under: the folder's name.")
