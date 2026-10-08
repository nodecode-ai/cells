;;;; package.lisp --- the NODECODE-ANTHROPIC package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-anthropic
  (:use #:cl))

(in-package #:nodecode-anthropic)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp and signin.lisp read the section through it.
;;; FAIL and ANTHROPIC-ERROR, which a sign-in that cannot go on signals, come
;;; from here too.
(nlk:define-peripheral anthropic :not-running t)

(defparameter +provider+ "anthropic"
  "The provider id the core already serves with a key, and the key the
sign-in is kept under in auth.json's oauth_tokens.")

(defparameter +key+ "nodecode-anthropic"
  "The key this cell's notices stand under: the folder's name.")
