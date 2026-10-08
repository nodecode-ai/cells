;;;; package.lisp --- the NODECODE-KILO package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-kilo
  (:use #:cl))

(in-package #:nodecode-kilo)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp and signin.lisp read the section through it.
;;; FAIL and KILO-ERROR, which a sign-in that cannot go on signals, come from
;;; here too.
(nlk:define-peripheral kilo :not-running t)

(defparameter +provider+ "kilo"
  "The provider id: what /connect saves a key under, what /models lists
models under, what a turn's frozen config names, and the key the sign-in is
kept under in auth.json's oauth_tokens.")

(defparameter +key+ "nodecode-kilo"
  "The key this cell's standing notice stands under: the folder's name.")
