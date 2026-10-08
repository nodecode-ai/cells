;;;; package.lisp --- the NODECODE-SNOWFLAKE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-snowflake
  (:use #:cl))

(in-package #:nodecode-snowflake)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp and signin.lisp read the section through it.
(nlk:define-peripheral snowflake :not-running t)

(defparameter +provider+ "snowflake"
  "The provider id: what /connect saves a PAT under, what the sign-in is kept
under in auth.json's oauth_tokens, what /models lists models under, and what
a turn's frozen config names.")

(defparameter +key+ "nodecode-snowflake"
  "The key this cell's notices stand under: the folder's name.")
