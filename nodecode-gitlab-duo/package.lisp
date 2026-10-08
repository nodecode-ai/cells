;;;; package.lisp --- the NODECODE-GITLAB-DUO package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-gitlab-duo
  (:use #:cl))

(in-package #:nodecode-gitlab-duo)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral gitlab-duo :not-running t)

(defparameter +provider+ "gitlab-duo"
  "The provider id: what /models lists models under, what a turn's frozen
config names, and the auth.json oauth_tokens entry the sign-in writes.")

(defparameter +key+ "nodecode-gitlab-duo"
  "The key this cell's notices stand under.")
