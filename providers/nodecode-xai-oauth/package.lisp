;;;; package.lisp --- the NODECODE-XAI-OAUTH package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-xai-oauth
  (:use #:cl))

(in-package #:nodecode-xai-oauth)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral xai-oauth :not-running t)

(defparameter +provider+ "xai-oauth"
  "The provider id: what the sign-in stores its token under, what /models
lists models under, and what a turn's frozen config names.")

(defparameter +key+ "nodecode-xai-oauth"
  "The key the cell files its hooks and its notices under: the folder's name.")
