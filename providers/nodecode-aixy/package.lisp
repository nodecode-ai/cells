;;;; package.lisp --- the NODECODE-AIXY package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-aixy
  (:use #:cl))

(in-package #:nodecode-aixy)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral aixy :not-running t)

(defparameter +provider+ "aixy"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names.")

(defparameter +env+ "AIXY_API_KEY"
  "The variable a project key is read from, which is the derived name for a
provider called aixy anyway.")

(defparameter +base+ "https://api.aixy-gateway.com/v1"
  "Where Aixy is served: the base the chat lane appends /chat/completions to
and the listing appends /models to.")

(defparameter +key-page+ "https://aixy-gateway.com"
  "Aixy's own page, where a project key is made.")
