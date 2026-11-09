;;;; package.lisp --- the NODECODE-SABI package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it.

(defpackage #:nodecode-sabi
  (:use #:cl))

(in-package #:nodecode-sabi)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral sabi :not-running t)

(defparameter +provider+ "sabi"
  "The provider id: what /models lists models under, and what a turn's
frozen config names. Sabi routes a request upstream itself, so there is no
key to save under it.")
