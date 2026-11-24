;;;; package.lisp --- the NODECODE-MODULAR package.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-modular
  (:use #:cl))

(in-package #:nodecode-modular)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral modular :not-running t)

(defparameter +provider+ "modular"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names.")
