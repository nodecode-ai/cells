;;;; package.lisp --- the NODECODE-ABSTRACT package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-abstract
  (:use #:cl))

(in-package #:nodecode-abstract)

(defparameter +provider+ "abstract"
  "The provider id: what /connect saves a key under, what /models lists its
models under, and what a turn's frozen config names.")

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral abstract :not-running t)
