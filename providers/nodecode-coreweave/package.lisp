;;;; package.lisp --- the NODECODE-COREWEAVE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-coreweave
  (:use #:cl))

(in-package #:nodecode-coreweave)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral coreweave :not-running t)

(defparameter +provider+ "coreweave"
  "The provider id: what /connect saves a key under, what /models lists models
under, and what a turn's frozen config names.")
