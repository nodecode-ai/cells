;;;; package.lisp --- the NODECODE-CLINE-PASS package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-cline-pass
  (:use #:cl))

(in-package #:nodecode-cline-pass)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral cline-pass :not-running t)

(defparameter +provider+ "cline-pass"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names.")
