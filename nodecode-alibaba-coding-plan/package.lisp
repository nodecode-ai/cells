;;;; package.lisp --- the NODECODE-ALIBABA-CODING-PLAN package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-alibaba-coding-plan
  (:use #:cl))

(in-package #:nodecode-alibaba-coding-plan)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral alibaba-coding-plan :not-running t)

(defparameter +provider+ "alibaba-coding-plan"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names.")
