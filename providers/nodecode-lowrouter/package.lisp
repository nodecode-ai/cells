;;;; package.lisp --- the NODECODE-LOWROUTER package.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-lowrouter
  (:use #:cl))

(in-package #:nodecode-lowrouter)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral lowrouter :not-running t)

(defparameter +provider+ "lowrouter"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names.")
