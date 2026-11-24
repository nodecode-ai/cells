;;;; package.lisp --- the NODECODE-FORTYTWO package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-fortytwo
  (:use #:cl))

(in-package #:nodecode-fortytwo)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral fortytwo :not-running t)

(defparameter +provider+ "fortytwo"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names.")
