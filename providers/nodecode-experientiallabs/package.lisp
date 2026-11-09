;;;; package.lisp --- the NODECODE-EXPERIENTIALLABS package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-experientiallabs
  (:use #:cl))

(in-package #:nodecode-experientiallabs)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp and usage.lisp read the section through it.
(nlk:define-peripheral experientiallabs :not-running t)

(defparameter +provider+ "experientiallabs"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names.")
