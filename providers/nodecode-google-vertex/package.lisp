;;;; package.lisp --- the NODECODE-GOOGLE-VERTEX package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-google-vertex
  (:use #:cl))

(in-package #:nodecode-google-vertex)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp and adc.lisp read the section through it.
(nlk:define-peripheral google-vertex :not-running t)

(defparameter +provider+ "google-vertex"
  "The provider id: what /connect saves an API key under, what /models lists
models under, and what a turn's frozen config names.")
