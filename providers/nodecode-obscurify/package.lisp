;;;; package.lisp --- the NODECODE-OBSCURIFY package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it.

(defpackage #:nodecode-obscurify
  (:use #:cl))

(in-package #:nodecode-obscurify)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp and cell.lisp read the section through it.
(nlk:define-peripheral obscurify :not-running t)

(defparameter +provider+ "obscurify"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names. The core's own
<ID>_API_KEY ladder reads OBSCURIFY_API_KEY from it.")
