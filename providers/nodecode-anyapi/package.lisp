;;;; package.lisp --- the NODECODE-ANYAPI package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-anyapi
  (:use #:cl))

(in-package #:nodecode-anyapi)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp and cell.lisp read the section through it.
(nlk:define-peripheral anyapi :not-running t)

(defparameter +provider+ "anyapi"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names. The core's own
<ID>_API_KEY ladder reads ANYAPI_API_KEY from it.")
