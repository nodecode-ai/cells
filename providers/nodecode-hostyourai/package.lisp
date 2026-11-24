;;;; package.lisp --- the NODECODE-HOSTYOURAI package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it. Its nickname is the folder's short name, and the
;;;; slash command and the manual topic are spelled with that same word.

(defpackage #:nodecode-hostyourai
  (:use #:cl)
  (:nicknames #:hostyourai))

(in-package #:nodecode-hostyourai)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral hostyourai :not-running t)

(defparameter +provider+ "hostyourai"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names.")
