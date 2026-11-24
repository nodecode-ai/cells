;;;; package.lisp --- the NODECODE-CROWBOT package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-crowbot
  (:use #:cl))

(in-package #:nodecode-crowbot)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral crowbot :not-running t)

(defparameter +provider+ "crowbot"
  "The provider id: what /connect saves a key under, what /models lists models
under, and what a turn's frozen config names.")

(defparameter +display-name+ "Crowbot"
  "What /connect and /models call it.")
