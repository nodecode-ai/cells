;;;; package.lisp --- the NODECODE-NETMIND package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-netmind
  (:use #:cl))

(in-package #:nodecode-netmind)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral netmind :not-running t)

(defparameter +provider+ "netmind"
  "The provider id: what /connect saves a key under, what /models lists
models under, and what a turn's frozen config names.")

(defparameter +env+ "NETMIND_API_KEY"
  "The variable a key is read from, which is the derived name for a provider
called netmind anyway: plugin.yaml names the same one.")
