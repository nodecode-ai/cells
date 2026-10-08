;;;; package.lisp --- the NODECODE-OLLAMA-CLOUD package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-ollama-cloud
  (:use #:cl))

(in-package #:nodecode-ollama-cloud)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; provider.lisp reads the section through it.
(nlk:define-peripheral ollama-cloud :not-running t)

(defparameter +provider+ "ollama-cloud"
  "The provider id and the name of the lane it rides: what /connect saves a
key under, what /models lists models under, and what a turn's frozen config
names.")

(defparameter +npm+ "nodecode-ollama-cloud"
  "The package the catalog row names, which resolves to this cell's lane.")
