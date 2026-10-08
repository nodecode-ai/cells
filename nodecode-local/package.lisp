;;;; package.lisp --- the NODECODE-LOCAL package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-local
  (:use #:cl))

(in-package #:nodecode-local)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the files before it read the section through it.
(nlk:define-peripheral local :not-running t)

(defparameter +provider+ "local"
  "The provider id and the name of the lane it rides: what /model-aux names,
what a turn's frozen config names.")

(defparameter +npm+ "nodecode-local"
  "The package the catalog row names, which resolves to this cell's lane.")
