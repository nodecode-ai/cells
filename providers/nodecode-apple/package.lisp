;;;; package.lisp --- the NODECODE-APPLE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-apple
  (:use #:cl))

(in-package #:nodecode-apple)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the files before it read the section through it.
(nlk:define-peripheral apple :not-running t)

(defparameter +provider+ "apple"
  "The provider id and the name of the lane it rides.")

(defparameter +npm+ "nodecode-apple"
  "The package the catalog row names, which resolves to this cell's lane.")

(defparameter +key+ "nodecode-apple"
  "The key this cell's notices stand under: the folder's name.")
