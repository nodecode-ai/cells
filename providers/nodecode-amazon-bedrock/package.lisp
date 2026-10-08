;;;; package.lisp --- the NODECODE-AMAZON-BEDROCK package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-amazon-bedrock
  (:use #:cl))

(in-package #:nodecode-amazon-bedrock)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the files between read the section through it.
(nlk:define-peripheral amazon-bedrock :not-running t)

(defparameter +provider+ "amazon-bedrock"
  "The provider id: what /connect saves a Bedrock API key under, what /models
lists models under, and what a turn's frozen config names.")

(defparameter +lane+ "amazon-bedrock"
  "The lane this cell registers: the Converse Stream wire, which none of the
core's four lanes speaks, under a name and a family of its own.")
