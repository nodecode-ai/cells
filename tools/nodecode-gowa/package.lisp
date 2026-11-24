;;;; package.lisp --- the NODECODE-GOWA package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it. SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; client.lisp reads the section
;;;; through it. FAIL and GOWA-ERROR come from NLK:DEFINE-PERIPHERAL and are
;;;; what a verb says when its call cannot be built at all.

(defpackage #:nodecode-gowa
  (:use #:cl)
  (:export #:gowa-error #:gowa-error-detail))

(in-package #:nodecode-gowa)

(nlk:define-peripheral gowa :not-running t)
