;;;; package.lisp --- the NODECODE-COLAB package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; COLAB is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp
;;;; reads the section through it. FAIL and COLAB-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-colab
  (:use #:cl)
  (:nicknames #:colab)
  (:export #:colab-error #:colab-error-detail #:runtime-gone))

(in-package #:nodecode-colab)

(nlk:define-peripheral colab :not-running t)
