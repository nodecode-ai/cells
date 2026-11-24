;;;; package.lisp --- the NODECODE-MACHINE-LIBRARY package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; MACHINE-LIBRARY is a nickname. SETTING comes
;;;; from the NLE:DEFINE-CELL declaration in cell.lisp, which loads last;
;;;; wire.lisp and tools.lisp read the section through it. FAIL and
;;;; MACHINE-LIBRARY-ERROR come from NLK:DEFINE-PERIPHERAL: what a tool
;;;; signals when its call cannot be made.

(defpackage #:nodecode-machine-library
  (:use #:cl)
  (:nicknames #:machine-library)
  (:export #:machine-library-error #:machine-library-error-detail))

(in-package #:nodecode-machine-library)

(nlk:define-peripheral machine-library :not-running t)
