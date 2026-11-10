;;;; package.lisp --- the NODECODE-DEVELOPH package: the cell's condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; DEVELOPH is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it. FAIL and DEVELOPH-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-developh
  (:use #:cl)
  (:nicknames #:developh)
  (:export #:developh-error #:developh-error-detail))

(in-package #:nodecode-developh)

(nlk:define-peripheral developh :not-running t)
