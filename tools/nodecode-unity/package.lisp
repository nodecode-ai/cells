;;;; package.lisp --- the NODECODE-UNITY package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; UNITY is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it. FAIL and UNITY-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made,
;;;; and the detail is the editor's own words, which is what the model repairs
;;;; from.

(defpackage #:nodecode-unity
  (:use #:cl)
  (:nicknames #:unity)
  (:export #:unity-error #:unity-error-detail))

(in-package #:nodecode-unity)

(nlk:define-peripheral unity :not-running t)
