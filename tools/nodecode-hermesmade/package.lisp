;;;; package.lisp --- the NODECODE-HERMESMADE package: the cell's tools, its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; HERMESMADE is a nickname, and with it the slash
;;;; command and the manual topic. SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; the other files read the
;;;; section through it. FAIL and HERMESMADE-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-hermesmade
  (:use #:cl)
  (:nicknames #:hermesmade)
  (:export #:hermesmade-error #:hermesmade-error-detail))

(in-package #:nodecode-hermesmade)

(nlk:define-peripheral hermesmade :not-running t)
