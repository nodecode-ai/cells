;;;; package.lisp --- the NODECODE-POSTEL package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; POSTEL is a nickname, and with it the slash
;;;; command and the manual topic. SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; wire.lisp and tools.lisp read
;;;; the section through it. FAIL and POSTEL-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-postel
  (:use #:cl)
  (:nicknames #:postel)
  (:export #:postel-error #:postel-error-detail))

(in-package #:nodecode-postel)

(nlk:define-peripheral postel :not-running t)
