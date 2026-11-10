;;;; package.lisp --- the NODECODE-JP-CORPORATE package: the two tools and the wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; JP-CORPORATE is a nickname, and it is also the
;;;; slash command and the manual topic. SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; normalize.lisp, gbiz.lisp and
;;;; tools.lisp read the section through it. FAIL and JP-CORPORATE-ERROR come
;;;; from NLK:DEFINE-PERIPHERAL: what a tool signals when the API refuses, the
;;;; input is malformed, or the cell is not running.

(defpackage #:nodecode-jp-corporate
  (:use #:cl)
  (:nicknames #:jp-corporate)
  (:export #:corp-search #:corp-profile
           #:jp-corporate-error #:jp-corporate-error-detail))

(in-package #:nodecode-jp-corporate)

(nlk:define-peripheral jp-corporate :not-running t)
