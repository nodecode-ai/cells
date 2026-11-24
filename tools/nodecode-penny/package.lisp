;;;; package.lisp --- the NODECODE-PENNY package: the MCP client and the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; PENNY is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it. FAIL and PENNY-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-penny
  (:use #:cl)
  (:nicknames #:penny)
  (:export #:penny-error #:penny-error-detail
           #:penny-profile #:penny-search #:penny-remember))

(in-package #:nodecode-penny)

(nlk:define-peripheral penny :not-running t)
