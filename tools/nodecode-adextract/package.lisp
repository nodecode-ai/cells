;;;; package.lisp --- the NODECODE-ADEXTRACT package: the MCP client and the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; ADEXTRACT is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it. FAIL and ADEXTRACT-ERROR come
;;;; from NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be
;;;; made.

(defpackage #:nodecode-adextract
  (:use #:cl)
  (:nicknames #:adextract)
  (:export #:adextract-error #:adextract-error-detail))

(in-package #:nodecode-adextract)

(nlk:define-peripheral adextract :not-running t)
