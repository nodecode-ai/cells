;;;; package.lisp --- the NODECODE-JINA package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; JINA is a nickname, and with it the slash command
;;;; and the manual topic. SETTING comes from the NLE:DEFINE-CELL declaration
;;;; in cell.lisp, which loads last; wire.lisp and tools.lisp read the
;;;; section through it. FAIL and JINA-ERROR come from NLK:DEFINE-PERIPHERAL:
;;;; what a tool signals when its call cannot be made.

(defpackage #:nodecode-jina
  (:use #:cl)
  (:nicknames #:jina)
  (:export #:jina-error #:jina-error-detail))

(in-package #:nodecode-jina)

(nlk:define-peripheral jina :not-running t)
