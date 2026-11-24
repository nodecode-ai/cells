;;;; package.lisp --- the NODECODE-MARKDOWN-NEW package: the tool and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it. SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; wire.lisp and tools.lisp read
;;;; the section through it. FAIL and MARKDOWN-NEW-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when a call cannot be made,
;;;; and what the harness hands back to the model as the text to repair from.

(defpackage #:nodecode-markdown-new
  (:use #:cl)
  (:export #:markdown-new-error))

(in-package #:nodecode-markdown-new)

(nlk:define-peripheral markdown-new :not-running t)
