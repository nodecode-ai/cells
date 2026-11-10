;;;; package.lisp --- the NODECODE-OPENROUTER-EVERYTHING package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; OPENROUTER-EVERYTHING is a nickname. SETTING
;;;; comes from the NLE:DEFINE-CELL declaration in cell.lisp, which loads
;;;; last; wire.lisp and verbs.lisp read the section through it. FAIL and
;;;; OPENROUTER-EVERYTHING-ERROR come from NLK:DEFINE-PERIPHERAL: what a verb
;;;; signals when its call cannot be made.

(defpackage #:nodecode-openrouter-everything
  (:use #:cl)
  (:shadow #:search)
  (:nicknames #:openrouter-everything)
  (:export #:openrouter-everything-error
           #:openrouter-everything-error-detail
           #:decide #:extract-fields #:rerank #:embed #:search))

(in-package #:nodecode-openrouter-everything)

(nlk:define-peripheral openrouter-everything :not-running t)
