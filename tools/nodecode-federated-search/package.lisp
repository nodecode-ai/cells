;;;; package.lisp --- the NODECODE-FEDERATED-SEARCH package: the model's verb and its condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; FEDERATED-SEARCH is a nickname, the slug this
;;;; cell's command and manual topic take. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; lines.lisp and
;;;; search.lisp read the section through it. FAIL and
;;;; FEDERATED-SEARCH-ERROR come from NLK:DEFINE-PERIPHERAL: what a tool or a
;;;; command signals when its call cannot be made.

(defpackage #:nodecode-federated-search
  (:use #:cl)
  (:nicknames #:federated-search)
  (:export #:federated-search-error #:federated-search-error-detail))

(in-package #:nodecode-federated-search)

(nlk:define-peripheral federated-search :not-running t)
