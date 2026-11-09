;;;; package.lisp --- the NODECODE-AGENT-BATCH package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it. SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; plan.lisp and github.lisp
;;;; read the section through it. FAIL and AGENT-BATCH-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL and are what a verb says when its call cannot be
;;;; built at all.

(defpackage #:nodecode-agent-batch
  (:use #:cl)
  (:export #:agent-batch-error #:agent-batch-error-detail))

(in-package #:nodecode-agent-batch)

(nlk:define-peripheral agent-batch :not-running t :lock "agent-batch")
