;;;; package.lisp --- the NODECODE-AGENT-BUILDER package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed AGENT-BUILDER so an eval form reads
;;;; (agent-builder:build "a brief"). It uses only CL: every name below is the
;;;; model's or the operator's verb, and none of them shadows a CL symbol.

(defpackage #:nodecode-agent-builder
  (:use #:cl)
  (:nicknames #:agent-builder)
  (:export #:drafts #:show #:build #:refine #:trial #:compare #:export-blueprint
           #:drop #:agent-builder-error))

(in-package #:nodecode-agent-builder)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; blueprint.lisp, store.lisp and cell.lisp read the section
;;; through it. FAIL and AGENT-BUILDER-ERROR, which every refusal signals,
;;; and the running-settings refusal come from here.
(nlk:define-peripheral agent-builder :not-running t)
