;;;; package.lisp --- the GBrain pointer package and its one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of praggybuilds/hermes-gbrain-pointer (see NOTICE).
;;;;
;;;; One package, nicknamed GBRAIN-POINTER so an eval form reads
;;;; (gbrain-pointer:recall "what did we settle about the schema"). Every verb
;;;; answers a string; every refusal is GBRAIN-POINTER-ERROR, which the tool
;;;; renders as "ERROR: GBRAIN-POINTER-ERROR: <detail>" and /gbrain-pointer
;;;; prints as a sentence.
;;;;
;;;; The settings this cell runs on are NOT declared here: they derive from the
;;;; one gbrain-pointer section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :mcp-url), and *GBRAIN-POINTER* holds them while the
;;;; cell runs.

(defpackage #:nodecode-gbrain-pointer
  (:use #:cl)
  (:nicknames #:gbrain-pointer)
  (:export
   ;; the one condition
   #:gbrain-pointer-error
   ;; the verb the model, the operator and an eval snippet all reach
   #:recall))

(in-package #:nodecode-gbrain-pointer)
