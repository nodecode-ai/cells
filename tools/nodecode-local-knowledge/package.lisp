;;;; package.lisp --- the LOCAL-KNOWLEDGE package: five verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, so an eval form reads (knowledge:search "backup runbook").
;;;; Every verb answers a STRING a model reads, and every refusal is
;;;; KNOWLEDGE-ERROR, which the eval snippet renders as
;;;; "ERROR: KNOWLEDGE-ERROR: <detail>".
;;;;
;;;; The settings these verbs run on derive from the one `local_knowledge'
;;;; section declared in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :roots) and its siblings.

(defpackage #:nodecode-local-knowledge
  (:use #:cl)
  (:nicknames #:knowledge)
  (:export
   ;; verbs, every one a string
   #:search #:fetch #:neighbors #:feedback #:report
   ;; data, so a layer can reword it
   #:*primer* #:*artifact-types*
   ;; the one condition
   #:local-knowledge-error))

(in-package #:nodecode-local-knowledge)

(nlk:define-peripheral local-knowledge :not-running t)
