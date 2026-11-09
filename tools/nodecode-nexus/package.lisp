;;;; package.lisp --- the NEXUS package: five tools, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed NEXUS so an eval form reads
;;;; (nexus:recall "where do I live"). The tools carry the original
;;;; plugin's own names; a person can call the same functions from eval.

(defpackage #:nodecode-nexus
  (:use #:cl)
  (:nicknames #:nexus)
  (:export #:start-cell
           #:stop-cell
           ;; the verbs, every one a string
           #:recall #:remember #:forget #:guardrail-check #:find-entities #:status
           ;; the one condition
           #:nexus-error #:nexus-error-detail))

(in-package #:nodecode-nexus)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and NEXUS-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral nexus :not-running t)
