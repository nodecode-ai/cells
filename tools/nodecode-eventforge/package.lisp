;;;; package.lisp --- the NODECODE-EVENTFORGE package: ten tools, their condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed EVENTFORGE so an eval form reads
;;;; (eventforge:ef-list-events ...). The tools are what the model calls; a
;;;; person can call the same functions from eval.

(defpackage #:nodecode-eventforge
  (:use #:cl)
  (:nicknames #:eventforge)
  (:export #:start-cell
           #:stop-cell
           #:report
           #:ef-list-events
           #:ef-emit-event
           #:ef-query-memory
           #:ef-list-pending-actions
           #:ef-approve-action
           #:ef-list-workflows
           #:ef-spawn-subagent
           #:ef-forge-mcp
           #:ef-approve-forge
           #:ef-listen-for-webhook
           #:ef-redacted-event
           #:eventforge-error
           #:eventforge-error-detail))

(in-package #:nodecode-eventforge)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and EVENTFORGE-ERROR, which a call that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral eventforge :not-running t)
