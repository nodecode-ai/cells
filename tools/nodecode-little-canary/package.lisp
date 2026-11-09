;;;; package.lisp --- the LITTLE-CANARY package: one screening tool, its
;;;; slash command, its gate, and the condition a refusal signals.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed LITTLE-CANARY so an eval form reads
;;;; (little-canary:screen ...). The verb is a tool the model calls; a person
;;;; can call the same function from eval, or type /little-canary.

(defpackage #:nodecode-little-canary
  (:use #:cl)
  (:nicknames #:little-canary)
  (:export #:start-cell
           #:stop-cell
           #:screen #:run-canary #:gate-tool-call
           #:little-canary-error #:little-canary-error-detail))

(in-package #:nodecode-little-canary)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and LITTLE-CANARY-ERROR, which a screen that cannot go
;;; on signals, come from here: every refusal starts as one of these, and the
;;; tool answers it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral little-canary :not-running t)
