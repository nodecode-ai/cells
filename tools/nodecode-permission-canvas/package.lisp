;;;; package.lisp --- the NODECODE-PERMISSION-CANVAS package: the policy, and its verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed PERMISSION-CANVAS so an eval form reads
;;;; (permission-canvas:run-check ...). The pure half -- the levels, the rules,
;;;; the observation and the decision -- is exported because it is where the
;;;; behaviour is decided and what the tests read; the tools and the command
;;;; wrap it.

(defpackage #:nodecode-permission-canvas
  (:use #:cl)
  (:nicknames #:permission-canvas)
  (:export
   ;; the pure half: a control, its level, what an observation withholds, a decision
   #:control #:control-text #:effective-level #:observation #:decide-action
   #:policy-load #:policy-rules #:policy-paused-p #:policy-target #:validate-policy
   #:ledger-load #:ledger-prune #:pending-ids
   ;; the cell's own verbs
   #:run-check #:run-observe #:run-policy #:run-slash #:start-cell #:stop-cell
   ;; the one condition
   #:permission-canvas-error #:error-text))

(in-package #:nodecode-permission-canvas)

;;; SETTING, FAIL and RUNNING-SETTINGS come from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last; policy.lisp reads the section
;;; through SETTING.
(nlk:define-peripheral permission-canvas :not-running t)
