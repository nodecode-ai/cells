;;;; package.lisp --- the NODECODE-CODER package: three tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed CODER so an eval form reads
;;;; (coder:run-command ...). The three verbs are tools the model calls; a
;;;; person can call the same functions from eval.

(defpackage #:nodecode-coder
  (:use #:cl)
  (:nicknames #:coder)
  (:export #:start-cell
           #:stop-cell
           #:list-workspaces #:read-workspace #:run-command
           #:coder-error #:coder-error-detail))

(in-package #:nodecode-coder)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and CODER-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral coder :not-running t)
