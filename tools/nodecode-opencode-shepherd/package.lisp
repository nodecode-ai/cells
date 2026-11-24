;;;; package.lisp --- the SHEPHERD package: seven tools, one command, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed SHEPHERD so an eval form reads
;;;; (shepherd:list-sessions ""). The verbs are tools the model calls; a person
;;;; can call the same functions from eval.

(defpackage #:nodecode-opencode-shepherd
  (:use #:cl)
  (:nicknames #:shepherd)
  (:export #:start-cell
           #:stop-cell
           #:list-sessions #:waiting-sessions #:tail-session #:send-prompt
           #:reply-permission #:answer-form #:interrupt-session
           #:waiting-report #:session-report #:run-opencode #:watch-step
           #:opencode-shepherd-error #:opencode-shepherd-error-detail))

(in-package #:nodecode-opencode-shepherd)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL, RUNNING-SETTINGS and DEFINE-VERB, which a verb that
;;; cannot go on uses, come from here: every refusal starts as one of these,
;;; and the tools answer it as the call's failure rather than as an unhandled
;;; error.
(nlk:define-peripheral opencode-shepherd :not-running t)
