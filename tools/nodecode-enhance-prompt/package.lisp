;;;; package.lisp --- the ENHANCE-PROMPT package: one tool, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed ENHANCE-PROMPT so an eval form reads
;;;; (enhance-prompt:enhance "..."). The rewrite is a tool the model calls and
;;;; a slash command a person runs; a person can call the same function from
;;;; eval.

(defpackage #:nodecode-enhance-prompt
  (:use #:cl)
  (:nicknames #:enhance-prompt)
  (:export #:start-cell
           #:stop-cell
           #:enhance
           #:strip-wrappers
           #:system-prompt
           #:stage-hint
           #:enhance-prompt-error #:enhance-prompt-error-detail))

(in-package #:nodecode-enhance-prompt)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and ENHANCE-PROMPT-ERROR, which a rewrite that cannot go
;;; on signals, come from here: a model that answers nothing, or a side call
;;; that fails, is reported to the caller rather than thrown at it. The lock
;;; guards the per-chat chain of drafts the command keeps.
(nlk:define-peripheral enhance-prompt :not-running t :lock "enhance-prompt drafts")
