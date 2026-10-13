;;;; cell.lisp --- a built-in skill, outside editable or ranked memory.
;;;; SPDX-License-Identifier: MIT
(defpackage #:nodecode-context (:use #:cl))
(in-package #:nodecode-context)

(defparameter *skill*
  "During substantial tool work, review (context) at useful checkpoints, such as after settling a subproblem, and before your final answer. Keep constraints, decisions, verified state, unresolved work and exact evidence pointers; remove redundant dumps and superseded exploration. Preserve the first line and unique facts needed to continue, answer or resume. Edit with ordinary code without printing the file. Make one useful cleanup pass per checkpoint, or none when unnecessary; not after every tool call. Correctness outranks size.")

(defun standing-skill (next session-id)
  "Always supply the full skill in protected standing context, never the ranked index."
  (acons "context" *skill*
         (remove "context" (funcall next session-id) :key #'car :test #'equal)))

(nle:hook 'nle::read-harness-sections "nodecode-context" #'standing-skill)
