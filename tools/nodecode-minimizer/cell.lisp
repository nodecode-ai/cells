;;;; cell.lisp --- the simplest complete change, said to the model as it starts
;;;; a turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is one static section of the system prompt: reuse what
;;;; already fits, put an invariant in the layer that owns it, add no
;;;; speculative layer, and prove the requested behavior. Nodecode has no
;;;; section a folder may append to the system prompt, so the same words ride
;;;; the :ROUND point instead: at the first request of every turn the cell
;;;; records the guidance as that turn's reminder text, which lands in the
;;;; very request being built. Nothing else of the original is here -- it had
;;;; no tools, no hooks and no dependencies -- and nothing of it is enforced.
;;;;
;;;; The policy is the original's policy.md, whole and unchanged, so a later
;;;; version of it is one DEFPARAMETER to lift.

(defpackage #:nodecode-minimizer
  (:use #:cl))

(in-package #:nodecode-minimizer)

;;; --- the policy ------------------------------------------------------------

(defparameter +policy+
  "# Minimizer

Build the simplest complete system. Trace behavior and invariants before editing. System, user, and repository instructions outrank this default.

## Choose the first fitting rung

Consider these in order. Accept the first that satisfies correctness and architectural fit without distortion:

1. Required behavior already exists: reuse it or change nothing.
2. The responsible layer, type, helper, or pattern owns it: extend there.
3. The standard library, native platform, browser, database, runtime, or an installed dependency owns it: use it when its semantics fit.
4. A small new implementation fits the current architecture: build it where the invariant belongs.
5. Existing structure obstructs clear ownership: make the coherent refactor the task needs.

This ordering is a brief decision, not a research project. Do not contort a design to force an earlier rung.

## Understand and own the invariant

Before editing, trace the relevant inputs, callers, state changes, outputs, and failure paths. Distinguish symptoms from root causes. Put a shared invariant in the responsible layer instead of duplicating guards in callers.

Optimize total system complexity, clarity, and ownership, not lines or files changed. Prefer consolidation and fitting abstractions. A coherent wider change is better than a cramped patch or misplaced logic.

Avoid speculative features, imagined extension points, single-implementation interfaces without an architectural reason, configuration for fixed values, premature services, and unnecessary scaffolding. A dependency, abstraction, or public surface is valid when correct design or lower lifecycle cost justifies it.

Deliver all explicitly requested behavior. Never silently substitute a smaller feature, defer a requirement, or describe a plan as a result. If a requirement is blocked, state the blocker and remaining work.

## Preserve safeguards

Simplicity never removes trust-boundary validation, authorization, security, data-loss prevention, required error handling, accessibility, hardware calibration, migration or rollback safety, concurrency protection, compatibility, required tests, or explicitly requested behavior.

Respect approval boundaries. Ask only when a material public, security, data, billing, hard-to-reverse, or otherwise approval-gated choice requires a decision. Proceed with routine reversible work.

## Prove completion

Run the smallest sufficient proof of the requested behavior. Prefer an existing test, command, runtime check, or artifact inspection that can catch the likely failure. Add focused regression coverage when changed logic needs it. Preserve governing test-first and release requirements.

Report the usable result, material changes, observed evidence, unresolved risks, and material omissions. Do not claim completion for unverified work. Stop when the task is satisfied."
  "The original's policy.md, whole: the text the model reads at each turn's start.")

(defun brief-policy ()
  "The opening and the five rungs alone, cut before POLICY's second section."
  (let ((cut (search (format nil "~%## Understand") +policy+)))
    (if cut
        (string-right-trim '(#\Space #\Newline) (subseq +policy+ 0 cut))
        +policy+)))

(defun clipped (text)
  "TEXT at most (setting :max-chars) characters, marked when it had to be cut."
  (let ((limit (setting :max-chars)))
    (if (<= (length text) limit)
        text
        (concatenate 'string (subseq text 0 (max 0 (- limit 3))) "..."))))

(defun guidance ()
  "What this request carries: the policy in the configured mode, capped."
  (clipped (if (equal "brief" (setting :mode)) (brief-policy) +policy+)))

;;; --- the first request of a turn -------------------------------------------

(defun observe-round (op)
  "At a turn's first request: the guidance, as that request's reminder text.
Later rounds of the same turn build on it; a section of the system prompt is
what the original had, and this is the nearest seam Nodecode offers."
  (let ((turn (getf op :turn)))
    (when (and turn (eql 1 (getf op :round)))
      (nlk:record-turn-context-prompt turn :text (guidance)))))

;;; --- /minimizer ------------------------------------------------------------

(defun status-text ()
  "What the cell is doing, then the very text the next turn's first request carries."
  (format nil "minimizer: ~a mode, up to ~d characters, in the first request of every turn. ~
/minimizer brief or /minimizer full previews one whole.~2%~a"
          (setting :mode) (setting :max-chars) (guidance)))

(defun run-slash (args session-id)
  "One /minimizer invocation -- nothing for what lands now, or a mode to
preview -- => (values TEXT DIALOG)."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond
      ((zerop (length word)) (status-text))
      ((equal word "full") (clipped +policy+))
      ((equal word "brief") (clipped (brief-policy)))
      (t (format nil "minimizer has no ~a mode: /minimizer takes full or brief, or nothing for what lands now."
                 word)))))

;;; --- the declaration --------------------------------------------------------

(defparameter +manual+
  "minimizer: the simplest complete change, stated at the start of every turn.
Nothing to run and nothing to ask for: with the cell on, the first request of
each turn carries the guidance, and the model is told to reuse what already
fits, to put an invariant in the layer that owns it, to add no speculative
layer, to keep every safeguard and requirement, and to prove the requested
behavior before calling it done.
  /minimizer            what is being said, and the exact text of it
  /minimizer full       the whole policy
  /minimizer brief      the rung ladder alone
Settings, in /settings: mode is full or brief, and max_chars caps how much
one request carries. It is guidance, not enforcement: nothing here checks that
a change was small or that a check ran. System, user and repository
instructions outrank it, as the policy itself says."
  "What (help :minimizer) answers while the cell runs.")

(nlk:define-peripheral minimizer :not-running t)

(nle:define-cell minimizer
  (:section ("minimizer")
    (:guide "No key and no network: mode picks how much of the policy rides a turn, max_chars how long one request's copy may be")
    ("mode" :choice :options '("full" "brief") :default "full"
     :doc "full carries the whole policy; brief the opening and the five rungs alone")
    ("max_chars" :integer :default 3000 :min 300
     :doc "the longest the guidance one request carries, marked when it is cut"))
  (:help :minimizer "minimizer: the simplest complete change, stated at the start of every turn" +manual+)
  (:command "minimizer" 'run-slash
            :description "The guidance every turn opens with"
            :argument-hint "[full|brief]")
  (:hook :round (nle:observer #'observe-round "minimizer")))
