;;;; cell.lisp --- the cell: two tools, /calling-mom, and the tool-call hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's __init__.py around register(): what the
;;;; Hermes plugin did through seven hooks, a Nodecode cell does through the
;;;; one seam the organism offers for a tool call - NLE:HOOK on :TOOL, which
;;;; runs the call, reads its arguments and rewrites its answer.
;;;;
;;;; What the original reached through a Kanban board (cards, comments,
;;;; budgets counted in comments) has no counterpart here, so a budget is
;;;; counted in calls and a comment is a line appended to the tool's result.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "cells": ["nodecode-calling-mom"], and
;;;;   "calling-mom": {"assist": true, "mom": "provider/model", "olymp": "provider/model"}

(in-package #:nodecode-calling-mom)

(nlk:define-peripheral calling-mom :not-running t)

(defparameter +primer+
  "Calling mom hands a second, cheaper model one short question when the work
it is watching gets stuck or is about to do something irreversible, and
escalates to an expensive model only when the cheap one says ESCALATE.

  /calling-mom                 who is mom, who is olymp, the day's calls
  /calling-mom on | off        watch tool calls, or stop
  /calling-mom ask QUESTION    ask mom one question now
  /calling-mom mom NAME        the cheap advisor's model (mom.model keeps it)
  /calling-mom olymp NAME      the expensive advisor (empty = none)
  /calling-mom reset           start the day's counters over

The second_opinion tool asks mom about a design decision; risk_check reads a
command against the same rule list the watcher uses. With `assist' on, every
tool call is watched, and mom is asked when the same call or the same answer
repeats three times in a row, or when a command matches the critical tier
(git push --force, terraform apply, DROP TABLE, rm -rf of a broad folder,
gh pr merge, ...). Her answer is appended to that call's result, where the
working model reads it. Advice warns; it cannot undo what already ran.
Nothing leaves the machine until `assist' is on and a tool call is watched."
  "What (help :calling-mom) answers while the cell runs.")

;;; --- the call's own text ----------------------------------------------------

(defun hash-values-text (hash)
  "Every string HASH carries, space separated: a call's arguments as text."
  (if (hash-table-p hash)
      (with-output-to-string (out)
        (loop for value being the hash-values of hash
              when (stringp value) do (format out "~a " value)))
      ""))

(defun call-text (arguments)
  "What a tool call carries as text: the command or form it runs, its
arguments, all of it. The rules read this text, so one of them matches
wherever the command sits in it."
  (hash-values-text arguments))

(defun result-text (result)
  "RESULT as text, which is what the tracker fingerprints."
  (cond ((stringp result) result)
        ((null result) "")
        (t (format nil "~a" result))))

;;; --- what the watcher says ---------------------------------------------------

(defun remember-step (name text result)
  "Keep the last few calls, which a stuck question is built from."
  (push (list name (clip text 200) (clip (result-text result) 400)) *history*)
  (when (> (length *history*) 8) (setf *history* (subseq *history* 0 8))))

(defun cooling-down-p ()
  "Whether the last unsolicited question is too recent to ask another."
  (< (- (get-universal-time) (or *last-asked-at* 0))
     (or (ignore-errors (setting :cooldown-seconds)) 60)))

(defun review-call (text)
  "The advice a command in the call deserves, or NIL. One command is
reviewed once: a critical tier below the cooldown is skipped, not queued."
  (multiple-value-bind (tier id) (classify text :extra (or (ignore-errors (setting :commands-extra)) '()))
    (when tier
      (let ((key (format nil "~a ~a" tier (clip text 200))))
        (unless (or (gethash key *asked-already*) (cooling-down-p))
          (setf (gethash key *asked-already*) t
                *last-asked-at* (get-universal-time))
          (if (eq tier :critical)
              (ask-advice "architecture" "critical_change"
                          :note (format nil "A critical command (~a) just ran. The call:~%~a"
                                        id (clip text 1500)))
              (ask-advice "watch" "watch_change"
                          :note (format nil "A command on another machine (~a) just ran. The call:~%~a"
                                        id (clip text 1500)))))))))

(defun stuck-advice (signals)
  "The advice a run of repeated calls deserves, or NIL."
  (when (and signals (not (cooling-down-p)))
    (setf *last-asked-at* (get-universal-time))
    (ask-advice "error" "stuck" :note (format nil "Signal: ~a" (tracker-notice signals)))))

(defun watch-call (op next)
  "The :TOOL point: run the call, learn from it, and hand back the result
with mom's line after it when she had something to say."
  (let* ((name (getf op :name))
         (text (call-text (getf op :arguments)))
         (result (funcall next op)))
    (when (and *assist* *tracker*)
      (let* ((signals (ignore-errors (tracker-feed *tracker* name (clip text 200) (result-text result))))
             (advice (or (ignore-errors (review-call text))
                         (ignore-errors (stuck-advice signals)))))
        (remember-step name text result)
        (when (and advice (stringp result))
          (setf result (format nil "~a~%~%~a" result advice)))))
    result))

;;; --- the tools ------------------------------------------------------------------

(nlk:define-contract second-opinion-arguments
  (:doc "Ask a stronger model one short question about a decision that is expensive to get wrong: a schema, an
infrastructure change, a public interface, an irreversible step. It answers in a few sentences - one recommendation,
the deciding reason, one risk. Use it before the decision, not for routine work; it is budgeted per day.")
  ("question" :text :required t :doc "the decision, and the options you are weighing")
  ("context" :string :doc "the constraints that matter, short"))

(defun second-opinion (arguments)
  "The second_opinion tool: one question to mom, up to olymp when she escalates."
  (let ((question (second-opinion-arguments-question arguments))
        (context (second-opinion-arguments-context arguments)))
    (cond ((not (budget-left-p))
           (format nil "no second opinion today: all ~d of the day's questions are used (calling-mom.mom_calls_per_day)"
                   *calls-today*))
          (t (let ((advice (ask-advice "architecture" "second_opinion"
                                        :note (format nil "Question: ~a~@[~%Context: ~a~]"
                                                      question context))))
               (or advice
                   "the advisor had nothing to add (or could not be reached); continue as you were"))))))

(nlk:define-contract risk-arguments
  (:doc "Read a shell command, or a file about to change, against the list of commands worth a second look: force
pushes, production deploys, DDL, secret writes, recursive deletes of a broad folder, merges of unapproved pull
requests, guards switched off. It classifies; it does not block anything.")
  ("command" :text :required t :doc "the command line, as you would run it")
  ("path" :string :doc "a file the command is about to change, when that is what matters"))

(defun risk-check (arguments)
  "The risk_check tool: which tier a command or a path falls in, and under which rule."
  (let* ((command (risk-arguments-command arguments))
         (path (risk-arguments-path arguments))
         (extra (or (ignore-errors (setting :commands-extra)) '())))
    (multiple-value-bind (tier id) (classify command :extra extra)
      (multiple-value-bind (file file-id) (and path (plusp (length path)) (critical-file path))
        (cond ((eq tier :critical)
               (format nil "critical (~a): irreversible or public. Worth one question before it runs, or right after."
                       id))
              (file
               (format nil "critical (~a): a change to a file the deployment reads." file-id))
              (tier
               (format nil "watch (~a): privileged access to another machine. One look, nothing more." id))
              (t "routine: no rule matches; nothing to ask."))))))

;;; --- /calling-mom -----------------------------------------------------------------

(defun target-name (spec)
  "A section target as it is written, or the auxiliary model's own name."
  (let ((spec (if (stringp spec) (string-trim " " spec) "")))
    (if (plusp (length spec)) spec "(the auxiliary model)")))

(defun status-line ()
  (roll-day)
  (format nil "calling-mom ~a - mom ~a, olymp ~a, ~d/mom and ~d/olymp calls today~%~
Usage: /calling-mom [on|off|ask|mom|olymp|reset]"
          (if *assist* "on (watching tool calls)" "off (the tools answer, nothing is watched)")
          (target-name (mom-spec))
          (target-name (olymp-spec))
          *calls-today* *olymp-today*))

(defun run-slash (args session-id)
  "The answer to one /calling-mom invocation."
  (declare (ignore session-id))
  (let* ((raw (nlk:trimmed (or args "")))
         (space (position #\Space raw))
         (head (string-downcase (if space (subseq raw 0 space) raw)))
         (value (if space (string-trim " " (subseq raw (1+ space))) "")))
    (nlk:dispatch head string=
      ("" (status-line))
      ("status" (status-line))
      ("on" (setf *assist* t)
       "calling-mom on: tool calls are watched, and mom is asked when one repeats or runs something critical")
      ("off" (setf *assist* nil)
       "calling-mom off: second_opinion and risk_check still answer, nothing is watched")
      ("ask" (if (plusp (length value))
                 (or (ask-advice "architecture" "second_opinion" :note (format nil "Question: ~a" value))
                     "the advisor had nothing to add (or could not be reached)")
                 "usage: /calling-mom ask <the question>"))
      ("mom" (if (plusp (length value))
                 (progn (setf *mom-override* value)
                        (format nil "mom ~a (this run; set calling-mom.mom to keep it)" value))
                 (format nil "mom ~a" (target-name (setting :mom)))))
      ("olymp" (if (plusp (length value))
                   (progn (setf *olymp-override* value)
                          (format nil "olymp ~a (this run; set calling-mom.olymp to keep it)" value))
                   (format nil "olymp ~a" (target-name (setting :olymp)))))
      ("reset" (setf *day* nil *asked-already* (make-hash-table :test #'equal) *last-asked-at* 0)
       (roll-day)
       "the day's counters start over")
      (t (format nil "~a is not one of its choices; /calling-mom alone shows them" head)))))

;;; --- the entry ------------------------------------------------------------------

(defun install ()
  (setf *assist* (not (null (setting :assist)))
        *history* '()
        *tracker* (make-tracker)
        *asked-already* (make-hash-table :test #'equal)
        *day* nil *calls-today* 0 *olymp-today* 0 *last-asked-at* 0 *last-error* nil
        *mom-override* "" *olymp-override* "")
  (roll-day)
  (nle:on-stop (lambda ()
                 (setf *assist* nil *history* '() *tracker* nil
                       *mom-override* "" *olymp-override* "")))
  (when *assist*
    (ignore-errors
     (nle:notice (format nil "calling-mom watches tool calls: mom is ~a, olymp ~a"
                         (target-name (setting :mom))
                         (target-name (setting :olymp)))
                 :level :info :key "nodecode-calling-mom"))))

(nle:define-cell calling-mom
  (:section ("calling-mom")
    (:guide "mom is the cheap advisor and needs nothing else: empty asks the auxiliary model; olymp is the expensive one and stays off when empty; assist is the whole watcher switch; a key of your own is only needed when the provider needs one")
    ("assist" :boolean :default nil
     :doc "watch tool calls and ask mom when a call repeats or runs something critical")
    ("mom" :string :doc "the cheap advisor, as provider/model; empty uses the auxiliary model")
    ("olymp" :string :doc "the expensive advisor, as provider/model; empty switches it off")
    ("mom_calls_per_day" :integer :default 20 :min 1 :doc "the cheap advisor's hard limit, in calls")
    ("olymp_calls_per_day" :integer :default 6 :min 0 :doc "the expensive advisor's hard limit, in calls")
    ("cooldown_seconds" :integer :default 60 :min 0
     :doc "how long between two unsolicited questions, so a burst of calls costs one")
    ("context_chars" :integer :default 4000 :min 500 :doc "how much of the recent steps one question carries")
    ("max_tokens" :integer :default 400 :min 64 :doc "one answer's ceiling")
    ("commands_extra" :list :doc "regexes of your own for the critical tier, added to the built-in rules"))
  (:start #'install)
  (:help :calling-mom "a cheap advisor answers when the work is stuck or about to do something irreversible; a rare ESCALATE reaches the expensive one" +primer+)
  (:hook :tool #'watch-call)
  (:tool "second_opinion" (nlk:contract 'second-opinion-arguments) 'second-opinion)
  (:tool "risk_check" (nlk:contract 'risk-arguments) 'risk-check)
  (:command "calling-mom" #'run-slash
            :description "Who is mom, who is olymp; /calling-mom on to watch tool calls"
            :argument-hint "on|off|ask|mom|olymp|reset"))
