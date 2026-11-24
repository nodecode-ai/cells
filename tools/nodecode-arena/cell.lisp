;;;; cell.lisp --- the manual, the arena tool, /arena, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of J182Razor/hermes-arena-skill, which carried a
;;;; skill and a Python state machine: 100 sub-agents take one task, each
;;;; with its own strategy card, and fight a single-elimination bracket in
;;;; which they attack each other's answer, defend and revise their own, and a
;;;; fresh judge scores both against a written rubric until one solution is
;;;; left. The rewrite keeps the tournament exactly - the same cards, rubric,
;;;; phases, verdict arithmetic and sub-agent briefs - and moves the state
;;;; machine from python3 bracket.py into the cell: the model drives the whole
;;;; run through one tool, arena, and no Python is needed at all.

(in-package #:nodecode-arena)

(defparameter +manual+
  "The arena runs a tournament over one task: N sub-agents (16 with quick, the default size in
settings) each get the same task and a different strategy card, then a single-elimination
bracket has them attack each other's answer, defend and revise their own, and a fresh judge
score both against skill/rubric.md until one solution is left. You are the orchestrator: you
never compete, never attack, never judge, and never pick a winner.
  (arena:run \"plan\" :agents 16)                  rounds, sub-agent calls and waves; writes nothing
  (arena:run \"init\" :task \"...\" :quick t)         a run: writes the task, the rubric and the cards dealt
  (arena:run \"next\")                             what to do now, and the exact call for it
  (arena:run \"prompts\" :phase \"spawn\")           write every brief of the phase, list the jobs in waves
  (arena:run \"check\" :phase \"attack\")            which outputs are still missing
  (arena:run \"collect\")                          read the judges' verdicts and record the winners
  (arena:run \"advance\")                          close the round, eliminate the losers, pair the survivors
  (arena:run \"status\")  (arena:run \"pairings\")  (arena:run \"winner\")  (arena:run \"card\" :agent \"a004\")
  (arena:run \"record\" :match \"r2-m3\" :winner \"a004\")   only to fix bookkeeping the user asked about
The loop: init, then next; for each phase next names, prompts <phase>, launch one wave of jobs at
a time (each job is a sub-agent call told \"Read <prompt path> and follow it exactly\"), then next
again. collect and advance are steps of their own. A run opens at spawn, then every round is
attack, defend, judge, collect, advance; with a rejected answer to beat there is one last blind
final check. next says DONE when one solution has won: read the file winner names and give it to
the user whole.
The state lives in arena.json in the run folder, under .arena/ by default, so a compacted
conversation loses nothing: (arena:run \"status\") and (arena:run \"next\") resume it.
Cost is real: a 16-agent arena is about 91 sub-agent calls in 16 waves; 100 is about 595 in 70, 71 with the final check.
Re-run a job that wrote nothing; if it fails twice, write NO OUTPUT into its file - a missing
attack counts as no attacks, a missing solution loses its match, and a judge that fails twice
gets a third, fresh run. Never decide a match yourself.
ERROR: ARENA-ERROR says what is wrong: no run here, an unknown phase, a match that is not there."
  "What (help :arena) answers while the cell runs.")

;;; --- the tool's boundary ---------------------------------------------------

(defparameter +commands+
  '("plan" "init" "next" "prompts" "check" "pairings" "collect" "advance" "status" "record" "winner" "card")
  "The arena's verbs, the value its command member takes.")

(nlk:define-contract arena-call
  (:doc "The tournament state machine on disk. command=plan says what a run would cost and writes
nothing; command=init starts one (task is the task, in the user's own words, written to stand
alone: sub-agents cannot see this conversation); command=next says what to do now and prints the
exact call for it; command=prompts writes every sub-agent brief of one phase (phase) and lists the
jobs in waves; command=check says which outputs are still missing; command=collect reads the
judges' verdicts and records the winners; command=advance closes the round and pairs the survivors;
command=status, command=pairings, command=card and command=winner report; command=record fixes one
match's winner by hand. Read (help :arena) before the first run: it has the loop and its rules.")
  ("command" :text :required t :options +commands+ :doc "the verb: plan, init, next, prompts, check, pairings, collect, advance, status, record, winner, card")
  ("task" :string :default "" :doc "init only: the task, written for a stranger who cannot see this conversation")
  ("baseline" :string :default "" :doc "init only: the answer the user was not satisfied with, verbatim, for the blind final check")
  ("agents" :integer :default 0 :doc "init and plan: competitors; 0 means the arena size in settings")
  ("quick" :boolean :default nil :doc "init and plan: 16 competitors instead of the default")
  ("wave" :integer :default 0 :doc "init and plan: sub-agent jobs launched at once; 0 means the wave in settings")
  ("seed" :integer :default 0 :doc "init: fixes the cards and the pairings; 0 draws one and records it")
  ("phase" :string :default "" :slot phase-name :doc "prompts and check: one of spawn, attack, defend, judge, final")
  ("match" :string :default "" :doc "record: the match id, as pairings prints it")
  ("winner" :string :default "" :doc "record: the agent id that won that match")
  ("agent" :string :default "" :doc "card: the agent id to describe")
  ("reason" :string :default "" :doc "record: one sentence on why that agent won")
  ("dir" :string :default "" :doc "the run folder; empty means the run init named last, under .arena/"))

(defun run-tool (call)
  "The arena tool: one verb, its text back, and a refusal as text the model
can act on rather than a broken call."
  (handler-case
      (run (arena-call-command call)
           :task (arena-call-task call)
           :baseline (arena-call-baseline call)
           :agents (arena-call-agents call)
           :quick (arena-call-quick call)
           :wave (arena-call-wave call)
           :seed (arena-call-seed call)
           :phase (arena-call-phase-name call)
           :match (arena-call-match call)
           :winner (arena-call-winner call)
           :agent (arena-call-agent call)
           :reason (arena-call-reason call)
           :dir (arena-call-dir call))
    (arena-error (condition) (format nil "ERROR: ~a" condition))
    (error (condition) (format nil "ERROR: ~a" condition))))

;;; --- /arena ----------------------------------------------------------------

(defun run-slash (args session-id)
  "/arena typed alone: where the run stands, or the verb it names."
  (declare (ignore session-id))
  (let* ((words (uiop:split-string (nlk:trimmed (or args "")) :separator " "))
         (verb (string-downcase (first words))))
    (cond ((or (null words) (equal verb ""))
           (handler-case (run "status")
             (arena-error (condition)
               (format nil "arena: ~a~%Give the task to a sub-agent tournament with the arena tool (command=init), or (help :arena) for the loop." condition))))
          ((equal verb "help") +manual+)
          ((member verb +commands+ :test #'equal) (run verb))
          (t (format nil "arena: ~a is not one of its words (~{~a~^, ~}).~%" verb +commands+)))))

;;; --- the declaration -------------------------------------------------------

(nle:define-cell arena
  (:section ("arena")
    (:guide "nothing here is required; agents is the default arena size (quick always deals 16), wave how many sub-agent jobs go out at once -- raise it only if the harness runs that many at a time")
    ("agents" :integer :default 100 :min 1
              :doc "competitors in an arena when neither agents nor quick is passed")
    ("wave" :integer :default 10 :min 1
            :doc "sub-agent jobs launched at once; a wave is one round of calls"))
  (:help :arena "arena: a knockout tournament of sub-agents over one task - each gets a different strategy card, they attack, defend and are judged until one answer wins; the arena tool drives it"
         +manual+)
  (:tool "arena" (nlk:contract 'arena-call) 'run-tool)
  (:command "arena" 'run-slash
            :description "Run a sub-agent tournament over one task"
            :argument-hint "plan | next | status | winner"))
