;;;; cell.lisp --- the section, /cron-plan, the manual, and nothing that runs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Moodtuner997's hermes-cron-consolidator. The cell reads a
;;;; cron folder and answers a plan; it changes nothing, so it starts no
;;;; thread, takes no lease and writes no file. One declaration and the verb
;;;; behind it: NLK:DEFINE-PERIPHERAL gives this package its condition, FAIL,
;;;; the settings variable *CRON-PLAN* and RUNNING-SETTINGS, and
;;;; NLE:DEFINE-CELL declares the config section, the command and the manual
;;;; topic, and derives START-CELL, STOP-CELL and SETTING from them.
;;;;
;;;; Config, a sibling top-level key next to `cron' and `notify':
;;;;   "cron-plan": {"enabled": true, "home": "~/.hermes", "days": 14}

(in-package #:nodecode-cron-plan)

(nlk:define-peripheral cron-plan :not-running t)

;;; --- the manual ---------------------------------------------------------------
;;; (help :cron-plan) answers it while the cell runs; a request carries the
;;; one line the :HELP clause below gives, never this text.

(defparameter +primer+
  "A read-only plan for a crowded cron list, through the nodecode-cron-plan cell:
/cron-plan in a session, or (cron-plan:plan \"report\") through eval. Both take
the same words; a refusal is the cron-plan-error condition.
  /cron-plan [report] [--json] [--days N] [--home PATH] [--profile NAME]
      [--jobs PATH]
It reads <home>/cron/jobs.json, and <home>/cron/usage_audit.jsonl when it is
there, and groups the jobs that look like one job written several times: prompts
that are near-duplicates, jobs sharing skills and destination, jobs running the
same script. It lists jobs with a fixed time that fire in the same minute, jobs
on a failure streak of three or more, and deliveries that were accepted but never
confirmed. Token savings are a ceiling, counted from usage_audit.jsonl over
`days' days, and claimed only where the audit saw fires.
Nothing is applied and nothing is written: there is no --apply, because a merge
changes what an agent does. A schedule it cannot parse is listed, not guessed.
--json answers the same plan as one JSON object; --profile NAME reads
<home>/profiles/NAME/cron; --home reads another home's cron folder."
  "What (help :cron-plan) answers while the cell runs.")

;;; --- the verbs ------------------------------------------------------------------

(define-verb plan (&optional (arguments "report"))
  "The consolidation plan for ARGUMENTS, the words /cron-plan takes."
  (report arguments))

(defun run-slash (arguments session-id)
  "The /cron-plan answer for ARGUMENTS, in SESSION-ID's shell."
  (declare (ignore session-id))
  (plan arguments))

(nle:define-cell cron-plan
  (:section ("cron-plan")
    (:guide "home is the home whose cron/ folder is read; days bounds the usage window")
    ("home" :path :required t
     :doc "the home holding cron/jobs.json, which is a Hermes profile's cron folder")
    ("days" :integer :default 14 :min 1
     :doc "how many days of usage_audit.jsonl the token ceiling counts"))
  (:command "cron-plan" 'run-slash
            :description "Plan merges for overlapping cron jobs (read-only)"
            :argument-hint "[report] [--json] [--days N] [--home PATH] [--profile NAME]")
  (:help :cron-plan
         "cron-plan: a read-only plan for merging and rescheduling cron jobs"
         +primer+))
