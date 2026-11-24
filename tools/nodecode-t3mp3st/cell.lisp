;;;; cell.lisp --- the one declaration: five tools, /t3mp3st, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A mission is state the War Room keeps, so this cell keeps none: every
;;;; verb reads or moves that state, and a restart of either side changes
;;;; nothing a person has to reconcile. The vocabulary is (t3mp3st:status),
;;;; (t3mp3st:launch "10.20.0.5"), (t3mp3st:findings),
;;;; (t3mp3st:approvals :action "approve" :id "..."), from eval; the five
;;;; t3mp3st_* tools for one answer mid-turn; and /t3mp3st for the operator.

(in-package #:nodecode-t3mp3st)

;;; --- the manual ----------------------------------------------------------------

(defparameter +primer+
  "Drive a T3MP3ST War Room -- an autonomous red-teaming platform that runs a
mission with a team of operators -- over the HTTP API its own UI uses. T3MP3ST
is a separate program with its own licence (AGPL-3.0); this cell ships none of
it and only speaks to a War Room already running (in its checkout: `npm run
server`, then http://127.0.0.1:3333/ui/). If no War Room answers, every tool
says so and names `npm run server`; that is a setup fact, not a failure of the
run.
  t3mp3st_status    what the mission is doing: its name and state, the current
      phase, progress, ticks, the targets in scope and the operators at work --
      or that the server is up with no mission.
  t3mp3st_launch    start a mission against one or more targets (hosts, URLs,
      CIDR blocks), separated by commas or spaces. Only for a target the user
      is authorized to test: ask for the scope and the written permission
      before launching, and put them in the brief you were given. A launch is
      real traffic at a real host. The War Room may hold a launch at an
      approval gate: it answers 'the mission has NOT started', names the gate
      and asks for a decision. Approve it (t3mp3st_approvals), then launch
      again with approval_id=<that id> -- approving does not start anything by
      itself.
  t3mp3st_findings  what the mission's vault holds: the findings, and the
      credential records (metadata only -- the War Room redacts the secrets).
  t3mp3st_approvals list one approval request per gate -- the id, what it asks
      for, the target and its state -- or clear one with action=approve or
      action=reject and its id. An approval expires (ttl_minutes, 30 by
      default); it does not stand.
  t3mp3st_control   stop, pause or resume the running mission.
Reading the answers: a launch that says 'holding the mission at an approval
gate' has NOT started -- read the gate, decide, then approve it or report what
it asks for. Report findings as the War Room stated them, with the target they
belong to, and never paraphrase a finding into certainty the mission did not
claim."
  "What (help :t3mp3st) answers while the cell runs.")

;;; --- the boundaries the model crosses ----------------------------------------------

(nlk:define-contract t3mp3st-launch
  (:doc "Start a T3MP3ST mission against one or more targets. Use it when the user asks for a red-team run, a pentest, or an autonomous scan of named hosts, and has said the targets and the authorization. A mission is real traffic at a real host: state the scope and the written permission in the call, and do not launch at anything the user did not name. Returns what the War Room took -- the mission id and its targets -- or the approval gate that is holding it.")
  ("targets" :text :required t
             :doc "one or more targets separated by commas or spaces: a host, a URL, or a CIDR block, as \"10.20.0.5\" or \"app.example, 10.20.0.0/24\"")
  ("name" :text
          :doc "the mission's name, as the user would say it; the minute it started when omitted")
  ("provider" :text
              :doc "the LLM provider the War Room should use for this mission (openrouter, anthropic, openai, deepseek, venice, local, local-agent); the server's own default when omitted")
  ("model" :text
           :doc "the model id for that provider, as `t3mp3st models' lists them; the provider's default when omitted")
  ("repo_path" :text
               :doc "an absolute path to a LOCAL repository the user owns, to be read white-box: the War Room ingests and security-ranks the source instead of probing blind. Only for a repo on this machine")
  ("approval_id" :text
                 :doc "the gate a previous launch was held at, exactly as t3mp3st_launch or t3mp3st_approvals named it. A launch that came back 'the mission has NOT started' is retried with this: approve the gate first, then launch again with approval_id"))

(nlk:define-contract t3mp3st-control
  (:doc "Stop, pause or resume the running T3MP3ST mission. Use it when the user says to hold the run, resume it, or end it -- and before launching a second mission, since one mission runs at a time.")
  ("action" :text :required t
            :doc "one of stop, pause, resume"))

(nlk:define-contract t3mp3st-findings
  (:doc "Read what the running T3MP3ST mission has found: its vault's findings, and the credential records (metadata only, the secrets themselves are redacted). Use it when the user asks what the run has turned up so far, before reporting results or deciding the next target.")
  ("limit" :integer
           :doc "show at most this many findings; the count is always the real one"))

(nlk:define-contract t3mp3st-approvals
  (:doc "List the T3MP3ST approval gates, or clear one by its id. The War Room stops at a gate before an action that needs a human: launching a mission at a new target, running a tool, reaching the network. Use list when a launch came back held, or the user asks what the run is waiting on; use approve or reject with the id the list gave you, and only after the user has said so. An approval expires after ttl_minutes, it does not stand.")
  ("action" :text
            :doc "list (the default), approve, or reject")
  ("id" :text
        :doc "the gate's id, exactly as t3mp3st_approvals listed it")
  ("ttl_minutes" :integer
                 :doc "how long an approval stands, in minutes (30 by default)"))

;;; --- what the model calls ------------------------------------------------------------

(defparameter +status-schema+
  (list :description
        (concatenate 'string
                     "Report what the T3MP3ST War Room is doing right now: the mission's name and "
                     "state, its current phase, progress and ticks, the targets in scope and the "
                     "operators at work -- or that the server is up with no mission. Use it to "
                     "answer 'how is the run going?', to check a mission started, and to see whether "
                     "a gate is holding it.")
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)))
  "t3mp3st_status takes no members: it asks the War Room one question of its own.")

(defun status-tool (arguments)
  "The t3mp3st_status tool; its arguments are empty by declaration."
  (declare (ignore arguments))
  (status))

(defun launch-tool (arguments)
  "The t3mp3st_launch tool: one mission, as the block the model reads."
  (launch (t3mp3st-launch-targets arguments)
          :name (t3mp3st-launch-name arguments)
          :provider (t3mp3st-launch-provider arguments)
          :model (t3mp3st-launch-model arguments)
          :repo-path (t3mp3st-launch-repo-path arguments)
          :approval-id (t3mp3st-launch-approval-id arguments)))

(defun control-tool (arguments)
  "The t3mp3st_control tool: one mission verb."
  (control (t3mp3st-control-action arguments)))

(defun findings-tool (arguments)
  "The t3mp3st_findings tool: the vault, bounded."
  (findings :limit (t3mp3st-findings-limit arguments)))

(defun approvals-tool (arguments)
  "The t3mp3st_approvals tool: list the gates, or clear one."
  (approvals :action (t3mp3st-approvals-action arguments)
             :id (t3mp3st-approvals-id arguments)
             :ttl-minutes (t3mp3st-approvals-ttl-minutes arguments)))

;;; --- /t3mp3st -------------------------------------------------------------------------

(defparameter +usage+
  "usage: /t3mp3st <what> - the War Room it drives must be running (npm run server in the T3MP3ST checkout)
  /t3mp3st            what the mission is doing (same as status)
  /t3mp3st launch 10.20.0.5[, app.example]   start a mission at authorized targets
  /t3mp3st findings   what the mission has found
  /t3mp3st approvals  the gates it is waiting on
  /t3mp3st approve <id> | reject <id> | stop | pause | resume")

(defun run-slash (args session-id)
  "One /t3mp3st invocation: status, launch, findings, approvals, or a mission verb."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (words (remove-if (lambda (word) (zerop (length word))) (cl-ppcre:split "\\s+" text)))
         (head (string-downcase (or (first words) "")))
         (rest (format nil "~{~a~^ ~}" (rest words))))
    (cond ((or (zerop (length head)) (string= head "status")) (status))
          ((string= head "findings") (findings))
          ((string= head "approvals") (approvals))
          ((string= head "launch") (launch rest))
          ((member head +controls+ :test #'string=) (control head))
          ((string= head "approve") (approvals :action "approve" :id rest))
          ((string= head "reject") (approvals :action "reject" :id rest))
          (t (format nil "~a~%  (there is no ~s; a mission's targets follow the word launch)~%" +usage+ head)))))

;;; --- the entry ------------------------------------------------------------------------

(nle:define-cell t3mp3st
  (:section ("t3mp3st")
    (:guide "nothing here is required when the War Room runs where it starts by itself. url is where
 it answers; timeout_seconds bounds one call to it. T3MP3ST itself is a separate program, run from its
 own checkout with `npm run server'.")
    ("url" :string :default "http://127.0.0.1:3333"
           :doc "where the T3MP3ST War Room answers")
    ("timeout_seconds" :integer :default 20 :min 1
                       :doc "how long one War Room call may take"))
  (:help :t3mp3st
         "Drive a running T3MP3ST War Room: launch a mission, watch it, read findings - the t3mp3st_* tools"
         +primer+)
  (:tool "t3mp3st_status" +status-schema+ 'status-tool)
  (:tool "t3mp3st_launch" (nlk:contract 't3mp3st-launch) 'launch-tool)
  (:tool "t3mp3st_control" (nlk:contract 't3mp3st-control) 'control-tool)
  (:tool "t3mp3st_findings" (nlk:contract 't3mp3st-findings) 'findings-tool)
  (:tool "t3mp3st_approvals" (nlk:contract 't3mp3st-approvals) 'approvals-tool)
  (:command "t3mp3st" 'run-slash
            :description "Drive a running T3MP3ST War Room"
            :argument-hint "status|launch <target>|findings|approvals")
  (:start (lambda () nil)))
