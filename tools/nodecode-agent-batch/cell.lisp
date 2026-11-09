;;;; cell.lisp --- Agent Batch: a task list, phased into parallel groups, dispatched to GitHub Actions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of raaaas/hermes-agent-batch (see NOTICE). The original was a
;;;; desktop panel plus two agent tools over one shared plan: the panel stored
;;;; the task list, asked the model to phase it, and dispatched a phase on a
;;;; click; the tools read the plan and wrote the phasing decision. A cell has
;;;; no panel, so the plan is the same JSON under (nlk:home "agent-batch/"),
;;;; the two tools keep their names and their decisions, the panel's other
;;;; routes are three more tools (task list, dispatch, activity), and the click
;;;; that launched real CI is the /agent-batch command or the dispatch tool.
;;;;
;;;; What the original's workflow does with a dispatch is not this cell's: the
;;;; repository's own agent-batch.yml takes a tasks input and starts one agent
;;;; per task. This cell only says what ran.

(in-package #:nodecode-agent-batch)

;;; --- what the model is told -------------------------------------------------------

(defparameter +primer+
  "Agent Batch turns a task list into parallel groups and dispatches each group as agents on
GitHub's runners, one agent per task, each on its own branch, and a later group starts after
the earlier one lands. The plan lives in this machine's own folder: a task list, the phases
you decide, and the status of each.
  agent_batch_tasks {tasks, repo?, model?}   store the task list (clears the phasing)
  agent_batch_plan {}                        the tasks by index, the phases, the dispatches
  agent_batch_phase {phases: [[0,1],[2]]}    the parallel groups, in the order they run
  agent_batch_dispatch {phase}               launch one group: a real workflow run
  agent_batch_runs {repo?}                   recent workflow runs and open pull requests
How to work: store the list as the operator gave it (agent_batch_tasks), read it back
(agent_batch_plan), then group every task into exactly one phase by its index, tasks that
can run at the same time in the same group and a task that needs an earlier one's result in a
later group. A phase that names a task twice, leaves one out or invents one is refused, so
the whole list always runs. Dispatch is the click the panel used to be: it spends the
operator's Actions minutes and starts agents with write access to their repository, so
dispatch one phase when they ask for it, not for them. agent_batch_runs says how a dispatch
went. The agent-batch section of config.jsonc names the repo (owner/name), the workflow file
in it that takes this input (default agent-batch.yml), the branch it runs on, and the token
(token_env, or a token_file) with actions:write and pull-requests:write; the plan's own repo
and model override the section's. /agent-batch prints the plan, /agent-batch dispatch N
launches a phase, /agent-batch reset forgets the plan."
  "What (help :agent-batch) answers while the cell runs: the original's workflow, in the
cell's voice.")

;;; --- the tools' schemas -----------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema, as the
original wrote its two."
  (list :description description
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:make-json-object "type" type
                                                                           "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *plan-schema*
  (tool-schema
   "Read the Agent Batch plan: the stored task list by index, the target repository, the model, and the current phases with their status. Call this before agent_batch_phase so the phases you propose cover the tasks that are actually there."
   '())
  "The agent_batch_plan tool, as the original's PLAN_SCHEMA had it.")

(defparameter *phase-schema*
  (tool-schema
   "Save a phasing decision for the Agent Batch plan: groups of tasks that can run in parallel, ordered by dependency. Tasks inside one group dispatch together as independent agents on their own branches; a later group starts after the earlier one lands. Put every stored task in exactly one group. Reference tasks by their index from agent_batch_plan (e.g. [[0, 1], [2]]), or by their exact text."
   '(("phases" "array" "Parallel groups, in execution order: [[0, 1], [2]]." t)))
  "The agent_batch_phase tool, as the original's PHASE_SCHEMA had it.")

(defparameter *tasks-schema*
  (tool-schema
   "Store the task list Agent Batch is to run, one task per entry, as the operator gave it. Storing a list clears the phases and their status, because the old phasing no longer covers the tasks; phase it again with agent_batch_phase."
   '(("tasks" "array" "The tasks to run, one per entry, in the order they matter." t)
     ("repo" "string" "owner/name this list targets (default: the section's repo, or the plan's).")
     ("model" "string" "provider/model handed to every agent of this list (default: the runner's own).")))
  "The panel's POST /plan, as a tool.")

(defparameter *dispatch-schema*
  (tool-schema
   "Launch ONE stored phase on GitHub: a workflow_dispatch to the repository's agent-batch workflow, which starts one agent per task in the group. Real CI on the operator's account, write access to their branches, their Actions minutes -- dispatch when they ask. The phase is marked running and the dispatch is recorded."
   '(("phase" "integer" "The phase's index, as agent_batch_plan prints it." t)
     ("repo" "string" "owner/name to dispatch to (default: the plan's, then the section's).")
     ("model" "string" "provider/model for this run (default: the plan's, then the section's).")
     ("context" "string" "One paragraph every agent of the run is given, e.g. the house rules.")
     ("base_branch" "string" "The branch the workflow runs on (default: the section's).")
     ("workflow" "string" "The workflow file (default: the section's, agent-batch.yml).")))
  "The panel's POST /dispatch, as a tool.")

(defparameter *runs-schema*
  (tool-schema
   "Recent workflow runs and open pull requests of the repository Agent Batch dispatches to: what a dispatched phase did, and what it opened. Read-only."
   '(("repo" "string" "owner/name to read (default: the plan's, then the section's).")))
  "The panel's GET /runs, as a tool.")

;;; --- the answers ----------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: agent-batch: ~a" condition)))))

(define-verb plan-text ()
  "The plan as the model and the operator read it: what is stored, and what runs when."
  (let ((plan (load-plan)))
    (with-output-to-string (out)
      (format out "Agent Batch: repo ~a, workflow ~a, model ~a~%"
              (or (secret-text (getf plan :repo)) (secret-text (setting :repo)) "none set")
              (setting :workflow)
              (or (secret-text (getf plan :model)) "the runner's own default"))
      (format out "Tasks (~d):~%~a" (length (getf plan :tasks)) (tasks-text plan))
      (format out "Phases (~d):~%~a" (length (getf plan :phases)) (phases-text plan))
      (format out "Dispatched from this machine:~%~a" (dispatches-text plan)))))

(define-verb save-tasks (args)
  "Store ARGS' task list and clear the phasing it invalidates, as the panel's POST
/plan did; REPO and MODEL, when given, become the plan's own."
  (let* ((tasks (texts (gethash "tasks" args)))
         (plan (load-plan)))
    (unless tasks (fail "tasks must be a non-empty list of task texts"))
    (setf (getf plan :tasks) tasks
          (getf plan :phases) '()
          (getf plan :status) '())
    (let ((repo (gethash "repo" args))
          (model (gethash "model" args)))
      (when (secret-text repo) (setf (getf plan :repo) (repository repo)))
      (when (stringp model) (setf (getf plan :model) (nlk:trimmed model))))
    (save-plan plan)
    (format nil "Stored ~d task~:p~@[ for ~a~]. Read them back with agent_batch_plan, then group every one of them with agent_batch_phase."
            (length tasks) (or (secret-text (getf plan :repo)) (secret-text (setting :repo))))))

(define-verb save-phases (args)
  "Persist ARGS' phasing decision: groups of parallel tasks, in execution order."
  (let* ((plan (set-phases (load-plan) (gethash "phases" args))))
    (save-plan plan)
    (format nil "Saved ~d phase~:p:~%~aDispatch one with agent_batch_dispatch (or /agent-batch dispatch N)."
            (length (getf plan :phases)) (phases-text plan))))

(define-verb dispatch-phase (index &key repo model context base-branch workflow)
  "Launch stored phase INDEX through the repository's workflow -- the original's
workflow_dispatch, with the same four inputs -- and record it locally."
  (let* ((plan (load-plan))
         (phases (getf plan :phases)))
    (unless phases (fail "no phases stored -- call agent_batch_phase first"))
    (unless (and (integerp index) (<= 0 index) (< index (length phases)))
      (fail "phase ~a is not one of the ~d stored (0..~d)"
            index (length phases) (1- (length phases))))
    (let* ((tasks (nth index phases))
           (repo (repository (or repo (getf plan :repo) "")))
           (model (or (secret-text model) (secret-text (getf plan :model))
                      (secret-text (setting :model)) ""))
           (workflow (or (secret-text workflow) (setting :workflow)))
           (base (or (secret-text base-branch) (setting :base-branch))))
      (unless tasks (fail "phase ~d is empty" index))
      (github :post (format nil "/repos/~a/actions/workflows/~a/dispatches" repo workflow)
              :body (nlk:make-json-object
                     "ref" base
                     "inputs" (nlk:make-json-object
                               "tasks" (format nil "~{~a~^~%~}" tasks)
                               "context" (or context "")
                               "model" model
                               "base_branch" base)))
      (save-runs (append (load-runs)
                         (list (list :phase index :tasks tasks :repo repo :model model
                                     :base-branch base :dispatched-at (utc-stamp)))))
      (setf (getf plan :status) (set-phase-status plan index "running"))
      (save-plan plan)
      (format nil "Dispatched phase ~d: ~d task~:p on ~a (~a, ref ~a), one agent per task. agent_batch_runs follows the runs and the pull requests they open."
              index (length tasks) repo workflow base))))

(defun json-word (object &rest path)
  "OBJECT's string at PATH, or NIL -- the shortened keys the original returned."
  (apply #'nlk:json-value object :string path))

(define-verb repo-activity (repo)
  "Recent workflow runs and open pull requests for REPO: each best effort, so one
failing does not hide the other, as the original's /runs route had it."
  (let* ((repo (repository repo))
         (runs '()) (runs-error nil)
         (prs '()) (prs-error nil))
    (handler-case
        (let ((answer (github :get (format nil "/repos/~a/actions/runs?per_page=10" repo))))
          (setf runs (items (and (hash-table-p answer)
                                 (nlk:json-value answer :array "workflow_runs")))))
      (agent-batch-error (condition) (setf runs-error (princ-to-string condition))))
    (handler-case
        (setf prs (items (github :get (format nil "/repos/~a/pulls?state=open&per_page=20" repo))))
      (agent-batch-error (condition) (setf prs-error (princ-to-string condition))))
    (with-output-to-string (out)
      (format out "GitHub activity for ~a~%" repo)
      (cond (runs-error (format out "Workflow runs: ~a~%" runs-error))
            ((null runs) (format out "Workflow runs: none~%"))
            (t (format out "Workflow runs (~d, newest first):~%" (length runs))
               (dolist (run runs)
                 (format out "  ~a ~@[~a~]~@[ on ~a~]~@[ -- ~a~]~@[ ~a~]~%"
                         (or (json-word run "status") "?")
                         (json-word run "conclusion")
                         (json-word run "head_branch")
                         (json-word run "name")
                         (json-word run "html_url")))))
      (cond (prs-error (format out "Open pull requests: ~a~%" prs-error))
            ((null prs) (format out "Open pull requests: none~%"))
            (t (format out "Open pull requests (~d):~%" (length prs))
               (dolist (pull prs)
                 (format out "  #~a ~@[~a~]~@[ (~a)~]~@[ ~a~]~%"
                         (or (nlk:json-value pull :integer "number") "?")
                         (json-word pull "title")
                         (json-word pull "head" "ref")
                         (json-word pull "html_url"))))))))



;;; --- the tools ------------------------------------------------------------------

(defun tool-plan (args)
  (declare (ignore args))
  (answering (plan-text)))

(defun tool-tasks (args)
  (answering (save-tasks args)))

(defun tool-phase (args)
  (answering (save-phases args)))

(defun tool-dispatch (args)
  (answering
    (let ((index (gethash "phase" args)))
      (unless (integerp index) (fail "phase is required: the index agent_batch_plan prints"))
      (dispatch-phase index
                      :repo (gethash "repo" args)
                      :model (gethash "model" args)
                      :context (gethash "context" args)
                      :base-branch (gethash "base_branch" args)
                      :workflow (gethash "workflow" args)))))

(defun tool-runs (args)
  (answering (repo-activity (gethash "repo" args))))

;;; --- the operator's command ------------------------------------------------------

(define-verb reset-plan ()
  "Forget the plan and the dispatches this machine recorded: the original's POST /plan/reset."
  (let ((plan (load-plan)))
    (save-plan (empty-plan))
    (save-runs '())
    (format nil "Cleared the plan: ~d task~:p and ~d phase~:p forgotten, with every dispatch record."
            (length (getf plan :tasks)) (length (getf plan :phases)))))

(defun phase-number (word)
  "WORD as a phase index, or the refusal that says where the indices are."
  (let ((number (and word (ignore-errors (parse-integer word :junk-allowed t)))))
    (unless (and number (>= number 0))
      (fail "dispatch takes a phase's number from the plan, e.g. dispatch 0; it read ~s"
            (or word "")))
    number))

(define-verb run-command (args session-id)
  "One /agent-batch invocation: nothing for the plan, `dispatch N' to launch a
phase, `reset' to forget it. The original's panel, typed."
  (handler-case
      (let ((words (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space #\Tab))))
        (cond
          ((or (null words) (string= (first words) "")) (plan-text))
          ((string-equal (first words) "dispatch") (dispatch-phase (phase-number (second words))))
          ((string-equal (first words) "reset") (reset-plan))
          (t (format nil "/agent-batch takes nothing for the plan, or one of: dispatch N, reset; it read ~s"
                     (nlk:one-line (or args ""))))))
    ;; a command answers words, never a backtrace: the operator reads why in the shell
    (agent-batch-error (condition) (format nil "/agent-batch: ~a" condition))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell agent-batch
  (:section ("agent-batch")
    (:guide "set repo to the owner/name the workflow runs in, and token_env to the environment variable holding a GitHub token (GITHUB_TOKEN is read either way) with actions:write and pull-requests:write; workflow is the file in that repository that takes the tasks input; then store a task list with agent_batch_tasks and group it with agent_batch_phase; /agent-batch prints the plan")
    ("repo" :string
     :doc "owner/name the plan targets; a stored plan's own repo overrides it, and a fresh cell has none")
    ("workflow" :string :default "agent-batch.yml"
     :doc "the workflow file in that repository, which takes tasks, context, model and base_branch inputs")
    ("model" :string
     :doc "provider/model handed to every agent; empty, the runner's own default")
    ("base_branch" :string :default "main"
     :doc "the branch a dispatch runs on, and the branch the agents branch from")
    ("token_env" :env
     :doc "the environment variable holding the GitHub token (GITHUB_TOKEN is read either way)")
    ("token_file" :path
     :doc "a file holding the GitHub token, when it is not in an environment variable")
    ("state_dir" :path
     :doc "the folder holding plan.json and runs.json: <home>/agent-batch/ unless named here")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one GitHub request's ceiling"))
  (:help :agent-batch
         "agent-batch: phase a task list into parallel groups and dispatch each to GitHub Actions agents - (help :agent-batch)"
         +primer+)
  (:command "agent-batch" 'run-command
            :description "Agent Batch: the stored plan, dispatch a phase, or reset it"
            :argument-hint "[dispatch N | reset]")
  (:tool "agent_batch_plan" *plan-schema* 'tool-plan)
  (:tool "agent_batch_tasks" *tasks-schema* 'tool-tasks)
  (:tool "agent_batch_phase" *phase-schema* 'tool-phase)
  (:tool "agent_batch_dispatch" *dispatch-schema* 'tool-dispatch)
  (:tool "agent_batch_runs" *runs-schema* 'tool-runs))
