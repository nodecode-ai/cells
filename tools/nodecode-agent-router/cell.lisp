;;;; cell.lisp --- the section, the agent_route tool, /agents, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the original exposed as an MCP server with one tool, a CLI, an HTTP
;;;; route and a demo page is here one clause of NLE:DEFINE-CELL: a tool the
;;;; model calls, a command the operator types, a topic on the manual, and the
;;;; `agent-router' section they run on. Stop takes every one of them back,
;;;; because the declaration derives it.
;;;;
;;;; Nothing here needs a key, a network or a model: the registry is data, the
;;;; reading of a task is a tokenizer and the decision is arithmetic.

(in-package #:nodecode-agent-router)

;;; --- the words ------------------------------------------------------------------

(defparameter +primer+
  "agent-router: choose one agent for a task from a list, and say why.
  agent_route(task, agents, ...)  one decision - select, fallback or abstain -
      with the chosen agent, its score, the reasons, the runner-up, the
      alternatives, and every agent thrown out with the reason it was.
Each agent is an object: id, name, description, capabilities, tags, tools,
permissions, cost, latency, reliability (the three numbers 0.0-1.0), enabled,
fallback_id, max_actions, scope. Pass agents: [] to rank the registry the
`agent-router' section names.
The caller narrows, never the task: required_capabilities and
required_permissions are the caller's own statement, and forbidden_permissions
is a permission the chosen agent must not hold. allow_fallback: false leaves
the choice alone; failed_agent_id answers what to try after one agent failed,
in one hop, using its fallback_id. min_confidence (0.0-1.0) abstains under a
score; the section holds the floor a call can raise, never lower.
The tool ranks and filters: it does not run the chosen agent, and it writes no
answer of its own. From eval, (agent-router:route \"...\") prints the same
decision and (agent-router:route-json \"...\") answers the JSON; /agents in a
shell asks the registry. (help :agent-router) has this while the cell runs."
  "What (help :agent-router) answers while the cell runs.")

;;; --- the example registry ---------------------------------------------------------

(defparameter *example-registry*
  '((:id "coding-agent" :name "Coding Agent"
     :description "Fixes CORS bugs and writes Python, FastAPI or TypeScript."
     :capabilities ("coding" "python" "typescript" "review" "fastapi")
     :tags ("backend" "frontend" "bug" "api")
     :tools ("read_file" "edit_file" "run_tests")
     :permissions ("read_code" "write_code")
     :cost 0.4 :latency 0.4 :reliability 0.85
     :fallback-id "research-agent" :max-actions 20)
    (:id "devops-agent" :name "DevOps Agent"
     :description "Watches CI, Docker, logs and deploys."
     :capabilities ("devops" "docker" "ci" "logs")
     :tags ("infra" "pipeline")
     :tools ("read_logs" "read_metrics")
     :permissions ("read_logs" "read_metrics")
     :cost 0.5 :latency 0.5 :reliability 0.8
     :fallback-id "research-agent" :max-actions 10)
    (:id "research-agent" :name "Research Agent"
     :description "Searches the documentation and summarises, with no side effect."
     :capabilities ("research" "docs" "search")
     :tags ("documentation")
     :tools ("search_docs")
     :permissions ("read_docs")
     :cost 0.2 :latency 0.3 :reliability 0.9
     :max-actions 8)
    (:id "support-agent" :name "Support Agent"
     :description "Answers the customer ticket and the billing questions."
     :capabilities ("support" "customer" "billing")
     :tags ("ticket" "tickets")
     :tools ("read_tickets")
     :permissions ("read_tickets")
     :cost 0.3 :latency 0.2 :reliability 0.75
     :max-actions 5)
    (:id "security-agent" :name "Security Agent"
     :description "Audits code and logs, without raising any privilege."
     :capabilities ("security" "audit" "review")
     :tags ("audit")
     :tools ("read_code" "read_logs")
     :permissions ("read_code" "read_logs")
     :cost 0.45 :latency 0.45 :reliability 0.88
     :max-actions 8))
  "The original's five demo agents, as the registry a fresh home ranks.")

(defun example-agents ()
  (mapcar #'agent-of *example-registry*))

(defun registry-agents ()
  "The agents /agents ranks: the file the section names, or the example."
  (let ((path (setting :registry)))
    (if (and path (plusp (length path)))
        (let ((data (handler-case (nlk:decode-json (uiop:read-file-string path))
                      (error (condition)
                        (fail "~a is not a JSON file of agents: ~a" path condition)))))
          (let ((rows (or (nlk:json-value data :array)
                          (nlk:json-value data :array "agents"))))
            (unless rows (fail "~a holds no array of agents" path))
            (mapcar #'agent-of (coerce rows 'list))))
        (example-agents))))

(defun registry-text ()
  "The registry as /agents prints it when typed alone, and how to use it."
  (let ((agents (registry-agents))
        (path (setting :registry)))
    (with-output-to-string (out)
      (format out "agent-router: ~d agent~:p in ~a~%"
              (length agents)
              (if (and path (plusp (length path)))
                  path
                  "the example registry (agent-router.registry names your own file)"))
      (dolist (agent agents)
        (format out "  ~a - ~a~%    ~@[capabilities ~{~a~^, ~}~]~@[; permissions ~{~a~^, ~}~]~@[; fallback ~a~]~@[ (disabled)~]~%"
                (agent-id agent) (agent-name agent)
                (agent-capabilities agent)
                (agent-permissions agent)
                (agent-fallback-id agent)
                (not (agent-enabled agent))))
      (format out "Type a task after /agents to rank them, or let the model call agent_route with the agents it has."))))

;;; --- what one call may say -----------------------------------------------------------

(nlk:define-contract route-call
  (:doc "Choose one agent for a task from a list, and say why. Pass the task's
words and the agents to choose between. The answer is a decision - select,
fallback or abstain - with the chosen agent, its score, the reasons, the
runner-up, the alternatives, and every agent thrown out with the reason it was.
It chooses; it never runs the chosen agent and writes no answer of its own.")
  ("task" :text :required t
          :doc "What has to be done, in your own words: it is read, never sent anywhere.")
  ("agents" :array :required t
           :doc "The agents to choose between, one object each: id, name, description,
capabilities, tags, tools, permissions, cost, latency, reliability, enabled,
fallback_id, max_actions, scope. An empty array ranks the configured registry.")
  ("required_capabilities" :array :of :string
                            :doc "Every one of these must be in the agent's capabilities.")
  ("required_permissions" :array :of :string
                           :doc "Every one of these must be among the agent's declared permissions.")
  ("forbidden_permissions" :array :of :string
                            :doc "An agent holding any of these is rejected.")
  ("allow_fallback" :boolean :default t
                     :doc "Answer one runner-up under the choice; false leaves the selection alone.")
  ("min_confidence" :number
                     :doc "0.0 to 1.0: answer abstain when the best score is under it. The section's floor can be raised, never lowered.")
  ("max_alternatives" :integer :doc "How many runners-up to list after the choice.")
  ("failed_agent_id" :string
                      :doc "An agent that just failed: answer what to try instead, one hop, honouring its fallback_id.")
  ("scope" :string :doc "Which namespace of agents to consider; default is the default one."))

;;; --- the verbs ----------------------------------------------------------------------

(defun agents-for (agents)
  (or agents (registry-agents)))

(define-verb route-json (task &rest options)
  "Route TASK and answer the JSON the agent_route tool answers. OPTIONS are the
call's own keys as keywords: (agent-router:route-json \"fix the bug\" :agents '((:id \"a\" ...)))."
  (multiple-value-bind (request agents) (apply #'request-of task options)
    (result-json (decide (agents-for agents) request))))

(define-verb route (task &rest options)
  "Route TASK and answer the lines /agents prints: what was chosen, why, what
came second, and what was thrown out."
  (multiple-value-bind (request agents) (apply #'request-of task options)
    (result-text (decide (agents-for agents) request))))

(define-verb agents-text ()
  "The registry as the table /agents prints, and how to use it."
  (registry-text))

;;; --- the tool ------------------------------------------------------------------------

(defun route-call-tool (call)
  "agent_route: one decision over the agents the caller names, as JSON."
  (multiple-value-bind (request agents)
      (request-of (route-call-task call)
                  :agents (route-call-agents call)
                  :required-capabilities (route-call-required-capabilities call)
                  :required-permissions (route-call-required-permissions call)
                  :forbidden-permissions (route-call-forbidden-permissions call)
                  :allow-fallback (route-call-allow-fallback call)
                  :min-confidence (route-call-min-confidence call)
                  :max-alternatives (route-call-max-alternatives call)
                  :failed-agent-id (route-call-failed-agent-id call)
                  :scope (route-call-scope call))
    (result-json (decide (agents-for agents) request))))

;;; --- /agents -------------------------------------------------------------------------

(defun run-command (args session-id)
  "/agents: typed alone it shows the registry and how to use it; a task after it
routes that task over the registry. => the answer's text."
  (declare (ignore session-id))
  (running-settings)
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (registry-text)
        (route text))))

;;; --- the section, once ---------------------------------------------------------------

(defun read-settings (values table)
  "The section's members, checked where the declaration cannot hold them: a
registry that is not a file refuses at start, not at the first call."
  (declare (ignore table))
  (let ((file (getf values :registry)))
    (when (and file (plusp (length file)) (not (probe-file file)))
      (nlk:config-error "agent-router.registry is ~s, which is not a file" file))
    values))

(nle:define-cell agent-router
  ;; Nothing here is required: an unanswered section ranks the example registry,
  ;; and a task is ranked by its words alone. `registry' turns the five examples
  ;; into the operator's own agents; `min_confidence' is how cautious the
  ;; answer is; `max_alternatives' is how much of the ranking it shows.
  (:section ("agent-router")
    (:guide "nothing here is required: with no registry the router ranks five example agents; min_confidence is a percent and the router abstains under it; registry names a JSON file of your agents - the same array the agent_route tool takes; max_alternatives is how many runners-up an answer lists")
    ("min_confidence" :integer :default 15 :min 0
                      :doc "percent of the best possible score under which the router abstains instead of choosing; a call may raise it, never lower it")
    ("max_alternatives" :integer :default 3 :min 0
                        :doc "how many runners-up one answer lists")
    ("registry" :path
                :doc "a JSON file of agents - an array, or an object with an \"agents\" array; empty ranks the example"))
  (:settings #'read-settings)
  (:tool "agent_route" (nlk:contract 'route-call) 'route-call-tool)
  (:command "agents" 'run-command
            :catches 'agent-router-error
            :description "Which agent should take this task, and why")
  (:help :agent-router "agent-router: choose one agent for a task from a list, and say why"
         +primer+))
