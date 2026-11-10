;;;; cell.lisp --- mFlow boards, tasks, Knowledge Base and coding agent, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of vladyslavpatrick/mflow-hermes (see NOTICE). The original is
;;;; a pointer plus a skill: an mcp.json naming mFlow's hosted MCP server at
;;;; https://mcp.m-flow.io/mcp behind one bearer token, and a skill teaching a
;;;; model the server's snake_case tools. Here that same endpoint is driven by
;;;; this cell's own JSON-RPC client -- initialize once, the Mcp-Session-Id
;;;; echoed after it, one tools/call per verb -- so what Hermes got as a
;;;; skill arrives as ten tools the model calls, (help :mflow) with the
;;;; skill's own guidance, and /mflow for the operator.

(defpackage #:nodecode-mflow
  (:use #:cl)
  (:export #:mflow-error #:mflow-error-detail))

(in-package #:nodecode-mflow)

(nlk:define-peripheral mflow :not-running t)

;;; --- what does not change -----------------------------------------------------

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks: the one Nodecode's own MCP cell speaks.")

(defparameter +task-cap+ 50
  "The most tasks one create call may carry: the original's own ceiling.")

(defparameter +text-limit+ 8000
  "How many characters of a tool's answer an answer carries.")

(defparameter +primer+
  "mFlow boards, tasks, Knowledge Base and coding agent are reachable through this cell's tools;
each one is one call to mFlow's own MCP server.
  mflow_projects {project?}                    the projects you can see, or one by id
  mflow_tasks {project, key?, status?, limit?} a project's tasks, or one by its readable key
  mflow_create_tasks {project, tasks}          up to 50 tasks, one all-or-nothing call
  mflow_update_task {task, fields}             change a task's fields
  mflow_comment {task, content}                a comment on one task
  mflow_docs {project?, doc_type?}             the Knowledge Base: convention, workflow, metric,
                                               guide or adr
  mflow_update_doc {project, doc?, title, content, doc_type?, adr_status?, superseded_by?}
  mflow_delegate {task, tier?, repo_url?, no_repo_output?, extra_prompt?, ai_provider?}
  mflow_agent_status {task}                    where that delegation stands
  mflow_call {tool, arguments?}                any mFlow tool by name
Most tools need ids, not names: list the projects first and pass the project id after. labelIds
is a full replace, so send every label the task should keep. Nest a subtask with localRef and its
parent's ref as parentRef. An ADR's content carries ## Context, ## Decision and ## Consequences,
stays proposed until the user accepts it, and is superseded rather than deleted. List the docs
before writing one, so a doc is updated and not duplicated. Confirm with the user before delete_*
and transfer_knowledge_base mode move. A delegation answers at once: poll mflow_agent_status and
never in a tight loop. A 401 means the token expired -- the operator puts a fresh Personal Access
Token in the variable the section names. ERROR: MFLOW-ERROR names what failed."
  "What (help :mflow) answers while the cell runs.")

;;; --- the section, read back ---------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the token the calls authenticate with, read once,
here: token written inline, else the variable token_env names. The endpoint is
kept without its trailing slash, and must say how to reach it."
  (declare (ignore table))
  (let* ((env (nonblank (getf values :token-env)))
         (inline (nonblank (getf values :token)))
         (token (or inline (and env (nonblank (uiop:getenv env)))))
         (endpoint (string-right-trim "/" (or (getf values :endpoint) "")))
         (plist (copy-list values)))
    (unless token
      (if env
          (nlk:config-error "token_env names ~a, but nothing is set there: put an mFlow Personal Access Token in it, or write the token here as token" env)
          (nlk:config-error "no mFlow token: set token_env to the variable holding a Personal Access Token, or write the token here as token")))
    (unless (or (uiop:string-prefix-p "https://" endpoint)
                (uiop:string-prefix-p "http://" endpoint))
      (nlk:config-error "endpoint ~s has no http:// or https:// scheme: set it to https://mcp.m-flow.io/mcp, or to the proxy that answers for it" endpoint))
    (setf (getf plist :token) token
          (getf plist :endpoint) endpoint)
    plist))

(defun token-hint ()
  "Where the token came from, for a refusal to name."
  (let ((env (setting :token-env)))
    (if (and (stringp env) (plusp (length env)))
        (format nil "the variable ~a names" env)
        "the section's own token")))

;;; --- one exchange with the endpoint -------------------------------------------

(defvar *session* nil
  "The MCP session id the endpoint issued this start, or NIL: the handshake is
redone when it is gone, and a session the server forgot is taken once more.")

(defun headers-for (session)
  "The headers of one request. Header values are secrets and never appear in a
refusal: a failed status reports its code only."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "mcp-protocol-version" +protocol-version+)
                (cons "authorization" (format nil "Bearer ~a" (setting :token))))
          (and session (list (cons "mcp-session-id" session)))))

(defun post (body &optional session)
  "BODY, one JSON-RPC frame, posted to the configured endpoint => (values TEXT
STATUS HEADERS). A status is a value, not a condition; only a dial that never
completed is raised as one."
  (handler-case
      (nlk:http :post (setting :endpoint)
                :headers (headers-for session)
                :content (nlk:encode-json-object body)
                :timeout (setting :timeout-seconds)
                :connect-timeout (min 10 (setting :timeout-seconds)))
    (error (condition)
      (fail "mFlow's endpoint ~a did not answer: ~a"
            (setting :endpoint) (nlk:one-line (princ-to-string condition))))))

(defun session-of (headers)
  "The session id HEADERS carries, when it is a non-empty string."
  (let ((session (and (hash-table-p headers) (gethash "mcp-session-id" headers))))
    (and (stringp session) (plusp (length session)) session)))

(defun messages-of (text)
  "TEXT, an HTTP body, as the JSON objects it carries: one object, a batch
array, or the `data:' payloads of a server-sent stream."
  (let ((text (nlk:trimmed (or (and (stringp text) text) ""))))
    (cond
      ((zerop (length text)) '())
      ((or (char= (char text 0) #\{) (char= (char text 0) #\[))
       (let ((value (ignore-errors (nlk:decode-json text))))
         (cond ((hash-table-p value) (list value))
               ((and (vectorp value) (not (stringp value)))
                (remove-if-not #'hash-table-p (coerce value 'list)))
               (t '()))))
      (t
       (let ((payloads (loop for line in (uiop:split-string text :separator '(#\Newline))
                             for body = (nlk:trimmed line)
                             when (uiop:string-prefix-p "data:" body)
                               collect (nlk:trimmed (subseq body 5)))))
         (loop for payload in payloads
               for value = (and (plusp (length payload))
                                (ignore-errors (nlk:decode-json payload)))
               when (hash-table-p value) collect value))))))

(defun reply-of (messages id)
  "The object among MESSAGES whose id is ID, or NIL."
  (find id messages :key (lambda (message) (gethash "id" message)) :test #'equal))

(defun rpc-error-text (message)
  "MESSAGE's JSON-RPC error object, as one line."
  (let ((error (nlk:json-value message :object "error")))
    (or (nonblank (nlk:json-value error :string "message"))
        "the server sent no message")))

(defun failure-detail (text)
  "TEXT, a failed answer, in mFlow's own words, one line."
  (let ((message (first (messages-of text))))
    (or (and message (nlk:json-value message :object "error") (rpc-error-text message))
        (nonblank (nlk:json-value message :string "error"))
        (nonblank (nlk:json-value message :string "message"))
        (nonblank (nlk:clip (nlk:one-line text) 300))
        "no body")))

(defun sign-in-advice (status)
  "What a refused token means, in the operator's words; nothing otherwise."
  (if (member status '(401 403))
      (format nil " -- the token expired or was revoked: put a fresh mFlow Personal Access Token in ~a"
              (token-hint))
      ""))

(defun handshake ()
  "initialize, then notifications/initialized => the session id the endpoint
issued, or NIL when it issued none."
  (multiple-value-bind (text status headers)
      (post (nlk:json-object "jsonrpc" "2.0" "id" 1 "method" "initialize"
                              "params" (nlk:json-object
                                        "protocolVersion" +protocol-version+
                                        "capabilities" (nlk:make-json-object)
                                        "clientInfo" (nlk:json-object "name" "nodecode"
                                                                       "version" "0.1.0"))))
    (when (and (integerp status) (>= status 400))
      (fail "mFlow refused the session: HTTP ~a~@[: ~a~]~a"
            status (failure-detail text) (sign-in-advice status)))
    (let ((session (session-of headers)))
      (setf *session* session)
      (post (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized")
            session)
      session)))

(defun result-text (result)
  "RESULT's own words: the text blocks of its content, else the object itself
as JSON, clipped."
  (let ((texts (loop for block across (nlk:json-array result "content")
                     for text = (nlk:json-value block :string "text")
                     when (nonblank text) collect text)))
    (nlk:clip (cond (texts (format nil "~{~a~^~%~}" texts))
                    (result (nlk:encode-json-object result))
                    (t ""))
              +text-limit+)))

(defun call-tool (name arguments &optional retried)
  "NAME, one mFlow tool, with ARGUMENTS on the endpoint => the text its result
carries. A session the server forgot is taken once more, and an answer that
says it failed is a refusal carrying mFlow's words."
  (unless *session* (handshake))
  (let ((id 2))
    (multiple-value-bind (text status)
        (post (nlk:json-object "jsonrpc" "2.0" "id" id "method" "tools/call"
                                "params" (nlk:json-object
                                          "name" name
                                          "arguments" (or arguments (nlk:make-json-object))))
              *session*)
      (let ((message (reply-of (messages-of text) id)))
        (cond
          ((and (not retried) (or (null message) (member status '(400 404))))
           (setf *session* nil)
           (call-tool name arguments t))
          ((and message (nth-value 1 (gethash "error" message)))
           (fail "mFlow's ~a failed: ~a" name (rpc-error-text message)))
          ((null message)
           (fail "mFlow answered nothing to ~a: HTTP ~a~@[: ~a~]~a"
                 name status (and status (failure-detail text)) (sign-in-advice status)))
          (t
           (let* ((result (nlk:json-value message :object "result"))
                  (body (result-text result)))
             (if (nlk:json-value result :boolean "isError")
                 (fail "~a" (or (nonblank body) "mFlow reported an error with no words"))
                 (or (nonblank body)
                     (format nil "mFlow's ~a answered an empty result." name))))))))))

;;; --- arguments the tools shape ------------------------------------------------

(defun merged-arguments (base fields)
  "BASE with each member of the decoded object FIELDS copied in, FIELDS last."
  (let ((arguments (nlk:make-json-object)))
    (flet ((copy (object)
             (when object
               (maphash (lambda (key value) (setf (gethash key arguments) value)) object))))
      (copy base)
      (copy fields))
    arguments))

(defun task-key-p (task)
  "Whether TASK reads as a readable key -- letters, one dash, digits, as
MVPB-123 does -- rather than an id."
  (let ((dash (position #\- task)))
    (and dash
         (plusp dash)
         (< (1+ dash) (length task))
         (null (position #\- task :start (1+ dash)))
         (every #'alpha-char-p (subseq task 0 dash))
         (every #'digit-char-p (subseq task (1+ dash))))))

(defun task-arguments (task)
  "TASK as the one member a call that names a task sends: taskKey for a
readable key such as MVPB-123, taskId for an id."
  (if (task-key-p task)
      (nlk:json-object "taskKey" task)
      (nlk:json-object "taskId" task)))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (mflow-error (condition)
       (nle:failure (format nil "ERROR: ~a" (mflow-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: mFlow: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- the ten tools ------------------------------------------------------------

(defun projects-tool (call)
  "mflow_projects: every project the token can see, or the one CALL names."
  (answering
    (let ((project (nonblank (mflow-projects-project call))))
      (if project
          (call-tool "get_standalone_project" (nlk:json-object "projectId" project))
          (call-tool "list_standalone_projects" (nlk:make-json-object))))))

(defun tasks-tool (call)
  "mflow_tasks: one task by its readable key, or a project's tasks."
  (answering
    (let ((key (nonblank (mflow-tasks-key call)))
          (project (nonblank (mflow-tasks-project call))))
      (cond
        (key (call-tool "get_standalone_task_by_key" (nlk:json-object "taskKey" key)))
        ((null project)
         (fail "mflow_tasks needs the project's id (call mflow_projects first), or a task key such as MVPB-123"))
        (t (call-tool "list_standalone_tasks"
                       (nlk:json-object "projectId" project
                                         :opt "status" (nonblank (mflow-tasks-status call))
                                         :opt "limit" (mflow-tasks-limit call))))))))

(defun create-tasks-tool (call)
  "mflow_create_tasks: one all-or-nothing create, never past the cap."
  (answering
    (let* ((tasks (mflow-create-tasks-tasks call))
           (count (length tasks)))
      (cond
        ((zerop count) (fail "no tasks: mflow_create_tasks needs at least one"))
        ((> count +task-cap+)
         (fail "~d tasks is past mFlow's ~d a call: split the breakdown, or create the parents first and the rest under them"
               count +task-cap+))
        (t (call-tool "create_standalone_tasks"
                       (nlk:json-object "projectId" (mflow-create-tasks-project call)
                                         "tasks" tasks)))))))

(defun update-task-tool (call)
  "mflow_update_task: one task's fields, as the caller wrote them."
  (answering
    (call-tool "update_standalone_task"
                (merged-arguments (task-arguments (mflow-update-task-task call))
                                  (mflow-update-task-fields call)))))

(defun comment-tool (call)
  "mflow_comment: a comment on one task."
  (answering
    (call-tool "create_standalone_comment"
                (merged-arguments (task-arguments (mflow-comment-task call))
                                  (nlk:json-object "content" (mflow-comment-content call))))))

(defun docs-tool (call)
  "mflow_docs: a project's Knowledge Base, or the global one."
  (answering
    (call-tool "list_knowledge_docs"
                (nlk:json-object "projectId" (or (nonblank (mflow-docs-project call)) "__global__")
                                  :opt "type" (nonblank (mflow-docs-doc-type call))))))

(defun update-doc-tool (call)
  "mflow_update_doc: an existing doc updated, or a new one written."
  (answering
    (call-tool "update_knowledge_doc"
                (nlk:json-object "projectId" (mflow-update-doc-project call)
                                  "title" (mflow-update-doc-title call)
                                  "content" (mflow-update-doc-content call)
                                  :opt "docId" (nonblank (mflow-update-doc-doc call))
                                  :opt "type" (nonblank (mflow-update-doc-doc-type call))
                                  :opt "adrStatus" (nonblank (mflow-update-doc-adr-status call))
                                  :opt "supersededBy" (nonblank (mflow-update-doc-superseded-by call))))))

(defun delegate-tool (call)
  "mflow_delegate: one task handed to mFlow's coding agent."
  (answering
    (call-tool "assign_to_coding_agent"
                (merged-arguments
                 (task-arguments (mflow-delegate-task call))
                 (nlk:json-object :opt "tier" (nonblank (mflow-delegate-tier call))
                                   :opt "repoUrl" (nonblank (mflow-delegate-repo-url call))
                                   :opt "noRepoOutput" (nonblank (mflow-delegate-no-repo-output call))
                                   :opt "extraPrompt" (nonblank (mflow-delegate-extra-prompt call))
                                   :opt "aiProvider" (nonblank (mflow-delegate-ai-provider call)))))))

(defun agent-status-tool (call)
  "mflow_agent_status: where a delegation stands."
  (answering
    (call-tool "get_coding_agent_status" (task-arguments (mflow-agent-status-task call)))))

(defun any-tool (call)
  "mflow_call: any mFlow tool by its own name, arguments as written."
  (answering
    (call-tool (mflow-call-tool call) (mflow-call-arguments call))))

;;; --- /mflow -------------------------------------------------------------------

(defun status-line ()
  "What /mflow answers with no question: where it points, and with what."
  (format nil "mFlow: ~a, every call a bearer token from ~a, ~ds a call at most. /mflow QUERY asks mFlow across the boards."
          (setting :endpoint) (token-hint) (setting :timeout-seconds)))

(defun run-command (args session-id)
  "/mflow: with no argument, where the cell points; with a question, mFlow's
own semantic search across the boards (the original's ask_mflow, its last
resort)."
  (declare (ignore session-id))
  (let ((question (nlk:trimmed (or args ""))))
    (if (plusp (length question))
        (answering (call-tool "ask_mflow" (nlk:json-object "query" question)))
        (status-line))))

;;; --- the boundaries ------------------------------------------------------------

(nlk:define-contract mflow-projects
  (:doc "List the mFlow projects this token can see, or fetch one by its id with its statuses, labels, custom columns and counts. Most other mFlow tools take a project id, so this is the first call. Names are not ids: resolve the project here, then pass the id.")
  ("project" :text
             :doc "the project's id; leave it out to list every project"))

(nlk:define-contract mflow-tasks
  (:doc "List a project's tasks, or fetch one by its readable key such as MVPB-123. Filter by status when a board is large.")
  ("project" :text
             :doc "the project's id whose tasks to list; not needed when key names one")
  ("key" :text
         :doc "one task's readable key, MVPB-123")
  ("status" :text
            :doc "keep the tasks in this status, by name or id")
  ("limit" :integer
           :doc "how many tasks to list at most"))

(nlk:define-contract mflow-create-tasks
  (:doc "Create tasks in one project, at most 50 in a call, as one all-or-nothing request: one invalid task creates none. Nest a subtask by giving it a localRef and its parent's localRef as parentRef.")
  ("project" :text :required t
             :doc "the project's id")
  ("tasks" :array :of :object :required t
           :doc "the tasks to create, each an object of mFlow's own fields (title, description, localRef, parentRef, ...), passed as written"))

(nlk:define-contract mflow-update-task
  (:doc "Change one task's fields. labelIds is a full replace: send every label the task should keep, not only the new ones.")
  ("task" :text :required t
          :doc "the task's id, or its readable key such as MVPB-123")
  ("fields" :object :required t
            :doc "the members to change, as mFlow names them (title, description, statusId, assigneeId, labelIds, ...), passed as written"))

(nlk:define-contract mflow-comment
  (:doc "Add a comment to one mFlow task.")
  ("task" :text :required t
          :doc "the task's id, or its readable key such as MVPB-123")
  ("content" :text :required t
             :doc "the comment, in markdown"))

(nlk:define-contract mflow-docs
  (:doc "List a project's Knowledge Base docs: conventions, workflows, metrics, guides and ADRs. List before writing one, so a doc is updated rather than duplicated.")
  ("project" :text
             :doc "the project's id; leave it out for the global docs")
  ("doc_type" :text
              :doc "keep one kind: convention, workflow, metric, guide or adr"))

(nlk:define-contract mflow-update-doc
  (:doc "Write an mFlow Knowledge Base doc: update the one doc names, or write a new one when it names none. An ADR's content carries ## Context, ## Decision and ## Consequences; a new one stays proposed until the user accepts it, and an accepted one is superseded with adr_status and superseded_by rather than deleted.")
  ("project" :text :required t
             :doc "the project's id, or __global__ for a global doc")
  ("doc" :text
         :doc "the id of the doc to update; leave it out to write a new one")
  ("title" :text :required t
           :doc "the doc's title")
  ("content" :text :required t
             :doc "the doc's markdown")
  ("doc_type" :text
              :doc "convention, workflow, metric, guide or adr")
  ("adr_status" :text
                :doc "for an ADR: proposed, or accepted once the user confirms it")
  ("superseded_by" :text
                   :doc "the id of the ADR that replaces this one"))

(nlk:define-contract mflow-delegate
  (:doc "Hand one task to mFlow's coding agent, which clones the repo and works in a sandbox while answering here at once: poll mflow_agent_status afterwards, never in a tight loop. light is 2GB and 1 CPU, mid the default, heavy 7GB and 4 CPUs.")
  ("task" :text :required t
          :doc "the task's id, or its readable key such as MVPB-123")
  ("tier" :text :options '("light" "mid" "heavy")
          :doc "how much machine the sandbox gets")
  ("repo_url" :text
              :doc "the repository to clone, if any")
  ("no_repo_output" :text :options '("zip" "new_repo")
                     :doc "what the agent hands back when it clones no repo")
  ("extra_prompt" :text
                  :doc "instructions beside the task")
  ("ai_provider" :text
                 :doc "which provider the agent runs on"))

(nlk:define-contract mflow-agent-status
  (:doc "Where a coding-agent delegation stands: queued with its queue position, running, or finished.")
  ("task" :text :required t
          :doc "the task's id, or its readable key such as MVPB-123"))

(nlk:define-contract mflow-call
  (:doc "Call any mFlow tool by its own name, for the ones the tools above do not cover: delete_*, transfer_knowledge_base, ask_mflow, and anything mFlow ships later. Arguments are passed as written, so the rules still apply: ids not names, labelIds a full replace, destructive calls confirmed with the user first.")
  ("tool" :text :required t
          :doc "the tool's name on mFlow's MCP server, such as delete_standalone_task")
  ("arguments" :object
               :doc "its arguments, as mFlow names them, passed as written"))

;;; --- the declaration -----------------------------------------------------------

(nle:define-cell mflow
  (:section ("mflow")
    (:guide "put an mFlow Personal Access Token in the environment and the name of that variable here as token_env (MFLOW_TOKEN is the usual one); endpoint stays as it is unless your own proxy answers for mFlow; next step: ask for the projects, then for a project's tasks")
    (:one-of "token" "token_env")
    ("token" :secret
             :doc "the mFlow Personal Access Token, written here")
    ("token_env" :env
                 :doc "the environment variable holding the token")
    ("endpoint" :string :default "https://mcp.m-flow.io/mcp"
                :doc "mFlow's MCP endpoint")
    ("timeout_seconds" :integer :default 120 :min 1
                        :doc "one call's ceiling, in seconds"))
  (:settings #'read-settings)
  (:help :mflow
         "mflow: mFlow boards, tasks, Knowledge Base and coding agent, through mFlow's own MCP server"
         +primer+)
  (:tool "mflow_projects" (nlk:contract 'mflow-projects) #'projects-tool)
  (:tool "mflow_tasks" (nlk:contract 'mflow-tasks) #'tasks-tool)
  (:tool "mflow_create_tasks" (nlk:contract 'mflow-create-tasks) #'create-tasks-tool)
  (:tool "mflow_update_task" (nlk:contract 'mflow-update-task) #'update-task-tool)
  (:tool "mflow_comment" (nlk:contract 'mflow-comment) #'comment-tool)
  (:tool "mflow_docs" (nlk:contract 'mflow-docs) #'docs-tool)
  (:tool "mflow_update_doc" (nlk:contract 'mflow-update-doc) #'update-doc-tool)
  (:tool "mflow_delegate" (nlk:contract 'mflow-delegate) #'delegate-tool)
  (:tool "mflow_agent_status" (nlk:contract 'mflow-agent-status) #'agent-status-tool)
  (:tool "mflow_call" (nlk:contract 'mflow-call) #'any-tool)
  (:command "mflow" 'run-command
            :description "Ask mFlow, or see where the cell points"
            :argument-hint "QUERY")
  (:start (lambda ()
            (setf *session* nil)
            (nle:on-stop (lambda () (setf *session* nil))))))
