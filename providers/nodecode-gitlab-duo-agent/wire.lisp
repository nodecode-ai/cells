;;;; wire.lisp --- the Duo Workflow wire, as data: the start request, the goal, the frames.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure transforms, no I/O, ported from oh-my-pi's ai/src/providers/
;;;; gitlab-duo-workflow.ts and its gitlab-duo-workflow-chatml-note.md (see
;;;; NOTICE).
;;;;
;;;; A Duo workflow is not a chat request. The client starts one with a
;;;; startRequest over a WebSocket: an inline `ambient' flow whose single
;;;; agent's system slot is the round's system prompt and whose user slot is
;;;; the goal, the conversation so far rendered as a flat ChatML transcript
;;;; (a lone user turn as its bare text); the round's tools ride as MCP tools,
;;;; all pre-approved. The service answers with checkpoints, each a whole
;;;; snapshot of the workflow's ui_chat_log: agent messages (reasoning, or the
;;;; answer) between request and tool boundaries. When the agent calls a tool
;;;; the service sends an action (runMCPTool) and waits for the client's
;;;; actionResponse on the same socket. A status says how the workflow stands:
;;;; FINISHED or INPUT_REQUIRED ends it, FAILED or STOPPED fails it, a
;;;; *_APPROVAL_REQUIRED wants the start request again with an approval.

(in-package #:nodecode-gitlab-duo-agent)

(defparameter +client-type+ "node-websocket"
  "What the client says it is: GitLab's language server's WebSocket client.")

(defparameter +language-server-version+ "8.104.0"
  "The GitLab language server version the client headers carry.")

(defparameter +client-capabilities+
  #("incremental_streaming" "read_file_chunked" "shell_command" "command_timeout" "tool_call_approval"))

(defparameter +workflow-definition+ "ambient"
  "The inline flow omp runs, and so this cell.")

(defparameter +agent-name+ "nodecode_agent"
  "The inline flow's single agent component.")

(defparameter +prompt-id+ "nodecode_inline_prompt"
  "The inline flow's prompt.")

(defparameter +server-name+ "nodecode"
  "The MCP server name the tools are advertised under.")

(defparameter +ui-log-events+
  #("on_agent_reasoning" "on_agent_final_answer" "on_tool_execution_success" "on_tool_execution_failed")
  "The ui_chat_log events the inline agent opts into: on_agent_reasoning is
what tags its pre-tool-call commentary as reasoning.")

(defparameter +available-models-query+
  "query nodecode_gitlabDuoWorkflowAvailableModels($rootNamespaceId: GroupID!) {
  aiChatAvailableModels(rootNamespaceId: $rootNamespaceId) {
    defaultModel { name ref }
    selectableModels { name ref }
    pinnedModel { name ref }
  }
}"
  "The GraphQL that names the models a root namespace offers.")

(defparameter +chatml-start+ "<|im_start|>")
(defparameter +chatml-end+ "<|im_end|>")

(defparameter +chatml-note+
  "The task below is a transcript of the conversation so far, written as a plain-text log. Turn boundaries (`<|im_start|>role` … `<|im_end|>`) and any `<ran NAME>{…}</ran>` / `<ran:result>` entries inside it are a RECORD of what already happened — past tool calls and their results. They are not a syntax for you to emit. To call a tool, use your normal structured tool-calling channel; never write `<ran …>`, `<tool_call>`, `<|im_start|>`, or similar markers as your own output."
  "omp's gitlab-duo-workflow-chatml-note.md: appended to the system prompt
whenever the goal is a transcript, so the model does not copy its markers.")

(defparameter +goal-soft-bytes+ 1048576
  "A goal at least this large still goes, but a failure is read as the goal's size.")

(defparameter +goal-hard-bytes+ 2000000
  "A goal at least this large is not sent: the transport fails it nearly always.")

;;; --- namespace and project ids ------------------------------------------------------

(defun graphql-namespace-id (id)
  "ID as GraphQL names a group: a bare number becomes gid://gitlab/Group/N."
  (if (and (plusp (length id)) (every #'digit-char-p id))
      (format nil "gid://gitlab/Group/~a" id)
      id))

(defun rest-namespace-id (id)
  "ID as REST names a group: gid://gitlab/Group/N (or Namespace) becomes N."
  (or (ppcre:register-groups-bind (number) ("^gid://gitlab/(?:Group|Namespace)/(\\d+)$" id) number)
      id))

;;; --- the conversation, rendered -------------------------------------------------------

(defun part-text (content)
  "CONTENT, a message's, as one text: its text parts, an image as
`[image/png image]', joined by newlines."
  (if (stringp content)
      content
      (format nil "~{~a~^~%~}"
              (loop for part in (nle::message-content-parts content)
                    for type = (nle::content-part-type part)
                    when (equal type "text")
                      collect (or (nlk:json-value part :string "text") "")
                    when (equal type "image_url")
                      collect (format nil "[~a image]"
                                      (or (nle::parse-data-uri (nlk:json-value part :string "image_url" "url"))
                                          "image"))))))

(defun replay-arguments (arguments)
  "A tool call's ARGUMENTS, a JSON text, as the object the transcript shows:
the `i' intent key dropped, as omp drops it."
  (let ((object (or (nlk:json-value (ignore-errors (nlk:decode-json (or arguments ""))) :object)
                    (make-hash-table :test 'equal))))
    (if (nth-value 1 (gethash "i" object))
        (let ((copy (nlk:copy-json-object object))) (remhash "i" copy) copy)
        object)))

(defun replay-message (message)
  "MESSAGE, a chat-shaped one, as a transcript turn (a plist), or NIL when it
says nothing."
  (let ((role (nlk:json-value message :string "role"))
        (content (gethash "content" message)))
    (cond ((equal role "tool")
           (let ((text (part-text content)))
             (list :role "tool" :content text
                   :tool-call-id (nlk:json-value message :string "tool_call_id")
                   :tool-name (nlk:json-value message :string "name")
                   :error-p (uiop:string-prefix-p "ERROR" text))))
          ((equal role "assistant")
           (let ((text (if (eq content :null) "" (part-text content)))
                 (calls (loop for call across (nlk:json-array message "tool_calls")
                              collect (list :name (or (nlk:json-value call :string "function" "name") "")
                                            :arguments (replay-arguments
                                                        (nlk:json-value call :string "function" "arguments"))))))
             (when (or (plusp (length text)) calls)
               (list :role "assistant" :content text :tool-calls calls))))
          (t (let ((text (part-text content)))
               (when (plusp (length text))
                 (list :role "user" :content text)))))))

(defun conversation (messages)
  "MESSAGES as the flat transcript's turns, every one equal-weight."
  (loop for message across messages
        for turn = (and (hash-table-p message) (replay-message message))
        when turn collect turn))

(defun render-turn (turn)
  "One TURN as a ChatML block: an assistant's tool calls as past-tense
`<ran NAME>{args}</ran>' records, a tool result under `<ran:result>'."
  (let ((role (getf turn :role))
        (content (getf turn :content)))
    (format nil "~a~a~%~a~a" +chatml-start+ role
            (if (equal role "tool")
                (format nil "<ran:result~:[~; status=error~]>~%~a~%" (getf turn :error-p) content)
                (format nil "~{~a~^~%~}~%"
                        (append (and (plusp (length content)) (list content))
                                (loop for call in (getf turn :tool-calls)
                                      collect (format nil "<ran ~a>~a</ran>" (getf call :name)
                                                      (nlk:encode-json-object (getf call :arguments)))))))
            +chatml-end+)))

(defun latest-user-text (messages)
  "The text of the last user message of MESSAGES, or \"\"."
  (let ((last (find-if (lambda (message) (member (nlk:json-value message :string "role")
                                                 '("user" "developer") :test #'equal))
                       messages :from-end t)))
    (if last (part-text (gethash "content" last)) "")))

(defun goal (messages)
  "The goal MESSAGES make: the bare text of a lone turn, else the ChatML transcript."
  (let ((turns (conversation messages)))
    (if (<= (length turns) 1)
        (latest-user-text messages)
        (format nil "~{~a~^~%~}" (mapcar #'render-turn turns)))))

(defun system-prompt (base messages)
  "The system slot: BASE, with the ChatML note when the goal is a transcript."
  (if (> (length (conversation messages)) 1)
      (if (plusp (length base)) (format nil "~a~%~%~a" base +chatml-note+) +chatml-note+)
      base))

;;; --- the start request ----------------------------------------------------------------

(defun mcp-tools (tools)
  "TOOLS, the round's chat function wrappers, as the MCP tools the flow
advertises: each under its bare name, its schema as a JSON text, approved."
  (map 'vector
       (lambda (wrapper)
         (let* ((function (nlk:json-value wrapper :object "function"))
                (name (nlk:json-value function :string "name")))
           (nlk:json-object "name" name
                            "originalToolName" name
                            "serverName" +server-name+
                            "description" (or (nlk:json-value function :string "description") "")
                            "inputSchema" (nlk:encode-json-object
                                           (or (nlk:json-value function :object "parameters")
                                               (nlk:json-object "type" "object"
                                                                "properties" (make-hash-table :test 'equal)
                                                                "required" #())))
                            "isApproved" t)))
       (or tools #())))

(defun inline-flow-config (system-prompt)
  "The inline ambient flow: one agent whose system slot is SYSTEM-PROMPT and
whose user slot is the goal, routed to the end."
  (nlk:json-object
   "version" "v1"
   "environment" "ambient"
   "flow" (nlk:json-object "entry_point" +agent-name+)
   "components" (vector (nlk:json-object "name" +agent-name+
                                         "type" "AgentComponent"
                                         "prompt_id" +prompt-id+
                                         "toolset" #()
                                         "inputs" (vector (nlk:json-object "from" "context:goal" "as" "goal"))
                                         "ui_log_events" +ui-log-events+))
   "routers" (vector (nlk:json-object "from" +agent-name+ "to" "end"))
   "prompts" (vector (nlk:json-object "name" +prompt-id+
                                      "prompt_id" +prompt-id+
                                      "unit_primitives" #("duo_agent_platform")
                                      "prompt_template" (nlk:json-object "system" system-prompt
                                                                         "user" "{{goal}}"
                                                                         "placeholder" "history")))))

(defun start-request (workflow-id &key system messages tools model-ref project-id namespace-id
                                       (definition +workflow-definition+))
  "The startRequest for WORKFLOW-ID: the goal MESSAGES make, the SYSTEM
prompt in the inline flow, TOOLS as MCP tools, the metadata naming the
MODEL-REF and the scope."
  (let ((mcp (mcp-tools tools)))
    (nlk:json-object
     "workflowID" workflow-id
     "clientVersion" "1.0"
     "workflowDefinition" definition
     "goal" (goal messages)
     "workflowMetadata" (nlk:encode-json-object
                         (nlk:json-object "environment" "ide"
                                          "client_type" +client-type+
                                          :opt "projectId" project-id
                                          :opt "namespaceId" (and namespace-id (rest-namespace-id namespace-id))
                                          :opt "rootNamespaceId" (and namespace-id (rest-namespace-id namespace-id))
                                          "selectedModelIdentifier" model-ref))
     "additional_context" #()
     "clientCapabilities" +client-capabilities+
     "mcpTools" mcp
     "preapproved_tools" (map 'vector (lambda (tool) (gethash "name" tool)) mcp)
     "flowConfigSchemaVersion" "v1"
     "flowConfig" (inline-flow-config (system-prompt system messages)))))

(defun approval-request (start)
  "START, a startRequest, asking again with the pending approval granted."
  (nlk:copy-json-object start "goal" "" "additional_context" #()
                        "approval" (nlk:json-object "approval" (make-hash-table :test 'equal))))

(defun action-response (request-id text error-p)
  "The actionResponse returning a tool's TEXT for REQUEST-ID."
  (nlk:json-object "actionResponse"
                   (nlk:json-object "requestID" request-id
                                    "plainTextResponse" (if error-p
                                                            (nlk:json-object "error" text)
                                                            (nlk:json-object "response" text)))))

(defun create-body (namespace-id project-id &key (definition +workflow-definition+))
  "The body that creates a workflow: in PROJECT-ID when there is one, else
in NAMESPACE-ID; the inline flow's goal rides the socket, not this."
  (nlk:json-object "workflow_definition" definition
                   "environment" "ide"
                   "allow_agent_to_request_user" nil
                   "agent_privileges" #(6)
                   "pre_approved_agent_privileges" #(6)
                   "requires_duo_cli_enabled" nil
                   :when (and namespace-id (not project-id)) "namespace_id" namespace-id
                   :opt "project_id" project-id
                   "goal" ""))

(defun direct-access-body (root-namespace-id project-id &key (definition +workflow-definition+))
  "The body that asks for a workflow token."
  (nlk:json-object "workflow_definition" definition
                   "root_namespace_id" (graphql-namespace-id root-namespace-id)
                   :opt "project_id" project-id))

(defun settings-body ()
  "The group settings the inline MCP flow needs on, and nothing else."
  (nlk:json-object "experiment_features_enabled" t
                   "ai_settings_attributes" (nlk:json-object "duo_agent_platform_enabled" t
                                                             "duo_workflow_mcp_enabled" t)))

;;; --- the socket's address -------------------------------------------------------------

(defun origin-of (url)
  "URL's origin: scheme://host, with the port when it is not the scheme's own."
  (let* ((uri (quri:uri url))
         (port (quri:uri-port uri)))
    (format nil "~a://~a~@[:~a~]" (quri:uri-scheme uri) (quri:uri-host uri)
            (and port (not (member port '(80 443))) port))))

(defun socket-url (base &key project-id namespace-id model-ref (definition +workflow-definition+) service-p)
  "The WebSocket address: the instance's /api/v4/ai/duo_workflows/ws, or the
root of a Duo Workflow Service host (SERVICE-P), with the scope in its query."
  (let* ((base (string-right-trim "/" base))
         (url (if service-p
                  (concatenate 'string (origin-of base) "/")
                  (concatenate 'string base "/api/v4/ai/duo_workflows/ws")))
         ;; http is ws, anything else wss
         (url (let ((scheme-end (search "://" url)))
                (concatenate 'string (if (uiop:string-prefix-p "http://" url) "ws://" "wss://")
                             (if scheme-end (subseq url (+ 3 scheme-end)) url))))
         (query (append (and project-id `(("project_id" . ,project-id)))
                        (and namespace-id (not service-p) `(("namespace_id" . ,(rest-namespace-id namespace-id))))
                        (and namespace-id `(("root_namespace_id" . ,(rest-namespace-id namespace-id))))
                        (and model-ref `(("user_selected_model_identifier" . ,model-ref)))
                        (and definition `(("workflow_definition" . ,definition))))))
    (if query (format nil "~a?~a" url (query-string query)) url)))

(defun socket-headers (token origin &key project-id namespace-id extra)
  "The handshake headers: the workflow TOKEN, the language server's client
identity, the instance's origin (ORIGIN), the scope, and EXTRA (the
service's own) beside them, never over them."
  (let ((own (append
              `(("authorization" . ,(format nil "Bearer ~a" token))
                ("x-gitlab-client-type" . ,+client-type+)
                ("x-gitlab-language-server-version" . ,+language-server-version+)
                ("user-agent" . ,(format nil "unknown/unknown unknown/unknown gitlab-language-server/~a"
                                         +language-server-version+))
                ("origin" . ,(origin-of origin)))
              (and project-id `(("x-gitlab-project-id" . ,project-id)))
              (and namespace-id `(("x-gitlab-namespace-id" . ,(rest-namespace-id namespace-id))
                                  ("x-gitlab-root-namespace-id" . ,(rest-namespace-id namespace-id)))))))
    (append own (remove-if (lambda (pair) (assoc (car pair) own :test #'string-equal)) extra))))

(defun form-encode (text)
  "TEXT as application/x-www-form-urlencoded spells it, the way URLSearchParams does."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets text :external-format :utf-8)
          for char = (code-char byte)
          do (cond ((or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
                        (find char "*-._"))
                    (write-char char out))
                   ((= byte 32) (write-char #\+ out))
                   (t (format out "%~2,'0X" byte))))))

(defun query-string (pairs)
  "PAIRS, an alist of strings, as a query string (or a form body)."
  (format nil "~{~a~^&~}"
          (loop for (key . value) in pairs
                collect (format nil "~a=~a" (form-encode key) (form-encode value)))))

(defun path-segment (text)
  "TEXT encoded as one URL path segment, as encodeURIComponent does."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets text :external-format :utf-8)
          for char = (code-char byte)
          do (if (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
                     (find char "-_.!~*'()"))
                 (write-char char out)
                 (format out "%~2,'0X" byte)))))

;;; --- what the service streams -----------------------------------------------------------

(defun record (value key)
  "VALUE's KEY when it is an object, else NIL."
  (nlk:json-value value :object key))

(defun record-string (value key)
  "VALUE's KEY as a string, a number printed, else NIL."
  (let ((field (and (hash-table-p value) (gethash key value))))
    (typecase field
      (string field)
      (number (princ-to-string field)))))

(defun status-of (event)
  "The workflow status EVENT carries, wherever it carries it."
  (or (record-string event "status")
      (record-string (record event "workflowStatus") "status")
      (record-string (record event "newCheckpoint") "status")))

(defun context-usage (&rest sources)
  "(USED . WINDOW): the agent context occupancy a checkpoint reports, the
Chat Agent's first, or NIL."
  (dolist (source sources)
    (let ((usage (record source "agent_context_usage")))
      (when usage
        (flet ((read-usage (value)
                 (let ((used (nlk:json-value value :number "total_tokens"))
                       (window (nlk:json-value value :number "max_tokens")))
                   (and used window (>= used 0) (plusp window) (cons (round used) (round window))))))
          (let ((found (or (some (lambda (agent) (read-usage (gethash agent usage)))
                                 '("Chat Agent" "context_builder"))
                           (loop for value being the hash-values of usage
                                 thereis (read-usage value)))))
            (when found (return found))))))))

(defun checkpoint-entries (checkpoint-json)
  "The entries of the ui_chat_log CHECKPOINT-JSON holds: (:text KEY CONTENT),
(:thinking KEY CONTENT) for an agent's message, (:boundary) for a request or
a tool; NIL when it holds no log."
  (let* ((checkpoint (ignore-errors (nlk:decode-json checkpoint-json)))
         (log (nlk:json-value checkpoint :array "channel_values" "ui_chat_log")))
    (when log
      (values (loop for entry across log
                    for index from 0
                    for type = (record-string entry "message_type")
                    when (and (equal type "agent") (plusp (length (or (record-string entry "content") ""))))
                      collect (let* ((reasoning-p (equal (record-string entry "message_sub_type") "reasoning"))
                                     (id (record-string entry "message_id")))
                                (list (if reasoning-p :thinking :text)
                                      (if id
                                          (format nil "agent:~a" id)
                                          (format nil "~:[agent~;reasoning~]:~d" reasoning-p index))
                                      (record-string entry "content")))
                    when (member type '("request" "tool") :test #'equal)
                      collect (list :boundary))
              t))))

(defun checkpoint-of (event)
  "(values ENTRIES LENGTH USAGE FOUND) of the checkpoint EVENT carries; FOUND
is NIL when it carries none."
  (let* ((action (record event "action"))
         (checkpoint (or (record action "newCheckpoint") (record event "newCheckpoint") (record event "checkpoint"))))
    (when checkpoint
      (let ((direct (or (record-string checkpoint "message") (record-string checkpoint "text")
                        (record-string checkpoint "content")
                        (record-string (record checkpoint "checkpoint") "message")
                        (record-string (record checkpoint "checkpoint") "text")))
            (usage (context-usage event action checkpoint))
            (json (record-string checkpoint "checkpoint")))
        (multiple-value-bind (entries logged) (and json (checkpoint-entries json))
          (cond (direct (values (list (list :text "direct:text" direct)) (length direct) usage t))
                (logged (values entries (length json) usage t))
                (usage (values '() 0 usage t))))))))

(defun action-of (event)
  "(REQUEST-ID NAME ARGS) of the tool action EVENT carries, or NIL. An action
without its requestID signals: an answer the service cannot match would
never resolve, and the model would ask again forever."
  (flet ((with-call-id (args request-id)
           (let ((object (if (hash-table-p args) args (make-hash-table :test 'equal))))
             (if (or (stringp (gethash "toolCallId" object)) (stringp (gethash "tool_call_id" object)))
                 object
                 (nlk:copy-json-object object "toolCallId" request-id "tool_call_id" request-id))))
         (required (request-id name source)
           (or request-id
               (error 'nle::provider-error
                      :detail (format nil "GitLab Duo Workflow action ~s missing requestID (keys: ~{~a~^, ~})"
                                      name (alexandria:hash-table-keys source))))))
    (let ((wrapped (or (record event "action") (record event "workflowAction") (record event "toolCall"))))
      (if wrapped
          (unless (record wrapped "newCheckpoint")
            (let ((name (or (record-string wrapped "name") (record-string wrapped "action")
                            (record-string wrapped "type") (record-string event "actionName"))))
              (when name
                (let ((request-id (required (or (record-string wrapped "requestID") (record-string wrapped "requestId")
                                                (record-string wrapped "id") (record-string event "requestID")
                                                (record-string event "requestId"))
                                            name wrapped)))
                  (list request-id name
                        (with-call-id (or (record wrapped "args") (record wrapped "arguments") wrapped)
                          request-id))))))
          (loop for name in '("runMCPTool" "run_mcp_tool")
                for args = (record event name)
                when args
                  return (let ((request-id (required (or (record-string event "requestID")
                                                         (record-string event "requestId"))
                                                     name event)))
                           (list request-id name (with-call-id args request-id))))))))

(defun action-tool-call (name args)
  "(values TOOL ARGUMENTS) the action NAME with ARGS asks for: an MCP call's
tool under its bare name and its arguments object; another action as itself."
  (if (member name '("runMCPTool" "run_mcp_tool") :test #'equal)
      (let* ((raw (or (record-string args "toolName") (record-string args "tool_name")
                      (record-string args "name") ""))
             (prefix (format nil "mcp__~a__" +server-name+))
             (tool (if (uiop:string-prefix-p prefix raw) (subseq raw (length prefix)) raw))
             (value (multiple-value-bind (value present) (gethash "args" args)
                      (if present value (gethash "arguments" args))))
             (arguments (cond ((stringp value)
                               (or (nlk:json-value (ignore-errors (nlk:decode-json value)) :object)
                                   (make-hash-table :test 'equal)))
                              ((hash-table-p value) value)
                              (t (make-hash-table :test 'equal)))))
        (values tool arguments))
      (values name (nlk:copy-json-object args))))

(defun step-limit-p (message)
  "Whether MESSAGE is the service's graph-recursion limit."
  (search "reached its maximum step limit" (string-downcase message)))

(defun generic-failure-p (message)
  "Whether MESSAGE is the service's de-identified catch-all failure."
  (search "error processing your request in the duo agent platform" (string-downcase message)))

(defun overflow-message (bytes)
  "What a goal of BYTES says when it is past the byte budget: the core reads
`prompt is too long' on a 400 as the context overflowing, and evicts."
  (format nil "prompt is too long: ~d bytes exceeds the GitLab Duo Agent goal byte budget (soft ~d, hard ~d)"
          bytes +goal-soft-bytes+ +goal-hard-bytes+))
