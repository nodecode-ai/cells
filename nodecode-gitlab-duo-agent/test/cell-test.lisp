;;;; cell-test.lisp --- the gitlab-duo-agent cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every token variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every REST and GraphQL call a stubbed dex:request,
;;;; and the workflow socket a scripted one bound to *SOCKET-FACTORY*: nothing
;;;; touches the network. The stubs wrap the cell's whole life, so even its
;;;; stop (which asks GitLab to stop a waiting workflow) meets them.

(in-package #:nodecode.test)

(define-test-slice "gitlab-duo-agent" "GITLAB-DUO-AGENT-CELL-" :start nodecode-gitlab-duo-agent:start-cell)

(define-cell-lifecycle-tests "gitlab-duo-agent"
  (:hooks 'nle::models-catalog-table :credential)
  (:command "gitlab-duo-agent")
  (:running (is (nle::find-lane-by-name "gitlab-duo-agent" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "gitlab-duo-agent" nil)) "and taken out"))
  (:refused ("gitlab_url" 5) ("namespace_id" 5)))

(defun duo-agent-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun duo-agent-now ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))

;;; --- a GitLab that answers, and a socket that follows a script -------------------------

(defun duo-agent-answer (log url args)
  "What a GitLab with group 9 (acme), its project 77 (acme/app) and one Duo
model answers dex:request for URL; every request lands on LOG (a box)."
  (let ((method (getf args :method))
        (body (ignore-errors (nlk:decode-json (getf args :content)))))
    (push (list method url (getf args :headers) body) (car log))
    (flet ((ok (text) (values text 200 (make-hash-table :test 'equal))))
      (cond ((and (eq method :get) (search "/api/v4/groups/9/projects" url))
             (ok "[{\"id\":77,\"path_with_namespace\":\"acme/app\"}]"))
            ((and (eq method :get) (search "/api/v4/groups/9" url)) (ok "{\"id\":9,\"full_path\":\"acme\"}"))
            ((eq method :put) (ok "{}"))
            ((search "/direct_access" url) (ok "{\"gitlab_rails\":{\"token\":\"rails-tok\"}}"))
            ((and (eq method :post) (search "/duo_workflows/workflows" url))
             (ok (format nil "{\"id\":~d}" (+ 554 (count-if (lambda (entry) (and (eq (first entry) :post)
                                                                                     (search "/duo_workflows/workflows" (second entry))))
                                                           (car log))))))
            ((search "/api/graphql" url)
             (ok "{\"data\":{\"aiChatAvailableModels\":{\"pinnedModel\":null,\"defaultModel\":{\"name\":\"Claude Sonnet 4.6 - Vertex\",\"ref\":\"claude_sonnet_4_6_vertex\"},\"selectableModels\":[{\"name\":\"Claude Sonnet 4.6 - Vertex\",\"ref\":\"claude_sonnet_4_6_vertex\"},{\"name\":\"Claude Opus 4.8\",\"ref\":\"claude_opus_4_8\"}]}}}"))
            ((eq method :patch) (ok "{}"))
            (t (values "{\"message\":\"404 Not Found\"}" 404 (make-hash-table :test 'equal)))))))

(defun duo-agent-sockets (log script)
  "A *SOCKET-FACTORY* whose Nth socket hands each frame the client sends to
SCRIPT as (N FRAME PUSH), PUSH queueing one event; everything lands on LOG."
  (let ((count 0))
    (lambda (url headers)
      (let* ((n (incf count))
             (socket nil))
        (setf socket (nodecode-gitlab-duo-agent::make-socket
                      (lambda (text)
                        (let ((frame (nlk:decode-json text)))
                          (push (list :sent n frame) (car log))
                          (funcall script n frame
                                   (lambda (event)
                                     (sb-concurrency:send-message
                                      (nodecode-gitlab-duo-agent::socket-mailbox socket) event)))))
                      (lambda () (push (list :closed n) (car log)))))
        (push (list :open n url headers) (car log))
        socket))))

(defun duo-log (&rest entries)
  "A ui_chat_log: each entry (TYPE &key ID CONTENT SUB)."
  (coerce (loop for (type . keys) in entries
                collect (nlk:json-object "message_type" (string-downcase type)
                                         :opt "message_id" (getf keys :id)
                                         :opt "content" (getf keys :content)
                                         :opt "message_sub_type" (getf keys :sub)))
          'vector))

(defun duo-checkpoint (status log)
  "A checkpoint frame at STATUS holding LOG."
  (list :message
        (nlk:encode-json-object
         (nlk:json-object "newCheckpoint"
                          (nlk:json-object "status" status
                                           "checkpoint" (nlk:encode-json-object
                                                         (nlk:json-object "channel_values"
                                                                          (nlk:json-object "ui_chat_log" log))))))))

(defun duo-action (request-id tool arguments)
  "An action frame calling TOOL with the JSON text ARGUMENTS."
  (list :message (nlk:encode-json-object
                  (nlk:json-object "requestID" request-id
                                   "runMCPTool" (nlk:json-object "name" tool "args" arguments)))))

(defun duo-status (status &optional message)
  (list :message (nlk:encode-json-object (nlk:json-object "workflowStatus" (nlk:json-object "status" status)
                                                          :opt "message" message
                                                          "status" status))))

(defmacro with-duo-agent ((log &rest section) script &body body)
  "BODY with GitLab stubbed, the socket scripted by SCRIPT, the cell started
with SECTION (namespace 9 unless it says otherwise), and a gitlab-duo-agent
selection with the GitLab token glpat-test; LOG holds every request, frame
and socket event, newest first."
  `(let ((,log (list nil)))
     (with-stubbed-fdefinition (dex:request (url &rest args) (duo-agent-answer ,log url args))
       (let ((nodecode-gitlab-duo-agent::*socket-factory* (duo-agent-sockets ,log ,script)))
         (with-cell-stop ((gitlab-duo-agent-start ,@(or section '("namespace_id" "9"))))
           (let ((nle::*provider* "gitlab-duo-agent") (nle::*model* "claude_sonnet_4_6_vertex")
                 (nle::*api-key* "glpat-test") (nle::*endpoint* nil) (nle::*model-capability-memo* nil))
             ,@body))))))

(defun duo-sent (log &optional socket)
  "The frames the client sent, oldest first, on SOCKET when given."
  (reverse (loop for entry in (car log)
                 when (and (eq (first entry) :sent) (or (null socket) (eql socket (second entry))))
                   collect (third entry))))

(defun duo-requests (log method fragment)
  "The REST requests of METHOD whose URL holds FRAGMENT, oldest first."
  (reverse (loop for entry in (car log)
                 when (and (eq (first entry) method) (stringp (second entry)) (search fragment (second entry)))
                   collect entry)))

(defun duo-round (messages)
  "One lane round over MESSAGES (chat-shaped); => (MESSAGE FINISH PARTS)."
  (let ((parts '()))
    (multiple-value-bind (message usage finish)
        (nodecode-gitlab-duo-agent::call-duo-workflow-streaming
         (compiled-context (coerce messages (quote list)))
         :on-part (lambda (part) (push (nle::stream-part-kind part) parts)))
      (declare (ignore usage))
      (list message finish (reverse parts)))))

;;; --- the catalog, the lane, the credential ---------------------------------------------

(deftest gitlab-duo-agent-cell-puts-its-row-and-lane-in-place ()
  (with-cell-stop ((gitlab-duo-agent-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "gitlab-duo-agent")))
      (is (equal "GitLab Duo Agent" (nlk:json-value row :string "name")))
      (is (equal "nodecode-gitlab-duo-agent" (nlk:json-value row :string "npm")))
      (is (equal "https://gitlab.com" (nlk:json-value row :string "api")))
      (is (gethash "claude_sonnet_4_6_vertex" (nlk:json-value row :object "models")) "omp's fallback row")
      (is (equal "gitlab-duo-agent" (nle::configured-provider-lane "gitlab-duo-agent"))
          "the row's package resolves to the cell's lane")
      (is (equal "https://gitlab.com" (nle::lane-endpoint "gitlab-duo-agent" "gitlab-duo-agent"))))
    (let ((nodecode-gitlab-duo-agent::*discovered* '(("claude_opus_4_8" . "Claude Opus 4.8") ("gpt_5_codex" . "GPT-5 Codex"))))
      (nodecode-gitlab-duo-agent::forget-catalog)
      (let ((models (nlk:json-value (nle::models-catalog-table) :object "gitlab-duo-agent" "models")))
        (is (null (gethash "claude_sonnet_4_6_vertex" models)) "discovery is authoritative")
        (is (eql 1000000 (nle::catalog-model-context (gethash "claude_opus_4_8" models))))
        (is (eql 400000 (nle::catalog-model-context (gethash "gpt_5_codex" models)))))
      (nodecode-gitlab-duo-agent::forget-catalog))))

(deftest gitlab-duo-agent-cell-credential-ladder ()
  (with-cell-stop ((gitlab-duo-agent-start))
    (with-temp-auth (auth (format nil "{\"oauth_tokens\":{\"gitlab-duo-agent\":{\"access_token\":\"gl-oauth\",\"refresh_token\":\"r1\",\"expires_at\":~d}}}"
                                  (+ (duo-agent-now) 3600)))
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "GITLAB_TOKEN") "glpat-env"))
        (let ((credential (nle::resolve-provider-credential "gitlab-duo-agent" :auth-path auth :probe t)))
          (is (equal "gl-oauth" (nle:credential-key credential)))
          (is (eq :oauth (nle:credential-source credential))))))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "GITLAB_TOKEN") "glpat-env"))
        (is (equal "glpat-env" (nle:credential-key
                                (nle::resolve-provider-credential "gitlab-duo-agent" :auth-path auth :probe t)))))
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (is (eq :public (nle:credential-source
                         (nle::resolve-provider-credential "gitlab-duo-agent" :auth-path auth :probe t)))
            "another provider's key never goes to GitLab")))))

(deftest gitlab-duo-agent-cell-never-sends-another-familys-key ()
  (with-cell-stop ((gitlab-duo-agent-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (cond ((equal name "OPENAI_API_KEY") "sk-openai")
                                       ((equal name "ANTHROPIC_API_KEY") "sk-ant")))
        (let ((credential (nle::resolve-provider-credential "gitlab-duo-agent" :auth-path auth :probe t)))
          (is (not (member (nle:credential-key credential) '("sk-openai" "sk-ant") :test #'equal))
              "no lane family's default variable reaches GitLab")
          (is (eq :public (nle:credential-source credential))))))))

(deftest gitlab-duo-agent-cell-refreshes-with-the-redirect ()
  (with-cell-stop ((gitlab-duo-agent-start))
    (let ((now (duo-agent-now)) (posts '()))
      (with-temp-auth (auth (format nil "{\"oauth_tokens\":{\"gitlab-duo-agent\":{\"access_token\":\"old\",\"refresh_token\":\"r1\",\"expires_at\":~d}}}"
                                    (- now 5)))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name) nil)
             (dex:post (url &rest args)
              (push (list url (getf args :content)) posts)
              (values (format nil "{\"access_token\":\"new\",\"refresh_token\":\"r2\",\"expires_in\":7200,\"created_at\":~d}" now) 200)))
          (is (equal "new" (nle:credential-key (nle::resolve-provider-credential "gitlab-duo-agent" :auth-path auth))))
          (is (equal "https://gitlab.com/oauth/token" (first (first posts))))
          (is (equal "grant_type=refresh_token&client_id=36f2a70cddeb5a0889d4fd8295c241b7e9848e89cf9e599d0eed2d8e5350fbf5&refresh_token=r1&redirect_uri=vscode%3A%2F%2Fgitlab.gitlab-workflow%2Fauthentication"
                     (second (first posts)))
              "this application's refresh sends the redirect again")
          (let ((entry (nlk:json-value (nlk:decode-json (uiop:read-file-string auth)) :object "oauth_tokens" "gitlab-duo-agent")))
            (is (equal "r2" (nlk:json-value entry :string "refresh_token")))
            (is (eql (+ now 7200 -300) (nlk:json-value entry :integer "expires_at")))))))))

;;; --- the wire, as data --------------------------------------------------------------------

(deftest gitlab-duo-agent-cell-renders-the-goal ()
  (let* ((user (nle::message "user" "what is 1+2?"))
         (call (nle::message "assistant" "I will evaluate it."
                             :tool-calls (vector (nle::chat-tool-call-object "req-1" "eval" "{\"form\":\"(+ 1 2)\",\"i\":\"adding\"}"))))
         (result (nle::message "tool" "3" :name "eval" :tool-call-id "req-1"))
         (failed (nle::message "tool" "ERROR: no such form" :name "eval" :tool-call-id "req-2"))
         (next (nle::message "user" "and 2+2?")))
    (is (equal "what is 1+2?" (nodecode-gitlab-duo-agent::goal (vector user))) "a lone turn is its bare text")
    (is (equal "base" (nodecode-gitlab-duo-agent::system-prompt "base" (vector user))) "and needs no note")
    (is (equal (format nil "<|im_start|>user~%what is 1+2?~%<|im_end|>~%<|im_start|>assistant~%I will evaluate it.~%<ran eval>{\"form\":\"(+ 1 2)\"}</ran>~%<|im_end|>~%<|im_start|>tool~%<ran:result>~%3~%<|im_end|>~%<|im_start|>tool~%<ran:result status=error>~%ERROR: no such form~%<|im_end|>~%<|im_start|>user~%and 2+2?~%<|im_end|>")
               (nodecode-gitlab-duo-agent::goal (vector user call result failed next)))
        "a conversation is a flat ChatML transcript, calls as past-tense records, the intent key dropped")
    (let ((system (nodecode-gitlab-duo-agent::system-prompt "base" (vector user call result next))))
      (is (uiop:string-prefix-p (format nil "base~%~%The task below is a transcript") system)
          "a transcript goal brings the note that its markers are a record"))))

(deftest gitlab-duo-agent-cell-reads-ids-and-addresses ()
  (is (equal "gid://gitlab/Group/9" (nodecode-gitlab-duo-agent::graphql-namespace-id "9")))
  (is (equal "acme" (nodecode-gitlab-duo-agent::graphql-namespace-id "acme")))
  (is (equal "9" (nodecode-gitlab-duo-agent::rest-namespace-id "gid://gitlab/Group/9")))
  (is (equal "wss://dws.example/?project_id=7&root_namespace_id=9&user_selected_model_identifier=m&workflow_definition=ambient"
             (nodecode-gitlab-duo-agent::socket-url "https://dws.example" :project-id "7" :namespace-id "9"
                                                                            :model-ref "m" :service-p t))
      "the service host's root, no namespace_id")
  (is (equal "ws://gitlab.local:8080/sub/api/v4/ai/duo_workflows/ws?workflow_definition=ambient"
             (nodecode-gitlab-duo-agent::socket-url "http://gitlab.local:8080/sub/")))
  (is (equal "acme/app" (nodecode-gitlab-duo-agent::remote-project-path "git@gitlab.com:acme/app.git" "https://gitlab.com")))
  (is (equal "acme/app" (nodecode-gitlab-duo-agent::remote-project-path "https://gitlab.com/acme/app.git" "https://gitlab.com")))
  (is (equal "acme/app" (nodecode-gitlab-duo-agent::remote-project-path "https://host/gitlab/acme/app" "https://host/gitlab"))
      "a relative install path is not part of the project")
  (is (null (nodecode-gitlab-duo-agent::remote-project-path "git@github.com:acme/app.git" "https://gitlab.com"))))

;;; --- one workflow, as a lane round ----------------------------------------------------------

(defun duo-agent-two-round-script (n frame push)
  "Socket N: a startRequest is answered with reasoning, text and a tool
call; the tool's result with the answer and FINISHED."
  (declare (ignore n))
  (cond ((nlk:json-value frame :object "startRequest")
         (funcall push (duo-checkpoint "RUNNING" (duo-log '(request :id "q1" :content "what is 1+2?")
                                                          '(agent :id "r1" :content "Let me compute." :sub "reasoning")
                                                          '(agent :id "a1" :content "I will evaluate it."))))
         (funcall push (duo-action "req-1" "eval" "{\"form\":\"(+ 1 2)\"}")))
        ((nlk:json-value frame :object "actionResponse")
         (funcall push (duo-checkpoint "FINISHED" (duo-log '(request :id "q1" :content "what is 1+2?")
                                                           '(agent :id "r1" :content "Let me compute." :sub "reasoning")
                                                           '(agent :id "a1" :content "I will evaluate it.")
                                                           '(tool :id "t1" :content "3")
                                                           '(agent :id "a2" :content "The answer is 3.")))))))

(deftest gitlab-duo-agent-cell-runs-a-workflow-across-a-tool-call ()
  (with-duo-agent (log) #'duo-agent-two-round-script
    (let* ((user (nle::message "user" "what is 1+2?"))
           (first-round (duo-round (vector user))))
      (destructuring-bind (message finish parts) first-round
        (is (equal "tool_calls" finish) "the round ends on the tool call")
        (is (equal "I will evaluate it." (nlk:json-value message :string "content")))
        (is (equal "Let me compute." (nlk:json-value message :string "reasoning_content"))
            "reasoning commentary is reasoning")
        (let ((call (aref (nlk:json-value message :array "tool_calls") 0)))
          (is (equal "req-1" (nlk:json-value call :string "id")) "the call id is the service's requestID")
          (is (equal "eval" (nlk:json-value call :string "function" "name")))
          (is (equal "(+ 1 2)" (nlk:json-value (nlk:decode-json (nlk:json-value call :string "function" "arguments"))
                                               :string "form"))))
        (is (equal :stream-start (first parts)))
        (is (member :reasoning-delta parts))
        (is (member :text-delta parts))
        (is (member :tool-call parts))
        (is (equal :finish (car (last parts)))))
      ;; the setup GitLab saw
      (is (= 1 (length (duo-requests log :put "/api/v4/groups/9"))) "the group's Duo flags, once")
      (let ((access (first (duo-requests log :post "/direct_access"))))
        (is (equal "Bearer glpat-test" (duo-agent-header (third access) "Authorization")))
        (is (equal "gid://gitlab/Group/9" (nlk:json-value (fourth access) :string "root_namespace_id")))
        (is (equal "acme/app" (nlk:json-value (fourth access) :string "project_id")))
        (is (equal "ambient" (nlk:json-value (fourth access) :string "workflow_definition"))))
      (let ((create (fourth (first (duo-requests log :post "/duo_workflows/workflows")))))
        (is (equal "acme/app" (nlk:json-value create :string "project_id")))
        (is (equal "" (nlk:json-value create :string "goal")) "the goal rides the socket"))
      ;; the socket
      (destructuring-bind (marker n url headers) (find :open (car log) :key #'first)
        (declare (ignore marker n))
        (is (equal "wss://gitlab.com/api/v4/ai/duo_workflows/ws?project_id=77&namespace_id=9&root_namespace_id=9&user_selected_model_identifier=claude_sonnet_4_6_vertex&workflow_definition=ambient"
                   url))
        (is (equal "Bearer rails-tok" (duo-agent-header headers "authorization")) "the workflow token, not the GitLab token")
        (is (equal "node-websocket" (duo-agent-header headers "x-gitlab-client-type")))
        (is (equal "https://gitlab.com" (duo-agent-header headers "origin")))
        (is (equal "77" (duo-agent-header headers "x-gitlab-project-id"))))
      (let ((start (nlk:json-value (first (duo-sent log)) :object "startRequest")))
        (is (equal "555" (nlk:json-value start :string "workflowID")))
        (is (equal "what is 1+2?" (nlk:json-value start :string "goal")))
        (is (equal "nodecode_agent" (nlk:json-value start :string "flowConfig" "flow" "entry_point")))
        (is (equal "{{goal}}" (nlk:json-value (aref (nlk:json-value start :array "flowConfig" "prompts") 0)
                                              :string "prompt_template" "user")))
        (let ((metadata (nlk:decode-json (nlk:json-value start :string "workflowMetadata"))))
          (is (equal "claude_sonnet_4_6_vertex" (nlk:json-value metadata :string "selectedModelIdentifier")))
          (is (equal "77" (nlk:json-value metadata :string "projectId"))))
        (let ((tools (nlk:json-value start :array "mcpTools")))
          (is (plusp (length tools)) "the round's tools ride as MCP tools")
          (is (every (lambda (tool) (stringp (nlk:json-value tool :string "inputSchema"))) tools))
          (is (equalp (map 'vector (lambda (tool) (nlk:json-value tool :string "name")) tools)
                      (nlk:json-value start :array "preapproved_tools"))
              "every one pre-approved")))
      ;; the second round: the tool's result goes back on the same socket
      (let* ((call (nle::message "assistant" "I will evaluate it."
                                 :tool-calls (vector (nle::chat-tool-call-object "req-1" "eval" "{\"form\":\"(+ 1 2)\"}"))))
             (result (nle::message "tool" "3" :name "eval" :tool-call-id "req-1")))
        (destructuring-bind (message finish parts) (duo-round (vector user call result))
          (declare (ignore parts))
          (is (equal "stop" finish))
          (is (equal "The answer is 3." (nlk:json-value message :string "content"))
              "only what is new streams: the earlier text is not said again")
          (is (null (nlk:json-value message :string "reasoning_content")))))
      (let ((response (second (duo-sent log 1))))
        (is (equal "req-1" (nlk:json-value response :string "actionResponse" "requestID")))
        (is (equal "3" (nlk:json-value response :string "actionResponse" "plainTextResponse" "response"))))
      (is (null (find :open (remove (find :open (car log) :key #'first :from-end t) (car log)) :key #'first))
          "one socket for both rounds")
      (is (= 1 (length (duo-requests log :post "/duo_workflows/workflows"))) "one workflow")
      (is (null (duo-requests log :patch "/duo_workflows/workflows")) "a finished workflow is not stopped")
      (is (zerop (hash-table-count nodecode-gitlab-duo-agent::*sessions*)) "and nothing waits"))))

(deftest gitlab-duo-agent-cell-a-steer-starts-afresh ()
  (with-duo-agent (log) (lambda (n frame push)
                          (declare (ignore frame))
                          (if (= n 1)
                              (funcall push (duo-action "req-1" "eval" "{\"form\":\"1\"}"))
                              (funcall push (duo-checkpoint "FINISHED" (duo-log '(agent :id "a9" :content "Done."))))))
    (let ((user (nle::message "user" "go")))
      (duo-round (vector user))
      (let* ((call (nle::message "assistant" "" :tool-calls (vector (nle::chat-tool-call-object "req-1" "eval" "{\"form\":\"1\"}"))))
             (result (nle::message "tool" "1" :name "eval" :tool-call-id "req-1"))
             (steer (nle::message "user" "stop, do this instead")))
        (destructuring-bind (message finish parts) (duo-round (vector user call result steer))
          (declare (ignore parts))
          (is (equal "stop" finish))
          (is (equal "Done." (nlk:json-value message :string "content")))))
      (is (= 1 (length (duo-requests log :patch "/duo_workflows/workflows/555"))) "the abandoned workflow is stopped")
      (is (= 2 (length (duo-requests log :post "/duo_workflows/workflows"))) "and a fresh one carries the steer")
      (is (search "stop, do this instead"
                  (nlk:json-value (first (duo-sent log 2)) :string "startRequest" "goal"))))))

(deftest gitlab-duo-agent-cell-restarts-past-the-step-limit ()
  (with-duo-agent (log) (lambda (n frame push)
                          (declare (ignore frame))
                          (if (= n 1)
                              (funcall push (duo-status "FAILED" "The workflow reached its maximum step limit and could not complete."))
                              (funcall push (duo-checkpoint "FINISHED" (duo-log '(agent :id "a1" :content "Finished after all."))))))
    (destructuring-bind (message finish parts) (duo-round (vector (nle::message "user" "long task")))
      (declare (ignore parts))
      (is (equal "stop" finish))
      (is (equal "Finished after all." (nlk:json-value message :string "content"))))
    (is (= 2 (length (duo-requests log :post "/duo_workflows/workflows"))) "a fresh workflow resets the budget")
    (is (= 1 (length (duo-requests log :patch "/duo_workflows/workflows/555"))) "the exhausted one is stopped")))

(deftest gitlab-duo-agent-cell-a-failure-is-a-provider-error ()
  (with-duo-agent (log) (lambda (n frame push)
                          (declare (ignore n frame))
                          (funcall push (duo-status "FAILED" "There was an error processing your request in the Duo Agent Platform, please contact support if the issue persists.")))
    (let ((condition (handler-case (progn (duo-round (vector (nle::message "user" "hi"))) nil)
                       (nle::provider-error (it) it))))
      (is (eql 422 (and condition (nle::provider-error-status condition))) "not retried again by the core")
      (is (search "Duo Agent Platform" (nle::provider-error-detail condition))))
    (is (= 2 (length (duo-requests log :post "/duo_workflows/workflows"))) "omp's one retry on a fresh workflow")
    (is (zerop (hash-table-count nodecode-gitlab-duo-agent::*sessions*)))))

(deftest gitlab-duo-agent-cell-grants-an-approval ()
  (with-duo-agent (log) (lambda (n frame push)
                          (declare (ignore frame))
                          (if (= n 1)
                              (progn (funcall push (duo-status "TOOL_CALL_APPROVAL_REQUIRED"))
                                     (funcall push (list :close 1000 "")))
                              (funcall push (duo-checkpoint "FINISHED" (duo-log '(agent :id "a1" :content "Approved and done."))))))
    (destructuring-bind (message finish parts) (duo-round (vector (nle::message "user" "hi")))
      (declare (ignore parts finish))
      (is (equal "Approved and done." (nlk:json-value message :string "content"))))
    (let ((again (nlk:json-value (first (duo-sent log 2)) :object "startRequest")))
      (is (equal "555" (nlk:json-value again :string "workflowID")) "the same workflow")
      (is (equal "" (nlk:json-value again :string "goal")))
      (is (nlk:json-value again :object "approval" "approval") "with the approval granted"))))

(deftest gitlab-duo-agent-cell-a-silent-socket-restarts-once ()
  (let ((nodecode-gitlab-duo-agent::*idle-seconds* 1))
    (with-duo-agent (log) (lambda (n frame push)
                            (declare (ignore frame))
                            (unless (= n 1)
                              (funcall push (duo-checkpoint "FINISHED" (duo-log '(agent :id "a1" :content "Back."))))))
      (destructuring-bind (message finish parts) (duo-round (vector (nle::message "user" "hi")))
        (declare (ignore parts finish))
        (is (equal "Back." (nlk:json-value message :string "content"))))
      (is (= 2 (length (duo-requests log :post "/duo_workflows/workflows"))))
      (is (= 1 (length (duo-requests log :patch "/duo_workflows/workflows/555")))))))

(deftest gitlab-duo-agent-cell-an-oversized-goal-is-an-overflow ()
  (with-duo-agent (log) (lambda (n frame push) (declare (ignore n frame push)))
    (let ((condition (handler-case
                         (progn (duo-round (vector (nle::message "user" (make-string 2000001 :initial-element #\a)))) nil)
                       (nle::provider-error (it) it))))
      (is (eql 400 (and condition (nle::provider-error-status condition))))
      (is (search "prompt is too long" (nle::provider-error-detail condition)) "the core evicts on this"))
    (is (null (find :open (car log) :key #'first)) "nothing is sent")
    (is (= 1 (length (duo-requests log :patch "/duo_workflows/workflows/555"))) "and the workflow made for it is stopped")))

(deftest gitlab-duo-agent-cell-discovers-the-models-a-namespace-offers ()
  (let ((log (list nil)))
    (with-stubbed-fdefinition (dex:request (url &rest args) (duo-agent-answer log url args))
      (is (equal '(("claude_sonnet_4_6_vertex" . "Claude Sonnet 4.6 - Vertex") ("claude_opus_4_8" . "Claude Opus 4.8"))
                 (nodecode-gitlab-duo-agent::discover-models "https://gitlab.com" "glpat-test" :namespace-id "9"))
              "the selectable models, the pinned one alone when there is one"))))

;;; --- the sign-in ---------------------------------------------------------------------------

(defun duo-agent-notice ()
  "The newest notice said, or NIL."
  (first (first (nlk:notice-log :limit 1))))

(deftest gitlab-duo-agent-cell-signs-in-by-paste ()
  (let ((log (list nil)) (now (duo-agent-now)))
    (with-stubbed-fdefinitions
        ((nle::credential-env (name) nil)
         (dex:request (url &rest args)
          (if (search "/oauth/token" url)
              (progn (push (list :token url (getf args :content)) (car log))
                     (values (format nil "{\"access_token\":\"gl-at\",\"refresh_token\":\"gl-rt\",\"expires_in\":7200,\"created_at\":~d}" now)
                             200 (make-hash-table :test 'equal)))
              (duo-agent-answer log url args))))
      (with-cell-stop ((gitlab-duo-agent-start "namespace_id" "9"))
        (with-temp-auth (auth "{}")
          (let ((nle::*auth-file-path* auth))
            (let* ((answer (nodecode-gitlab-duo-agent::run-command "login" "s1"))
                   (flow nodecode-gitlab-duo-agent::*flow*))
              (is (search (format nil "https://gitlab.com/oauth/authorize?client_id=36f2a70cddeb5a0889d4fd8295c241b7e9848e89cf9e599d0eed2d8e5350fbf5&response_type=code&redirect_uri=vscode%3A%2F%2Fgitlab.gitlab-workflow%2Fauthentication&scope=api&code_challenge=~a&code_challenge_method=S256&state=~a"
                                  (nodecode-gitlab-duo-agent::flow-challenge flow) (nodecode-gitlab-duo-agent::flow-state flow))
                          answer)
                  "GitLab's authorize request for the VS Code application, with PKCE")
              (is (search "received"
                          (nodecode-gitlab-duo-agent::run-command
                           (format nil "code vscode://gitlab.gitlab-workflow/authentication?code=gl-code&state=~a"
                                   (nodecode-gitlab-duo-agent::flow-state flow))
                           "s1")))
              (is (await (:timeout 10) (search "signed in" (or (duo-agent-notice) ""))))
              (let ((token (find :token (car log) :key #'first)))
                (is (equal "https://gitlab.com/oauth/token" (second token)))
                (is (equal (format nil "grant_type=authorization_code&client_id=36f2a70cddeb5a0889d4fd8295c241b7e9848e89cf9e599d0eed2d8e5350fbf5&code=gl-code&redirect_uri=vscode%3A%2F%2Fgitlab.gitlab-workflow%2Fauthentication&code_verifier=~a"
                                   (nodecode-gitlab-duo-agent::flow-verifier flow))
                           (third token))))
              (let ((entry (nlk:json-value (nlk:decode-json (uiop:read-file-string auth)) :object "oauth_tokens" "gitlab-duo-agent")))
                (is (equal "gl-at" (nlk:json-value entry :string "access_token")))
                (is (equal "gl-rt" (nlk:json-value entry :string "refresh_token")))
                (is (eql (+ now 7200 -300) (nlk:json-value entry :integer "expires_at"))))
              (is (await (:timeout 10) (assoc "claude_opus_4_8" nodecode-gitlab-duo-agent::*discovered* :test #'equal))
                  "a sign-in finds the models the account offers")
              (is (search "Signed out" (nodecode-gitlab-duo-agent::run-command "logout" "s1"))))))))))
