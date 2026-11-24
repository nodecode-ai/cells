;;;; cell-test.lisp --- lifecycle, the tools, /mflow and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a token inline,
;;;; a named-but-empty variable refused, an endpoint without a scheme refused,
;;;; and a missing token refused), the MCP exchange (initialize once, the
;;;; session echoed after it, a session the server forgot taken once more, a
;;;; `data:' framed answer read), the ten tools through the seam the model
;;;; calls them by -- the tool they name, the arguments they send and the
;;;; answer they build -- /mflow with and without a question, and a refusal
;;;; mFlow itself wrote. Every request is a stubbed NLK:HTTP, so nothing
;;;; reaches mFlow or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *mflow-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *mflow-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of ARGS => (values TEXT STATUS HEADERS).")

(defun mflow-headers (&optional (session "s-1"))
  "A response header table carrying SESSION as the endpoint's initialize answer does."
  (let ((headers (make-hash-table :test #'equal)))
    (when session (setf (gethash "mcp-session-id" headers) session))
    headers))

(defmacro with-mflow-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *MFLOW-REQUESTS* and
answered by *MFLOW-ANSWER-FN*, so no test dials mFlow. UIOP:GETENV answers
nothing either, so a token in the machine's environment cannot reach a test
that did not ask for one."
  `(let ((*mflow-requests* '())
         (*mflow-answer-fn* (lambda (args)
                               (declare (ignore args))
                               (error "unexpected mFlow request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *mflow-requests*)
                                (funcall *mflow-answer-fn* args))
       (with-stubbed-fdefinition (uiop:getenv (name)
                                  (declare (ignore name))
                                  nil)
         ,@body))))

(defun mflow-asked ()
  "The stubbed requests, oldest first."
  (reverse *mflow-requests*))

(defun mflow-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun mflow-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun mflow-params (request)
  "REQUEST's JSON-RPC params, the object mFlow's tool is called with."
  (nlk:json-value (mflow-request-body request) :object "params"))

(defun mflow-called ()
  "The tool names the stubbed requests carried, oldest first."
  (loop for request in (mflow-asked)
        when (equal "tools/call" (nlk:json-value (mflow-request-body request) :string "method"))
          collect (nlk:json-value (mflow-params request) :string "name")))

(defun mflow-serve (&key (text "ok") (is-error nil) (call-status 200)
                          (initialize-status 200) (session "s-1") refused-on-call)
  "The stubbed endpoint: initialize issues SESSION, the notification is 202,
and each tools/call answers TEXT as its content (or IS-ERROR's refusal) with
CALL-STATUS -- REFUSED-ON-CALL, a count, answers that many calls with a 404
first, as an endpoint that forgot the session does."
  (let ((calls 0))
    (setf *mflow-answer-fn*
          (lambda (args &aux (body (nlk:decode-json (getf args :content))))
            (let ((method (nlk:json-value body :string "method")))
              (cond
                ((equal method "initialize")
                 (values (nlk:encode-json-object
                          (nlk:json-object "jsonrpc" "2.0" "id" 1
                                           "result" (nlk:json-object "protocolVersion" "2025-06-18")))
                         initialize-status (mflow-headers session)))
                ((equal method "notifications/initialized")
                 (values "" 202 (mflow-headers session)))
                ((equal method "tools/call")
                 (if (and refused-on-call (< (incf calls) refused-on-call))
                     (values "{\"error\":\"session not found\"}" 404 (mflow-headers session))
                     (values (format nil "data: ~a"
                                     (nlk:encode-json-object
                                      (nlk:json-object
                                       "jsonrpc" "2.0" "id" (gethash "id" body)
                                       "result" (nlk:json-object
                                                 "content" (vector (nlk:json-object "type" "text"
                                                                                     "text" text))
                                                 "isError" (and is-error t)))))
                             call-status (mflow-headers session))))
                (t (error "unexpected method ~a" method))))))))

(defun mflow-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defun mflow-sent (name)
  "The arguments the last tools/call of NAME carried."
  (loop for request in (reverse (mflow-asked))
        when (equal name (nlk:json-value (mflow-params request) :string "name"))
          return (nlk:json-value (mflow-params request) :object "arguments")))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "mflow" "MFLOW-CELL-" :start nodecode-mflow:start-cell)

(define-cell-lifecycle-tests "mflow"
  (:config "token" "mf_pat_test")
  (:fixture with-mflow-wire)
  (:help :mflow)
  (:command "mflow")
  (:running (is (nle::find-tool "mflow_projects") "mflow_projects is registered")
            (is (nle::find-tool "mflow_call") "and the last of the ten")
            (is (equal "https://mcp.m-flow.io/mcp" (nodecode-mflow::setting :endpoint)) "the default endpoint is mFlow's own")
            (is (equal "mf_pat_test" (nodecode-mflow::setting :token)) "the section's token")
            (is (null nodecode-mflow::*session*) "no session before the first call"))
  (:stopped (is (null (nle::find-tool "mflow_projects")) "stop unregisters the tools")
            (is (null (nle::find-tool "mflow_call")) "every one of them"))
  (:refused ()
            ("token" "mf_pat_test" "endpoint" "mcp.m-flow.io/mcp")
            ("token" "mf_pat_test" "timeout_seconds" 0))
  (:idle nodecode-mflow:mflow-error (nodecode-mflow::run-command "" nil)))

(deftest mflow-cell-a-named-but-empty-token-variable-refuses ()
  (with-mflow-wire
    (is-carrying (answer (refusal-text nlk:config-refusal
                                       (mflow-start "token_env" "MFLOW_TEST_TOKEN")))
                 ("token_env names MFLOW_TEST_TOKEN" "the refusal names the variable")
                 ("Personal Access Token" "and says what to put in it"))
    (is (null (nle::find-tool "mflow_projects")) "a refused start installed nothing")))

(deftest mflow-cell-the-token-may-come-from-the-environment ()
  (with-mflow-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                               (and (string= name "MFLOW_HOME_TOKEN") "mf_pat_env"))
      (with-cell-stop ((mflow-start "token_env" "MFLOW_HOME_TOKEN"))
        (is (equal "mf_pat_env" (nodecode-mflow::setting :token)) "token_env names the variable")))))

;;; --- the exchange ---------------------------------------------------------------

(deftest mflow-cell-one-handshake-then-one-call-a-verb ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (mflow-serve :text "MVPB one, MVPB two")
      (is (equal "MVPB one, MVPB two"
                 (mflow-tool "mflow_projects" (nlk:make-json-object))) "mFlow's own words are the tool's answer")
      (is (equal "MVPB one, MVPB two"
                 (mflow-tool "mflow_projects"
                              (nlk:json-object "project" "proj-1"))) "a second call answers too")
      (let ((requests (mflow-asked)))
        (is (= 4 (length requests)) "initialize, the notification, then one call each")
        (is (equal (list "initialize" "notifications/initialized" "tools/call" "tools/call")
                   (mapcar (lambda (request)
                             (nlk:json-value (mflow-request-body request) :string "method"))
                           requests)))
        (let ((call (third requests)))
          (is (equal "https://mcp.m-flow.io/mcp" (second call)))
          (is (equal "Bearer mf_pat_test" (mflow-request-header call "authorization")) "the token rides as a bearer")
          (is (equal "s-1" (mflow-request-header call "mcp-session-id")) "the session the handshake issued")
          (is (equal "application/json, text/event-stream" (mflow-request-header call "accept")))
          (is (equal "list_standalone_projects" (nlk:json-value (mflow-params call) :string "name")))
          (is (equal "get_standalone_project"
                     (nlk:json-value (mflow-params (fourth requests)) :string "name")) "a project named: the one-project tool")
          (is (equal "proj-1" (nlk:json-value (mflow-sent "get_standalone_project") :string "projectId"))))))))

(deftest mflow-cell-a-forgotten-session-is-taken-once-more ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (mflow-serve :text "MVPB one" :refused-on-call 2)
      (is (equal "MVPB one" (mflow-tool "mflow_projects" (nlk:make-json-object))))
      (is (equal '("initialize" "notifications/initialized" "tools/call"
                   "initialize" "notifications/initialized" "tools/call")
                 (mapcar (lambda (request)
                           (nlk:json-value (mflow-request-body request) :string "method"))
                         (mflow-asked))) "a 404 on the call hands the session over once, then tries the call again"))))

(deftest mflow-cell-a-refused-token-names-where-to-put-a-new-one ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (mflow-serve :initialize-status 401)
      (is-carrying (answer (mflow-tool "mflow_projects" (nlk:make-json-object)))
                   ("ERROR: mFlow refused the session: HTTP 401" "the status is named")
                   ("the section's own token" "and where a fresh one goes")))))

(deftest mflow-cell-a-tool-error-is-a-repair-not-a-crash ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (mflow-serve :text "Task MVPB-9 is not in this project" :is-error t)
      (is-carrying (answer (mflow-tool "mflow_tasks" (nlk:json-object "key" "MVPB-9")))
                   ("ERROR: Task MVPB-9 is not in this project" "mFlow's words, marked failed")))))

;;; --- the tools ---------------------------------------------------------------------

(deftest mflow-cell-tasks-by-key-or-by-project ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (mflow-serve :text "two tasks")
      (is (equal "two tasks" (mflow-tool "mflow_tasks" (nlk:json-object "key" "MVPB-123"))))
      (is (equal "get_standalone_task_by_key" (first (mflow-called))) "a key: the by-key tool")
      (is (equal "MVPB-123" (nlk:json-value (mflow-sent "get_standalone_task_by_key") :string "taskKey")))
      (mflow-tool "mflow_tasks" (nlk:json-object "project" "proj-1" "status" "Todo" "limit" 20))
      (is (equal "list_standalone_tasks" (second (mflow-called))))
      (let ((arguments (mflow-sent "list_standalone_tasks")))
        (is (equal "proj-1" (nlk:json-value arguments :string "projectId")))
        (is (equal "Todo" (nlk:json-value arguments :string "status")))
        (is (= 20 (nlk:json-value arguments :integer "limit"))))
      (is-carrying (answer (mflow-tool "mflow_tasks" (nlk:make-json-object)))
                   ("ERROR: mflow_tasks needs the project's id" "neither a key nor a project: said so"))))) 

(deftest mflow-cell-create-refuses-past-the-cap ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (mflow-serve :text "created MVPB-1 and MVPB-2")
      (let ((two (vector (nlk:json-object "title" "one" "localRef" "a")
                          (nlk:json-object "title" "two" "localRef" "b" "parentRef" "a"))))
        (is (equal "created MVPB-1 and MVPB-2"
                   (mflow-tool "mflow_create_tasks"
                               (nlk:json-object "project" "proj-1" "tasks" two))))
        (let ((arguments (mflow-sent "create_standalone_tasks")))
          (is (equal "proj-1" (nlk:json-value arguments :string "projectId")))
          (is (= 2 (length (nlk:json-array arguments "tasks"))) "both tasks, as written")))
      (let ((many (coerce (loop repeat 51 collect (nlk:json-object "title" "t")) 'vector)))
        (is-carrying (answer (mflow-tool "mflow_create_tasks"
                                          (nlk:json-object "project" "proj-1" "tasks" many)))
                     ("ERROR: 51 tasks is past mFlow's 50 a call" "the cap is named")))
      (is (= 1 (length (mflow-called))) "the refused breakdown never reached the wire"))))

(deftest mflow-cell-update-merges-the-fields-beside-the-task ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (mflow-serve :text "MVPB-123 updated")
      (mflow-tool "mflow_update_task"
                  (nlk:json-object "task" "MVPB-123"
                                   "fields" (nlk:json-object "labelIds" (vector "l1" "l2")
                                                             "statusId" "s-done")))
      (let ((arguments (mflow-sent "update_standalone_task")))
        (is (equal "MVPB-123" (nlk:json-value arguments :string "taskKey")) "a key, not an id")
        (is (equal "s-done" (nlk:json-value arguments :string "statusId")) "the field, at the top")
        (is (equalp #("l1" "l2") (nlk:json-value arguments :array "labelIds")) "the full label list, as the rule says"))
      (mflow-tool "mflow_comment"
                  (nlk:json-object "task" "8f14e45f-ceea-467a-9a3b-9a0e0b0b0b0b" "content" "on it"))
      (let ((arguments (mflow-sent "create_standalone_comment")))
        (is (equal "8f14e45f-ceea-467a-9a3b-9a0e0b0b0b0b" (nlk:json-value arguments :string "taskId")) "an id, not a key")
        (is (equal "on it" (nlk:json-value arguments :string "content")))))))

(deftest mflow-cell-delegates-and-reads-the-status ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (mflow-serve :text "queued at position 3")
      (mflow-tool "mflow_delegate"
                  (nlk:json-object "task" "MVPB-123"
                                   "tier" "heavy"
                                   "repo_url" "https://github.com/example/repo"))
      (let ((arguments (mflow-sent "assign_to_coding_agent")))
        (is (equal "MVPB-123" (nlk:json-value arguments :string "taskKey")))
        (is (equal "heavy" (nlk:json-value arguments :string "tier")))
        (is (equal "https://github.com/example/repo" (nlk:json-value arguments :string "repoUrl"))))
      (is (equal "queued at position 3" (mflow-tool "mflow_agent_status" (nlk:json-object "task" "MVPB-123"))))
      (is (equal "get_coding_agent_status" (second (mflow-called))))
      (is-carrying (answer (mflow-tool "mflow_delegate" (nlk:json-object "task" "MVPB-1" "tier" "huge")))
                   ("tier" "a tier outside the three is refused before any call")))))

(deftest mflow-cell-docs-are-listed-and-written ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (mflow-serve :text "3 docs")
      (mflow-tool "mflow_docs" (nlk:make-json-object))
      (is (equal "__global__" (nlk:json-value (mflow-sent "list_knowledge_docs") :string "projectId")) "no project: the global docs, as the original's own convention")
      (mflow-tool "mflow_docs" (nlk:json-object "project" "proj-1" "doc_type" "adr"))
      (let ((arguments (mflow-sent "list_knowledge_docs")))
        (is (equal "proj-1" (nlk:json-value arguments :string "projectId")))
        (is (equal "adr" (nlk:json-value arguments :string "type"))))
      (mflow-tool "mflow_update_doc"
                  (nlk:json-object "project" "proj-1" "doc" "doc-7" "title" "Use queue"
                                   "content" "## Context\n## Decision\n## Consequences"
                                   "doc_type" "adr" "adr_status" "superseded"
                                   "superseded_by" "doc-9"))
      (let ((arguments (mflow-sent "update_knowledge_doc")))
        (is (equal "doc-7" (nlk:json-value arguments :string "docId")))
        (is (equal "adr" (nlk:json-value arguments :string "type")))
        (is (equal "superseded" (nlk:json-value arguments :string "adrStatus")))
        (is (equal "doc-9" (nlk:json-value arguments :string "supersededBy")))))))

(deftest mflow-cell-any-tool-passes-its-arguments-as-written ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (mflow-serve :text "deleted")
      (is (equal "deleted"
                 (mflow-tool "mflow_call"
                             (nlk:json-object "tool" "delete_standalone_task"
                                              "arguments" (nlk:json-object "taskId" "task-1")))))
      (is (equal '("delete_standalone_task") (mflow-called)) "the name goes through as given")
      (is (equal "task-1" (nlk:json-value (mflow-sent "delete_standalone_task") :string "taskId"))))))

;;; --- /mflow -----------------------------------------------------------------

(deftest mflow-cell-the-command-shows-the-endpoint-or-asks ()
  (with-mflow-wire
    (with-cell-stop ((mflow-start "token" "mf_pat_test"))
      (let ((text (cell-entry "nodecode-mflow" "mflow" "")))
        (is (search "mFlow: https://mcp.m-flow.io/mcp" text))
        (is (search "120s a call at most" text))
        (is (search "/mflow QUERY" text) "the question form is named")
        (is (null (mflow-asked)) "a status read asks mFlow nothing"))
      (mflow-serve :text "MVPB-4 is the one about the queue")
      (let ((text (cell-entry "nodecode-mflow" "mflow" "what is queued")))
        (is (equal "MVPB-4 is the one about the queue" text)))
      (is (equal "ask_mflow" (first (mflow-called))) "a question: mFlow's own search")
      (is (equal "what is queued" (nlk:json-value (mflow-sent "ask_mflow") :string "query"))))))
