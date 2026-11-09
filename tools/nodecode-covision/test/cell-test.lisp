;;;; cell-test.lisp --- lifecycle, the two tools, a listed one, and /covision.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an integer under its floor and a missing token refused), the
;;;; handshake (one initialize per start, the session id carried back), the
;;;; connector's tools as Nodecode tools through the seam the model calls them
;;;; by -- their names, their arguments and what they answer -- and /covision
;;;; with and without a tool name. Every request is a stubbed NLK:HTTP, so
;;;; nothing reaches the connector or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS HEADERS).")

(defmacro with-covision-wire (&body body)
  "BODY with NLK:HTTP stubbed to a fake connector, as MCP's Streamable HTTP
transport shapes one: a JSON body, or a status with no body."
  `(let ((*requests* '())
         (*answer-fn* #'covision-connector-answer))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun covision-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun covision-message (request)
  "REQUEST's JSON-RPC message, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun covision-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun covision-methods ()
  "The JSON-RPC methods the cell sent, oldest first."
  (mapcar (lambda (request) (nlk:json-value (covision-message request) :string "method"))
          (covision-asked)))

(defun covision-rpc (id result)
  (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun covision-tool (name description properties)
  (nlk:json-object "name" name
                    "description" description
                    "inputSchema" (nlk:json-object
                                   "type" "object"
                                   "properties" (apply #'nlk:make-json-object properties))))

(defun covision-listed-tools ()
  (vector (covision-tool "covision_list_tasks" "List the caller's assigned tasks." '())
          (covision-tool "covision_search_knowledge" "Search published Covision knowledge."
                          '("query" (nlk:json-object "type" "string")))
          (covision-tool "covision_broken" "A tool that reports an error." '())))

(defun covision-connector-answer (method url args)
  "The fake connector: what one MCP message is answered with."
  (declare (ignore method url))
  (let* ((message (nlk:decode-json (getf args :content)))
         (name (nlk:json-value message :string "method"))
         (id (nlk:json-value message :integer "id")))
    (cond
      ((equal name "initialize")
       (values (covision-rpc id (nlk:json-object "protocolVersion" "2025-06-18"
                                                  "capabilities" (nlk:json-object)
                                                  "serverInfo" (nlk:json-object "name" "covision")))
               200 (list (cons "mcp-session-id" "session-1"))))
      ((equal name "notifications/initialized")
       (values "" 202 '()))
      ((equal name "tools/list")
       (values (covision-rpc id (nlk:json-object "tools" (covision-listed-tools))) 200 '()))
      ((equal name "tools/call")
       (let ((tool (nlk:json-value (nlk:json-value message :object "params") :string "name")))
         (values (covision-rpc id
                               (nlk:json-object
                                "content" (vector (nlk:json-object
                                                   "type" "text"
                                                   "text" (if (equal tool "covision_broken")
                                                              "the grant has no such scope"
                                                              (format nil "~a answered: two tasks are overdue." tool))))
                                "isError" (equal tool "covision_broken")))
                 200 '())))
      (t (values (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id
                                                           "error" (nlk:json-object "code" -32601
                                                                                     "message" "no such method")))
                 200 '())))))

(defun covision-tool-fn (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun covision-args (&rest pairs)
  (apply #'nlk:make-json-object pairs))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "covision" "COVISION-CELL-" :start nodecode-covision:start-cell)

(define-cell-lifecycle-tests "covision"
  (:fixture with-covision-wire)
  (:config "access_token" "t-test")
  (:help :covision)
  (:command "covision")
  (:running (is (nle::find-tool "covision_tools") "covision_tools is registered")
            (is (nle::find-tool "covision_call"))
            (is (nle::find-tool "covision_list_tasks") "a tool the connector lists is registered")
            (is (equal "session-1" nodecode-covision::*session-id*) "the handshake kept the session")
            (is (equal "https://connector.covisionperformance.com/mcp" (nodecode-covision::endpoint)) "the original's endpoint is the default")
            (is (= 30 (nodecode-covision::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "covision_tools")) "stop unregisters the tools")
            (is (null (nle::find-tool "covision_list_tasks")) "the connector's too")
            (is (null nodecode-covision::*session-id*) "and the session is forgotten"))
  (:refused ("timeout_seconds" 0) ("access_token" ""))
  (:idle nodecode-covision:covision-error (nodecode-covision::run-command "" nil)))

;;; --- the tools ----------------------------------------------------------------

(deftest covision-cell-call-runs-a-listed-tool (with-covision-wire)
  (with-cell-stop ((covision-start "access_token" "t-test"))
    (let ((answer (covision-tool-fn "covision_call"
                                    (covision-args "tool" "covision_search_knowledge"
                                                   "arguments" (covision-args "query" "onboarding")))))
      (is (string= "covision_search_knowledge answered: two tasks are overdue." answer) "the tool answers what the connector said")
      (let* ((request (car (last (covision-asked))))
             (message (covision-message request)))
        (is (equal "tools/call" (nlk:json-value message :string "method")) "one tools/call went out")
        (is (equal "covision_search_knowledge" (nlk:json-value (nlk:json-value message :object "params") :string "name")) "with the connector's own tool name")
        (is (equal "onboarding" (nlk:json-value (nlk:json-value (nlk:json-value message :object "params") :object "arguments")
                                                 :string "query")) "and the arguments the model gave")
        (is (equal "Bearer t-test" (covision-request-header request "authorization")) "the section's token rides every message")
        (is (equal "session-1" (covision-request-header request "mcp-session-id")) "so does the session the handshake answered")))))

(deftest covision-cell-tools-lists-and-registers (with-covision-wire)
  (with-cell-stop ((covision-start "access_token" "t-test"))
    (let ((answer (covision-tool-fn "covision_tools" (covision-args))))
      (is (search "covision_list_tasks" answer) "the listing names the connector's tools")
      (is (search "arguments: query" answer) "and the arguments each takes")
      (is (nle::find-tool "covision_search_knowledge") "every listed tool is a Nodecode tool, not only the two declared ones")
      (is (equal "covision_search_knowledge answered: two tasks are overdue."
                 (covision-tool-fn "covision_search_knowledge" (covision-args "query" "onboarding"))) "and a listed tool answers through the connector"))))

(deftest covision-cell-tool-error-is-a-failure-line (with-covision-wire)
  (with-cell-stop ((covision-start "access_token" "t-test"))
    (let ((answer (covision-tool-fn "covision_call"
                                    (covision-args "tool" "covision_broken"))))
      (is (search "ERROR: covision" answer) "a tool that reports an error answers the ERROR: line")
      (is (search "the grant has no such scope" answer) "in the connector's own words"))))

(deftest covision-cell-handshakes-once (with-covision-wire)
  (with-cell-stop ((covision-start "access_token" "t-test"))
    (covision-tool-fn "covision_call" (covision-args "tool" "covision_list_tasks"))
    (covision-tool-fn "covision_tools" (covision-args))
    (is (= 1 (count "initialize" (covision-methods) :test #'equal)) "one initialize serves every call of the start")
    (is (= 1 (count "notifications/initialized" (covision-methods) :test #'equal)))))

(deftest covision-cell-command-answers (with-covision-wire)
  (with-cell-stop ((covision-start "access_token" "t-test"))
    (is (assoc "covision" (nle::read-harness-sections "s1") :test #'equal) "the original's skill rides every request")
    (let ((status (nodecode-covision::run-command "" nil))
          (described (nodecode-covision::run-command "covision_list_tasks" nil)))
      (is (search "Covision: https://connector.covisionperformance.com/mcp" status) "no argument answers the connection")
      (is (search "signed in" status) "and whether a token is set")
      (is (search "List the caller's assigned tasks." described) "a tool name describes that tool"))))
