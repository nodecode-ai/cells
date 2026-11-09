;;;; cell-test.lisp --- lifecycle, the two tools and /birta, against a stubbed MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches birta.dev, needs
;;;; a token or touches the network. What is proved is the cell's own wiring:
;;;; the section (a healthy start, wrong values and a missing token refused),
;;;; the handshake and the session it carries, the two tools through the seam
;;;; the model calls them by -- their JSON-RPC method, their params and their
;;;; answer -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS HEADERS).")

(defmacro with-birta-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials Birta."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected birta request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun birta-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun birta-payload (request)
  "REQUEST's JSON-RPC body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun birta-method (request)
  "REQUEST's JSON-RPC method, as the wire spells it."
  (nlk:json-value (birta-payload request) :text "method"))

(defun birta-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun birta-methods ()
  "The methods the stub was asked, oldest first."
  (mapcar #'birta-method (birta-asked)))

(defun birta-stub (&key (session "s-1") (status 200) (list-status 200)
                         (tools "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[]}}")
                         (answer "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"content\":[]}}"))
  "Answer the handshake, TOOLS for tools/list and ANSWER for tools/call."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((payload (nlk:decode-json (getf args :content)))
                 (name (nlk:json-value payload :text "method")))
            (cond
              ((equal name "initialize")
               (values "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"serverInfo\":{\"name\":\"birta\",\"version\":\"1.0.0\"}}}"
                       status
                       (when (and session (integerp status) (< status 300))
                         (list (cons "mcp-session-id" session)))))
              ((equal name "notifications/initialized") (values "" 202 nil))
              ((equal name "tools/list") (values tools list-status nil))
              ((equal name "tools/call") (values answer 200 nil))
              (t (error "unexpected MCP method ~a" name)))))))

(defun birta-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "birta-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "birta" "BIRTA-CELL-" :start nodecode-birta:start-cell)

(define-cell-lifecycle-tests "birta"
  (:fixture with-birta-wire)
  (:config "token" "test-token")
  (:help :birta)
  (:command "birta")
  (:running
   (is (nle::find-tool "birta_tools") "the listing tool is registered")
   (is (nle::find-tool "birta_call") "and the calling tool"))
  (:stopped
   (is (null (nle::find-tool "birta_tools")) "and stop takes both back")
   (is (null (nle::find-tool "birta_call"))))
  (:refused ("timeout_seconds" 0)
            ("max_chars" 10)))

(deftest birta-cell-a-start-with-no-token-refuses-and-installs-nothing (with-birta-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (birta-start))))
      (is-carrying text "needs a token" "BIRTA_TOKEN")
      (is (null (nle::find-tool "birta_call")) "nothing was installed"))))

(deftest birta-cell-the-token-may-come-from-the-environment (with-birta-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (and (equal name "BIRTA_TOKEN") "env-token"))
    (with-cell-stop ((birta-start))
      (birta-stub)
      (birta-call "birta_tools" "{}")
      (let ((asked (birta-asked)))
        (is (equal "Bearer env-token" (birta-header (third asked) "authorization")) "the exported token is the one sent")))))

;;; --- the handshake and the listing ----------------------------------------------

(deftest birta-cell-lists-the-tools-the-server-offers (with-birta-wire)
  (with-cell-stop ((birta-start "token" "test-token"))
    (birta-stub
     :tools (concatenate 'string
                         "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":["
                         "{\"name\":\"list_projects\",\"description\":\"List the projects you can open.\","
                         " \"inputSchema\":{\"type\":\"object\",\"properties\":{},\"required\":[]}},"
                         "{\"name\":\"publish_version\",\"description\":\"Publish the draft at the project address.\\nMore words here.\","
                         " \"inputSchema\":{\"type\":\"object\",\"required\":[\"project\",\"files\"]}}]}}"))
    (let ((answer (birta-call "birta_tools" "{}")))
      (is (search "list_projects" answer) "the names the server gave")
      (is (search "publish_version" answer) "every one of them")
      (is (search "required: project, files" answer) "and each tool's required arguments")
      (is (null (search "More words here" answer)) "a description is one line")
      (let ((asked (birta-asked)))
        (is (equal '("initialize" "notifications/initialized" "tools/list") (birta-methods)) "the protocol's opening, then the listing")
        (is (eq :post (first (first asked))) "one POST each")
        (is (equal "https://birta.dev/mcp" (second (first asked))) "to the endpoint the section defaults to")
        (is (equal "Bearer test-token" (birta-header (first asked) "authorization")) "with the section's token")
        (is (null (birta-header (first asked) "mcp-session-id")) "the first request carries no session")
        (is (equal "s-1" (birta-header (third asked) "mcp-session-id")) "the session the server named, carried back")))))

(deftest birta-cell-a-server-sent-event-answer-reads-the-same (with-birta-wire)
  (with-cell-stop ((birta-start "token" "test-token"))
    (birta-stub
     :tools (format nil "event: message~%data: {\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[{\"name\":\"stats\"}]}}~%~%"))
    (let ((answer (birta-call "birta_tools" "{}")))
      (is (search "stats" answer) "the tool the event carried"))))

(deftest birta-cell-the-listing-honours-a-match (with-birta-wire)
  (with-cell-stop ((birta-start "token" "test-token"))
    (birta-stub
     :tools (concatenate 'string
                         "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":["
                         "{\"name\":\"list_projects\",\"description\":\"List the projects you can open.\"},"
                         "{\"name\":\"list_leads\",\"description\":\"Read a form's submissions.\"}]}}"))
    (let ((answer (birta-call "birta_tools" "{\"match\": \"leads\"}")))
      (is (search "list_leads" answer) "the tool that matched")
      (is (null (search "list_projects" answer)) "and not the one that did not"))))

;;; --- calling one tool ------------------------------------------------------------

(deftest birta-cell-calls-a-tool-and-reads-its-text (with-birta-wire)
  (with-cell-stop ((birta-start "token" "test-token"))
    (birta-stub
     :answer "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"Published https://birta.dev/p/abc version 4\"}]}}")
    (let ((answer (birta-call "birta_call" "{\"tool\": \"publish_version\", \"arguments\": {\"project\": \"abc\"}}")))
      (is (search "Published https://birta.dev/p/abc version 4" answer) "the tool's own words reach the model")
      (let* ((call (third (birta-asked)))
             (params (nlk:json-value (birta-payload call) :object "params")))
        (is (equal "tools/call" (birta-method call)) "one tools/call")
        (is (equal "publish_version" (nlk:json-value params :text "name")) "naming the tool the model named")
        (is (equal "abc" (nlk:json-value (nlk:json-value params :object "arguments") :text "project")) "with the arguments object it was given")))))

(deftest birta-cell-a-call-with-no-arguments-sends-an-empty-object (with-birta-wire)
  (with-cell-stop ((birta-start "token" "test-token"))
    (birta-stub :answer "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"two projects\"}]}}")
    (let ((answer (birta-call "birta_call" "{\"tool\": \"list_projects\"}")))
      (is (search "two projects" answer) "the answer")
      (let* ((call (third (birta-asked)))
             (params (nlk:json-value (birta-payload call) :object "params")))
        (is (hash-table-p (nlk:json-value params :object "arguments")) "the server still gets an object")))))

(deftest birta-cell-a-failed-call-reads-error-first (with-birta-wire)
  (with-cell-stop ((birta-start "token" "test-token"))
    (birta-stub
     :answer "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"isError\":true,\"content\":[{\"type\":\"text\",\"text\":\"project not found\"}]}}")
    (let ((answer (birta-call "birta_call" "{\"tool\": \"publish_version\"}")))
      (is (search "ERROR: project not found" answer) "a failed call is the model's repair, not a condition up the stack"))))

(deftest birta-cell-a-refused-token-is-the-model-s-repair (with-birta-wire)
  (with-cell-stop ((birta-start "token" "test-token"))
    (birta-stub :status 401)
    (let ((answer (birta-call "birta_tools" "{}")))
      (is (search "ERROR: " answer) "the refusal is the tool result")
      (is (search "token" answer) "and names what to look at"))))

;;; --- the operator's command ------------------------------------------------------

(deftest birta-cell-the-command-reports-what-it-is-connected-to (with-birta-wire)
  (with-cell-stop ((birta-start "token" "test-token"))
    (let ((text (nodecode-birta::run-command "" nil)))
      (is (search "https://birta.dev/mcp" text) "no argument reports the endpoint")
      (is (search "token set" text) "and that a token is there")
      (is (null (search "test-token" text)) "without ever printing it")
      (is (null (birta-asked)) "and sends nothing"))))

(deftest birta-cell-the-command-calls-a-tool-with-its-json (with-birta-wire)
  (with-cell-stop ((birta-start "token" "test-token"))
    (birta-stub :answer "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"two projects\"}]}}")
    (let ((text (nodecode-birta::run-command "  list_projects {\"limit\": 2}  " nil)))
      (is (search "two projects" text) "the tool's answer, as typed at the keyboard")
      (let* ((call (third (birta-asked)))
             (params (nlk:json-value (birta-payload call) :object "params")))
        (is (equal "list_projects" (nlk:json-value params :text "name")) "the tool named")
        (is (= 2 (nlk:json-value (nlk:json-value params :object "arguments") :integer "limit")) "and the JSON object after it")))
    (let ((text (nodecode-birta::run-command "list_projects {oops" nil)))
      (is (search "JSON object" text) "arguments that are not an object are said so"))))
