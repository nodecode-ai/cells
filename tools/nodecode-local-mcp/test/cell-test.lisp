;;;; cell-test.lisp --- the lifecycle, the two tools, /local-mcp, and the scoping.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops, the catalog the server lists is
;;;; read at start, local_mcp_tools answers it (with the arguments each tool
;;;; requires), local_mcp_call runs one and reports a failed call as a failure,
;;;; the section's include list is what an unattended run may call, and a
;;;; machine with no LMCP app refuses by naming the setting that fixes it.
;;;; Every exchange goes to a canned responder: no process is started here.

(in-package #:nodecode.test)

;;; --- the canned server -------------------------------------------------------------

(defun lmcp-canned-tools ()
  "The catalog the canned server lists: three tools, two with required arguments."
  (vector (nlk:json-object "name" "mail_search" "description" "Search Apple Mail."
                            "inputSchema" (nlk:json-object "type" "object"
                                                            "required" (vector "query")))
          (nlk:json-object "name" "calendar_today" "description" "Today's calendar events."
                            "inputSchema" (nlk:json-object "type" "object"))
          (nlk:json-object "name" "terminal_run" "description" "Run a shell command."
                            "inputSchema" (nlk:json-object "type" "object"
                                                            "required" (vector "command")))))

(defun lmcp-canned-answer (method params)
  "What the canned server answers METHOD with."
  (cond
    ((equal method "initialize")
     (nlk:json-object "serverInfo" (nlk:json-object "name" "LMCP" "version" "9.9.9")))
    ((equal method "tools/list")
     (nlk:json-object "tools" (lmcp-canned-tools)))
    ((equal method "tools/call")
     (let ((name (nlk:json-value params :string "name")))
       (if (equal name "terminal_run")
           (nlk:json-object "isError" t
                            "content" (vector (nlk:json-object "type" "text"
                                                               "text" "refused: read-only mode")))
           (nlk:json-object "content" (vector (nlk:json-object "type" "text"
                                                               "text" (format nil "~a answered" name)))))))
    (t (nlk:json-object))))

(defmacro with-canned-lmcp (&body body)
  "BODY with the process start replaced by the canned server, as if the app
were installed where the launcher looks."
  `(with-stubbed-fdefinitions
       ((nodecode-local-mcp::connect (&rest arguments)
          (let ((connection (nodecode-local-mcp::make-connection
                             :label "LMCP.app" :responder #'lmcp-canned-answer)))
            (nodecode-local-mcp::initialize connection)
            (setf (nodecode-local-mcp::connection-catalog connection)
                  (nodecode-local-mcp::list-tools connection))
            connection))
        (nodecode-local-mcp::server-path ()
          "/Applications/LMCP.app/Contents/MacOS/local-mcp-server")
        (nodecode-local-mcp::existing-file (path)
          "/Applications/LMCP.app/Contents/MacOS/local-mcp-server"))
     ,@body))

(defun lmcp-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun lmcp-json (text)
  (nlk:decode-json text))

;;; --- lifecycle ------------------------------------------------------------------

(define-test-slice "local-mcp" "LOCAL-MCP-CELL-" :start nodecode-local-mcp:start-cell)

(defun lmcp-catalog-length ()
  (length (nodecode-local-mcp::connection-catalog nodecode-local-mcp::*connection*)))

(defun lmcp-requires (tool)
  "The arguments one catalog row says a call needs."
  (nlk:json-array tool "requires"))

(define-cell-lifecycle-tests "local-mcp"
  (:fixture with-canned-lmcp)
  (:config)
  (:help :local-mcp)
  (:command "local-mcp")
  (:running (is (nle::find-tool "local_mcp_tools") "local_mcp_tools is registered")
            (is (nle::find-tool "local_mcp_call") "local_mcp_call is registered")
            (is (= 3 (lmcp-catalog-length)) "the server's catalog is read at start"))
  (:stopped (is (null nodecode-local-mcp::*connection*) "stop drops the connection"))
  (:refused ("timeout_seconds" "soon"))
  (:idle nodecode-local-mcp:local-mcp-error (nodecode-local-mcp::connection)))

(deftest local-mcp-cell-a-machine-without-the-app-refuses-by-naming-the-setting ()
  (with-stubbed-fdefinitions ((nodecode-local-mcp::server-path () nil))
    (is-carrying (refusal (refusal-text nlk:config-refusal (local-mcp-start)))
                 "server_path"
                 "no LMCP server found"))
  (is (null (nle::find-tool "local_mcp_tools")) "a refused start installed nothing"))

;;; --- the tools ------------------------------------------------------------------

(deftest local-mcp-cell-the-catalog-answers-with-the-required-arguments ()
  (with-canned-lmcp
    (with-cell-stop ((local-mcp-start))
      (let* ((answer (lmcp-json (lmcp-tool "local_mcp_tools" (nlk:json-object))))
             (tools (nlk:json-array answer "tools")))
        (is (= 3 (nlk:json-value answer :integer "total")) "every tool the server listed is reported")
        (is (equal "mail_search" (nlk:json-value (aref tools 0) :string "name")) "in the server's order")
        (is (equal "query" (aref (lmcp-requires (aref tools 0)) 0)) "the arguments a call needs are named")
        (is (zerop (length (lmcp-requires (aref tools 1)))) "a tool that needs nothing says so")))))

(deftest local-mcp-cell-one-call-answers-its-text-and-a-refusal-is-a-failure ()
  (with-canned-lmcp
    (with-cell-stop ((local-mcp-start))
      (let ((call (nlk:json-object "tool" "mail_search"
                                    "arguments" (nlk:json-object "query" "invoice"))))
        (is (equal "mail_search answered" (lmcp-tool "local_mcp_call" call)) "the tool's text comes back"))
      (is-carrying (answer (lmcp-tool "local_mcp_call" (nlk:json-object "tool" "terminal_run")))
                   "ERROR:"
                   "refused: read-only mode"
                   (:absent "terminal_run answered"))
      (is-carrying (answer (lmcp-tool "local_mcp_call" (nlk:json-object "tool" "no_such_tool")))
                   "ERROR: local-mcp:"
                   "no_such_tool"))))

(deftest local-mcp-cell-the-include-list-is-what-an-unattended-run-may-call ()
  (with-canned-lmcp
    (with-cell-stop ((local-mcp-start "include" (vector "calendar_today")))
      (let* ((answer (lmcp-json (lmcp-tool "local_mcp_tools" (nlk:json-object))))
             (tools (nlk:json-array answer "tools")))
        (is (= 1 (nlk:json-value answer :integer "total")) "one tool is offered")
        (is (equal "calendar_today" (nlk:json-value (aref tools 0) :string "name")) "and it is the one named"))
      (is-carrying (answer (lmcp-tool "local_mcp_call" (nlk:json-object "tool" "mail_search")))
                   "ERROR: local-mcp:"
                   "mail_search"
                   (:absent "mail_search answered")))))

(deftest local-mcp-cell-the-command-answers-status-list-and-a-call ()
  (with-canned-lmcp
    (with-cell-stop ((local-mcp-start))
      (let ((status (cell-entry "nodecode-local-mcp" "local-mcp" "" nil))
            (listed (cell-entry "nodecode-local-mcp" "local-mcp" "list calendar" nil)))
        (is (search "3 tools" status) "status counts what the server listed")
        (is (search "calendar_today" listed) "list finds a tool by a word of its name"))
      (let ((called (cell-entry "nodecode-local-mcp" "local-mcp"
                                "call mail_search {\"query\": \"invoice\"}" nil)))
        (is (equal "mail_search answered" called) "call runs one tool with the JSON after its name"))
      (is-carrying (answer (cell-entry "nodecode-local-mcp" "local-mcp" "send" nil))
                   "ERROR: local-mcp:"
                   "unknown verb"))))
