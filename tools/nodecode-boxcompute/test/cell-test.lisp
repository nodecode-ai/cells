;;;; cell-test.lisp --- lifecycle, the six tools and /boxcompute, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches BoxCompute, needs
;;;; a key or touches the network. What is proved is the cell's own wiring:
;;;; the section (a healthy start, wrong values and a missing key refused),
;;;; the MCP handshake, the six tools through the seam the model calls them
;;;; by -- their tool name, their arguments, their headers and their answer
;;;; -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed wire answers a tools/call with: a function of the tool
name and its arguments => the RESULT object the server answers.")

(defvar *error-text* nil
  "When it is a string, the stubbed server answers every tools/call with a
JSON-RPC error carrying it, as a refused call does.")

(defun stub-headers (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (name value) on pairs by #'cddr do (setf (gethash name table) value))
    table))

(defun stub-reply (id result)
  "One JSON-RPC result answer to ID."
  (nlk:encode-json-object (nlk:make-json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun stub-tool-result (text)
  "The result object a server answers a tool call with, carrying TEXT."
  (nlk:make-json-object
   "content" (coerce (list (nlk:make-json-object "type" "text" "text" text)) 'vector)
   "isError" nil))

(defun answer-request (message)
  "What the stubbed server answers MESSAGE -- a decoded JSON-RPC request =>
 (values BODY STATUS HEADERS)."
  (let ((method (nlk:json-value message :text "method"))
        (id (nlk:json-value message :integer "id")))
    (cond ((equal method "initialize")
           (values (stub-reply id (nlk:make-json-object
                                   "protocolVersion" "2025-06-18"
                                   "capabilities" (nlk:json-object)
                                   "serverInfo" (nlk:make-json-object "name" "boxcompute")))
                   200 (stub-headers "content-type" "application/json"
                                     "mcp-session-id" "sess-test")))
          ((equal method "notifications/initialized")
           (values "" 202 (stub-headers "content-type" "application/json")))
          ((and (equal method "tools/call") *error-text*)
           (values (nlk:encode-json-object
                    (nlk:make-json-object "jsonrpc" "2.0" "id" id
                                           "error" (nlk:make-json-object
                                                    "code" -32602 "message" *error-text*)))
                   200 (stub-headers "content-type" "application/json")))
          ((and (equal method "tools/call") *reply*)
           (let ((result (funcall *reply*
                                   (nlk:json-value (nlk:json-value message :object "params") :text "name")
                                   (nlk:json-value (nlk:json-value message :object "params") :object "arguments"))))
             (values (stub-reply id result) 200
                     (stub-headers "content-type" "application/json"))))
          (t (values (stub-reply id (nlk:make-json-object "tools" (vector)))
                     200 (stub-headers "content-type" "application/json"))))))

(defmacro with-boxcompute-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by ANSWER-REQUEST, so no test dials BoxCompute."
  `(let ((*requests* '())
         (*reply* nil)
         (*error-text* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (answer-request (nlk:decode-json (getf args :content))))
       (let ((nodecode-boxcompute::*session-id* nil)
             (nodecode-boxcompute::*session-open-p* nil))
         ,@body))))

(defun bc-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun bc-asked-for (method)
  "The stubbed requests whose JSON-RPC method is METHOD, oldest first."
  (remove-if-not (lambda (request)
                   (equal (bc-request-method request) method))
                 (bc-asked)))

(defun bc-request-message (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun bc-request-method (request)
  (nlk:json-value (bc-request-message request) :text "method"))

(defun bc-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun bc-tool-arguments (request)
  "REQUEST's tools/call arguments, as a JSON object."
  (nlk:json-value (nlk:json-value (bc-request-message request) :object "params") :object "arguments"))

(defun bc-tool-name (request)
  "REQUEST's tools/call tool name."
  (nlk:json-value (nlk:json-value (bc-request-message request) :object "params") :text "name"))

(defun bc-answer-text (text)
  "Make every tool call answer TEXT."
  (setf *reply* (lambda (name arguments)
                  (declare (ignore name arguments))
                  (stub-tool-result text))))

(defun bc-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "boxcompute-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "boxcompute" "BOXCOMPUTE-CELL-" :start nodecode-boxcompute:start-cell)

(define-cell-lifecycle-tests "boxcompute"
  (:fixture with-boxcompute-wire)
  (:config "api_key" "bxc-test")
  (:help :boxcompute)
  (:command "boxcompute")
  (:running
   (is (nle::find-tool "boxcompute_sandbox") "sandbox is registered")
   (is (nle::find-tool "boxcompute_run") "and run")
   (is (nle::find-tool "boxcompute_process") "and process")
   (is (nle::find-tool "boxcompute_file") "and file")
   (is (nle::find-tool "boxcompute_preview") "and preview")
   (is (nle::find-tool "boxcompute_tool") "and the escape hatch")
   (is (= 6 (count-if (lambda (entry) (eql 0 (search "boxcompute_" (first entry))))
                       nle::*tools*)) "six of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "boxcompute_sandbox")) "and stop takes every one back")
   (is (null (nle::find-tool "boxcompute_tool"))))
  (:refused ("timeout_seconds" 1)))

(deftest boxcompute-cell-a-start-with-no-key-refuses-and-installs-nothing (with-boxcompute-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (boxcompute-start))))
      (is-carrying text "needs a key" "BOXCOMPUTE_API_KEY")
      (is (null (nle::find-tool "boxcompute_sandbox")) "nothing was installed"))))

(deftest boxcompute-cell-the-key-may-come-from-the-environment (with-boxcompute-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (and (equal name "BOXCOMPUTE_API_KEY") "bxc-env"))
    (with-cell-stop ((boxcompute-start))
      (bc-answer-text "ok")
      (bc-call "boxcompute_tool" "{\"tool\": \"account_get\"}")
      (let ((call (first (bc-asked-for "tools/call"))))
        (is (equal "Bearer bxc-env" (bc-request-header call "authorization")) "the exported key is the one sent")))))

;;; --- the handshake ----------------------------------------------------------------

(deftest boxcompute-cell-a-call-opens-the-session-first (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (bc-answer-text "two VMs")
    (bc-call "boxcompute_sandbox" "{\"action\": \"list\"}")
    (let ((asked (bc-asked)))
      (is (equal '("initialize" "notifications/initialized" "tools/call")
                 (mapcar #'bc-request-method asked)) "initialize, then the notification, then the call")
      (let ((hello (first asked)))
        (is (equal "2025-06-18"
                   (nlk:json-value (nlk:json-value (bc-request-message hello) :object "params")
                                   :text "protocolVersion")) "the client names the protocol revision it speaks")
        (is (null (bc-request-header hello "mcp-session-id")) "with no session yet"))
      (is (equal "sess-test" (bc-request-header (third asked) "mcp-session-id")) "and the session the server issued rides the call"))))

;;; --- sandbox --------------------------------------------------------------------

(deftest boxcompute-cell-list-names-the-servers-tool (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (bc-answer-text "sb-1 small running")
    (let ((answer (bc-call "boxcompute_sandbox" "{\"action\": \"list\"}")))
      (let ((call (first (bc-asked-for "tools/call"))))
        (is (equal "sandbox_list" (bc-tool-name call)) "sandbox_list is what the server hears")
        (is (null (nlk:json-value (bc-tool-arguments call) :text "sandbox")) "and a list sends no id"))
      (is (equal "sb-1 small running" answer) "the server's own text is the answer"))))

(deftest boxcompute-cell-create-carries-the-size-and-the-network (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (bc-answer-text "created sb-2")
    (bc-call "boxcompute_sandbox" "{\"action\": \"create\", \"size\": \"xl\", \"internet\": false}")
    (let* ((call (first (bc-asked-for "tools/call")))
           (arguments (bc-tool-arguments call)))
      (is (equal "sandbox_create" (bc-tool-name call)) "one call, sandbox_create")
      (is (equal "xl" (nlk:json-value arguments :text "size")) "at the size the call named")
      (is (eq nil (nlk:json-value arguments :boolean "internet")) "and a sealed machine"))))

(deftest boxcompute-cell-stop-needs-a-vm (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (let ((answer (bc-call "boxcompute_sandbox" "{\"action\": \"stop\"}")))
      (is (search "sandbox" answer) "the refusal names the member left out")
      (is (null (bc-asked-for "tools/call")) "and nothing was sent"))))

;;; --- commands, processes, files and links ------------------------------------------

(deftest boxcompute-cell-run-sends-the-command-to-the-vm (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (bc-answer-text "hello")
    (let ((answer (bc-call "boxcompute_run" "{\"sandbox\": \"sb-1\", \"command\": \"echo hello\"}")))
      (let* ((call (first (bc-asked-for "tools/call")))
             (arguments (bc-tool-arguments call)))
        (is (equal "command_run" (bc-tool-name call)) "the server's command_run")
        (is (equal "sb-1" (nlk:json-value arguments :text "sandbox")) "on the VM named")
        (is (equal "echo hello" (nlk:json-value arguments :text "command")) "with the command verbatim"))
      (is (equal "hello" answer) "and its output is the answer"))))

(deftest boxcompute-cell-a-process-polls-the-id-start-answered (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (bc-answer-text "running")
    (bc-call "boxcompute_process" "{\"action\": \"output\", \"sandbox\": \"sb-1\", \"process\": \"pr-9\"}")
    (let* ((call (first (bc-asked-for "tools/call")))
           (arguments (bc-tool-arguments call)))
      (is (equal "process_output" (bc-tool-name call)) "process_output")
      (is (equal "pr-9" (nlk:json-value arguments :text "process")) "for the process named"))))

(deftest boxcompute-cell-writing-a-file-sends-its-whole-content (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (bc-answer-text "wrote /workspace/a.txt")
    (bc-call "boxcompute_file"
             "{\"action\": \"write\", \"sandbox\": \"sb-1\", \"path\": \"/workspace/a.txt\", \"content\": \"hi\"}")
    (let* ((call (first (bc-asked-for "tools/call")))
           (arguments (bc-tool-arguments call)))
      (is (equal "file_write" (bc-tool-name call)) "file_write")
      (is (equal "/workspace/a.txt" (nlk:json-value arguments :text "path")) "at the path named")
      (is (equal "hi" (nlk:json-value arguments :text "content")) "with the content named"))))

(deftest boxcompute-cell-a-preview-link-names-its-port (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (bc-answer-text "https://preview.boxcompute.ai/p/abc")
    (let ((answer (bc-call "boxcompute_preview" "{\"action\": \"create\", \"sandbox\": \"sb-1\", \"port\": 8000}")))
      (let* ((call (first (bc-asked-for "tools/call")))
             (arguments (bc-tool-arguments call)))
        (is (equal "preview_create" (bc-tool-name call)) "preview_create")
        (is (= 8000 (nlk:json-value arguments :integer "port")) "on the port the app listens on"))
      (is (search "preview.boxcompute.ai" answer) "and the link reaches the model"))))

(deftest boxcompute-cell-the-escape-hatch-passes-its-arguments-through (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (bc-answer-text "12 credits")
    (let ((answer (bc-call "boxcompute_tool"
                           "{\"tool\": \"account_get\", \"arguments\": {\"verbose\": true}}")))
      (let* ((call (first (bc-asked-for "tools/call")))
             (arguments (bc-tool-arguments call)))
        (is (equal "account_get" (bc-tool-name call)) "the server's own tool name")
        (is (eq t (nlk:json-value arguments :boolean "verbose")) "and its own arguments, untouched"))
      (is (search "12 credits" answer) "its answer is the answer"))))

;;; --- what the server says back -----------------------------------------------------

(deftest boxcompute-cell-a-json-rpc-error-is-the-models-repair (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (setf *error-text* "no tool named nope")
    (let ((answer (bc-call "boxcompute_tool" "{\"tool\": \"nope\"}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "no tool named nope" answer) "and carries the server's own words"))))

(deftest boxcompute-cell-an-sse-answer-is-read (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (setf *reply* (lambda (name arguments)
                    (declare (ignore name arguments))
                    (stub-tool-result "from the stream")))
    (let ((nodecode-boxcompute::*session-open-p* t))
      (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                 (let* ((message (nlk:decode-json (getf args :content)))
                                        (id (nlk:json-value message :integer "id")))
                                   (values (format nil "event: message~%data: ~a~%~%"
                                                   (stub-reply id (stub-tool-result "from the stream")))
                                           200
                                           (stub-headers "content-type" "text/event-stream"))))
        (is (search "from the stream" (nle::message-content
                                       (execute-wire-call "boxcompute-call" "boxcompute_tool"
                                                          "{\"tool\": \"account_get\"}"))) "an event-stream body yields the reply it carries")))))

(deftest boxcompute-cell-the-catalog-is-read-live (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (with-stubbed-fdefinition (nlk:http (method url &rest args)
                               (let ((message (nlk:decode-json (getf args :content))))
                                 (values (stub-reply (nlk:json-value message :integer "id")
                                                      (nlk:make-json-object
                                                       "tools" (coerce (list (nlk:make-json-object
                                                                               "name" "sandbox_create"
                                                                               "description" "Make a VM"
                                                                               "inputSchema" (nlk:json-object)))
                                                                       'vector)))
                                         200 (stub-headers "content-type" "application/json"))))
      (let ((rows (nodecode-boxcompute:tools)))
        (is (search "sandbox_create - Make a VM" rows) "the server's catalog, as it is")))))

;;; --- the operator's command -------------------------------------------------------

(deftest boxcompute-cell-the-command-reports-the-connection (with-boxcompute-wire)
  (with-cell-stop ((boxcompute-start "api_key" "bxc-test"))
    (let ((status (nodecode-boxcompute::run-command "" nil)))
      (is (search "mcp.boxcompute.ai" status) "the endpoint it talks to")
      (is (search "key set" status) "and that a key is there")
      (is (null (search "bxc-test" status)) "without ever printing the key")
      (is (null (bc-asked)) "and sends nothing"))))
