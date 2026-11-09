;;;; support.lisp --- the Patsnap test slice: a stubbed MCP server, no network.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the same nodecode.test registry every cell's do
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; PATSNAP-CELL- name prefix; RUN-PATSNAP-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NLK:HTTP, the only way this cell
;;;; touches the network. WITH-WIRE replaces it with a small MCP server --
;;;; initialize, notifications/initialized, tools/list, tools/call -- and
;;;; records every call, so a test asserts on the exchange and nothing dials.

(in-package #:nodecode.test)

(define-test-slice "patsnap" "PATSNAP-CELL-" :start nodecode-patsnap:start-cell)

;;; --- the canned server ----------------------------------------------------------

(defparameter +endpoint+ "https://mcp.test/patsnap"
  "The server a test's section names.")

(defparameter +key-env+ "NODECODE_PATSNAP_TEST_KEY"
  "A variable nothing sets: the key in these tests is the one the section carries.")

(defparameter +no-key-env+ "NODECODE_PATSNAP_TEST_ABSENT"
  "The variable a test names when the key must be missing.")

(defparameter +catalog-json+
  (vector (nlk:json-object
           "name" "search_patent"
           "description" "Search patents."
           "inputSchema" (nlk:json-object
                          "type" "object"
                          "properties" (nlk:json-object
                                        "semantic_query" (nlk:json-object "type" "string"))))
          (nlk:json-object
           "name" "fetch_patent"
           "description" "One patent.\\nSecond line."
           "inputSchema" (nlk:json-object "type" "object")))
  "GET tools/list: two tools, one with a schema.")

;;; --- the wire --------------------------------------------------------------------

(defvar *calls* '()
  "Every (METHOD URL RPC-METHOD BODY HEADERS) the stubbed wire took, newest first.")

(defvar *overrides* '()
  "((KEY . FUNCTION) ...) a test sets to script one answer; the newest entry
for a key wins and the rest of the server answers as canned. FUNCTION is
called with the decoded request and answers (values TEXT STATUS HEADERS).")

(defun stub-key (message)
  "What an override is filed under: the RPC method, and for a tool call the
tool's name as well, so one tool can be scripted alone."
  (let ((method (nlk:json-value message :string "method")))
    (if (equal method "tools/call")
        (format nil "tools/call:~a"
                (nlk:json-value (nlk:json-value message :object "params") :string "name"))
        method)))

(defun script (key function)
  "Answer KEY with FUNCTION for the rest of the test."
  (push (cons key function) *overrides*))

(defun answer-json (message result)
  "RESULT as the JSON-RPC answer to MESSAGE."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0"
                    "id" (nlk:json-value message :any "id")
                    "result" result)))

(defun text-result (text)
  "An MCP tools/call result carrying TEXT as its one content part."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))))

(defun sse-body (message text)
  "TEXT as the answer to MESSAGE, on an event stream: the transport's other
shape, which the client reads the same way."
  (format nil "event: message~%data: ~a~%~%"
          (nlk:encode-json-object
           (nlk:json-object "jsonrpc" "2.0"
                            "id" (nlk:json-value message :any "id")
                            "result" (text-result text)))))

(defun session-headers ()
  "The headers an initialized server answers with, the session id among them."
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) "sess-1"
          (gethash "content-type" headers) "application/json")
    headers))

(defun answer-for (tool arguments)
  "What the canned server says to one tool call."
  (let ((query (or (nlk:json-value arguments :string "semantic_query")
                   (nlk:json-value arguments :string "keywords")
                   (nlk:json-value arguments :string "name"))))
    (cond
      ((equal tool "search_patent")
       (format nil "Found 2 patents for ~s.~%  US-11234567-B2  Solid-state electrolyte interface  2022~%  US-10987654-B2  Sulfide solid electrolyte  2021"
               (or query "everything")))
      ((equal tool "search_literature")
       "Found 1 paper.\n  W-7788  Interface stability in solid electrolytes  2023")
      ((equal tool "fetch_patent")
       (format nil "US-11234567-B2, one patent, id ~a: claim 1, a solid electrolyte."
               (or (nlk:json-value arguments :string "patent_id") "none")))
      ((equal tool "get_standard_assignee_name")
       "c-42  a-42  the standard assignee for that name")
      (t (format nil "~a answered" tool)))))

(defun stub-wire (method url &rest arguments)
  "The stubbed NLK:HTTP: a small Patsnap MCP server, and a record of every call."
  (let* ((body (getf arguments :content))
         (headers (getf arguments :headers))
         (message (and body (ignore-errors (nlk:decode-json body))))
         (rpc-method (nlk:json-value message :string "method")))
    (push (list method url rpc-method body headers) *calls*)
    (let ((override (assoc (stub-key message) *overrides* :test #'equal)))
      (when override (return-from stub-wire (funcall (cdr override) message))))
    (cond
      ((equal rpc-method "initialize")
       (values (answer-json message (nlk:json-object
                                      "protocolVersion" "2025-06-18"
                                      "capabilities" (nlk:json-object)
                                      "serverInfo" (nlk:json-object "name" "patsnap" "version" "1")))
               200 (session-headers)))
      ((equal rpc-method "notifications/initialized")
       (values "" 202 (session-headers)))
      ((equal rpc-method "tools/list")
       (values (answer-json message (nlk:json-object "tools" +catalog-json+)) 200 (session-headers)))
      ((equal rpc-method "tools/call")
       (let* ((params (nlk:json-value message :object "params"))
              (tool (nlk:json-value params :string "name"))
              (tool-arguments (nlk:json-value params :object "arguments")))
         (values (answer-json message (text-result (answer-for tool tool-arguments)))
                 200 (session-headers))))
      (t (values (answer-json message (nlk:json-object)) 200 (session-headers))))))

(defmacro with-wire (&body body)
  "Run BODY with NLK:HTTP stubbed and the client's per-start state cleared."
  `(with-saved-globals (*calls* *overrides*
                        nodecode-patsnap::*session* nodecode-patsnap::*remote-tools*
                        nodecode-patsnap::*calls* nodecode-patsnap::*request-id*)
     (setf *calls* '()
           *overrides* '()
           nodecode-patsnap::*session* nil
           nodecode-patsnap::*remote-tools* nil
           nodecode-patsnap::*calls* 0
           nodecode-patsnap::*request-id* 0)
     (with-stubbed-fdefinition (nlk:http (method url &rest arguments)
                                 (apply #'stub-wire method url arguments))
       ,@body)))

(defmacro with-patsnap ((&rest pairs) &body body)
  "Run BODY with the cell started under PAIRS as the `patsnap' section, its
wire stubbed, and stopped on unwind."
  `(with-wire
     (with-cell-stop ((patsnap-start ,@pairs))
       ,@body)))

;;; --- asking the cell what a model would -------------------------------------------

(defun call (name &optional (arguments "{}"))
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun calls-to (rpc-method)
  "The calls the stub took for RPC-METHOD, newest first."
  (remove rpc-method *calls* :key #'third :test-not #'equal))

(defun newest-body (rpc-method)
  "The request body of the newest call to RPC-METHOD."
  (fourth (first (calls-to rpc-method))))

(defun newest-headers (rpc-method)
  "The request headers of the newest call to RPC-METHOD, as an alist."
  (fifth (first (calls-to rpc-method))))
