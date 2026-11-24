;;;; support.lisp --- the test slice: a stubbed MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; PARALLEL-SEARCH-CELL- name prefix; RUN-PARALLEL-SEARCH-TESTS runs exactly
;;;; that slice.
;;;;
;;;; The one seam that reaches outside is NLK:HTTP, the cell's only way to the
;;;; network. WITH-WIRE replaces it with a small MCP server -- an initialize
;;;; that hands out a session, and one answer per tool -- and records every
;;;; message, so a test asserts on the exchange and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "parallel-search" "PARALLEL-SEARCH-CELL-" :start nodecode-parallel-search:start-cell)

;;; --- the canned server ---------------------------------------------------------

(defparameter +endpoint+ "https://search.test/mcp"
  "The address a test's section names.")

(defparameter +initialize-answer+
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{\"tools\":{}},\"serverInfo\":{\"name\":\"search\",\"version\":\"0.1.0\"}}}"
  "What the stubbed server answers to initialize.")

(defparameter +session-headers+ '(("mcp-session-id" . "sess-1"))
  "What it hands out with that answer: one session for the whole start.")

(defparameter +search-text+
  "FastAPI response models: declare a Pydantic model and return an instance.
https://fastapi.tiangolo.com/tutorial/response-model/

The response_model argument validates and filters the returned data."
  "What the stubbed web_search answers.")

(defparameter +fetch-text+
  "# Introduction - Model Context Protocol

MCP connects an agent to tools and data through one JSON-RPC interface."
  "What the stubbed web_fetch answers.")

(defvar *calls* '()
  "Every JSON-RPC message the stub took, newest first: (:rpc :tool :body :headers).")

(defvar *routes* '()
  "((TOOL . FUNCTION) ...) a test scripts; FUNCTION takes the request body and
answers (values TEXT STATUS HEADERS). The newest entry for a tool wins.")

(defun script (tool function)
  "Answer TOOL with FUNCTION for the rest of the test."
  (push (cons tool function) *routes*))

(defun body-of (content)
  "CONTENT -- a JSON request body -- decoded, or NIL."
  (let ((body (ignore-errors (nlk:decode-json (or content "")))))
    (and (hash-table-p body) body)))

(defun answer-message (body text)
  "A tools/call answer carrying TEXT, under BODY's own id."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0"
                     "id" (or (nlk:json-value body :integer "id") 1)
                     "result" (nlk:json-object
                               "content" (vector (nlk:json-object "type" "text"
                                                                   "text" text))))))

(defun stub-http (method url &rest arguments)
  "The stubbed NLK:HTTP: a small MCP server, and a record of every message."
  (declare (ignore method url))
  (let* ((body (body-of (getf arguments :content)))
         (name (and body (nlk:json-value body :string "method")))
         (tool (and body (nlk:json-value body :string "params" "name"))))
    (push (list :rpc name :tool tool :body body :headers (getf arguments :headers))
          *calls*)
    (let ((route (and tool (assoc tool *routes* :test #'equal))))
      (cond
        (route (funcall (cdr route) body))
        ((equal name "initialize") (values +initialize-answer+ 200 +session-headers+))
        ((equal name "notifications/initialized") (values "" 202 '()))
        ((equal name "tools/call")
         (values (answer-message body (if (equal tool "web_fetch") +fetch-text+ +search-text+))
                 200 '()))
        (t (values "" 404 '()))))))

(defun calls-of (rpc)
  "The messages the stub took for RPC, newest first."
  (remove rpc *calls* :key (lambda (call) (getf call :rpc)) :test-not #'equal))

(defmacro with-wire (&body body)
  "Run BODY with NLK:HTTP stubbed and the cell's session state cleared."
  `(with-saved-globals (*calls* *routes* nodecode-parallel-search::*session-id*
                        nodecode-parallel-search::*stateless-p*
                        nodecode-parallel-search::*rpc-counter*)
     (setf *calls* '()
           *routes* '()
           nodecode-parallel-search::*session-id* nil
           nodecode-parallel-search::*stateless-p* nil
           nodecode-parallel-search::*rpc-counter* 0)
     (with-stubbed-fdefinition (nlk:http (method url &rest arguments)
                                 (apply #'stub-http method url arguments))
       ,@body)))

(defmacro with-parallel ((&rest pairs) &body body)
  "Run BODY with the cell started under PAIRS as the `parallel_search'
section, its wire stubbed, and stopped on unwind."
  `(with-wire
     (with-cell-stop ((parallel-search-start "endpoint" +endpoint+ ,@pairs))
       ,@body)))

;;; --- asking the cell what a model would -------------------------------------

(defun parallel-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))
