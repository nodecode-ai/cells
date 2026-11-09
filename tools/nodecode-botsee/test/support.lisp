;;;; support.lisp --- the test slice: a stubbed BotSee MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; BOTSEE-CELL- name prefix; RUN-BOTSEE-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-BOTSEE::REQUEST, the
;;;; cell's only way to the network. WITH-WIRE replaces it with an in-memory
;;;; MCP server that answers initialize, notifications/initialized, tools/list
;;;; and tools/call, and records every frame, so a test asserts on the
;;;; exchange and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "botsee" "BOTSEE-CELL-" :start nodecode-botsee:start-cell)

;;; --- the canned server ------------------------------------------------------------

(defparameter +key+ "botsee-test-key"
  "The token a test's section carries.")

(defparameter +tool-text+ "3 completed runs for site s1; the newest finished 2026-11-02."
  "What the canned tools/call answers as its one text block.")

(defparameter +handshake-and-call+ '("initialize" "notifications/initialized" "tools/call")
  "The frames one tool call puts on a fresh session's wire.")

(defparameter +handshake-and-list+ '("initialize" "notifications/initialized" "tools/list")
  "The frames one tool list puts on a fresh session's wire.")

(defvar *calls* '()
  "Every request the stub took, newest first: (:method :url :headers :content).")

(defvar *reply* nil
  "A function of (METHOD FRAME) answering (values BODY STATUS HEADERS), or NIL
for the canned server.")

(defvar *tool-text* +tool-text+)
(defvar *tool-failed* nil)

(defun reply-json (id result)
  "A JSON-RPC answer to ID carrying RESULT, as a body string."
  (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun sse-body (json)
  "JSON framed as one server-sent event, as a streamable-HTTP server may answer."
  (format nil "event: message~%data: ~a~%~%" json))

(defun canned-tools ()
  "The catalogue the canned server advertises: one paid tool, one free."
  (vector
   (nlk:json-object
    "name" "run_analysis"
    "description" "Run an AI-visibility analysis for a site."
    "inputSchema" (nlk:json-object
                   "type" "object"
                   "properties" (nlk:json-object
                                 "domain" (nlk:json-object "type" "string"
                                                            "description" "the site to analyze")
                                 "dry_run" (nlk:json-object "type" "boolean"
                                                             "description" "plan without spending"))
                   "required" (vector "domain")))
   (nlk:json-object
    "name" "list_runs"
    "description" "List completed analyses for a site."
    "inputSchema" (nlk:json-object
                   "type" "object"
                   "properties" (nlk:json-object
                                 "site_id" (nlk:json-object "type" "string"))
                   "required" (vector "site_id")))))

(defun canned (method id)
  "What the canned server answers to METHOD for request ID."
  (cond
    ((equal method "initialize")
     (values (reply-json id (nlk:json-object
                             "protocolVersion" "2025-06-18"
                             "capabilities" (nlk:json-object)
                             "serverInfo" (nlk:json-object "name" "botsee" "version" "0.1.0")))
             200 (list (cons "mcp-session-id" "session-1"))))
    ((equal method "notifications/initialized") (values "" 202 '()))
    ((equal method "tools/list")
     (values (reply-json id (nlk:json-object "tools" (canned-tools))) 200 '()))
    ((equal method "tools/call")
     (values (reply-json id (nlk:json-object
                             "content" (vector (nlk:json-object "type" "text" "text" *tool-text*))
                             "isError" *tool-failed*))
             200 '()))
    (t (values "{}" 200 '()))))

(defun stub-request (method url &key headers content timeout)
  "The cell's one seam, replaced: record the frame, answer it in memory."
  (declare (ignore timeout))
  (push (list :method method :url url :headers headers :content content) *calls*)
  (let* ((frame (or (ignore-errors (nlk:decode-json (or content ""))) (nlk:json-object)))
         (rpc-method (nlk:json-value frame :string "method"))
         (id (nlk:json-value frame :integer "id")))
    (if *reply*
        (funcall *reply* rpc-method frame)
        (canned rpc-method id))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam answered in memory."
  `(let ((*calls* '())
         (*reply* nil)
         (*tool-text* +tool-text+)
         (*tool-failed* nil))
     (with-stubbed-fdefinition (nodecode-botsee::request
                                (method url &key headers content timeout)
                                (stub-request method url :headers headers
                                               :content content :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defun botsee-run-start (&rest pairs)
  "START-CELL with a key unless PAIRS name one."
  (apply #'botsee-start
         (append (unless (member "api_key" pairs :test #'string=)
                   (list "api_key" +key+))
                 (copy-list pairs))))

(defmacro with-botsee ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((botsee-run-start ,@pairs))
       ,@body)))

(defun botsee-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the exchange a call made --------------------------------------------

(defun call-frame (call)
  "CALL's frame as decoded JSON."
  (nlk:decode-json (getf call :content)))

(defun frame-method (call)
  "The JSON-RPC method CALL carried."
  (nlk:json-value (call-frame call) :string "method"))

(defun call-methods ()
  "Every method the stub answered, oldest first."
  (loop for call in (reverse *calls*) collect (frame-method call)))

(defun header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun params-of (call)
  "CALL's JSON-RPC params as an object, or NIL."
  (nlk:json-value (call-frame call) :object "params"))
