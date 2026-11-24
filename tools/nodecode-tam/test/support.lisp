;;;; support.lisp --- the test slice: a stubbed TAM server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one seam that reaches outside is NODECODE-TAM::HTTP, the cell's only
;;;; way to the network. WITH-TAM-SERVER replaces it with a canned MCP server
;;;; and records every message, so a test asserts on the handshake and the
;;;; call and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "tam" "TAM-CELL-" :start nodecode-tam:start-cell)

(defparameter +url+ "http://127.0.0.1:3737/mcp/"
  "The endpoint a test's section names.")

(defparameter +session+ "session-1"
  "The session id the stubbed server hands back.")

(defvar *messages* '()
  "Every JSON-RPC document the stub took, newest first.")

(defvar *headers* '()
  "Every header alist the stub took, newest first.")

(defvar *documents* '()
  "Every JSON-RPC document the stub decoded, newest first.")

(defvar *status* 200
  "The status the stub answers, so a test can refuse a token.")

(defun memory-record (id content type score)
  "One TAM record, as a local server answers it."
  (nlk:json-object "id" id "content" content "type" type "project" "nodecode"
                    "score" score "tags" (vector "deploy")
                    "created_at" "2026-11-01T10:00:00Z"))

(defvar *recall-payload*
  (nlk:json-object
   "results"
   (nlk:json-object
    "fact" (vector (memory-record 7 "staging runs on port 8080" "fact" 0.91))
    "lesson" (vector (memory-record 9 "the flaky test was a missing time zone" "lesson" 0.42))))
  "What the stubbed memory_recall answers: two groups, best first.")

(defvar *save-payload*
  (nlk:json-object "saved" t "id" 12 "deduplicated" t)
  "What the stubbed memory_save answers: it was already there.")

(defun tool-reply (call)
  "CALL's params as a tools/call result: the payload as its one text part."
  (let* ((name (nlk:json-value call :string "name"))
         (payload (if (string= name "memory_save") *save-payload* *recall-payload*)))
    (nlk:json-object "content" (vector (nlk:json-object "type" "text"
                                                          "text" (nlk:encode-json-object payload))))))

(defun stub-tam (url &key headers content timeout)
  "The stubbed TAM server: one answer per JSON-RPC method."
  (declare (ignore timeout))
  (let* ((document (nlk:decode-json content))
         (method (gethash "method" document))
         (params (gethash "params" document)))
    (push url *messages*)
    (push headers *headers*)
    (push document *documents*)
    (when (>= *status* 400)
      (return-from stub-tam (values "{\"error\":\"token refused\"}" *status* nil)))
    (cond ((string= method "initialize")
           (values (nlk:encode-json-object
                    (nlk:json-object "jsonrpc" "2.0" "id" (gethash "id" document)
                                     "result" (nlk:json-object
                                               "protocolVersion" "2025-06-18"
                                               "serverInfo" (nlk:json-object "name" "total-agent-memory"
                                                                               "version" "14.7.0"))))
                   200 (list (cons "mcp-session-id" +session+))))
          ((string= method "notifications/initialized") (values "" 202 nil))
          ((string= method "tools/list")
           (values (nlk:encode-json-object
                    (nlk:json-object "jsonrpc" "2.0" "id" (gethash "id" document)
                                     "result" (nlk:json-object
                                               "tools" (vector (nlk:json-object "name" "memory_save")
                                                                (nlk:json-object "name" "memory_recall")))))
                   200 nil))
          ((string= method "tools/call")
           (values (nlk:encode-json-object
                    (nlk:json-object "jsonrpc" "2.0" "id" (gethash "id" document)
                                     "result" (tool-reply params)))
                   200 nil))
          (t (values "{}" 500 nil)))))

(defmacro with-tam-server (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its messages recorded."
  `(let ((*messages* '()) (*headers* '()) (*status* 200))
     (with-stubbed-fdefinition (nodecode-tam::http
                                (url &key headers content timeout)
                                (stub-tam url :headers headers :content content :timeout timeout))
       ,@body)))

(defmacro with-tam ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS (its url unless PAIRS name one), its
server stubbed, stopped on unwind."
  `(with-tam-server
     (with-cell-stop ((tam-start ,@(unless (member "url" pairs :test #'equal)
                                     (list "url" +url+))
                                  ,@pairs))
       ,@body)))

(defun tam-call-tool (name arguments)
  "NAME called through the wire as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun tam-header (name)
  "The value the newest request sent in header NAME."
  (cdr (assoc name (first *headers*) :test #'string-equal)))

(defun tam-sent ()
  "The newest message the stub took, as a table."
  (first *messages*))
