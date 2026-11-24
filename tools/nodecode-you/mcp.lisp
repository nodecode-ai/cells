;;;; mcp.lisp --- the cell's one network seam: MCP's streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; You.com's tools are MCP servers, not a REST API: a call is a JSON-RPC
;;;; `tools/call` on a session opened by an `initialize`. This file is that
;;;; conversation and nothing else -- one POST per message, the `Mcp-Session-Id`
;;;; header echoed once issued, a JSON body or an SSE body read for the reply.
;;;; One connection is opened per call and dropped: the cell asks a question
;;;; and answers it, so there is no pool to keep and no state a server could
;;;; disagree with us about.
;;;;
;;;; Every byte moves through *RPC*, so a test stubs that one name and never
;;;; touches the network. HTTP status is a value: a 4xx/5xx is a fact the
;;;; caller reads, and only a transport failure (DNS, TLS, a timeout) is raised
;;;; as an ERROR for the tools' condition to carry.
;;;;
;;;; flexi-streams is used by name and not declared: it rides with nodecode
;;;; (dexador brings it), as the image's own NLK:READ-EVENTS does.

(in-package #:nodecode-you)

(defparameter +endpoint+ "https://api.you.com/mcp"
  "The keyed You.com MCP server: you-search and you-contents.")
(defparameter +free-endpoint+ "https://api.you.com/mcp?profile=free"
  "The keyless profile: you-search alone, no key, no contents.")
(defparameter +research-endpoint+ "https://api.you.com/mcp/research"
  "The research server: you-research, one shot, cited.")
(defparameter +finance-endpoint+ "https://api.you.com/mcp/finance"
  "The finance server: you-finance, company financials and filings.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks.")
(defparameter +client-info+ (nlk:json-object "name" "nodecode-you" "version" "0.1.0")
  "The clientInfo every initialize carries: who is asking.")

(defvar *rpc* nil
  "The seam: (URL TOOL ARGUMENTS HEADERS) => the call's result object, or
an ERROR. NIL is this file's own transport; a test binds a stub.")

;;; --- one HTTP exchange ------------------------------------------------------

(defun header (headers name)
  "NAME's value from a dexador header table, which lowercases every name."
  (and (hash-table-p headers) (gethash name headers)))

(defun character-stream (stream)
  "STREAM as a character stream: dexador hands back octets for some content types."
  (if (ignore-errors (subtypep (stream-element-type stream) 'character))
      stream
      (flexi-streams:make-flexi-stream stream :external-format :utf-8)))

(defun body-text (stream limit)
  "What STREAM still holds, up to LIMIT characters; never the whole of a big error page."
  (handler-case
      (with-output-to-string (out)
        (loop repeat limit
              for char = (read-char stream nil nil)
              while char do (write-char char out)))
    (error () "")))

(defun body-message (stream content-type)
  "The JSON-RPC message STREAM carries: one JSON body, or the SSE body's first reply."
  (if (and content-type (uiop:string-prefix-p "text/event-stream" content-type))
      (let ((stream (character-stream stream))
            (reply nil))
        (nlk:read-events stream
                          (lambda (text)
                            ;; A reply answers a request: it has an id and names no method.
                            (let ((message (ignore-errors (nlk:decode-json text))))
                              (when (and (hash-table-p message)
                                         (nth-value 1 (gethash "id" message))
                                         (null (gethash "method" message)))
                                (setf reply message)
                                t)))
                          :limit 4000000)
        (or reply (fail "You.com sent no reply")))
      (let ((value (ignore-errors (nlk:decode-json (body-text (character-stream stream) 4000000)))))
        (cond ((hash-table-p value) value)
              ((and (vectorp value) (plusp (length value)) (hash-table-p (aref value 0)))
               (aref value 0))
              (t (fail "You.com answered nothing readable"))))))

(defun post-message (url message session headers)
  "POST one JSON-RPC MESSAGE to URL under HEADERS, echoing SESSION;
=> (values REPLY SESSION). A notification answers no reply."
  (let ((headers (append `(("content-type" . "application/json")
                           ("accept" . "application/json, text/event-stream")
                           ("mcp-protocol-version" . ,+protocol-version+))
                         (when session `(("mcp-session-id" . ,session)))
                         headers)))
    (multiple-value-bind (stream status response-headers)
        (nlk:http :post url :headers headers
                  :content (nlk:encode-json-object message)
                  :timeout (setting :timeout-seconds)
                  :stream t)
      (unwind-protect
           (let ((session (or (header response-headers "mcp-session-id") session))
                 (content-type (header response-headers "content-type")))
             (cond
               ((member status '(202 204)) (values nil session))
               ((not (<= 200 status 299))
                (fail "You.com answered HTTP ~a~@[ ~a~]" status
                      (let ((text (nlk:trimmed (body-text (character-stream stream) 400))))
                        (and (plusp (length text)) text))))
               (t (values (body-message stream content-type) session))))
        (ignore-errors (close stream))))))

(defun result-of (reply)
  "REPLY's `result', or the JSON-RPC error it carries, as a refusal."
  (let ((error (nlk:json-value reply :object "error")))
    (when error
      (fail "You.com refused the call: ~a"
            (or (nlk:json-value error :string "message") "no message")))
    (or (nlk:json-value reply :object "result")
        (fail "You.com sent no result"))))

(defun rpc-call (url tool arguments headers)
  "One whole MCP exchange: initialize, initialized, tools/call. => the call's result."
  (let* ((init (post-message url
                              (nlk:json-object
                               "jsonrpc" "2.0"
                               "id" 1
                               "method" "initialize"
                               "params" (nlk:json-object
                                         "protocolVersion" +protocol-version+
                                         "capabilities" (nlk:json-object)
                                         "clientInfo" +client-info+))
                              nil headers))
         (session (nth-value 1 init)))
    (post-message url (nlk:json-object "jsonrpc" "2.0"
                                        "method" "notifications/initialized")
                  session headers)
    (result-of
     (post-message url
                   (nlk:json-object "jsonrpc" "2.0"
                                    "id" 2
                                    "method" "tools/call"
                                    "params" (nlk:json-object
                                              "name" tool
                                              "arguments" arguments))
                   session headers))))

(defun rpc (url tool arguments headers)
  "TOOL called with ARGUMENTS on URL, through the seam a test stubs."
  (funcall (or *rpc* #'rpc-call) url tool arguments headers))
