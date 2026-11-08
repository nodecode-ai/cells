;;;; mcp.lisp --- the hosted Unbrowse remote MCP: JSON-RPC 2.0 over streamable
;;;; HTTP, stateless, one POST per call. Ported from client.py.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call is an independent POST of one JSON-RPC message, answered with
;;;; a JSON body or (the server's streaming path) an SSE stream whose last
;;;; `data:' event is the message. The API key rides as a bearer header and
;;;; never appears in an answer, a refusal or a log.
;;;;
;;;; A refusal is UNBROWSE-ERROR carrying the server's own code where it sent
;;;; one (`browser_capacity', `unauthorized'), the RPC error code otherwise,
;;;; so the model can tell "busy, wait and retry" from "set a key".

(in-package #:nodecode-unbrowse)

(defvar *rpc-id* 0
  "The id the next request sends. The server does not correlate them: every call is its own POST.")

(defun endpoint ()
  "The MCP endpoint every call goes to: the section's, else the hosted one."
  (let ((url (setting :mcp-url)))
    (if (and (stringp url) (plusp (length url)))
        (string-right-trim "/" url)
        +endpoint+)))

(defun api-key ()
  "The key every call sends: the section's secret, else UNBROWSE_API_KEY, as the plugin reads it."
  (or (let ((key (setting :api-key)))
        (and (stringp key) (plusp (length key)) key))
      (nle::credential-env "UNBROWSE_API_KEY")))

(defun end-user ()
  "Org keys only: the user each call is made for, else NIL."
  (or (let ((who (setting :end-user)))
        (and (stringp who) (plusp (length who)) who))
      (nle::credential-env "UNBROWSE_END_USER")))

(defun require-key ()
  "API-KEY, or the refusal naming where an operator puts one."
  (or (api-key)
      (fail "no Unbrowse API key: create one at ~a, then set the config's unbrowse.api_key ~
(or export UNBROWSE_API_KEY)" +key-page+)))

(defun headers (key)
  "What every request carries. The key is a bearer header and nothing else names it."
  `(("content-type" . "application/json")
    ("accept" . "application/json, text/event-stream")
    ("mcp-protocol-version" . ,+protocol-version+)
    ("user-agent" . ,(format nil "unbrowse-nodecode/~a" +client-version+))
    ,@(when key `(("authorization" . ,(format nil "Bearer ~a" key))))
    ,@(let ((who (end-user)))
        (when who `(("x-unbrowse-end-user" . ,who))))))

(defun clip (text limit)
  "TEXT, at most LIMIT characters of it: what an answer that is not what was asked for shows."
  (let ((text (princ-to-string (or text ""))))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

(defun sse-data (text)
  "TEXT's last server-sent event's data, or NIL when it carries none."
  (let ((last nil))
    (dolist (block (uiop:split-string (substitute #\Newline #\Return text)
                                       :separator (format nil "~c~c" #\Newline #\Newline)))
      (let ((lines (loop for line in (uiop:split-string block :separator (string #\Newline))
                         when (uiop:string-prefix-p "data:" line)
                           collect (string-left-trim '(#\Space #\Tab)
                                                      (subseq line (length "data:"))))))
        (when lines
          (setf last (format nil "~{~a~^~c~}" lines #\Newline)))))
    last))

(defun parse-rpc-body (text)
  "The JSON-RPC message TEXT carries: plain JSON, or the last data: event of an SSE stream."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (cond
      ((zerop (length trimmed))
       (fail "Unbrowse MCP answered an empty body"))
      ((char= (char trimmed 0) #\{)
       (nlk:decode-json trimmed :whole t))
      (t
       (let ((data (sse-data trimmed)))
         (unless data
           (fail "Unbrowse MCP answered no JSON-RPC message: ~a" (clip trimmed 200)))
         (nlk:decode-json data :whole t))))))

(defun rpc (method &optional params)
  "One JSON-RPC call: METHOD with PARAMS (a JSON object or NIL). => the message's result.
Fails with UNBROWSE-ERROR on a transport failure, a body that is not JSON-RPC, or an error message."
  (let* ((key (require-key))
         (address (endpoint))
         (timeout (setting :timeout-seconds))
         (payload (nlk:encode-json-object
                   (nlk:json-object "jsonrpc" "2.0"
                                    "id" (incf *rpc-id*)
                                    "method" method
                                    :opt "params" params))))
    (multiple-value-bind (text status)
        (handler-case (nlk:http "POST" address
                                :headers (headers key) :content payload
                                :timeout timeout :connect-timeout (min 15 timeout))
          (error (condition)
            (fail "Unbrowse MCP at ~a did not answer: ~a"
                  address (nle::transport-failure-label condition address))))
      (let* ((message (handler-case (parse-rpc-body text)
                        (error ()
                          (fail "Unbrowse MCP answered ~a with a body that is not JSON-RPC: ~a"
                                status (clip text 200)))))
             (error-object (and message (nlk:json-value message :object "error"))))
        (cond
          (error-object
           (let* ((data (nlk:json-value error-object :object "data"))
                  (code (or (and data (nlk:json-value data :string "code"))
                            (format nil "rpc_~a" (nlk:json-value error-object :any "code"))))
                  (detail (or (nlk:json-value error-object :text "message") "Unbrowse MCP error")))
             (fail "~a (~a)~@[~a~]"
                   detail code
                   (when (or (eql status 401)
                             (member code '("unauthorized" "invalid_token") :test #'equal))
                     (format nil ": set a valid Unbrowse API key in the unbrowse section's api_key ~
(create one at ~a)" +key-page+)))))
          ((null message)
           (fail "Unbrowse MCP answered ~a with no JSON-RPC message" status))
          (t (nlk:json-value message :any "result")))))))

(defun result-text (result)
  "RESULT's text: its text parts, else its structured content as JSON, as the plugin's result_text."
  (let ((parts (loop for part across (or (nlk:json-value result :array "content") #())
                     for text = (nlk:json-value part :string "text")
                     when (and text (equal "text" (nlk:json-value part :string "type")))
                       collect text)))
    (if parts
        (format nil "~{~a~^~c~}" parts #\Newline)
        (let ((structured (nlk:json-value result :any "structuredContent")))
          (if structured (nlk:encode-json-object structured) "")))))

(defun as-object (arguments)
  "ARGUMENTS as the JSON object a tools/call takes: a decoded object, or an empty one for NIL."
  (cond ((null arguments) (nlk:json-object))
        ((hash-table-p arguments) arguments)
        (t (fail "a tool's arguments are a JSON object — (nlk:json-object \"url\" \"...\") — not ~a"
                 arguments))))

(define-verb call-tool (name &optional arguments)
  "Call the Unbrowse tool NAME with ARGUMENTS, a JSON object, and answer its text."
  (let* ((result (rpc "tools/call"
                       (nlk:json-object "name" name
                                         "arguments" (as-object arguments))))
         (text (result-text result)))
    (cond
      ((nlk:json-value result :boolean "isError")
       (fail "~a: ~a" name (if (plusp (length text)) text "the tool reported an error")))
      ((null result)
       (fail "~a answered nothing" name))
      (t text))))

(defun list-tools ()
  "Every tool this key can call: the core ones plus the workspace's learned and indexed ones."
  (let ((tools '())
        (cursor nil)
        (seen '()))
    (loop
      (let* ((page (rpc "tools/list" (nlk:json-object :opt "cursor" cursor)))
             (batch (or (nlk:json-value page :array "tools") #())))
        (setf tools (append tools (coerce batch 'list)))
        (setf cursor (nlk:json-value page :string "nextCursor"))
        (when (or (null cursor) (member cursor seen :test #'equal))
          (return tools))
        (push cursor seen)))))

(define-verb tools ()
  "List the Unbrowse tools this key can call, one line each."
  (let ((entries (list-tools)))
    (if (null entries)
        "Unbrowse listed no tools for this key."
        (format nil "~d Unbrowse tool~:p:~%~{~a~^~c~}"
                (length entries)
                (loop for entry in entries
                      collect (format nil "  ~a — ~a"
                                      (or (nlk:json-value entry :string "name") "?")
                                      (clip (or (nlk:json-value entry :string "description") "") 160)))
                #\Newline))))
