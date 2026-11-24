;;;; wire.lisp --- Glasser's one remote MCP server, over streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is an Agent Plugins manifest: it declares one remote MCP
;;;; server and lets `npx mcp-remote' bridge to it. Here the bridge is this
;;;; file -- MCP's streamable HTTP, the transport the Nodecode release's own
;;;; MCP client speaks, which is what the manifest's URL needs:
;;;;
;;;;   POST <endpoint>    every JSON-RPC message, with the key as a bearer
;;;;   initialize         one session: the answer's Mcp-Session-Id is echoed
;;;;                      on every later request, and a notification follows
;;;;   tools/call         one of the server's seven tools, its text back
;;;;   text/event-stream  the answer may arrive as SSE frames; read to the
;;;;                      reply and abandoned, as the release's client does
;;;;
;;;; Every call opens its own session and leaves it to the server to expire:
;;;; a call is one exchange, and a session kept across calls is state nothing
;;;; cleans up after a kill.

(in-package #:nodecode-glasser)

(defparameter +default-endpoint+ "https://api.glasser.ai/mcp"
  "Glasser's MCP server, where the original package's mcp.json points, and
where the section's endpoint points unless it names another.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks, as the release's own MCP client does.")

(defparameter +client-version+ "0.1.0")

(defparameter +key-page+ "https://app.glasser.ai/keys"
  "Where a Glasser key is made and revoked, as the original's README says.")

(defparameter +body-limit+ (* 8 1024 1024)
  "How much of an answer's stream is read before the call gives up.")

(defvar *request-id* 0
  "The JSON-RPC id of the last request this image sent. A call's replies are
matched to it, so nothing else on the wire is mistaken for an answer.")

(defun next-id () (incf *request-id*))

(defun endpoint ()
  "The MCP endpoint this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :endpoint) ""))))
    (if (plusp (length given)) given +default-endpoint+)))

(defun non-empty (value)
  (and (stringp value) (plusp (length value)) value))

(defun api-key ()
  "The key the section names inline, else the variable it names, else
GLASSER_API_KEY -- so a key exported in a shell needs no section at all."
  (or (non-empty (setting :api-key))
      (let ((variable (non-empty (setting :api-key-env))))
        (and variable (non-empty (uiop:getenv variable))))
      (non-empty (uiop:getenv "GLASSER_API_KEY"))))

(defun check-config ()
  "Refuse a section that could never reach Glasser, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "glasser: no key: make one at " +key-page+ ", then set \"api_key\" "
                                 "(or \"api_key_env\") in the glasser section, or export GLASSER_API_KEY"))))

(defun header-value (headers name)
  "NAME's value from a response's header table, which lowercases every name."
  (and (hash-table-p headers) (gethash (string-downcase name) headers)))

(defun request-headers (&key session)
  "The headers of one request: the key as a bearer, the protocol revision,
and, once there is one, the session."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("mcp-protocol-version" . "2025-06-18"))
          (and session (list (cons "mcp-session-id" session)))
          (list (cons "authorization" (format nil "Bearer ~a" (api-key))))))

(defun body-text-of (body)
  "BODY -- an answer's body -- as text: a string, a character stream or an
octet stream, whichever the transport handed over."
  (cond ((stringp body) body)
        ((streamp body)
         (let ((stream (if (subtypep (stream-element-type body) 'character)
                           body
                           (flexi-streams:make-flexi-stream body :external-format :utf-8))))
           (with-output-to-string (text)
             (loop for line = (read-line stream nil nil)
                   while line do (write-string line text) (terpri text)))))
        (t "")))

(defun messages-in (text)
  "Every JSON-RPC message TEXT carries: one object, or a batch of them."
  (let ((value (handler-case (nlk:decode-json text) (error () nil))))
    (cond ((hash-table-p value) (list value))
          ((vectorp value) (loop for entry across value when (hash-table-p entry) collect entry))
          (t '()))))

(defun exchange (payload &key session)
  "POST PAYLOAD, one JSON-RPC message, on SESSION => (values MESSAGES STATUS
SESSION-ID). MESSAGES are the JSON-RPC messages the answer carried."
  (let ((body nil)
        (status nil)
        (headers nil)
        (messages '())
        (id (gethash "id" payload))
        (timeout (setting :timeout-seconds)))
    (handler-case
        (multiple-value-setq (body status headers)
          (nlk:http :post (endpoint)
                    :headers (request-headers :session session)
                    :content (nlk:encode-json-object payload)
                    :stream t
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a ~a: ~a" :post (endpoint) (nle:transport-failure-label condition (endpoint)))))
    (unwind-protect
         (let ((type (string-downcase (or (header-value headers "content-type") ""))))
           (cond
             ((uiop:string-prefix-p "text/event-stream" type)
              (nlk:read-events (if (streamp body) body (make-string-input-stream (or body "")))
                                (lambda (text)
                                  (let ((reply (find-if (lambda (message) (equal id (gethash "id" message)))
                                                        (messages-in text))))
                                    (when reply (push reply messages) t)))
                                :limit +body-limit+))
             (t (setf messages (messages-in (body-text-of body))))))
      (when (streamp body) (ignore-errors (close body :abort t))))
    (values messages status (or (header-value headers "mcp-session-id") session))))

(defun http-failure (status messages)
  "What a refused exchange says in one line, for the model to act on."
  (let ((said (let ((message (find-if (lambda (message) (hash-table-p (gethash "error" message)))
                                      messages)))
                (and message (nlk:json-value (gethash "error" message) :string "message")))))
    (format nil "HTTP ~@[~a: ~]~a"
            status
            (or said
                (cond ((eql status 400) "the request was malformed")
                      ((eql status 401) "the key was refused; make one at https://app.glasser.ai/keys")
                      ((eql status 403) "the key may not run this endpoint")
                      ((eql status 402) "insufficient balance; top up at https://app.glasser.ai")
                      ((eql status 404) "the endpoint or session is gone")
                      ((eql status 429) "rate limited; wait and try again")
                      (t "the server refused it"))))))

(defun rpc-error-text (message)
  "MESSAGE's JSON-RPC error, as one line, or NIL when it carries none."
  (let ((error (gethash "error" message)))
    (and (hash-table-p error)
         (or (nlk:json-value error :string "message")
             (let ((code (nlk:json-value error :integer "code")))
               (and code (format nil "JSON-RPC error ~a" code)))
             "the server refused the call"))))

(defun open-session ()
  "Initialize a session on the endpoint => its Mcp-Session-Id, or NIL when
the server keeps none. A refused initialize names the key."
  (let* ((id (next-id))
         (payload (nlk:json-object
                   "jsonrpc" "2.0" "id" id "method" "initialize"
                   "params" (nlk:json-object
                             "protocolVersion" +protocol-version+
                             "capabilities" (nlk:json-object)
                             "clientInfo" (nlk:json-object "name" "nodecode-glasser"
                                                            "version" +client-version+)))))
    (multiple-value-bind (messages status session) (exchange payload)
      (let ((message (find-if (lambda (message) (equal id (gethash "id" message))) messages)))
        (cond ((or (null status) (>= status 300))
               (fail "initialize: ~a" (http-failure status messages)))
              ((null message) (fail "initialize: the server answered no message"))
              ((rpc-error-text message) (fail "initialize: ~a" (rpc-error-text message))))
        (exchange (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized")
                  :session session)
        session))))

(defun call-tool (name arguments)
  "Call the remote MCP tool NAME with ARGUMENTS, a JSON object => its text.
A refusal the server reports is the answer text, prefixed ERROR:."
  (let* ((session (open-session))
         (id (next-id))
         (payload (nlk:json-object "jsonrpc" "2.0" "id" id "method" "tools/call"
                                    "params" (nlk:json-object
                                              "name" name
                                              "arguments" (or arguments (nlk:json-object))))))
    (multiple-value-bind (messages status) (exchange payload :session session)
      (when (or (null status) (>= status 300))
        (fail "~a: ~a" name (http-failure status messages)))
      (let ((message (find-if (lambda (message) (equal id (gethash "id" message))) messages)))
        (when (null message) (fail "~a: the server answered no message" name))
        (when (rpc-error-text message) (fail "~a: ~a" name (rpc-error-text message)))
        (let* ((result (gethash "result" message))
               (blocks (nlk:json-array result "content"))
               (text (format nil "~{~a~^~%~}"
                             (loop for block across blocks
                                   for value = (nlk:json-value block :string "text")
                                   when value collect value))))
          (cond ((zerop (length text)) (nlk:pretty-json result))
                ((eq t (nlk:json-value result :boolean "isError"))
                 (concatenate 'string "ERROR: " text))
                (t text)))))))

(defun call (name &optional arguments)
  "Call Glasser's MCP tool NAME -- search, inspect, run, runs_get, runs_list,
runs_stop, balance -- with ARGUMENTS, a JSON object made by (nlk:json-object
...), and answer its text. Through eval: (glasser:call \"balance\"). A call
that cannot be made answers an ERROR: line the model repairs from; a cell
that is not running refuses with GLASSER-ERROR."
  (running-settings)
  (handler-case (call-tool (string-downcase name) arguments)
    (glasser-error (condition) (format nil "ERROR: glasser: ~a" condition))))
