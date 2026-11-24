;;;; wire.lisp --- GuruSup Brain's one remote MCP server, over streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is an Agent Plugins manifest: it declares one remote MCP
;;;; server (mcp.json) and a skill that tells the model to call its `init'
;;;; tool first. Here the transport is this file -- MCP's streamable HTTP, the
;;;; one the release's own MCP client speaks, which is what the manifest's URL
;;;; needs:
;;;;
;;;;   POST <endpoint>    every JSON-RPC message, the account's token as a bearer
;;;;   initialize         one session: the answer's Mcp-Session-Id is echoed
;;;;                      on every later request, and a notification follows
;;;;   tools/list         what this account may call; the server names its own
;;;;                      tools, so the cell asks rather than assuming
;;;;   tools/call         one of them, its text back
;;;;   text/event-stream  the answer may arrive as SSE frames; read to the
;;;;                      reply and abandoned, as the release's client does
;;;;
;;;; Every call opens its own session and leaves it to the server to expire:
;;;; a call is one exchange, and a session kept across calls is state nothing
;;;; cleans up after a kill.

(in-package #:nodecode-gurusup-brain)

(defparameter +default-endpoint+ "https://mcp.brain.gurusup.com/mcp"
  "GuruSup Brain's MCP server, where the original package's mcp.json points,
and where the section's endpoint points unless it names another.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks, as the release's own MCP client does.")

(defparameter +client-version+ "0.1.0")

(defparameter +token-page+ "https://gurusup.com/brain/mcp"
  "Where a GuruSup Brain account signs in and its MCP server is documented.")

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

(defun token ()
  "The token the section names inline, else the variable it names, else
GURUSUP_BRAIN_TOKEN -- so a token exported in a shell needs no section."
  (or (non-empty (setting :token))
      (let ((variable (non-empty (setting :token-env))))
        (and variable (non-empty (uiop:getenv variable))))
      (non-empty (uiop:getenv "GURUSUP_BRAIN_TOKEN"))))

(defun token-source ()
  "Where the token comes from, in a few words; no network."
  (cond ((non-empty (setting :token)) "the section's token")
        ((let ((variable (non-empty (setting :token-env))))
           (and variable (non-empty (uiop:getenv variable))))
         (format nil "the variable ~a" (setting :token-env)))
        ((non-empty (uiop:getenv "GURUSUP_BRAIN_TOKEN")) "GURUSUP_BRAIN_TOKEN")
        (t (format nil "no token yet (sign in at ~a)" +token-page+))))

(defun check-config ()
  "Refuse a section that could never reach Brain, naming the member."
  (unless (token)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "gurusup-brain: no token: sign in at " +token-page+
                                 ", then set \"token\" (or \"token_env\") in the "
                                 "gurusup-brain section, or export GURUSUP_BRAIN_TOKEN"))))

(defun header-value (headers name)
  "NAME's value from a response's header table, which lowercases every name."
  (and (hash-table-p headers) (gethash (string-downcase name) headers)))

(defun request-headers (&key session)
  "The headers of one request: the token as a bearer, the protocol revision,
and, once there is one, the session."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream"))
          (list (cons "mcp-protocol-version" +protocol-version+))
          (and session (list (cons "mcp-session-id" session)))
          (list (cons "authorization" (format nil "Bearer ~a" (token))))))

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
          ((vectorp value) (loop for entry across value
                                 when (hash-table-p entry) collect entry))
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
                                  (let ((reply (find-if
                                                (lambda (message)
                                                  (equal id (gethash "id" message)))
                                                (messages-in text))))
                                    (when reply (push reply messages) t)))
                                :limit +body-limit+))
             (t (setf messages (messages-in (body-text-of body))))))
      (when (streamp body) (ignore-errors (close body :abort t))))
    (values messages status (or (header-value headers "mcp-session-id") session))))

(defun http-failure (status messages)
  "What a refused exchange says in one line, for the model to act on."
  (let ((said (let ((message (find-if (lambda (message)
                                        (hash-table-p (gethash "error" message)))
                                      messages)))
                (and message (nlk:json-value (gethash "error" message) :string "message")))))
    (format nil "HTTP ~@[~a: ~]~a"
            status
            (or said
                (cond ((eql status 400) "the request was malformed")
                      ((eql status 401) (format nil "the token was refused; sign in at ~a"
                                                 +token-page+))
                      ((eql status 403) "this account may not read that")
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

(defun reply-to (id messages)
  "The JSON-RPC message among MESSAGES answering ID, or NIL."
  (find-if (lambda (message) (equal id (gethash "id" message))) messages))

(defun open-session ()
  "Initialize a session on the endpoint => its Mcp-Session-Id, or NIL when
the server keeps none. A refused initialize names the token."
  (let* ((id (next-id))
         (payload (nlk:json-object
                   "jsonrpc" "2.0" "id" id "method" "initialize"
                   "params" (nlk:json-object
                             "protocolVersion" +protocol-version+
                             "capabilities" (nlk:json-object)
                             "clientInfo" (nlk:json-object "name" "nodecode-gurusup-brain"
                                                            "version" +client-version+)))))
    (multiple-value-bind (messages status session) (exchange payload)
      (let ((message (reply-to id messages)))
        (cond ((or (null status) (>= status 300))
               (fail "initialize: ~a" (http-failure status messages)))
              ((null message) (fail "initialize: the server answered no message"))
              ((rpc-error-text message) (fail "initialize: ~a" (rpc-error-text message))))
        (exchange (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized")
                  :session session)
        session))))

(defun lines-of (texts)
  (format nil "~{~a~^~%~}" texts))

(defun content-text (result)
  "RESULT's content blocks as text, one per line."
  (let ((blocks (nlk:json-array result "content")))
    (if (vectorp blocks)
        (lines-of (loop for block across blocks
                         for value = (nlk:json-value block :string "text")
                         when value collect value))
        "")))

(defun session-result (id messages status what)
  "The result object MESSAGES carries for ID, or a refusal naming WHAT."
  (when (or (null status) (>= status 300))
    (fail "~a: ~a" what (http-failure status messages)))
  (let ((message (reply-to id messages)))
    (when (null message) (fail "~a: the server answered no message" what))
    (when (rpc-error-text message) (fail "~a: ~a" what (rpc-error-text message)))
    (let ((result (gethash "result" message)))
      (if (hash-table-p result) result (nlk:json-object)))))

(defun session-call (session name arguments)
  "Call the remote MCP tool NAME on SESSION with ARGUMENTS, a JSON object
=> its text. A refusal the server reports is the answer text, prefixed ERROR:."
  (let* ((id (next-id))
         (payload (nlk:json-object "jsonrpc" "2.0" "id" id "method" "tools/call"
                                    "params" (nlk:json-object
                                              "name" name
                                              "arguments" (or arguments
                                                               (nlk:json-object))))))
    (multiple-value-bind (messages status) (exchange payload :session session)
      (let* ((result (session-result id messages status name))
             (text (content-text result)))
        (cond ((zerop (length text)) (nlk:pretty-json result))
              ((eq t (nlk:json-value result :boolean "isError"))
               (concatenate 'string "ERROR: " text))
              (t text))))))

(defun session-tools (session)
  "SESSION's worth of tools/list: a list of the server's tool objects."
  (let* ((id (next-id))
         (payload (nlk:json-object "jsonrpc" "2.0" "id" id "method" "tools/list")))
    (multiple-value-bind (messages status) (exchange payload :session session)
      (let* ((result (session-result id messages status "tools/list"))
             (tools (nlk:json-array result "tools")))
        (if (vectorp tools)
            (loop for tool across tools when (hash-table-p tool) collect tool)
            '())))))

(defun tool-name (tool)
  (nlk:json-value tool :string "name"))

(defun tool-lines (tools)
  "TOOLS, one line each: its name, then what it is for."
  (if (null tools)
      "Brain answered no tools for this account."
      (lines-of
       (loop for tool in tools
             for name = (tool-name tool)
             when name
               collect (format nil "~a - ~a" name
                                (nlk:one-line
                                 (or (nlk:json-value tool :string "description") "")
                                 :cap 160))))))

(defun init-tool-name (tools)
  "The Brain tool the original's skill calls first: `init', whatever prefix
the server puts before it."
  (let ((names (loop for tool in tools for name = (tool-name tool)
                     when name collect name)))
    (or (find "init" names :test #'string-equal)
        (find-if (lambda (name)
                   (let ((lower (string-downcase name)))
                     (or (uiop:string-suffix-p "_init" lower)
                         (uiop:string-suffix-p "/init" lower))))
                 names))))

(defun brain-init ()
  "One session: the server's tools, and the contract its init tool returns.
A server with no init tool answers its catalog instead."
  (let* ((session (open-session))
         (tools (session-tools session))
         (name (init-tool-name tools)))
    (if name
        (session-call session name (nlk:json-object))
        (format nil "Brain names no init tool; these are the tools it offers:~%~a"
                (tool-lines tools)))))

;;; --- the verbs an eval form calls -------------------------------------------

(defun call (name &optional arguments)
  "Call a GuruSup Brain MCP tool by the name gurusup_brain_tools answered, with
ARGUMENTS, a JSON object made by (nlk:json-object ...), and answer its text.
Through eval: (gurusup-brain:call \"search\" (nlk:json-object \"query\" \"acme\")).
A call that cannot be made answers an ERROR: line the model repairs from; a
cell that is not running refuses with GURUSUP-BRAIN-ERROR."
  (running-settings)
  (handler-case (let ((session (open-session)))
                  (session-call session (string-downcase name) arguments))
    (gurusup-brain-error (condition) (format nil "ERROR: gurusup-brain: ~a" condition))))

(defun tool-list ()
  "What this account's Brain offers, one tool a line. Through eval:
(gurusup-brain:tool-list)."
  (running-settings)
  (handler-case (tool-lines (session-tools (open-session)))
    (gurusup-brain-error (condition) (format nil "ERROR: gurusup-brain: ~a" condition))))

(defun init ()
  "The Brain contract the original's skill says to follow before answering.
Through eval: (gurusup-brain:init)."
  (running-settings)
  (handler-case (brain-init)
    (gurusup-brain-error (condition) (format nil "ERROR: gurusup-brain: ~a" condition))))
