;;;; mcp.lisp --- Patsnap's hosted MCP server, spoken from this side.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original ships no code at all: a manifest points the harness's own MCP
;;;; client at Patsnap's Streamable-HTTP server and a skill says how to use the
;;;; tools that appear. This cell is that client, small: initialize,
;;;; notifications/initialized, tools/list, tools/call, over the one POST
;;;; NLK:HTTP makes. Nothing else of MCP is used, and nothing else is dialled.
;;;;
;;;; The key rides an `apikey' request header and never the URL, so it cannot
;;;; appear in a log line, in a connection list, or in a refusal this cell
;;;; writes.

(in-package #:nodecode-patsnap)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client asks for. A server answers its own, which the
session keeps and sends back on every later call.")

(defvar *session* nil
  "This start's MCP session, a plist (:id ... :protocol ...), or NIL before
the first call and after the server has dropped one.")

(defvar *remote-tools* nil
  "The server's own tools/list answer, decoded once per start.")

(defvar *request-id* 0
  "The JSON-RPC id of the last request this start made.")

(defvar *calls* 0
  "Tool calls this start has made. Each one spends credits on the account.")

;;; --- the key, and the headers it rides ------------------------------------------

(defun api-key ()
  "The Patsnap key: the secret in the section, else the environment variable
the section names. => a string, or NIL when neither holds one."
  (or (setting :api-key)
      (let ((name (setting :api-key-env)))
        (and name
             (plusp (length name))
             (let ((value (nlk:trimmed (or (uiop:getenv name) ""))))
               (and (plusp (length value)) value))))))

(defun request-headers (&optional (session *session*))
  "The headers one POST carries: the session's, the key in `apikey', and the
two MCP requires. The key is a header here and nowhere else."
  (let ((key (api-key)))
    (append (when (and session (getf session :id))
              (list (cons "mcp-session-id" (getf session :id))))
            (when (and session (getf session :protocol))
              (list (cons "mcp-protocol-version" (getf session :protocol))))
            (when key (list (cons "apikey" key)))
            (list (cons "accept" "application/json, text/event-stream")
                  (cons "content-type" "application/json")))))

(defun header-value (headers name)
  "NAME's value from a response header table, whatever case it carries."
  (when (hash-table-p headers)
    (or (gethash name headers)
        (loop for key being the hash-keys of headers
              when (and (stringp key) (string-equal key name))
                return (gethash key headers)))))

;;; --- reading an answer ------------------------------------------------------------

(defun sse-payloads (text)
  "Every JSON value a text/event-stream body carries, in order."
  (let ((payloads '())
        (buffer nil))
    (flet ((flush ()
             (when buffer
               (let ((value (ignore-errors (nlk:decode-json buffer))))
                 (when value (push value payloads)))
               (setf buffer nil))))
      (with-input-from-string (in text)
        (loop for line = (read-line in nil nil)
              while line
              do (cond ((zerop (length (nlk:trimmed line))) (flush))
                       ((and (> (length line) 5) (string= "data:" (subseq line 0 5)))
                        (let ((piece (subseq line 5)))
                          (setf piece (string-left-trim '(#\Space #\Tab) piece))
                          (setf buffer (if buffer
                                           (concatenate 'string buffer piece)
                                           piece)))))
              finally (flush)))
      (nreverse payloads))))

(defun rpc-message (text id)
  "The JSON-RPC message TEXT carries under ID, or the last message it carries.
TEXT is a JSON body or an event stream, and an empty body answers NIL."
  (let* ((single (and (stringp text) (plusp (length (nlk:trimmed text)))
                       (ignore-errors (nlk:decode-json text))))
         (messages (cond ((hash-table-p single) (list single))
                         ((stringp text) (sse-payloads text)))))
    (or (find id messages :test #'equal
              :key (lambda (message) (nlk:json-value message :any "id")))
        (car (last messages)))))

(defun clip (text limit)
  "TEXT's first LIMIT characters on one line, for a message that quotes a body."
  (let* ((text (nlk:trimmed (or text "")))
         (text (with-output-to-string (out)
                 (loop for char across text
                       do (write-char (if (member char '(#\Newline #\Return #\Tab)) #\Space char)
                                      out)))))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

(defun error-sentence (text message)
  "The sentence a failed answer carries: the JSON error's own, else the body's
first line, else a bare statement that there is none."
  (or (let ((error (and message (nlk:json-value message :object "error"))))
        (and error (or (nlk:json-value error :text "message")
                       (nlk:json-value error :text "error"))))
      (let ((body (and (stringp text) (clip text 300))))
        (and body (plusp (length body)) body))
      "no detail"))

;;; --- the wire --------------------------------------------------------------------
;;; One function reaches the network, so a test stubs NLK:HTTP and everything
;;; above it — session, retry-free call, rendering — runs as it does live.

(defun post (body)
  "One JSON-RPC POST to the configured endpoint => (values TEXT STATUS HEADERS)."
  (nlk:http :post (setting :endpoint)
            :headers (request-headers)
            :content (nlk:encode-json-object body)
            :timeout (setting :timeout)))

(defun notify (method)
  "One JSON-RPC notification on the current session: no id, answer unread."
  (nlk:http :post (setting :endpoint)
            :headers (request-headers)
            :content (nlk:encode-json-object
                      (nlk:json-object "jsonrpc" "2.0" "method" method))
            :timeout (setting :timeout))
  nil)

(defun ensure-session ()
  "Initialize this start's MCP session, once, and answer it."
  (or *session*
      (let* ((id (incf *request-id*))
             (body (nlk:json-object
                    "jsonrpc" "2.0" "id" id "method" "initialize"
                    "params" (nlk:json-object
                              "protocolVersion" +protocol-version+
                              "capabilities" (nlk:json-object)
                              "clientInfo" (nlk:json-object
                                            "name" "nodecode-patsnap"
                                            "version" "0.1.0")))))
        (multiple-value-bind (text status headers) (post body)
          (when (>= status 400)
            (fail "Patsnap refused the connection at ~a (HTTP ~d): ~a"
                  (setting :endpoint) status (error-sentence text nil)))
          (let* ((message (rpc-message text id))
                 (result (and message (nlk:json-value message :object "result"))))
            (when (and message (nlk:json-value message :object "error"))
              (fail "Patsnap refused the initialize: ~a" (error-sentence text message)))
            (setf *session*
                  (list :id (header-value headers "mcp-session-id")
                        :protocol (or (nlk:json-value result :string "protocolVersion")
                                      +protocol-version+)))
            (notify "notifications/initialized")
            *session*)))))

(defun rpc (method &optional params)
  "One request on the current session => its decoded result. A server that
has dropped the session (HTTP 404 or 410) says so once; the next call
initializes a new one, and this one does not retry in a loop."
  (let* ((id (incf *request-id*))
         (body (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                                 "params" (or params (nlk:json-object)))))
    (multiple-value-bind (text status) (post body)
      (if (member status '(404 410))
          (progn
            (setf *session* nil
                  *remote-tools* nil)
            (fail "Patsnap has dropped this cell's session (HTTP ~d): call again, ~
and the next call opens a new one" status))
          (let ((message (rpc-message text id)))
            (when (>= status 400)
              (fail "Patsnap answered HTTP ~d for ~a: ~a"
                    status method (error-sentence text message)))
            (when (null message)
              (fail "Patsnap answered nothing for ~a (HTTP ~d)" method status))
            (when (nlk:json-value message :object "error")
              (fail "Patsnap refused ~a: ~a"
                    method
                    (let ((error (nlk:json-value message :object "error")))
                      (format nil "~a~@[ (code ~a)~]"
                              (or (nlk:json-value error :text "message") "no message")
                              (nlk:json-value error :integer "code")))))
            (or (nlk:json-value message :object "result")
                (fail "Patsnap answered ~a with no result (HTTP ~d)" method status)))))))

(defun mcp-call (method &optional params)
  "One MCP request, the session initialized on first use. Refuses before any
request when the section names no key."
  (unless (api-key)
    (fail "no Patsnap key: write api_key in the patsnap section, or export ~@[~a~]"
          (setting :api-key-env)))
  (ensure-session)
  (rpc method params))

;;; --- what a tool call answers -----------------------------------------------------

(defun result-text (result)
  "RESULT, an MCP tools/call result, as the text the model reads: every text
part in order, else the structured content, and an error said as one."
  (let* ((parts (nlk:json-value result :array "content"))
         (text (nlk:trimmed
                (with-output-to-string (out)
                  (loop for part across parts
                        for body = (nlk:json-value part :string "text")
                        when body do (write-string body out) (terpri out)))))
         (structured (nlk:json-value result :object "structuredContent"))
         (errorp (nlk:json-value result :boolean "isError")))
    (cond ((plusp (length text)) (if errorp (format nil "ERROR: ~a" text) text))
          (structured (nlk:pretty-json structured))
          (errorp "ERROR: Patsnap reported an error with no message")
          (t "Patsnap answered with no content"))))

(defun call-tool (tool &optional arguments)
  "One Patsnap tool, ARGUMENTS a JSON object (NIL for none) => its text."
  (running-settings)
  (let ((answer (mcp-call "tools/call"
                           (nlk:json-object "name" tool
                                             "arguments" (or arguments (nlk:json-object))))))
    (incf *calls*)
    (result-text answer)))

;;; --- the server's own schemas -------------------------------------------------------

(defun remote-tools ()
  "The server's tool list, fetched once per start."
  (or *remote-tools*
      (setf *remote-tools*
            (or (nlk:json-value (mcp-call "tools/list" (nlk:json-object)) :array "tools")
                #()))))

(defun remote-tool (name)
  "The server's entry for the tool NAME, or NIL."
  (find name (remote-tools) :test #'equal
        :key (lambda (tool) (nlk:json-value tool :string "name"))))

(defun tools-text (&optional name)
  "What the server says about its own tools: every name and its first line,
or one tool's full argument schema. The schema is Patsnap's, not this cell's
paraphrase of it, so a call can be built from the authority."
  (let ((name (and name (nlk:trimmed name))))
    (if (and name (plusp (length name)))
        (let ((tool (remote-tool name)))
          (if (null tool)
              (format nil "Patsnap has no tool named ~a; it lists: ~{~a~^, ~}"
                      name (map 'list (lambda (tool) (nlk:json-value tool :string "name"))
                                (remote-tools)))
              (format nil "~a~%~%~a"
                      (or (nlk:json-value tool :string "description") "")
                      (nlk:pretty-json (nlk:json-value tool :object "inputSchema")))))
        (let ((tools (remote-tools)))
          (format nil "Patsnap's server lists ~d tool~:p:~%~{~a~}"
                  (length tools)
                  (loop for tool across tools
                        collect (format nil "- ~a~@[: ~a~]~%"
                                        (nlk:json-value tool :string "name")
                                        (let ((description (nlk:json-value tool :string "description")))
                                          (and description (clip description 220))))))))))
