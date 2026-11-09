;;;; mcp.lisp --- vidIQ's hosted MCP: JSON-RPC 2.0 over streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The endpoint is vidIQ's own (the plugin carries it in .mcp.json and
;;;; nowhere else) and it wants an OAuth access token as a bearer header: a
;;;; bare POST answers 401 and a pointer to its resource metadata.
;;;;
;;;; One POST per message. The first call of a process sends initialize,
;;;; keeps the protocol version it answers and the Mcp-Session-Id header it
;;;; sends, and tells the server the client is ready; every later call
;;;; carries the session. A body is either JSON or an SSE stream whose last
;;;; `data:' event is the message. The token never appears in an answer, a
;;;; refusal or a log.

(in-package #:nodecode-vidiq)

(defvar *rpc-id* 0
  "The id the next request sends.")

(defvar *session* nil
  "What the initialize handshake left: a plist (:id ... :version ...), or NIL before it.")

(defun endpoint ()
  "The MCP endpoint every call goes to: the section's, else the hosted one."
  (let ((url (setting :mcp-url)))
    (if (and (stringp url) (plusp (length url)))
        (string-right-trim "/" url)
        +endpoint+)))

(defun access-token ()
  "The token every call sends: the section's secret, else VIDIQ_ACCESS_TOKEN."
  (or (let ((token (setting :access-token)))
        (and (stringp token) (plusp (length token)) token))
      (nle::credential-env "VIDIQ_ACCESS_TOKEN")))

(defun require-token ()
  "ACCESS-TOKEN, or the refusal naming where an operator puts one."
  (or (access-token)
      (fail "no vidIQ token: authorize ~a in a client that supports OAuth (or create a vidIQ API ~
key), then set the config's vidiq.access_token (or export VIDIQ_ACCESS_TOKEN)" +sign-in+)))

(defun headers (token &key (version +protocol-version+))
  "What every request carries. The token is a bearer header and nothing else names it."
  `(("content-type" . "application/json")
    ("accept" . "application/json, text/event-stream")
    ("mcp-protocol-version" . ,version)
    ("user-agent" . ,(format nil "vidiq-nodecode/~a" +client-version+))
    ,@(when token `(("authorization" . ,(format nil "Bearer ~a" token))))
    ,@(let ((id (getf *session* :id)))
        (when (and id (plusp (length id)))
          `(("mcp-session-id" . ,id))))))

(defun clip (text limit)
  "TEXT, at most LIMIT characters of it: what an answer that is not what was asked for shows."
  (let ((text (princ-to-string (or text ""))))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

(defun header-value (name headers)
  "HEADERS' value for NAME, whether they came as a hash table or an alist."
  (cond
    ((hash-table-p headers)
     (or (gethash name headers)
         (gethash (string-downcase name) headers)))
    ((listp headers)
     (cdr (assoc name headers :test #'string-equal)))
    (t nil)))

(defun join-lines (parts)
  "PARTS as one text, a newline between them: what an SSE event's data lines and a
result's text parts each answer."
  (with-output-to-string (out)
    (loop for (part . rest) on parts
          do (when part (princ part out))
             (when rest (terpri out)))))

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
          (setf last (join-lines lines)))))
    last))

(defun parse-rpc-body (text)
  "The JSON-RPC message TEXT carries: plain JSON, or the last data: event of an SSE stream."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (cond
      ((zerop (length trimmed))
       (fail "vidIQ MCP answered an empty body"))
      ((char= (char trimmed 0) #\{)
       (nlk:decode-json trimmed :whole t))
      (t
       (let ((data (sse-data trimmed)))
         (unless data
           (fail "vidIQ MCP answered no JSON-RPC message: ~a" (clip trimmed 200)))
         (nlk:decode-json data :whole t))))))

(defun post (message &key (version +protocol-version+))
  "Send one JSON-RPC MESSAGE. => (values BODY STATUS HEADERS). Fails on a transport failure."
  (let* ((token (require-token))
         (address (endpoint))
         (timeout (setting :timeout-seconds)))
    (handler-case
        (nlk:http "POST" address
                  :headers (headers token :version version)
                  :content (nlk:encode-json-object message)
                  :timeout timeout :connect-timeout (min 15 timeout))
      (error (condition)
        (fail "vidIQ MCP at ~a did not answer: ~a [~a]"
              address (nle::transport-failure-label condition address)
              (princ-to-string condition))))))

(defun unauthorized-p (status)
  (or (eql status 401) (eql status 403)))

(defun rpc (method &optional params
                   &aux (message (nlk:json-object "jsonrpc" "2.0" "id" (incf *rpc-id*)
                                                   "method" method
                                                   :opt "params" params)))
  "One JSON-RPC call: METHOD with PARAMS (a JSON object or NIL). => the message's result.
Fails with VIDIQ-ERROR on a transport failure, a body that is not JSON-RPC, or an error message."
  (multiple-value-bind (text status response-headers)
      (post message)
    (let* ((message (handler-case (parse-rpc-body text)
                       (error ()
                         (fail "vidIQ MCP answered ~a with a body that is not JSON-RPC: ~a"
                               status (clip text 200)))))
           (error-object (and message (nlk:json-value message :object "error"))))
      (cond
        (error-object
         (let* ((data (nlk:json-value error-object :object "data"))
                (code (or (and data (nlk:json-value data :string "code"))
                          (format nil "rpc_~a" (nlk:json-value error-object :any "code"))))
                (detail (or (nlk:json-value error-object :text "message") "vidIQ MCP error")))
           (fail "~a (~a)~@[~a~]"
                 detail code
                 (when (or (unauthorized-p status)
                           (member code '("unauthorized" "invalid_token") :test #'equal))
                   (format nil ": authorize ~a again and set a current token in the vidiq ~
section's access_token" +sign-in+)))))
        ((null message)
         (fail "vidIQ MCP answered ~a with no JSON-RPC message" status))
        (t (let ((version (nlk:json-value message :string "result" "protocolVersion"))
                 (id (header-value "mcp-session-id" response-headers)))
             (when (or version id)
               (setf *session* (list :id (or id (getf *session* :id) "")
                                     :version (or version (getf *session* :version)
                                                  +protocol-version+))))
             (nlk:json-value message :any "result")))))))

(defun initialize ()
  "The handshake MCP wants once per process: initialize, then the ready notification.
Errors are left to the call that follows, which reports them the same way."
  (setf *session* nil)
  (rpc "initialize" (nlk:json-object
                     "protocolVersion" +protocol-version+
                     "capabilities" (nlk:json-object)
                     "clientInfo" (nlk:json-object "name" "nodecode-vidiq"
                                                    "version" +client-version+)))
  (ignore-errors
   (post (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized"
                           :opt "params" (nlk:json-object))
         :version (getf *session* :version)))
  t)

(defun ensure-session ()
  (unless *session*
    (initialize)))

(defun result-text (result)
  "RESULT's text: its text parts, else its structured content as JSON."
  (let ((parts (loop for part across (or (nlk:json-value result :array "content") #())
                     for text = (nlk:json-value part :string "text")
                     when (and text (equal "text" (nlk:json-value part :string "type")))
                       collect text)))
    (if parts
        (join-lines parts)
        (let ((structured (nlk:json-value result :any "structuredContent")))
          (if structured (nlk:encode-json-object structured) "")))))

(defun as-object (arguments)
  "ARGUMENTS as the JSON object a tools/call takes."
  (cond ((null arguments) (nlk:json-object))
        ((hash-table-p arguments) arguments)
        (t (fail "a tool's arguments are a JSON object — (nlk:json-object \"channelId\" \"...\") ~
— not ~a" arguments))))

(defun call-tool (name &optional arguments)
  "Call the vidIQ tool NAME with ARGUMENTS, a JSON object, and answer its text."
  (ensure-session)
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

(defun tools-list ()
  "Every tool this account can call, as the server lists them."
  (ensure-session)
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
  "List the vidIQ tools this account can call, one line each."
  (let ((entries (tools-list)))
    (if (null entries)
        "vidIQ listed no tools for this account."
        (format nil "~d vidIQ tool~:p:~%~{~a~^~c~}"
                (length entries)
                (loop for entry in entries
                      collect (format nil "  ~a — ~a"
                                      (or (nlk:json-value entry :string "name") "?")
                                      (clip (or (nlk:json-value entry :string "description") "") 160)))
                #\Newline))))
