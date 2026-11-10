;;;; mcp.lisp --- Aave's MCP server, talked to over JSON-RPC on streamable
;;;; HTTP: initialize, then tools/call, with the session the server hands
;;;; back in the Mcp-Session-Id header. An answer is JSON or a server-sent
;;;; event stream, and both read the same here.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-aave)

(defparameter *http* 'nlk:http
  "The one network call: (funcall *HTTP* METHOD URL :headers H :content TEXT
:TIMEOUT N) => (values BODY STATUS HEADERS). Tests stub this.")

(defparameter *session* nil
  "The open MCP session: a plist of :ID (the server's Mcp-Session-Id),
:READY and :NEXT-ID. NIL while none is open.")

(defparameter +protocol-version+ "2025-06-18" "The MCP revision this client speaks.")

(defun reset-session ()
  "Forget the open session: the next call initializes again."
  (setf *session* nil))

(defun server-url ()
  "The endpoint the aave section names, or a refusal saying what to set."
  (let* ((raw (or (setting :endpoint) ""))
         (url (string-right-trim "/" (string-trim '(#\Space #\Tab) raw))))
    (unless (and (or (uiop:string-prefix-p "https://" url) (uiop:string-prefix-p "http://" url))
                 (> (length url) 8))
      (fail "the aave endpoint ~s is not an http(s) URL: set \"endpoint\" to https://mcp.aave.com ~
in the aave section of the config, then (restart-cells \"nodecode-aave\")" raw))
    url))

(defun header (headers name)
  "HEADERS' value for NAME, case-insensitively: nlk:http hands back the
response header table dexador fills, its names lowercased; the tests' stub may
hand an alist instead, and both read the same here."
  (if (hash-table-p headers)
      (or (gethash name headers) (gethash (string-downcase name) headers))
      (cdr (assoc name headers :test #'string-equal))))

(defun body-text (body)
  "BODY, a string or octets, as text."
  (typecase body
    (null "")
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (t (princ-to-string body))))

(defun sse-payloads (text)
  "TEXT's JSON-RPC messages: the `data:' lines of a server-sent event
stream, or TEXT itself when it carries no such line."
  (if (null (search "data:" text))
      (list text)
      (loop for line in (uiop:split-string text :separator (string #\Newline))
            for clean = (string-left-trim '(#\Return) line)
            when (and (>= (length clean) 5) (string= "data:" clean :end1 5))
              collect (string-trim '(#\Space #\Tab) (subseq clean 5)))))

(defun json-message (payload)
  "PAYLOAD decoded when it is a JSON-RPC message, else NIL."
  (let ((message (handler-case (shasht:read-json payload) (error () nil))))
    (and (hash-table-p message)
         (or (gethash "result" message) (gethash "error" message))
         message)))

(defun message-of (body)
  "The JSON-RPC message BODY carries — the first one with a result or an
error — or NIL. A body framed as server-sent events is read by its data
lines; any other framing falls back to the one JSON object it holds."
  (let ((text (body-text body)))
    (or (loop for payload in (sse-payloads text)
              for message = (json-message payload)
              when message return message)
        (let ((start (position #\{ text)) (end (position #\} text :from-end t)))
          (and start end (> end start)
               (json-message (subseq text start (1+ end))))))))

(defun error-text (error)
  "ERROR, a JSON-RPC error object, as one line."
  (if (hash-table-p error)
      (or (gethash "message" error) (nlk:encode-json-object error))
      (princ-to-string error)))

(defun exchange (method params &key id)
  "One JSON-RPC message to the server => (values MESSAGE STATUS). With no
ID the message is a notification and MESSAGE is NIL."
  (let* ((envelope (nlk:json-object "jsonrpc" "2.0" "method" method))
         (headers (append (when (getf *session* :id)
                            (list (cons "mcp-session-id" (getf *session* :id))))
                          ;; the revision this client speaks rides every request
                          ;; after the handshake, as the protocol asks.
                          (when (getf *session* :ready)
                            (list (cons "mcp-protocol-version" +protocol-version+))))))
    (when id (setf (gethash "id" envelope) id))
    (when params (setf (gethash "params" envelope) params))
    (multiple-value-bind (body status response-headers)
        (funcall *http* "POST" (server-url)
                 :headers (append headers '(("content-type" . "application/json")
                                            ("accept" . "application/json, text/event-stream")))
                 :content (nlk:encode-json-object envelope)
                 :timeout (setting :timeout-seconds))
      (let ((session-id (header response-headers "mcp-session-id")))
        (when session-id (setf (getf *session* :id) session-id)))
      (values (message-of body) status))))

(defun ensure-session ()
  "Open the session the first call needs: initialize, then the initialized
notification. => the session plist."
  (unless (getf *session* :ready)
    (let ((message (exchange "initialize"
                             (nlk:json-object
                              "protocolVersion" +protocol-version+
                              "capabilities" (nlk:json-object)
                              "clientInfo" (nlk:json-object "name" "nodecode"
                                                             "version" "0.1.0"))
                             :id 1)))
      (cond ((null message)
             (reset-session)
             (fail "the aave server at ~a did not answer an initialize" (server-url)))
            ((gethash "error" message)
             (reset-session)
             (fail "the aave server refused a session: ~a" (error-text (gethash "error" message)))))
      (setf *session* (list :id (getf *session* :id) :ready t :next-id 2))
      (ignore-errors (exchange "notifications/initialized" nil))))
  *session*)

(defun request (method params)
  "METHOD on an open session => its RESULT object, or a refusal."
  (ensure-session)
  (let ((message (exchange method params
                           :id (prog1 (getf *session* :next-id)
                                 (incf (getf *session* :next-id))))))
    (cond ((null message)
           (fail "the aave server answered ~a with nothing this client could read" method))
          ((gethash "error" message)
           (fail "the aave server refused ~a: ~a" method (error-text (gethash "error" message))))
          (t (gethash "result" message)))))

(defun arguments-object (arguments)
  "ARGUMENTS, a tool call's decoded object, as one without its nulls — a
null argument reads as absent to the server."
  (let ((object (nlk:json-object)))
    (when (hash-table-p arguments)
      (maphash (lambda (key value) (unless (null value) (setf (gethash key object) value)))
               arguments))
    object))

(defun result-text (result)
  "RESULT, a tools/call answer, as the text the model reads."
  (let* ((content (and (hash-table-p result) (gethash "content" result)))
         (text (and (vectorp content) (plusp (length content))
                    (format nil "~{~a~^~%~}"
                            (loop for part across content
                                  collect (if (hash-table-p part)
                                              (or (gethash "text" part) "")
                                              ""))))))
    (cond ((null text) (if result (nlk:pretty-json result) ""))
          ((and (hash-table-p result) (gethash "isError" result))
           (concatenate 'string "ERROR: " text))
          (t text))))

(defun call-tool (name arguments)
  "Call the server's tool NAME with ARGUMENTS, a JSON object, and answer
the text it returns. A failure of the call itself is an AAVE-ERROR."
  (result-text (request "tools/call"
                        (nlk:json-object "name" name
                                         "arguments" (or arguments (nlk:json-object))))))

(defun tool-names ()
  "What the server serves now => ((NAME . DESCRIPTION) ...), newest read."
  (let* ((result (request "tools/list" nil))
         (tools (and (hash-table-p result) (gethash "tools" result))))
    (when (vectorp tools)
      (loop for tool across tools
            when (hash-table-p tool)
              collect (cons (gethash "name" tool) (or (gethash "description" tool) ""))))))
