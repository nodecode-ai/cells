;;;; mcp.lisp --- the MCP client: JSON-RPC 2.0 over TAM's streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original spoke MCP to the `tam' binary over stdio, or to a TAM or
;;;; tam-team server over HTTP. This cell speaks the HTTP half: one POST per
;;;; message, a JSON or single-response SSE reply, the session id the server
;;;; hands back in Mcp-Session-Id, and the bearer token a team server asks
;;;; for. Nothing here is stateful between calls: a call opens a session,
;;;; asks, and lets it go.

(in-package #:nodecode-tam)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client opens with; the server may answer an older one it knows.")

(defparameter +client-name+ "nodecode"
  "What the initialize handshake names this client.")

(defparameter +request-id+ 0
  "The JSON-RPC id the next request carries; an id only pairs a reply with its ask.")

;;; --- text ---------------------------------------------------------------------

(defun nonblank (value)
  "VALUE trimmed, or NIL when it is not a string with something in it."
  (let ((text (and (stringp value) (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (and text (plusp (length text)) text)))

(defun clipped (text &optional (limit 300))
  "TEXT trimmed, cut to LIMIT characters. A refusal quotes a server, never all of it."
  (let ((text (or (nonblank text) "")))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

;;; --- the one seam to the wire ------------------------------------------------

(defun http (url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS HEADERS), every status a value."
  (nlk:http :post url :headers headers :content content :timeout timeout))

(defun endpoint ()
  "The TAM MCP endpoint the section names."
  (or (nonblank (setting :url))
      (fail "TAM is not running: the tam section names no url")))

(defun token ()
  "The bearer token a team server asks for, read from the variable the section
names, or NIL when it is unset."
  (let ((name (nonblank (setting :token-env))))
    (and name (nonblank (nle::credential-env name)))))

(defun headers (&key session)
  "The headers every request carries, the session id and the token among them
when there are any."
  (remove nil
          (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "mcp-protocol-version" +protocol-version+)
                (let ((token (token)))
                  (and token (cons "authorization" (format nil "Bearer ~a" token))))
                (and session (cons "mcp-session-id" session)))))

(defun header (headers name)
  "The value HEADERS carries for NAME, an alist, a plist or a table."
  (cond ((null headers) nil)
        ((hash-table-p headers)
         (or (gethash name headers)
             (loop for key being the hash-keys of headers using (hash-value value)
                   when (string-equal name (string key)) return value)))
        ((consp (first headers))
         (loop for (key . value) in headers
               when (string-equal name (string key)) return value))
        (t (loop for (key value) on headers by #'cddr
                 when (string-equal name (string key)) return value))))

(defun post (method-document &key session)
  "POST one JSON-RPC document to TAM. => (values BODY STATUS SESSION-ID)."
  (multiple-value-bind (body status response-headers)
      (http (endpoint) :headers (headers :session session)
                        :content (nlk:encode-json-object method-document)
                        :timeout (setting :timeout-seconds))
    (values body status (header response-headers "mcp-session-id"))))

;;; --- reading a reply ----------------------------------------------------------

(defun message-of (text)
  "TEXT read as one JSON-RPC message: a table carrying result or error, or NIL."
  (let ((value (and (stringp text) (plusp (length text))
                    (ignore-errors (nlk:decode-json text)))))
    (flet ((message-p (item)
             (and (hash-table-p item)
                  (or (gethash "result" item) (gethash "error" item))
                  item)))
      (cond ((message-p value))
            ((and (vectorp value) (plusp (length value)))
             (find-if #'message-p value))))))

(defun sse-message (text)
  "The first JSON-RPC message in TEXT, a server-sent stream, or NIL."
  (let ((pending '()))
    (flet ((take ()
             (prog1 (and pending (message-of (format nil "~{~a~^~%~}" (reverse pending))))
               (setf pending '()))))
      (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
        (cond ((uiop:string-prefix-p "data:" line)
               (push (string-left-trim " " (subseq line 5)) pending))
              ((zerop (length (string-trim '(#\Return #\Space #\Tab) line)))
               (let ((message (take)))
                 (when message (return-from sse-message message))))))
      (take))))

(defun reply-of (body status)
  "BODY read as the JSON-RPC reply to one request. => its RESULT table."
  (when (>= status 400)
    (let* ((message (message-of body))
           (error (and message (gethash "error" message))))
      (fail "TAM answered HTTP ~d: ~a" status
            (if (hash-table-p error)
                (or (gethash "message" error) (clipped body))
                (clipped body)))))
  (let ((message (or (message-of body) (sse-message body))))
    (unless message
      (fail "TAM answered with nothing this cell could read: ~a" (clipped body)))
    (let ((error (gethash "error" message)))
      (when (hash-table-p error)
        (fail "TAM answered JSON-RPC error ~a: ~a"
              (or (gethash "code" error) "?")
              (or (gethash "message" error) "no message")))
      (let ((result (gethash "result" message)))
        (unless (hash-table-p result)
          (fail "TAM answered with no result object"))
        result))))

(defun rpc (method params &key session)
  "One JSON-RPC request to TAM. => (values RESULT SESSION-ID)."
  (let* ((id (incf +request-id+))
         (document (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                                     "params" (or params (nlk:json-object)))))
    (multiple-value-bind (body status new-session)
        (post document :session session)
      (values (reply-of body status) (or new-session session)))))

(defun send-notification (method &key session)
  "One JSON-RPC notification, which the server answers with no message."
  (multiple-value-bind (body status) (post (nlk:json-object "jsonrpc" "2.0" "method" method)
                                            :session session)
    (declare (ignore body))
    (when (>= status 400)
      (fail "TAM refused the ~a notification: HTTP ~d" method status))))

;;; --- the session --------------------------------------------------------------

(defun open-session ()
  "The MCP handshake. => (values SESSION-ID SERVER-NAME VERSION TOOL-NAMES)."
  (multiple-value-bind (result session)
      (rpc "initialize" (nlk:json-object
                          "protocolVersion" +protocol-version+
                          "capabilities" (nlk:json-object)
                          "clientInfo" (nlk:json-object "name" +client-name+
                                                          "version" "0.1.0")))
    (send-notification "notifications/initialized" :session session)
    (let* ((server (gethash "serverInfo" result))
           (listing (rpc "tools/list" (nlk:json-object) :session session))
           (tools (gethash "tools" listing)))
      (values session
              (and (hash-table-p server) (or (gethash "name" server) ""))
              (and (hash-table-p server) (or (gethash "version" server) ""))
              (loop for tool across (if (vectorp tools) tools #())
                    when (hash-table-p tool) collect (gethash "name" tool))))))

(defun backend-of (tools)
  "Which TAM answers: :team when it exposes memory_scopes, else :local."
  (if (member "memory_scopes" tools :test #'string=) :team :local))

(defun content-text (result)
  "RESULT's text parts, joined."
  (let ((parts (gethash "content" result)))
    (format nil "~{~a~}" (loop for part across (if (vectorp parts) parts #())
                                when (and (hash-table-p part)
                                          (equal "text" (gethash "type" part)))
                                  collect (or (gethash "text" part) "")))))

(defun tool-payload (result)
  "RESULT as the value the caller works on: the structured content when the
server sends one, else its text read as JSON, else the text itself."
  (let ((text (content-text result))
        (structured (gethash "structuredContent" result)))
    (when (gethash "isError" result)
      (fail "TAM reported: ~a" (or (nonblank text) "the tool failed")))
    (or (and (hash-table-p structured) structured)
        (let ((read (and (nonblank text) (ignore-errors (nlk:decode-json text)))))
          (or (and (hash-table-p read) read) text)))))

(defun call-tool (name arguments-of)
  "Call TAM's tool NAME; ARGUMENTS-OF takes the backend and answers the
arguments object. => (values PAYLOAD BACKEND)."
  (multiple-value-bind (session server version tools) (open-session)
    (declare (ignore server version))
    (unless (member name tools :test #'string=)
      (fail "TAM at ~a does not expose ~a; it exposes ~{~a~^, ~}"
            (endpoint) name tools))
    (let ((backend (backend-of tools)))
      (values (tool-payload
               (rpc "tools/call" (nlk:json-object "name" name
                                                    "arguments" (funcall arguments-of backend))
                    :session session))
              backend))))
