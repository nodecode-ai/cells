;;;; mcp.lisp --- the one network call: JSON-RPC over Streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original talked to its host over the MCP transport Hermes speaks; a
;;;; cell has no MCP client of its own, so the wire is here: an `initialize'
;;;; handshake, the `notifications/initialized' notice, and one `tools/call'
;;;; per tool. Every answer is either a JSON body or an SSE stream carrying
;;;; one, which is what the server chooses between; both are read here.
;;;;
;;;; Every byte goes through *HTTP* -- NLK:HTTP -- so a test stubs that one
;;;; name and never opens a socket.

(in-package #:nodecode-super-artifacts)

(defparameter *http* #'nlk:http
  "The cell's one network call. A test binds this, so nothing dials the server.")

(defvar *session-id* nil
  "The Mcp-Session-Id the handshake answered with, sent back on every later
call; NIL before the handshake or after the server named none.")

(defvar *initialized* nil
  "Whether the handshake has completed for the token in hand.")

(defvar *request-id* 0
  "The last JSON-RPC id this cell sent.")

;;; --- reading an answer -----------------------------------------------------------

(defun body-text (body)
  "An HTTP BODY -- text, or octets -- as a string."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun header-value (headers name)
  "HEADERS -- an alist -- with the value under NAME, whatever its case."
  (loop for (key . value) in headers
        when (and (stringp key) (string-equal key name))
          return value))

(defun sse-data (text)
  "The last `data:' line of an SSE stream, joined the way the transport
writes it, or NIL when TEXT carries none."
  (let ((data (loop for line in (uiop:split-string text :separator '(#\Newline))
                     when (uiop:string-prefix-p "data:" (string-left-trim '(#\Return) line))
                       collect (string-left-trim '(#\Space #\Return)
                                                  (subseq (string-left-trim '(#\Return) line) 5)))))
    (and data (format nil "~{~a~}" data))))

(defun decode-answer (text)
  "TEXT -- a JSON body, or an SSE stream carrying one -- as decoded JSON, or
NIL for an empty body."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (cond
      ((or (null text) (string= text "")) :null)
      ((or (uiop:string-prefix-p "event:" text) (uiop:string-prefix-p "data:" text))
       (let ((payload (sse-data text)))
         (unless payload (fail "the server sent an event stream with no data: line"))
         (decode-answer payload)))
      (t (handler-case (nlk:decode-json text :whole t)
           (error (condition)
             (declare (ignore condition))
             (fail "the server answered something that is not JSON: ~a" (nlk:one-line text :cap 300))))))))

(defun rpc-error-text (json)
  "JSON's JSON-RPC error, as one line, or NIL."
  (let ((error (and (hash-table-p json) (gethash "error" json))))
    (when (hash-table-p error)
      (let ((message (nlk:json-value error :string "message")))
        (or message (nlk:encode-json-object error))))))

(defun refusal-line (json status)
  "The words a refused answer carries: the JSON-RPC error, else the body's
own text."
  (or (rpc-error-text json)
      (and (hash-table-p json) (nlk:json-value json :string "message"))
      (format nil "HTTP ~a" status)))

(defun result-of (json status)
  "JSON's result when STATUS is a success, else a refusal saying why."
  (let ((error-text (rpc-error-text json)))
    (when (or error-text (and (integerp status) (>= status 400)))
      (fail "~a" (refusal-line json status)))
    (let ((result (and (hash-table-p json) (gethash "result" json))))
      (if (eq result :null) nil result))))

;;; --- the wire --------------------------------------------------------------------

(defun endpoint-text ()
  "The section's base_url as an absolute https URL (plain http only on
loopback), or a refusal naming the member."
  (let* ((text (nlk:trimmed (or (setting :base-url) "")))
         (uri (and (plusp (length text)) (ignore-errors (quri:uri text))))
         (scheme (and uri (string-downcase (or (quri:uri-scheme uri) ""))))
         (host (and uri (string-downcase (or (quri:uri-host uri) "")))))
    (when (string= text "") (setf text +endpoint+))
    (unless (and uri host (plusp (length host))
                 (member scheme '("http" "https") :test #'string=)
                 (or (string= scheme "https")
                     (member host '("localhost" "::1" "[::1]") :test #'string=)
                     (uiop:string-prefix-p "127." host)))
      (fail "base_url must be an https URL to the MCP server (plain http only on loopback): ~s"
            (or (nlk:trimmed (or (setting :base-url) "")) "none")))
    text))

(defun origin-text ()
  "The product's origin: the scheme and host of base_url, where the OAuth
endpoints sit."
  (let* ((uri (quri:uri (endpoint-text)))
         (scheme (string-downcase (or (quri:uri-scheme uri) "https")))
         (host (string-downcase (or (quri:uri-host uri) "")))
         (port (quri:uri-port uri))
         (default (if (string= scheme "https") 443 80)))
    (if (and port (/= port default))
        (format nil "~a://~a:~d" scheme host port)
        (format nil "~a://~a" scheme host))))

(defun state-directory ()
  "The folder holding a finished sign-in's token: the section's state_dir,
else <home>/super-artifacts/."
  (let ((named (nlk:trimmed (or (setting :state-dir) ""))))
    (uiop:ensure-directory-pathname
     (if (plusp (length named)) (nlk:expand-home named) (nlk:home "super-artifacts/")))))

(defun token-path ()
  "The file a finished sign-in keeps its token in."
  (merge-pathnames "token.json" (state-directory)))

(defun file-text (path)
  "PATH's whole text, or NIL when there is no such file."
  (when (probe-file path) (uiop:read-file-string path)))

(defvar *token* nil
  "The bearer token resolved for this start: a key from the section, or the
one a sign-in kept. NIL until the first call.")

(defun key-from-file ()
  "The key the section's key_file holds, or NIL."
  (let ((path (nlk:trimmed (or (setting :key-file) ""))))
    (when (plusp (length path))
      (let ((text (nlk:trimmed (or (file-text (nlk:expand-home path)) ""))))
        (and (plusp (length text)) text)))))

(defun key-from-environment ()
  "The key the variable key_env names holds, or NIL."
  (let* ((name (nlk:trimmed (or (setting :key-env) +key-variable+)))
         (value (and (plusp (length name)) (uiop:getenv name))))
    (and value (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))

(defun wake ()
  "Forget the handshake: a new token is a new session."
  (setf *initialized* nil *session-id* nil))

(defun bearer-token ()
  "The token one request carries, or a refusal naming what to set."
  (or (key-from-file)
      (key-from-environment)
      (saved-access-token)
      (fail "no Super Artifacts key: set key_file or the environment variable ~a (default ~a), or run /super-artifacts signin"
            (or (nlk:trimmed (or (setting :key-env) "")) +key-variable+)
            +key-variable+)))

(defun request-headers (&key auth session)
  "The headers one MCP request carries."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "user-agent" "nodecode-super-artifacts/0.1")
                (cons "mcp-protocol-version" +protocol-version+))
          (when auth (list (cons "authorization" (format nil "Bearer ~a" auth))))
          (when session (list (cons "mcp-session-id" session)))))

(defun post-rpc (body &key auth)
  "BODY -- a JSON string -- to the endpoint, answering (VALUES TEXT STATUS HEADERS).
The transport's Mcp-Session-Id, when the answer names one, is kept for the
calls after it."
  (multiple-value-bind (text status headers)
      (funcall *http* :post (endpoint-text)
               :headers (request-headers :auth auth :session *session-id*)
               :content body
               :timeout (setting :timeout-seconds))
    (let ((session (header-value headers "mcp-session-id")))
      (when (and session (stringp session) (plusp (length session)))
        (setf *session-id* session)))
    (values text status headers)))

(defun rpc (method &optional params &key notify)
  "One JSON-RPC exchange: METHOD with PARAMS, answering its decoded result.
A notification (NOTIFY) sends no id and answers whatever came back."
  (let* ((token (bearer-token))
         (id (unless notify (incf *request-id*)))
         (body (nlk:encode-json-object
                (apply #'nlk:make-json-object
                       (append (list "jsonrpc" "2.0")
                               (when id (list "id" id))
                               (list "method" method)
                               (when params (list "params" params)))))))
    (multiple-value-bind (text status)
        (post-rpc body :auth token)
      (when (and (integerp status) (= status 401))
        (wake)
        (retry-with-refreshed-token method params :notify notify))
      (let ((json (decode-answer (body-text text))))
        (if notify
            (progn (result-of json status) t)
            (result-of json status))))))

(defun initialize ()
  "The handshake the transport wants before any tool call."
  (unless *initialized*
    (let ((answer (rpc "initialize"
                       (nlk:make-json-object
                        "protocolVersion" +protocol-version+
                        "capabilities" (nlk:make-json-object)
                        "clientInfo" (nlk:make-json-object "name" "nodecode"
                                                            "version" "0.1")))))
      (declare (ignore answer))
      (rpc "notifications/initialized" nil :notify t)
      (setf *initialized* t))))

(defun call-tool (name arguments)
  "One tools/call, answering the text its result carries."
  (initialize)
  (let ((result (rpc "tools/call"
                     (nlk:make-json-object "name" name
                                            "arguments" (or arguments (nlk:make-json-object))))))
    (when (and (hash-table-p result) (gethash "isError" result))
      (let ((words (content-text result)))
        (fail "~a~%~a" words
              (if (search "Not connected" words)
                  "The person has to sign in: /super-artifacts signin prints a code to approve."
                  "Retry, or call super_help through super_tool for the tool's own rules."))))
    (content-text result)))

(defun content-text (result)
  "RESULT's text: every text content block joined, else its structured
content, else the whole result as JSON."
  (let* ((content (and (hash-table-p result) (gethash "content" result)))
         (texts (and (vectorp content)
                     (loop for block across content
                           when (and (hash-table-p block)
                                     (equal "text" (nlk:json-value block :string "type")))
                             collect (nlk:json-value block :string "text")))))
    (cond
      (texts (format nil "~{~a~^~%~}" texts))
      ((and (hash-table-p result) (gethash "structuredContent" result))
       (nlk:encode-json-object (gethash "structuredContent" result)))
      (result (nlk:encode-json-object result))
      (t ""))))
