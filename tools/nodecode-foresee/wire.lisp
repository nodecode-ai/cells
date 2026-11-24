;;;; wire.lisp --- Foresee's hosted MCP server, over streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was one manifest entry pointing Hermes at
;;;; https://agents.go-foresee.com/mcp. There is no other surface: the quotes
;;;; are the server's, so this cell speaks MCP's streamable HTTP itself --
;;;; one JSON-RPC POST per exchange, the Mcp-Session-Id the initialize
;;;; answered echoed on every later request, and the answer read either as a
;;;; JSON body or from the SSE frame the server sends it in. No Node, no local
;;;; server, no MCP transport library.
;;;;
;;;; Every byte goes through POST, so a test stubs NLK:HTTP and nothing
;;;; dials out. A refused status or a JSON-RPC error is FORESEE-ERROR with
;;;; the server's own words in it, which is what the model repairs from.

(in-package #:nodecode-foresee)

(defparameter +default-url+ "https://agents.go-foresee.com/mcp"
  "Foresee's hosted MCP server, where both tools live unless the section names
another address.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks: the one the plugin's `type:
streamable-http' entry reached.")

(defparameter +default-timeout+ 120
  "Seconds one call may wait. An estimate answers in seconds; a live walk is
commissioned and collected over minutes, which is two calls.")

(defvar *session* nil
  "The Mcp-Session-Id Foresee gave this process, NIL before the first call
and after a server that forgot it.")

;;; --- where, and as whom -------------------------------------------------------

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun trim (text)
  "TEXT without its surrounding whitespace, or NIL."
  (let ((trimmed (and (stringp text) (nlk:trimmed text))))
    (and trimmed (plusp (length trimmed)) trimmed)))

(defun url-parts (url)
  "URL's (values SCHEME HOST), both lowercased, or NIL when it is not an
absolute url."
  (let ((mark (and (stringp url) (search "://" url))))
    (when (and mark (plusp mark))
      (let* ((scheme (string-downcase (subseq url 0 mark)))
             (rest (subseq url (+ mark 3)))
             (end (or (position-if (lambda (character) (find character "/?#")) rest)
                      (length rest)))
             (authority (subseq rest 0 end))
             (at (position #\@ authority :from-end t))
             (hostport (if at (subseq authority (1+ at)) authority))
             (colon (position #\: hostport)))
        (values scheme (string-downcase (if colon (subseq hostport 0 colon) hostport)))))))

(defun endpoint ()
  "The MCP endpoint this cell talks to: the section's, else Foresee's own."
  (or (endpoint-setting) +default-url+))

(defun local-host-p (host)
  "Whether HOST is this machine or a name only a private network resolves."
  (or (member host '("localhost" "127.0.0.1" "::1" "[::1]") :test #'string=)
      (some (lambda (suffix) (uiop:string-suffix-p host suffix))
            '(".local" ".lan" ".internal" ".home.arpa" ".ts.net"))))

(defun endpoint-allowed-p (url)
  "https anywhere; plain http only to this machine or a private network, so a
key never crosses the internet in the clear."
  (multiple-value-bind (scheme host) (url-parts url)
    (and (plusp (length host))
         (cond ((equal scheme "https") t)
               ((equal scheme "http") (local-host-p host))
               (t nil)))))

(defun endpoint-setting ()
  "The endpoint the section names, trimmed, or NIL when it names none."
  (trim (setting :endpoint)))

(defun check-endpoint ()
  "Refuse at start an endpoint this client will not dial; an endpoint the
section leaves out or blanks is Foresee's own."
  (let ((url (endpoint)))
    (unless (endpoint-allowed-p url)
      (nlk:config-error
       "foresee: endpoint ~s is not an address this cell dials: https anywhere, plain http only to this machine or a private network (localhost, *.local, *.lan, *.internal)"
       url))
    url))

(defun api-key ()
  "The bearer token the section carries inline or names a variable for, or NIL:
Foresee's own server asks for no sign-in."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))))

;;; --- JSON on the wire ---------------------------------------------------------

(defun json-value-of (text)
  "TEXT decoded as JSON, or NIL."
  (when (and (stringp text) (plusp (length (nlk:trimmed text))))
    (handler-case (nlk:decode-json text) (error () nil))))

(defun sse-payload (text)
  "The last `data: {...}' payload of an SSE body, or NIL."
  (let ((last nil))
    (when (stringp text)
      (dolist (line (uiop:split-string text :separator (list #\Newline)))
        (let ((line (string-left-trim '(#\Space #\Tab #\Return) line)))
          (when (uiop:string-prefix-p "data:" line)
            (setf last (trim (subseq line 5)))))))
    last))

(defun answer-value (text)
  "TEXT as the JSON value it carries: a JSON body, an SSE frame, or the last
message of an SSE stream."
  (or (json-value-of text)
      (let ((payload (sse-payload text)))
        (and payload (json-value-of payload)))))

(defun header-value (headers name)
  "NAME's value in HEADERS, a dexador header table or an alist."
  (cond ((hash-table-p headers)
         (or (gethash (string-downcase name) headers) (gethash name headers)))
        ((listp headers)
         (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun request-headers (&optional session)
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("mcp-protocol-version" . "2025-06-18"))
          (and session (list (cons "mcp-session-id" session)))
          (let ((key (api-key)))
            (and key (list (cons "authorization" (format nil "Bearer ~a" key)))))))

(defun timeout-seconds ()
  (or (setting :timeout-seconds) +default-timeout+))

(defun post (body &key session (timeout (timeout-seconds)))
  "One JSON-RPC POST to the endpoint => (values TEXT STATUS HEADERS)."
  (let ((endpoint (endpoint)))
    (handler-case
        (multiple-value-bind (text status headers)
            (nlk:http :post endpoint
                      :content body
                      :timeout timeout
                      :connect-timeout (min timeout 10)
                      :headers (request-headers session))
          (values (if (stringp text) text "") status headers))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "foresee: can't reach ~a (~a); check the connection, or the endpoint in the foresee section"
              endpoint (nle:transport-failure-label condition endpoint))))))

;;; --- the session ---------------------------------------------------------------

(defun encode (value)
  (nlk:encode-json-object value))

(defun initialize (&key (timeout (timeout-seconds)))
  "Open the MCP session, keeping its id; answer it."
  (let ((body (encode (nlk:json-object
                       "jsonrpc" "2.0" "id" 0 "method" "initialize"
                       "params" (nlk:json-object
                                 "protocolVersion" +protocol-version+
                                 "capabilities" (nlk:json-object)
                                 "clientInfo" (nlk:json-object
                                               "name" "nodecode-foresee"
                                               "version" "0.1.0"))))))
    (multiple-value-bind (text status headers) (post body :timeout timeout)
      (let ((value (answer-value text))
            (session (header-value headers "mcp-session-id")))
        (cond
          ((= status 401)
           (fail "foresee: the server refused the key (HTTP 401); check api_key or api_key_env in the foresee section"))
          ((and (= status 200) (hash-table-p value) (gethash "result" value))
           (setf *session* (and (stringp session) (plusp (length session)) session))
           (ignore-errors
             (post (encode (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized"))
                   :session *session* :timeout timeout))
           *session*)
          (t
           (fail "foresee: ~a did not start an MCP session (HTTP ~d~@[: ~a~])"
                 (endpoint) status (trim text))))))))

(defun error-text (value)
  "VALUE's JSON-RPC error as the one line the model reads."
  (let ((error (gethash "error" value)))
    (cond ((hash-table-p error)
           (or (trim (gethash "message" error)) (trim (gethash "detail" error)) "the server refused it"))
          ((stringp error) error)
          (t "the server refused it"))))

(defun rpc (method &optional parameters &key (timeout (timeout-seconds)))
  "One MCP request, starting the session as needed and once more after the
server forgot it; answer its result object."
  (loop with first = t
        repeat 2
        do (progn
             (unless *session* (initialize :timeout timeout))
             (let ((body (encode (nlk:json-object
                                  "jsonrpc" "2.0" "id" 1 "method" method
                                  "params" (or parameters (nlk:json-object))))))
               (multiple-value-bind (text status)
                   (post body :session *session* :timeout timeout)
                 (cond
                   ((and first (member status '(400 404)))
                    ;; the server forgot this session (a restart or a recycle): start over once
                    (setf *session* nil))
                   ((= status 401)
                    (fail "foresee: the server refused the key (HTTP 401)"))
                   ((not (and (integerp status) (< status 300)))
                    (fail "foresee: ~a answered HTTP ~d~@[: ~a~]" method status (trim text)))
                   (t
                    (let ((value (answer-value text)))
                      (unless (hash-table-p value)
                        (fail "foresee: ~a answered something that is not JSON: ~a"
                              method (or (trim text) "(nothing)")))
                      (when (nth-value 1 (gethash "error" value))
                        (fail "foresee: ~a" (error-text value)))
                      (let ((result (gethash "result" value)))
                        (return (if (hash-table-p result) result (nlk:json-object)))))))))
             (setf first nil))))

;;; --- what the server offers -----------------------------------------------------

(defun server-tools (&key (timeout (timeout-seconds)))
  "Every tool the server lists, each a decoded object, in its own order."
  (let ((tools (gethash "tools" (rpc "tools/list" nil :timeout timeout))))
    (cond ((vectorp tools) (coerce tools 'list))
          ((listp tools) tools)
          (t '()))))

(defun content-text (result)
  "RESULT's text content parts, joined."
  (let ((content (gethash "content" result))
        (out (make-string-output-stream)))
    (when (vectorp content)
      (loop for part across content
            when (and (hash-table-p part) (equal (gethash "type" part) "text"))
              do (write-string (or (gethash "text" part) "") out)
                 (write-char #\Newline out)))
    (string-right-trim '(#\Newline) (get-output-stream-string out))))

(defun call-tool (name arguments)
  "tools/call NAME with ARGUMENTS => the result object, the server's own."
  (rpc "tools/call" (nlk:json-object "name" name "arguments" arguments)))
