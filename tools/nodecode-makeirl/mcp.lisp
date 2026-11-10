;;;; mcp.lisp --- MakeIRL's MCP server, over streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One JSON-RPC POST per exchange to https://makeirl.com/api/mcp, the
;;;; account's access token in the Authorization header, an Mcp-Session-Id
;;;; reused until the server forgets it. Nothing here decides anything: every
;;;; check (ERC, DRC, schematic-to-PCB parity, BOM, exports) runs on MakeIRL,
;;;; from the account the token belongs to.

(in-package #:nodecode-makeirl)

;;; --- what the wire says -------------------------------------------------------

(defparameter +default-endpoint+ "https://makeirl.com/api/mcp"
  "Where MakeIRL's MCP server answers unless the section names another address.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks.")

(defparameter +default-timeout+ 120
  "Seconds one call may wait; a release run is polled, never waited on.")

(defvar *session* nil
  "The Mcp-Session-Id MakeIRL gave this process, or NIL before the first call.")

(defvar *request-id* 0
  "The JSON-RPC id of the last request this process sent.")

;;; --- where, and as whom -------------------------------------------------------

(defun trimmed-text (text)
  "TEXT without its edges, or NIL when it is not a string or holds nothing."
  (and (stringp text)
       (let ((trimmed (nlk:trimmed text)))
         (and (plusp (length trimmed)) trimmed))))

(defun expand-home (text)
  "TEXT as a pathname, ~/ read under the home directory."
  (if (uiop:string-prefix-p "~/" text)
      (merge-pathnames (subseq text 2) (user-homedir-pathname))
      (pathname text)))

(defun endpoint ()
  "The MCP address the section names, else MakeIRL's own."
  (or (trimmed-text (setting :endpoint)) +default-endpoint+))

(defun timeout-seconds ()
  (or (setting :timeout-seconds) +default-timeout+))

(defun token-file-text (path)
  "The token PATH holds, or NIL when it holds nothing."
  (let ((text (ignore-errors (uiop:read-file-string (expand-home path)))))
    (trimmed-text text)))

(defun token ()
  "The access token: the section's own, else the environment variable it
names, else the file it names; a refusal naming all three when none answers."
  (or (trimmed-text (setting :access-token))
      (let ((name (trimmed-text (setting :token-env))))
        (and name (trimmed-text (nle::credential-env name))))
      (let ((path (trimmed-text (setting :token-file))))
        (and path (token-file-text path)))
      (fail "no MakeIRL token: sign in at makeirl.com, then put the access token in this section's access_token, or in the environment variable token_env names, or in the file token_file names")))

(defun token-set-p ()
  "Whether the section names a token at all, by value, variable or file."
  (not (null (or (trimmed-text (setting :access-token))
                 (trimmed-text (setting :token-env))
                 (trimmed-text (setting :token-file))))))

;;; --- JSON on the wire ---------------------------------------------------------

(defun json-value-of (text)
  "TEXT decoded as JSON, or NIL."
  (when (trimmed-text text)
    (handler-case (nlk:decode-json text) (error () nil))))

(defun sse-payload (text)
  "The last `data: {...}' payload of an SSE body, or NIL."
  (let ((last nil))
    (when (stringp text)
      (dolist (line (uiop:split-string text :separator (list #\Newline)))
        (let ((line (string-left-trim '(#\Space #\Tab #\Return) line)))
          (when (uiop:string-prefix-p "data:" line)
            (setf last (trimmed-text (subseq line 5)))))))
    last))

(defun answer-value (text)
  "TEXT as the JSON value it carries, a JSON body or the last SSE frame."
  (or (json-value-of text)
      (let ((payload (sse-payload text)))
        (and payload (json-value-of payload)))))

(defun encode (value) (nlk:encode-json-object value))

(defun header-value (headers name)
  (let ((pair (find name headers
                    :key (lambda (entry) (if (consp entry) (car entry) entry))
                    :test #'string-equal)))
    (and (consp pair) (cdr pair))))

(defun clipped (text)
  "TEXT on one line, at most 300 characters, for a refusal."
  (let ((one-line (substitute #\Space #\Newline (or text ""))))
    (if (> (length one-line) 300) (subseq one-line 0 300) one-line)))

(defun next-id () (incf *request-id*))

;;; --- one exchange -------------------------------------------------------------

(defun request-headers (session)
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("mcp-protocol-version" . "2025-06-18"))
          (list (cons "authorization" (format nil "Bearer ~a" (token))))
          (and session (list (cons "mcp-session-id" session)))))

(defun post (body &key session (timeout (timeout-seconds)))
  "One JSON-RPC POST => (values TEXT STATUS HEADERS)."
  (let ((headers (request-headers session)))
    (handler-case
        (multiple-value-bind (text status answer-headers)
            (nlk:http :post (endpoint) :content body :timeout timeout
                      :connect-timeout (min timeout 10) :headers headers)
          (values (if (stringp text) text "") status answer-headers))
      ((or error sb-sys:deadline-timeout) (condition)
        (declare (ignore condition))
        (fail "can't reach MakeIRL at ~a: check the connection, then try again" (endpoint))))))

(defun initialize (&key (timeout (timeout-seconds)))
  "Open the MCP session, keeping its id; answer the id, or NIL."
  (let ((body (encode (nlk:json-object
                       "jsonrpc" "2.0" "id" (next-id) "method" "initialize"
                       "params" (nlk:json-object
                                 "protocolVersion" +protocol-version+
                                 "capabilities" (nlk:json-object)
                                 "clientInfo" (nlk:json-object
                                               "name" "nodecode" "version" "1"))))))
    (multiple-value-bind (text status headers) (post body :timeout timeout)
      (let ((value (answer-value text))
            (session (header-value headers "mcp-session-id")))
        (cond ((= status 401)
               (fail "MakeIRL refused the token (401): sign in at makeirl.com, then set the section's access_token, token_env or token_file again"))
              ((and (= status 200) (hash-table-p value) (gethash "result" value))
               (when session (setf *session* session))
               (ignore-errors
                 (post (encode (nlk:json-object "jsonrpc" "2.0"
                                                 "method" "notifications/initialized"))
                       :session *session* :timeout timeout))
               *session*)
              (t (fail "MakeIRL did not start an MCP session (HTTP ~d: ~a)"
                       status (clipped text))))))))

(defun rpc (method &key params (timeout (timeout-seconds)))
  "One MCP request, starting the session as needed and once more after the
server forgot it; answer its result object."
  (loop with first = t
        repeat 2
        do (progn
             (unless *session* (initialize :timeout timeout))
             (let ((body (encode (nlk:json-object
                                  "jsonrpc" "2.0" "id" (next-id) "method" method
                                  "params" (or params (nlk:json-object))))))
               (multiple-value-bind (text status)
                   (post body :session *session* :timeout timeout)
                 (cond
                   ((and first (member status '(400 404)))
                    ;; MakeIRL forgot this session: start over once
                    (setf *session* nil))
                   ((= status 401)
                    (fail "MakeIRL refused the token (401); it may have expired: sign in again and set the section's token"))
                   ((/= status 200)
                    (fail "MakeIRL answered HTTP ~d~@[: ~a~]" status (clipped text)))
                   (t
                    (let ((value (answer-value text)))
                      (unless (hash-table-p value)
                        (fail "MakeIRL answered something that is not JSON: ~a" (clipped text)))
                      (let ((error (gethash "error" value)))
                        (when error
                          (fail "~a" (if (hash-table-p error)
                                         (gethash "message" error)
                                         error))))
                      (return (or (gethash "result" value) (nlk:json-object))))))))
             (setf first nil))))

;;; --- the surface --------------------------------------------------------------

(defun server-tools (&key (timeout (timeout-seconds)))
  "Every tool MakeIRL offers right now, each a decoded object, in its own order."
  (let ((tools (gethash "tools" (rpc "tools/list" :timeout timeout))))
    (cond ((vectorp tools) (coerce tools 'list))
          ((listp tools) tools)
          (t '()))))

(defun content-text (result)
  "RESULT's text content parts, joined by a newline."
  (let ((content (gethash "content" result)))
    (format nil "~{~a~^~%~}"
            (loop for part across (if (vectorp content) content #())
                  when (and (hash-table-p part) (equal (gethash "type" part) "text"))
                    collect (or (gethash "text" part) "")))))

(defun arguments-object (arguments)
  "ARGUMENTS as the JSON object a tools/call carries: a decoded object, an
alist, JSON text, or NIL for none."
  (cond ((null arguments) (nlk:json-object))
        ((hash-table-p arguments) arguments)
        ((stringp arguments)
         (let ((value (json-value-of arguments)))
           (if (hash-table-p value)
               value
               (fail "the arguments are not a JSON object: ~a" (clipped arguments)))))
        ((listp arguments)
         (let ((object (nlk:make-json-object)))
           (loop for (key . value) in arguments
                 do (setf (gethash (string-downcase (string key)) object) value))
           object))
        (t (fail "the arguments must be a JSON object"))))

(defun call (name &optional arguments)
  "Call MakeIRL tool NAME with ARGUMENTS (an object, an alist, or JSON text)
and answer its text; a result MakeIRL marked failed answers \"ERROR: ...\"."
  (let* ((result (rpc "tools/call" :params (nlk:json-object "name" name
                                                          "arguments" (arguments-object arguments))))
         (text (trimmed-text (content-text result))))
    (cond ((gethash "isError" result)
           (nle:failure (format nil "ERROR: MakeIRL ~a: ~a" name
                                 (or text "the call failed"))))
          (text text)
          ((hash-table-p (gethash "structuredContent" result))
           (encode (gethash "structuredContent" result)))
          (t (format nil "MakeIRL ~a: done" name)))))
