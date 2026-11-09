;;;; wire.lisp --- the MCP client: JSON-RPC over the streamable HTTP transport.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Markifact's server speaks the MCP streamable-HTTP transport: one POST per
;;;; message to a single endpoint, a JSON answer or a server-sent stream, and a
;;;; Mcp-Session-Id the server issues at initialize. This file is that
;;;; transport and nothing about advertising: INITIALIZE, TOOLS/LIST and
;;;; TOOLS/CALL, the eight MCP tool names, and the text a result carries.
;;;;
;;;; The access token comes from the markifact section (access_token, or the
;;;; variable token_env names), else from the sign-in /markifact keeps in
;;;; auth.json under oauth_tokens.markifact, refreshed when it has run out.

(in-package #:nodecode-markifact)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client asks for; the server answers the one it speaks.")

(defparameter +client-version+ "0.1.0")

(defvar *session-id* nil
  "The Mcp-Session-Id INITIALIZE answered with, or NIL before one has.")

(defvar *initialized* nil
  "Whether this image has initialized its session; the id may be absent.")

(defvar *request-id* 0
  "The last JSON-RPC id sent; one per request, so an answer can be matched.")

(defvar *session-lock* (bt2:make-lock :name "nodecode-markifact session"))

(defvar *refreshed* nil
  "Whether the exchange in flight already spent its one token refresh.")

;;; --- bytes, headers, one exchange ------------------------------------------------

(defun endpoint ()
  "The endpoint the section names."
  (let ((value (nlk:trimmed (or (setting :endpoint) ""))))
    (if (plusp (length value)) value +default-endpoint+)))

(defun body-text (body)
  "BODY as dexador answered it, as a string."
  (typecase body
    (null "")
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (t (princ-to-string body))))

(defun exchange (method url &key headers content)
  "(values TEXT STATUS HEADERS) of one request; a refused status is a value,
 not an error, so the caller can read the server's own words."
  (multiple-value-bind (body status response-headers)
      (nlk:http method url :headers headers :content content :timeout (setting :timeout))
    (values (body-text body) status response-headers)))

(defun header (headers name)
  "NAME's value in HEADERS -- a hash table or an alist -- or NIL."
  (cond ((null headers) nil)
        ((hash-table-p headers)
         (or (gethash (string-downcase name) headers)
             (gethash name headers)
             (loop for key being the hash-keys of headers using (hash-value value)
                   when (string-equal key name) return value)))
        ((consp headers)
         (cdr (assoc name headers :test #'string-equal)))))

(defun excerpt (text)
  "TEXT's first 300 characters, as a refused answer is quoted."
  (let ((text (nlk:trimmed (or text ""))))
    (subseq text 0 (min 300 (length text)))))

(defun truncate-text (text limit)
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a~%... (~d more characters)" (subseq text 0 limit) (- (length text) limit)))))

;;; --- the token ------------------------------------------------------------------

(defun kept-token ()
  "The sign-in's entry in auth.json, or NIL."
  (ignore-errors
   (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" +provider+)))

(defun entry-string (entry key)
  (let ((value (and entry (gethash key entry))))
    (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))

(defun entry-integer (entry key)
  (let ((value (and entry (gethash key entry))))
    (and (integerp value) value)))

(defun save-token (&key access refresh expires-at client-id token-endpoint resource)
  "Write the sign-in into the shared auth.json, every other field kept; no
 ACCESS forgets it. Mode 0600."
  (nle::update-auth-file
   (lambda (auth)
     (let ((tokens (or (gethash "oauth_tokens" auth)
                       (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
       (if (null access)
           (remhash +provider+ tokens)
           (let ((entry (or (gethash +provider+ tokens)
                            (setf (gethash +provider+ tokens) (make-hash-table :test #'equal)))))
             (setf (gethash "provider" entry) +provider+
                   (gethash "access_token" entry) access)
             (loop for (key . value) in (list (cons "refresh_token" refresh)
                                              (cons "expires_at" expires-at)
                                              (cons "client_id" client-id)
                                              (cons "token_endpoint" token-endpoint)
                                              (cons "resource" resource))
                   do (if value
                          (setf (gethash key entry) value)
                          (remhash key entry)))))))))

(defun token-expired-p (entry)
  "Whether ENTRY's access token has run out, or is about to."
  (let ((at (entry-integer entry "expires_at")))
    (and at (< at (+ (get-universal-time) 60)))))

(defun env-token ()
  "The access token the section's token_env names, when the variable is set."
  (let ((name (nlk:trimmed (or (setting :token-env) ""))))
    (and (plusp (length name))
         (let ((value (nle::credential-env name)))
           (and value (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))))

(defun access-token ()
  "The bearer this request carries, or a refusal naming /markifact login."
  (or (let ((inline (nlk:trimmed (or (setting :access-token) ""))))
        (and (plusp (length inline)) inline))
      (env-token)
      (let ((entry (kept-token)))
        (cond ((null entry) nil)
              ((token-expired-p entry) (refresh-token entry))
              (t (entry-string entry "access_token"))))
      (fail "not signed in to Markifact: run /markifact login, or set the markifact section's ~
access_token or token_env")))

;;; --- JSON-RPC -------------------------------------------------------------------

(defun request-headers ()
  (append (list (cons "Content-Type" "application/json")
                (cons "Accept" "application/json, text/event-stream")
                (cons "MCP-Protocol-Version" +protocol-version+)
                (cons "User-Agent" (format nil "Nodecode/~a" +client-version+))
                (cons "Authorization" (format nil "Bearer ~a" (access-token))))
          (when *session-id*
            (list (cons "Mcp-Session-Id" *session-id*)))))

(defun sse-events (text)
  "TEXT's server-sent events, each a decoded JSON object, in order."
  (let ((events '())
        (data '()))
    (dolist (line (uiop:split-string text :separator '(#\Newline)) (nreverse events))
      (let ((line (string-right-trim '(#\Return) line)))
        (cond ((zerop (length line))
               (when data
                 (let ((object (ignore-errors
                                (nlk:decode-json (format nil "~{~a~^~%~}" (nreverse data)))))))
                   (setf data '())
                   (when object (push object events))))
              ((uiop:string-prefix-p "data:" line)
               (push (nlk:trimmed (subseq line 5)) data)))))))

(defun rpc-content (id method params)
  (nlk:encode-json-object
   (if id
       (nlk:json-object "jsonrpc" "2.0" "id" id "method" method "params" params)
       (nlk:json-object "jsonrpc" "2.0" "method" method "params" params))))

(defun post (id method params)
  "(values RESULT OBJECT HEADERS) of one JSON-RPC message; ID NIL is a
 notification, which answers no result. A 401 spends the one refresh and
 asks again; a JSON-RPC error is FAIL, carrying the server's own message."
  (multiple-value-bind (text status headers)
      (exchange :post (endpoint) :headers (request-headers)
                               :content (rpc-content id method params))
    (when (and (= status 401) (not *refreshed*) (kept-token) (refresh-token (kept-token)))
      (setf *refreshed* t)
      (unwind-protect (post id method params)
        (setf *refreshed* nil)))
    (when (null id)
      (unless (< status 300)
        (fail "Markifact ~a answered ~a~@[: ~a~]" method status (excerpt text)))
      (return-from post (values nil nil headers)))
    (let* ((objects (if (search "data:" text)
                        (sse-events text)
                        (let ((one (and (plusp (length text)) (ignore-errors (nlk:decode-json text)))))
                          (and one (list one)))))
           (response (or (find id objects :key (lambda (object) (gethash "id" object)) :test #'eql)
                         (and (= 1 (length objects)) (first objects)))))
      (cond ((null response)
             (fail "Markifact ~a answered ~a with nothing this client could read~@[: ~a~]"
                   method status (excerpt text)))
            ((gethash "error" response)
             (let ((error (gethash "error" response)))
               (fail "Markifact ~a refused: ~a" method
                     (or (and (hash-table-p error) (gethash "message" error))
                         (ignore-errors (nlk:pretty-json error)) "unknown error"))))
            (t (values (gethash "result" response) response headers))))))

(defun ensure-session ()
  "Initialize this client's session once, and keep its id."
  (bt2:with-lock-held (*session-lock*)
    (unless *initialized*
      (multiple-value-bind (result object headers)
          (post (incf *request-id*) "initialize"
                (nlk:json-object "protocolVersion" +protocol-version+
                                 "capabilities" (nlk:make-json-object)
                                 "clientInfo" (nlk:json-object "name" "Nodecode"
                                                                "version" +client-version+)))
        (declare (ignore result object))
        (setf *session-id* (header headers "mcp-session-id")
              *initialized* t)
        (post nil "notifications/initialized" (nlk:make-json-object))))
    *session-id*))

;;; --- the tools -------------------------------------------------------------------

(defun result-text (result)
  "RESULT's answer as text: its content blocks, else the result itself."
  (let ((content (and result (nlk:json-value result :array "content"))))
    (cond ((and content (plusp (length content)))
           (format nil "~{~a~^~%~}"
                   (loop for block across content
                         for text = (nlk:json-value block :string "text")
                         collect (or text (ignore-errors (nlk:pretty-json block)) ""))))
          (result (ignore-errors (nlk:pretty-json result)))
          (t ""))))

(defun call-tool (name arguments)
  "Call MCP tool NAME -- one of the eight Markifact exposes -- with
 ARGUMENTS, a JSON object, and answer its text. This is the model's one
 verb; (tools) says what NAME may be and what ARGUMENTS holds."
  (running-settings)
  (ensure-session)
  (let ((result (post (incf *request-id*) "tools/call"
                      (nlk:json-object "name" name
                                       "arguments" (or arguments (nlk:make-json-object))))))
    (let ((text (truncate-text (result-text result) (setting :max-chars))))
      (if (nlk:json-value result :boolean "isError")
          (fail "Markifact ~a answered an error: ~a" name text)
          text))))

(defun tools (&optional filter)
  "What the server exposes: one line per tool, its purpose, and the input
 schema its arguments must follow; FILTER keeps the ones that mention it."
  (running-settings)
  (ensure-session)
  (let* ((result (post (incf *request-id*) "tools/list" (nlk:make-json-object)))
         (listed (nlk:json-value result :array "tools"))
         (words (and filter (plusp (length (nlk:trimmed filter))) (nlk:trimmed filter))))
    (cond ((or (null listed) (zerop (length listed))) "Markifact listed no tools.")
          (t (let ((lines (loop for tool across listed
                                when (or (null words)
                                         (search (string-downcase words)
                                                 (string-downcase (format nil "~a ~a"
                                                                          (nlk:json-value tool :string "name")
                                                                          (or (nlk:json-value tool :string "description") "")))))
                                  collect (with-output-to-string (out) (tool-line out tool)))))
               (format nil "~{~a~^~%~}" lines))))))

(defun tool-line (stream tool)
  (format stream "~a~@[~%  ~a~]~@[~%  arguments: ~a~]"
          (nlk:json-value tool :string "name")
          (let ((description (nlk:json-value tool :string "description")))
            (and description (truncate-text (nlk:trimmed description) 400)))
          (let ((schema (nlk:json-value tool :object "inputSchema")))
            (and schema (ignore-errors (nlk:pretty-json schema))))))
