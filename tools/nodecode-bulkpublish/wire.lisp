;;;; wire.lisp --- MCP over streamable HTTP: one POST a frame, one message back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin is a manifest, an MCP server entry and prose skills: its
;;;; eighty-odd tools are the hosted server's, reached at one URL with the
;;;; user's own key as a bearer token. MCP speaks JSON-RPC 2.0, and its
;;;; streamable HTTP transport answers a request's POST with either one JSON
;;;; message or an SSE body whose data: lines carry it, and issues an
;;;; Mcp-Session-Id header on initialize that every later request echoes.
;;;; This file is that much of the protocol and no more: initialize,
;;;; tools/list, tools/call, and the text a result carries. The key is a
;;;; header and never a word in a condition -- a refused status reports the
;;;; code, what the server said clipped, and nothing of the credential.

(in-package #:nodecode-bulkpublish)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client asks for; the server may answer its own.")

(defparameter +client-name+ "nodecode-bulkpublish"
  "What initialize tells the server this client is.")

(defparameter +client-version+ "0.1.0"
  "The client's version, beside its name in initialize.")

(defparameter +frame-limit+ (* 1024 1024)
  "Largest frame posted, and the most of one answer that is read, in characters.")

(defparameter +max-pages+ 8
  "tools/list pages followed before the catalogue is taken as complete.")

(defparameter +text-limit+ 6000
  "How much of one tool result a call answers with; past it, clipped.")

(defvar *http* #'nlk:http
  "The exchange every call dials, called as NLK:HTTP is: (METHOD URL &KEY
HEADERS CONTENT TIMEOUT) => (VALUES BODY STATUS HEADERS). A test binds its
stub here; nothing else changes.")

(defvar *session* nil
  "The open MCP session: a plist of :endpoint, :key, :id, :protocol,
:next-id and :tools. NIL reopens one at the next call.")

;;; --- where, and with what -----------------------------------------------------

(defun url-host (url)
  "URL's host, for a refusal that says who failed; URL itself when it carries none."
  (or (ppcre:register-groups-bind (host) ("^[a-zA-Z]+://([^/?#]+)" url) host) url))

(defun endpoint ()
  "The MCP endpoint the section names, without its trailing slash."
  (let ((endpoint (nlk:trimmed (or (setting :endpoint) ""))))
    (if (plusp (length endpoint))
        (string-right-trim "/" endpoint)
        (fail "bulkpublish.endpoint is empty: it is the MCP URL, https://mcp.bulkpublish.com/mcp/full unless a proxy stands in the way"))))

(defun api-key ()
  "The BulkPublish key: the one the section holds, else the environment
variable the section names. NIL when neither is set."
  (or (let ((key (setting :api-key)))
        (and key (plusp (length (nlk:trimmed key))) (nlk:trimmed key)))
      (let ((name (nlk:trimmed (or (setting :api-key-env) ""))))
        (and (plusp (length name))
             (let ((value (uiop:getenv name)))
               (and value (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))))))

(defun key-or-refusal ()
  "The key, or a refusal naming both ways to give one."
  (or (api-key)
      (fail "no BulkPublish key: set bulkpublish.api_key in the config, or export the variable bulkpublish.api_key_env names (~a); a key is made at https://app.bulkpublish.com/developer"
            (let ((name (nlk:trimmed (or (setting :api-key-env) ""))))
              (if (plusp (length name)) name "BULKPUBLISH_API_KEY")))))

(defun header (headers name)
  "NAME's value from a response's header table; NIL when it carries none."
  (when (hash-table-p headers)
    (or (gethash name headers)
        (loop for key being the hash-keys of headers
              when (string-equal key name)
                do (return (gethash key headers))))))

(defun request-headers (session)
  "The headers of a request on SESSION: its key, its session when it has one."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream"))
          (let ((id (getf session :id)))
            (and id (list (cons "mcp-session-id" id))))
          (let ((protocol (getf session :protocol)))
            (and protocol (list (cons "mcp-protocol-version" protocol))))
          (list (cons "authorization" (format nil "Bearer ~a" (key-or-refusal))))))

(defun note-session (session headers)
  "Keep the Mcp-Session-Id a response issues, so the next request echoes it."
  (let ((id (header headers "mcp-session-id")))
    (when (and (stringp id) (plusp (length id)))
      (setf (getf session :id) id))))

;;; --- one exchange --------------------------------------------------------------

(defun post-frame (frame session)
  "POST one JSON-RPC FRAME on SESSION. => (values BODY STATUS HEADERS)."
  (let ((text (nlk:encode-json-object frame)))
    (when (> (length text) +frame-limit+)
      (fail "one message of ~:d characters is past the ~:d the server takes"
            (length text) +frame-limit+))
    (handler-case
        (funcall *http* :post (endpoint)
                        :headers (request-headers session)
                        :content text
                        :timeout (setting :timeout))
      (error (condition)
        (fail "~a did not answer: ~a" (url-host (endpoint))
              (nlk:one-line (princ-to-string condition) :cap 200))))))

(defun object-of (text)
  "TEXT as one JSON object, or NIL when it is not one."
  (let ((value (and (stringp text) (plusp (length text))
                    (handler-case (nlk:decode-json text) (error () nil)))))
    (and (hash-table-p value) value)))

(defun messages-of (body headers)
  "The JSON-RPC messages BODY carries: one object, a batch, or the data: lines
of an SSE body. An answer that carries none is the empty list."
  (let ((text (typecase body (string body) (t (and body (princ-to-string body)))))
        (type (or (header headers "content-type") "")))
    (cond
      ((null text) '())
      ((uiop:string-prefix-p "text/event-stream" type)
       (loop with objects = '()
             for line in (uiop:split-string text :separator '(#\Newline))
             for payload = (string-left-trim '(#\Space #\Tab #\Return) line)
             when (uiop:string-prefix-p "data:" payload)
               do (let ((object (object-of (string-left-trim '(#\Space #\Tab)
                                                             (subseq payload 5)))))
                    (when object (push object objects)))
             finally (return (nreverse objects))))
      (t (let ((value (handler-case (nlk:decode-json text) (error () nil))))
           (cond ((hash-table-p value) (list value))
                 ((vectorp value)
                  (loop for entry across value when (hash-table-p entry) collect entry))
                 (t '())))))))

(defun reply-for (messages id)
  "The message in MESSAGES answering request ID."
  (find-if (lambda (message)
             (let ((theirs (gethash "id" message)))
               (and theirs (equal theirs id))))
           messages))

(defun check-reply (messages id method)
  "METHOD's result from MESSAGES, or a refusal naming what the server said."
  (let ((reply (reply-for messages id)))
    (unless reply
      (fail "~a answered ~a without a reply" (url-host (endpoint)) method))
    (let ((error (gethash "error" reply)))
      (when (hash-table-p error)
        (fail "~a refused ~a: ~a" (url-host (endpoint)) method
              (nlk:one-line (or (nlk:json-value error :string "message") "no message")
                            :cap 300))))
    (gethash "result" reply)))

(defun call-on (session method params &key (retry t))
  "METHOD's result over SESSION. A session the server has forgotten is opened
again, once; anything else is a refusal the model reads."
  (let* ((id (incf (getf session :next-id)))
         (frame (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                                  "params" (or params (nlk:make-json-object)))))
    (multiple-value-bind (body status headers) (post-frame frame session)
      (note-session session headers)
      (cond
        ((member status '(404 410))
         (when (and (getf session :id) retry)
           (setf *session* nil)
           (return-from call-on (call-on (ensure-session) method params :retry nil)))
         (fail "~a answered HTTP ~a for ~a" (url-host (endpoint)) status method))
        ((not (and (integerp status) (<= 200 status 299)))
         (fail "~a answered HTTP ~a for ~a~@[: ~a~]" (url-host (endpoint)) status method
               (and (stringp body) (plusp (length body))
                    (nlk:one-line (nlk:clip body 200 :ellipsis "...") :cap 200))))
        (t (check-reply (messages-of body headers) id method))))))

(defun notify-on (session method)
  "Say METHOD, which takes no answer; a status outside 2xx is a refusal."
  (multiple-value-bind (body status headers)
      (post-frame (nlk:json-object "jsonrpc" "2.0" "method" method) session)
    (declare (ignore body))
    (note-session session headers)
    (unless (and (integerp status) (<= 200 status 299))
      (fail "~a answered HTTP ~a for ~a" (url-host (endpoint)) status method))))

;;; --- the session ---------------------------------------------------------------

(defun initialize-params ()
  "What initialize asks for: this revision, no capabilities, who is calling."
  (nlk:json-object "protocolVersion" +protocol-version+
                    "capabilities" (nlk:make-json-object)
                    "clientInfo" (nlk:json-object "name" +client-name+
                                                   "version" +client-version+)))

(defun open-session ()
  "Open a session: initialize, then the initialized notification."
  (let ((session (list :endpoint (endpoint) :key (key-or-refusal) :next-id 0
                       :id nil :protocol nil :tools nil)))
    (setf *session* session)
    (let ((result (call-on session "initialize" (initialize-params) :retry nil)))
      (setf (getf session :protocol)
            (or (nlk:json-value result :string "protocolVersion") +protocol-version+))
      (notify-on session "notifications/initialized")
      session)))

(defun ensure-session ()
  "The session, opened or reopened when the section now names another endpoint or key."
  (let ((live (and *session*
                    (equal (getf *session* :endpoint) (endpoint))
                    (equal (getf *session* :key) (api-key)))))
    (if live *session* (open-session))))

(defun tool-catalog (&key refresh)
  "The server's tools as a list of JSON objects, read once a session and kept
on it; tools/list pages are followed up to +MAX-PAGES+."
  (let ((session (ensure-session)))
    (if (and (not refresh) (getf session :tools))
        (getf session :tools)
        (setf (getf session :tools)
              (loop with tools = '() and cursor = nil and page = 0
                    do (let ((result (call-on session "tools/list"
                                               (if cursor
                                                   (nlk:json-object "cursor" cursor)
                                                   (nlk:make-json-object)))))
                         (loop for tool across (or (nlk:json-array (gethash "tools" result)) #())
                               when (hash-table-p tool) do (push tool tools))
                         (setf cursor (nlk:json-value result :string "nextCursor"))
                         (incf page)
                         (unless (and cursor (< page +max-pages+))
                           (return (nreverse tools)))))))))

;;; --- a tool's answer -----------------------------------------------------------

(defun result-text (result)
  "The text RESULT carries, or its whole object when it carries none."
  (let ((parts (loop for part across (or (nlk:json-array (gethash "content" result)) #())
                     when (and (hash-table-p part)
                               (equal "text" (nlk:json-value part :string "type")))
                       collect (or (nlk:json-value part :string "text") ""))))
    (cond ((plusp (length parts))
           (nlk:clip (format nil "~{~a~^~%~}" parts) +text-limit+ :ellipsis "..."))
          ((and (hash-table-p result) (plusp (hash-table-count result)))
           (nlk:clip (nlk:encode-json-object result) +text-limit+ :ellipsis "..."))
          (t "(the tool answered nothing)"))))

(defun call-tool (name arguments)
  "Call the server's tool NAME with ARGUMENTS, a JSON object or NIL; answers
the text its result carries, and a result the server marks as an error is a
refusal carrying that text."
  (let* ((result (call-on (ensure-session) "tools/call"
                           (nlk:json-object "name" name
                                             "arguments" (or arguments (nlk:make-json-object)))))
         (text (result-text result)))
    (if (nlk:json-value result :boolean "isError")
        (fail "~a failed: ~a" name text)
        text)))
