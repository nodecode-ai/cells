;;;; client.lisp --- the MCP wire: streamable HTTP, JSON-RPC, the tool table.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin carries no local code: its mcp.json names
;;;; https://mcp.lfg.gm and its skill teaches the tools the server lists. This
;;;; is the same connection made from the image -- JSON-RPC over MCP's
;;;; streamable HTTP transport (one POST per message, a JSON body or a
;;;; server-sent event stream in answer), the server's own tools/list turned
;;;; into Nodecode tools at start, and every call forwarded as tools/call, so
;;;; the model sees the server's own names, descriptions and argument schemas.
;;;;
;;;; Auth is the personal key the LFG Alarm app makes in its settings, sent as
;;;; Authorization: Bearer. The OAuth 2.1 flow the plugin's README walks a
;;;; person through needs a browser and a callback route; the key reaches the
;;;; same server, and this cell reads it from a variable or a file.

(in-package #:nodecode-lfg-alarm)

;;; --- constants -------------------------------------------------------------------

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client asks for; the server answers the one it speaks.")

(defparameter +client-version+ "0.1.0")

(defparameter +max-tool-pages+ 5
  "How many tools/list pages one connect follows before it stops.")

(defparameter +max-tools+ 200
  "How many tools one connect registers at most.")

(defparameter +note-chars+ 300
  "How much of a server's own error body a refusal quotes back.")

;;; --- the open connection, and what it listed ---------------------------------------

(defvar *session* nil
  "The open session: :ID (the server's Mcp-Session-Id) and :PROTOCOL-VERSION, or NIL.")

(defvar *remote-tools* '()
  "What the server last listed, in its order: (:NAME :DESCRIPTION :SCHEMA) plists.")

(defvar *registered* '()
  "The tool names this cell registered in the image, taken back by STOP.")

(defvar *request-id* 0
  "The JSON-RPC id of the last request sent; one more each time.")

;;; --- small readers ----------------------------------------------------------------

(defun clip-text (text &optional (limit +note-chars+))
  "TEXT trimmed and cut at LIMIT characters, or NIL when it says nothing."
  (let ((text (nlk:trimmed (or text ""))))
    (and (plusp (length text)) (nlk:clip text limit))))

(defun header-value (headers name)
  "HEADERS -- dexador's alist, or a hash table -- read case-insensitively."
  (cond ((hash-table-p headers)
         (loop for key being the hash-keys of headers using (hash-value value)
               when (and (stringp key) (string-equal key name)) return value))
        ((listp headers)
         (loop for entry in headers
               for key = (and (consp entry) (car entry))
               when (and key (string-equal (if (symbolp key) (symbol-name key) key) name))
                 return (cdr entry)))))

(defun json-of (text)
  "TEXT decoded, or NIL."
  (ignore-errors (nlk:decode-json (nlk:trimmed (or text "")))))

;;; --- envelopes and replies ---------------------------------------------------------

(defun json-envelope (id method params)
  "One JSON-RPC message; a notification (ID NIL) carries no id."
  (let ((object (nlk:json-object "jsonrpc" "2.0"
                                  "method" method
                                  "params" (or params (nlk:json-object)))))
    (when id (setf (gethash "id" object) id))
    object))

(defun sse-messages (text)
  "The JSON-RPC envelopes TEXT's server-sent events carry, one per event's
chained data: lines."
  (let ((messages '()) (data '()))
    (labels ((flush ()
               (when data
                 (let ((decoded (json-of (format nil "~{~a~%~}" (nreverse data)))))
                   (when (hash-table-p decoded) (push decoded messages)))
                 (setf data nil))))
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (let ((line (string-right-trim (list #\Return) line)))
          (cond ((zerop (length (nlk:trimmed line))) (flush))
                ((uiop:string-prefix-p "data:" line)
                 (push (string-left-trim " " (subseq line 5)) data)))))
      (flush))
    (nreverse messages)))

(defun json-messages (body)
  "BODY as the JSON-RPC envelopes it carries: one object, an array of them, or
an event stream's data lines."
  (let ((text (nlk:trimmed (or body ""))))
    (when (plusp (length text))
      (let ((decoded (json-of text)))
        (cond ((hash-table-p decoded) (list decoded))
              ((vectorp decoded) (coerce decoded 'list))
              (t (sse-messages text)))))))

(defun response-for (messages id)
  "MESSAGES' reply to ID, or the one reply that carries a result or an error."
  (or (and id (find-if (lambda (message)
                         (and (hash-table-p message) (equal id (gethash "id" message))))
                       messages))
      (find-if (lambda (message)
                 (and (hash-table-p message)
                      (or (gethash "result" message) (gethash "error" message))))
               messages)))

(defun unwrap (messages id status body)
  "The result of the reply to ID, or a refusal carrying the server's words."
  (let ((response (response-for messages id)))
    (cond ((null response)
           (fail "the server answered HTTP ~a with no reply to ~a~@[: ~a~]"
                 status (or id "the request") (clip-text body)))
          (t (let ((problem (gethash "error" response)))
               (when problem
                 (fail "~a" (or (and (hash-table-p problem) (gethash "message" problem))
                                 (and problem (ignore-errors (nlk:encode-json-object problem)))
                                 "the server refused the call")))
               (gethash "result" response))))))

(defun refusal-note (body)
  "The server's own words for a failed request, or the body as text."
  (let ((decoded (json-of body)))
    (cond ((and (hash-table-p decoded) (hash-table-p (gethash "error" decoded)))
           (or (gethash "message" (gethash "error" decoded)) ""))
          ((hash-table-p decoded)
           (or (gethash "error_description" decoded) (gethash "message" decoded) ""))
          (t (or body "")))))

;;; --- one exchange ----------------------------------------------------------------

(defun request-headers (token)
  "The headers every message carries: the key, what the transport accepts, and
what the session added to the handshake."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "authorization" (format nil "Bearer ~a" token)))
          (when (getf *session* :id)
            (list (cons "mcp-session-id" (getf *session* :id))))
          (when (getf *session* :protocol-version)
            (list (cons "mcp-protocol-version" (getf *session* :protocol-version))))))

(defun post (payload)
  "One POST to the server => (values BODY STATUS HEADERS). A dial that fails is
a refusal."
  (let ((url (setting :url))
        (token (setting :token))
        (timeout (setting :timeout)))
    (handler-case
        (multiple-value-bind (body status headers)
            (nlk:http :post url
                      :headers (request-headers token)
                      :content (nlk:encode-json-object payload)
                      :timeout timeout
                      :connect-timeout (min 10 timeout))
          (values (cond ((stringp body) body)
                        ((null body) "")
                        (t (or (nlk:body-text body) "")))
                  status headers))
      (error (condition)
        (fail "~a did not answer: ~a" url (nlk:one-line (princ-to-string condition)))))))

(defun notify (method &optional params)
  "One notification: no id, and no reply expected."
  (multiple-value-bind (body status) (post (json-envelope nil method params))
    (declare (ignore body))
    status))

(defun open-session ()
  "The handshake: initialize, keep what it named, then send the client's half."
  (setf *session* nil)
  (let* ((id (incf *request-id*))
         (payload (json-envelope id "initialize"
                                  (nlk:json-object
                                   "protocolVersion" +protocol-version+
                                   "capabilities" (nlk:json-object)
                                   "clientInfo" (nlk:json-object
                                                  "name" "nodecode-lfg-alarm"
                                                  "version" +client-version+)))))
    (multiple-value-bind (body status headers) (post payload)
      (when (and (integerp status) (>= status 400))
        (fail "connecting to ~a answered HTTP ~a~@[: ~a~]~a"
              (setting :url) status (clip-text (refusal-note body))
              (if (member status '(401 403))
                  " - check the key in the LFG Alarm app's settings"
                  "")))
      (let ((result (unwrap (json-messages body) id status body)))
        (setf *session* (list :id (header-value headers "mcp-session-id")
                              :protocol-version (or (and (hash-table-p result)
                                                         (gethash "protocolVersion" result))
                                                    +protocol-version+)))
        (ignore-errors (notify "notifications/initialized"))
        *session*))))

(defun rpc (method params &key (retry t))
  "METHOD's result, or a refusal; a session the server forgot is made again once."
  (unless *session* (open-session))
  (let* ((id (incf *request-id*))
         (payload (json-envelope id method params)))
    (multiple-value-bind (body status) (post payload)
      (cond ((and retry (getf *session* :id) (member status '(400 404)))
             (setf *session* nil)
             (rpc method params :retry nil))
            ((and (integerp status) (>= status 400))
             (fail "~a answered HTTP ~a~@[: ~a~]~a" method status (clip-text (refusal-note body))
                   (if (member status '(401 403))
                       " - check the key in the LFG Alarm app's settings"
                       "")))
            (t (unwrap (json-messages body) id status body))))))

;;; --- what the server offers -------------------------------------------------------

(defun list-remote-tools ()
  "Every tool the server lists, following its cursors => the tool plists."
  (let ((tools '()) (cursor nil) (pages 0))
    (loop
      (let* ((params (and cursor (nlk:json-object "cursor" cursor)))
             (result (or (rpc "tools/list" params) (nlk:json-object)))
             (page (nlk:json-array result "tools")))
        (when page
          (loop for tool across page
                when (hash-table-p tool) do (push tool tools)))
        (setf cursor (nlk:json-value result :string "nextCursor"))
        (incf pages)
        (when (or (null cursor) (>= pages +max-tool-pages+) (>= (length tools) +max-tools+))
          (return))))
    (setf *remote-tools*
          (loop for tool in (nreverse tools)
                for name = (nlk:json-value tool :string "name")
                when (and name (plusp (length name)))
                  collect (list :name name
                                :description (or (nlk:json-value tool :string "description") "")
                                :schema (nlk:json-value tool :object "inputSchema"))))
    *remote-tools*))

(defun connect ()
  "Open the session when none is open, and list what the server offers."
  (unless *session* (open-session))
  (list-remote-tools))

(defun remote-tools ()
  "What the last connect listed: (:NAME :DESCRIPTION :SCHEMA) plists."
  (copy-list *remote-tools*))

;;; --- the tools, as Nodecode tools ------------------------------------------------

(defun tool-name-for (remote)
  "The name the model sees: the server's own, unless another tool already
holds it, in which case LFG_ goes in front."
  (let ((taken (nle::find-tool remote)))
    (if (and taken (not (member remote *registered* :test #'string=)))
        (concatenate 'string "lfg_" remote)
        remote)))

(defun dispatch (remote arguments)
  "One model call, forwarded to the server as tools/call."
  (handler-case (call-remote-tool remote arguments)
    (lfg-alarm-error (condition)
      (nle:failure (format nil "ERROR: LFG Alarm: ~a" (lfg-alarm-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: LFG Alarm: ~a"
                           (nlk:one-line (princ-to-string condition)))))))

(defun register-remote-tools ()
  "Connect, then register each tool the server lists => their names in the image."
  (connect)
  (let ((names '()))
    (dolist (tool *remote-tools*)
      (let* ((remote (getf tool :name))
             (name (tool-name-for remote))
             (schema (or (getf tool :schema)
                         (nlk:json-object "type" "object"
                                           "properties" (nlk:json-object))))
             (description (getf tool :description)))
        (nle:register-tool name
                            (list :description (if (and description (plusp (length description)))
                                                   description
                                                   (format nil "the LFG Alarm server's ~a tool" remote))
                                  :parameters schema)
                            (let ((remote remote))
                              (lambda (arguments) (dispatch remote arguments))))
        (pushnew name *registered* :test #'string=)
        (push name names)))
    (nreverse names)))

(defun unregister-remote-tools ()
  "Take back every tool this cell registered, and forget the session."
  (dolist (name *registered*) (ignore-errors (nle:unregister-tool name)))
  (setf *registered* '() *session* nil *remote-tools* '()))

;;; --- calling one ---------------------------------------------------------------

(defun plist-json (plist)
  "A plist as the JSON object the wire takes."
  (let ((object (nlk:json-object)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash (string-downcase (string key)) object)
                   (if (and (listp value) (keywordp (first value))) (plist-json value) value)))
    object))

(defun tool-text (result)
  "RESULT's content as the model reads it: its text blocks, capped; a tool's
own error is the call's failure."
  (let* ((blocks (or (nlk:json-array result "content") #()))
         (texts (loop for block across blocks
                      for text = (nlk:json-value block :string "text")
                      when (and text (plusp (length text))) collect text))
         (structured (nlk:json-value result :object "structuredContent"))
         (text (cond (texts (format nil "~{~a~%~}}" texts))
                     (structured (nlk:encode-json-object structured))
                     (t "")))
         (cap (setting :max-output))
         (failed (nlk:json-value result :boolean "isError")))
    (when (and cap (> (length text) cap))
      (setf text (concatenate 'string (subseq text 0 cap)
                              (format nil "~%[cut at ~a characters]" cap))))
    (cond ((and failed (plusp (length text)))
           (nle:failure (format nil "ERROR: ~a" text)))
          ((plusp (length text)) text)
          (t "the tool answered nothing"))))

(defun call-remote-tool (remote arguments)
  "REMOTE with ARGUMENTS -- a decoded table, or a plist -- as the server's own answer."
  (let* ((arguments (cond ((hash-table-p arguments) arguments)
                          ((listp arguments) (plist-json arguments))
                          (t (nlk:json-object))))
         (result (rpc "tools/call"
                      (nlk:json-object "name" remote "arguments" arguments))))
    (tool-text (or result (nlk:json-object)))))
