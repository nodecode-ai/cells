;;;; client.lisp --- Mermail's hosted MCP server, spoken to directly.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Mermail (https://mermail.app) hosts an MCP server at
;;;; https://console.mermail.app/mcp: Streamable HTTP, JSON-RPC 2.0, an
;;;; x-api-key header (or an OAuth grant, which this cell does not do). The
;;;; original plugin carries no client code at all -- it is manifests and
;;;; skills that hand the endpoint to whatever MCP host loads it. This file
;;;; is the client: initialize, the initialized notification, tools/call and
;;;; tools/list, and the session header the server hands back.
;;;;
;;;; Every exchange goes through *HTTP* (NLK:HTTP by default), so a test
;;;; binds that one name and no socket is opened. A refused status is turned
;;;; into one refusal naming the status class the original's troubleshooting
;;;; page documents: unauthorized, insufficient scope, credits exhausted,
;;;; rate limited.
;;;;
;;;; An answer may come back as JSON or as server-sent events; both are read
;;;; here. An MCP session expires on the server's side -- it answers 404 or
;;;; 410 -- and a fresh initialize is all it takes: the refused call never
;;;; ran, so it is sent once more, never a replay of a call the server took.

(in-package #:nodecode-mermail)

;;; --- where and with what ---------------------------------------------------

(defparameter +endpoint+ "https://console.mermail.app/mcp"
  "Mermail's hosted MCP endpoint: the one the original's manifests name.")

(defparameter +console+ "https://console.mermail.app/"
  "Where a workspace's API key is made.")

(defparameter +env+ "MERMAIL_API_KEY"
  "The environment variable an API key is read from when the section has none.")

(defparameter +protocol+ "2025-06-18"
  "The MCP revision this client asks for; the server's own answer wins.")

(defparameter +client+ "nodecode"
  "The client name this cell announces in initialize.")

(defparameter +version+ "0.1.0"
  "This cell's version, announced beside the client name.")

(defvar *http* #'nlk:http
  "The cell's one network seam: NLK:HTTP's own argument list, answering
(values BODY STATUS HEADERS TRUNCATED-P). A test binds this name.")

(defun http (method url &key headers content timeout)
  "One exchange through *HTTP*, connect and read alike under TIMEOUT."
  (funcall *http* method url :headers headers :content content
                    :timeout timeout :connect-timeout (min 10 timeout)))

(defun setting-value (key)
  "KEY off the running settings, or NIL while the cell is not started: what a
refusal needs to say before any declaration has been read."
  (getf (ignore-errors (running-settings)) key))

(defun api-key ()
  "The key every request carries: the section's, else MERMAIL_API_KEY."
  (or (setting-value :api-key) (nle::credential-env +env+)))

(defun key-list ()
  "Every key this cell would send: what a refusal's text is cleaned of."
  (remove-duplicates
   (remove nil (list (setting-value :api-key) (nle::credential-env +env+)))
   :test #'string=))

(defun redact (text)
  "TEXT with this cell's keys taken out of it."
  (let ((keys (remove-if (lambda (key) (< (length key) 8)) (key-list))))
    (if (and (stringp text) keys)
        (uiop:frob-substrings text keys "[redacted]")
        text)))

(defun require-key ()
  "The key, or the refusal naming both places one is put."
  (or (api-key)
      (fail "no Mermail API key: make one in Mermail's workspace settings at ~a, then put it in ~
mermail.api_key in ~~/.nodecode/config.jsonc or export ~a before Nodecode starts"
            +console+ +env+)))

(defun endpoint ()
  "The MCP endpoint the section names, Mermail's own when it names none, with
the agent-inbox profile as its query when that profile is chosen."
  (let ((base (string-right-trim "/" (or (setting-value :base-url) ""))))
    (when (zerop (length base))
      (setf base +endpoint+))
    (if (equal (setting-value :profile) "agent-inbox")
        (format nil "~a~:[?~;&~]profile=agent-inbox" base (search "?" base))
        base)))

;;; --- one exchange ----------------------------------------------------------

(defstruct (mcp-session (:conc-name mcp-session-))
  "A live MCP session: the id the server handed back, and the protocol
revision it agreed to, both echoed on every later request."
  (id nil)
  (protocol nil))

(defvar *session* nil
  "The session this cell is talking through, or NIL before the first call.")
(defvar *request-id* 0
  "The JSON-RPC id of the last request: one more each time.")

(defun next-id ()
  (incf *request-id*))

(defun header (headers name)
  "NAME's value in HEADERS, a hash table or an alist, case-insensitively."
  (cond ((hash-table-p headers)
         (or (gethash name headers)
             (gethash (string-downcase name) headers)
             (loop for key being the hash-keys of headers
                   when (string-equal key name) return (gethash key headers))))
        ((listp headers)
         (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun request-headers (session)
  "What every request carries: the key, the accept the protocol wants, and
the session and revision once there is one."
  (list* (cons "content-type" "application/json")
         (cons "accept" "application/json, text/event-stream")
         (append (let ((key (api-key)))
                   (and key (plusp (length key)) (list (cons "x-api-key" key))))
                 (and session (mcp-session-id session)
                      (list (cons "mcp-session-id" (mcp-session-id session))))
                 (and session (mcp-session-protocol session)
                      (list (cons "mcp-protocol-version" (mcp-session-protocol session)))))))

(defun rpc-body (id method params)
  "One JSON-RPC message: a request when ID is given, a notification when not."
  (nlk:json-object "jsonrpc" "2.0"
                    :when id "id" id
                    "method" method
                    :opt "params" params))

(defun trim-to (text limit)
  "TEXT trimmed, cut at LIMIT characters with an ellipsis, or NIL."
  (when (stringp text)
    (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
      (cond ((zerop (length text)) nil)
            ((> (length text) limit)
             (concatenate 'string (subseq text 0 limit) "..."))
            (t text)))))

(defun sse-events (text)
  "The data of every event in TEXT, one string each."
  (let ((events '())
        (lines '()))
    (flet ((flush ()
             (when lines
               (push (format nil "~{~a~^~%~}" (nreverse lines)) events)
               (setf lines '()))))
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (cond ((zerop (length line)) (flush))
              ((uiop:string-prefix-p "data:" line)
               (push (string-left-trim " " (subseq line 5)) lines))))
      (flush))
    (nreverse events)))

(defun decode-text (text)
  "TEXT as JSON, or NIL when it is not."
  (ignore-errors (nlk:decode-json text :whole t)))

(defun response-object (text id)
  "The JSON-RPC answer carrying ID in TEXT, plain JSON or server-sent events."
  (let ((text (or text "")))
    (cond ((zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) text))) nil)
          ((or (uiop:string-prefix-p "data:" text)
               (uiop:string-prefix-p "event:" text)
               (search "data:" text))
           (loop for payload in (sse-events text)
                 for object = (decode-text payload)
                 when (and (hash-table-p object) (eql id (gethash "id" object)))
                   return object))
          (t (decode-text text)))))

(defun status-detail (status text)
  "What a refused STATUS means, in the original's own classes, with the body
the server sent when it sent one."
  (format nil "~a~@[ - ~a~]"
          (case status
            (400 "the request was refused (400)")
            (401 (format nil "the API key was refused (401), unauthorized: check it, or make a new one at ~a"
                         +console+))
            (402 "the workspace is out of credits (402)")
            (403 "the key is not allowed to do that (403), insufficient scope")
            (404 "not found (404)")
            (410 "the MCP session is gone (410)")
            (422 "the arguments did not fit the live tool schema (422)")
            (429 "rate limited (429): wait for the window and try again")
            (t (format nil "the server answered HTTP ~d" status)))
          (trim-to text 300)))

(defun post (method params &key id session)
  "One JSON-RPC POST => (values OBJECT STATUS TEXT HEADERS); OBJECT only for
a request, the answer carrying its id."
  (let ((timeout (or (setting-value :timeout-seconds) 30)))
    (multiple-value-bind (text status headers)
        (http :post (endpoint) :headers (request-headers session)
              :content (nlk:encode-json-object (rpc-body id method params))
              :timeout timeout)
      (values (and id (response-object text id)) status text headers))))

;;; --- the session -----------------------------------------------------------

(defun initialize-session ()
  "initialize, then the notification the protocol wants after it."
  (multiple-value-bind (object status text headers)
      (post "initialize"
            (nlk:json-object "protocolVersion" +protocol+
                              "capabilities" (nlk:make-json-object)
                              "clientInfo" (nlk:json-object "name" +client+
                                                             "version" +version+))
            :id (next-id))
    (unless (and (<= 200 status 299) object)
      (fail "mermail: ~a" (status-detail status text)))
    (let ((session (make-mcp-session
                    :id (header headers "mcp-session-id")
                    :protocol (nlk:json-value object :string "result" "protocolVersion"))))
      ;; The protocol's next step, and a server that does not track sessions
      ;; answers it 202 with nothing; a notice that fails changes no answer.
      (ignore-errors (post "notifications/initialized" nil :session session))
      session)))

(defun ensure-session ()
  "The session to talk through, made on first use."
  (or *session*
      (setf *session* (initialize-session))))

(defun catalog ()
  "Every tool the selected profile exposes, as the server lists them."
  (let ((session (ensure-session)))
    (multiple-value-bind (object status text)
        (post "tools/list" (nlk:make-json-object) :id (next-id) :session session)
      (unless (<= 200 status 299)
        (fail "mermail: ~a" (status-detail status text)))
      (nlk:json-array object "result" "tools"))))

;;; --- one tool call ---------------------------------------------------------

(defun content-text (content)
  "The text of every content item in CONTENT, one a line."
  (format nil "~{~a~^~%~}"
          (loop for item across (or content #())
                for text = (nlk:json-value item :string "text")
                when text collect text)))

(defun answer-text (object status text)
  "What a tools/call answer says to the model: its text, ERROR: first when the
tool itself failed, else its structured content as JSON."
  (unless (<= 200 status 299)
    (fail "mermail: ~a" (status-detail status text)))
  (let ((error (nlk:json-value object :object "error")))
    (when error
      (fail "mermail: ~a" (or (nlk:json-value error :string "message")
                               "the server answered an error"))))
  (let* ((result (nlk:json-value object :object "result"))
         (body (content-text (nlk:json-value result :array "content"))))
    (cond ((plusp (length body))
           (if (nlk:json-value result :boolean "isError")
               (concatenate 'string "ERROR: " body)
               body))
          ((nlk:json-value result :object "structuredContent")
           (nlk:encode-json-object (nlk:json-value result :object "structuredContent")))
          (result (nlk:encode-json-object result))
          (t (or (trim-to text 400) "")))))

(defun call-tool (name arguments)
  "MCP tools/call of NAME with ARGUMENTS, a JSON object => the text the model
reads. A session the server forgot is remade once: the refused call never
ran, so it is sent again."
  (require-key)
  (with-mermail-lock
    (let ((session (ensure-session)))
      (multiple-value-bind (object status text)
          (post "tools/call" (nlk:json-object "name" name :opt "arguments" arguments)
                :id (next-id) :session session)
        (when (member status '(404 410))
          (setf *session* nil
                session (ensure-session))
          (multiple-value-setq (object status text)
            (post "tools/call" (nlk:json-object "name" name :opt "arguments" arguments)
                  :id (next-id) :session session)))
        (answer-text object status text)))))
