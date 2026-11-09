;;;; cell.lisp --- Nodecode as an MCP server, for other agents.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Other MCP clients -- Claude Code, Cursor, Codex, any harness that
;;;; speaks MCP -- list this Nodecode's conversations, read a conversation's
;;;; messages, poll what is new since a cursor, and send a message into one.
;;;;
;;;; Transport: MCP streamable HTTP, one route on the gateway's own port
;;;; (nle:route).  A client POSTs one JSON-RPC message and is answered with
;;;; one JSON body; a notification is answered 202.  That is the shape every
;;;; MCP client understands natively:
;;;;
;;;;   {"mcpServers": {"nodecode": {"type": "http", "url": "http://HOST:PORT/mcp-serve"}}}
;;;;
;;;; /mcp-serve prints exactly that line, with the port this gateway answers
;;;; on, so nobody has to find it by hand.
;;;;
;;;; Nothing here is durable and nothing is written: reads go through the
;;;; event log (nlk:events), a send goes through the in-process ingress
;;;; (nle:submit), and both are function variables a test stands in front of.

(defpackage #:nodecode-mcp-serve
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:handle-http #:dispatch #:tool-names
           #:client-config))

(in-package #:nodecode-mcp-serve)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this server answers with when a client asks for none.")

(defparameter +server-name+ "nodecode-mcp-serve")
(defparameter +server-version+ "0.1.0")

;;; --- the seams, one function variable each --------------------------------
;;; Every read of the outside world is one of these, so the tests exercise
;;; the whole path -- JSON-RPC in, text out -- with no store, no gateway and
;;; no socket.

(defvar *sessions* #'nlk:list-sessions
  "The session directory, as (list :id :title :source :updated-at ...).")

(defvar *events* #'nlk:events
  "The one read over the event log.")

(defvar *submit* #'nle:submit
  "Put a prompt into a session: the in-process ingress.")

(defvar *endpoint* #'nle:gateway-endpoint
  "Where this organism's gateway answers => (values URL TOKEN).")

(defvar *route* nil
  "The path this cell answers on while it runs, or NIL.")

(defun server-running-p () *route*)

;;; --- small readers -------------------------------------------------------

(defun plist-value (plist key)
  (and (listp plist) (getf plist key)))

(defun one-line (value &optional (limit 200))
  "VALUE as one bounded line: an answer is a line, never a novel."
  (let* ((text (if (stringp value) value (princ-to-string (or value ""))))
         (flat (substitute #\Space #\Newline
                           (substitute #\Space #\Return
                                       (substitute #\Space #\Tab text)))))
    (if (> (length flat) limit)
        (concatenate 'string (subseq flat 0 limit) "...")
        flat)))

(defun session-json (entry)
  (nlk:json-object
   "session_id" (or (plist-value entry :id) "")
   "title" (or (plist-value entry :title) "")
   "prompt" (one-line (or (plist-value entry :prompt) "") 120)
   "source" (or (plist-value entry :source) "")
   "working_directory" (or (plist-value entry :working-directory) "")
   "parent" (plist-value entry :parent)
   "messages" (or (plist-value entry :message-count) 0)
   "updated_at" (or (plist-value entry :updated-at) 0)))

(defun find-conversation (session-id)
  (find session-id (funcall *sessions*)
        :key (lambda (entry) (plist-value entry :id)) :test #'equal))

;;; --- messages ------------------------------------------------------------

(defun message-event-p (event)
  (typep event 'nlk:message-event))

(defun message-json (event)
  (nlk:json-object
   "role" (or (nlk:message-event-role event) "")
   "text" (or (nlk:message-event-content event) "")
   "position" (nlk:event-log-position event)))

(defun newest-position (session-id)
  "The log position of SESSION-ID's newest fact, 0 when it has none."
  (or (funcall *events* :session-id session-id :order :newest :limit 1
                         :columns '("log_position") :as :value)
      0))

(defun recent-messages (session-id limit)
  "The newest LIMIT messages of SESSION-ID, oldest first."
  (let* ((window (max limit (* 4 limit)))
         (events (funcall *events* :session-id session-id :order :newest :limit window))
         (messages (remove-if-not #'message-event-p events)))
    (mapcar #'message-json
            (reverse (subseq messages 0 (min limit (length messages)))))))

(defun messages-after (cursor &key session-id (limit 50))
  "The messages logged after CURSOR, oldest first, and the newest position
read -- CURSOR itself when nothing new arrived."
  (let* ((events (apply *events*
                         (append (list :limit limit :order :oldest)
                                 (when session-id (list :session-id session-id))
                                 (list :after cursor))))
         (messages (remove-if-not #'message-event-p events))
         (end (if messages
                  (reduce #'max messages :key #'nlk:event-log-position)
                  cursor)))
    (values (mapcar #'message-json messages) end)))

;;; --- the tools -----------------------------------------------------------

(defmacro object-schema (&rest members)
  `(nlk:json-object "type" "object" ,@members))

(defparameter +tool-schemas+
  (list
   (cons "conversations_list"
         (nlk:json-object
          "name" "conversations_list"
          "description" "Active conversations on this Nodecode, newest activity first: session_id, title, the platform the session began on, its directory and last activity. Reading needs no gateway running; messages_send does."
          "inputSchema" (object-schema
                         "properties" (nlk:json-object
                                       "limit" (nlk:json-object "type" "integer"
                                                                 "description" "how many conversations, 50 at most")))))
   (cons "conversation_get"
         (nlk:json-object
          "name" "conversation_get"
          "description" "One conversation's details, by the session_id conversations_list answered."
          "inputSchema" (object-schema
                         "properties" (nlk:json-object
                                       "session_id" (nlk:json-object "type" "string"))
                         "required" (vector "session_id"))))
   (cons "messages_read"
         (nlk:json-object
          "name" "messages_read"
          "description" "A conversation's recent messages, oldest first: role and text, each with the log position events_poll resumes from."
          "inputSchema" (object-schema
                         "properties" (nlk:json-object
                                       "session_id" (nlk:json-object "type" "string")
                                       "limit" (nlk:json-object "type" "integer"
                                                                 "description" "how many messages, newest first"))
                         "required" (vector "session_id"))))
   (cons "events_poll"
         (nlk:json-object
          "name" "events_poll"
          "description" "Messages logged after a cursor. Call it with no cursor for the cursor to start from (nothing is replayed), then with the cursor it answered to see what is new."
          "inputSchema" (object-schema
                         "properties" (nlk:json-object
                                       "session_id" (nlk:json-object "type" "string"
                                                                      "description" "one conversation; every one when left out")
                                       "cursor" (nlk:json-object "type" "integer")
                                       "limit" (nlk:json-object "type" "integer")))))
   (cons "messages_send"
         (nlk:json-object
          "name" "messages_send"
          "description" "Send a message into a conversation, through this Nodecode's own ingress: the turn it starts answers in the session's own shell. Needs the gateway running."
          "inputSchema" (object-schema
                         "properties" (nlk:json-object
                                       "session_id" (nlk:json-object "type" "string")
                                       "text" (nlk:json-object "type" "string"))
                         "required" (vector "session_id" "text"))))
   (cons "channels_list"
         (nlk:json-object
          "name" "channels_list"
          "description" "The platforms this Nodecode's conversations began on -- tui, gateway, discord, cron, another agent -- and how many conversations each holds."
          "inputSchema" (object-schema)))))

(defun tool-names ()
  (mapcar #'car +tool-schemas+))

(defun integer-argument (arguments key default)
  (let ((value (nlk:json-value arguments :integer key)))
    (if (and (integerp value) (plusp value)) (min value 500) default)))

(defun string-argument (arguments key)
  (nlk:json-value arguments :string key))

(defun tool-body (name arguments)
  "NAME's answer for ARGUMENTS => (values TEXT FAILED-P)."
  (labels ((need (key)
             (or (string-argument arguments key)
                 (error "~a needs ~a" name key))))
    (cond
      ((string= name "conversations_list")
       (let* ((limit (integer-argument arguments "limit" 50))
              (rows (funcall *sessions*))
              (shown (last rows (min limit (length rows)))))
         (values (nlk:encode-json-object
                  (nlk:json-object "conversations" (coerce (mapcar #'session-json shown) 'vector)
                                   "count" (length shown)))
                 nil)))
      ((string= name "conversation_get")
       (let ((entry (find-conversation (need "session_id"))))
         (if entry
             (values (nlk:encode-json-object (session-json entry)) nil)
             (values (format nil "no conversation ~a on this Nodecode" (need "session_id")) t))))
      ((string= name "messages_read")
       (let* ((id (need "session_id"))
              (limit (integer-argument arguments "limit" (setting :max-messages)))
              (messages (recent-messages id limit)))
         (values (nlk:encode-json-object
                  (nlk:json-object "session_id" id
                                   "messages" (coerce messages 'vector)
                                   "count" (length messages)
                                   "cursor" (newest-position id)))
                 nil)))
      ((string= name "events_poll")
       (let ((cursor (nlk:json-value arguments :integer "cursor"))
             (id (string-argument arguments "session_id"))
             (limit (integer-argument arguments "limit" 50)))
         (if (null cursor)
             (values (nlk:encode-json-object
                      (nlk:json-object "cursor" (newest-position id)
                                       "messages" (vector)
                                       "count" 0))
                     nil)
             (multiple-value-bind (messages end) (messages-after cursor :session-id id :limit limit)
               (values (nlk:encode-json-object
                        (nlk:json-object "cursor" end
                                         "messages" (coerce messages 'vector)
                                         "count" (length messages)))
                       nil)))))
      ((string= name "messages_send")
       (let ((id (need "session_id")) (text (need "text")))
         (if (setting :read-only)
             (values "this Nodecode serves read_only: messages_send is off (mcp-serve.read_only)" t)
             (progn (funcall *submit* id text)
                    (values (format nil "sent to ~a" id) nil)))))
      ((string= name "channels_list")
       (let ((sources (make-hash-table :test #'equal)))
         (dolist (entry (funcall *sessions*))
           (incf (gethash (or (plist-value entry :source) "unknown") sources 0)))
         (let (rows)
           (maphash (lambda (source count)
                      (push (nlk:json-object "channel" source "conversations" count) rows))
                    sources)
           (values (nlk:encode-json-object
                    (nlk:json-object "channels" (coerce rows 'vector)
                                     "count" (length rows)))
                   nil))))
      (t (values (format nil "no tool ~a; this server has ~{~a~^, ~}" name (tool-names)) t)))))

(defun call-tool (name arguments)
  "The JSON-RPC result object for one tools/call."
  (handler-case
      (multiple-value-bind (text failed) (tool-body name arguments)
        (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                         "isError" (and failed t)))
    (error (condition)
      (nlk:json-object "content" (vector (nlk:json-object "type" "text"
                                                           "text" (one-line condition)))
                       "isError" t))))

;;; --- JSON-RPC ------------------------------------------------------------

(defun result-message (id result)
  (nlk:json-object "jsonrpc" "2.0" "id" id "result" result))

(defun error-message (id code message)
  (nlk:json-object "jsonrpc" "2.0" "id" id
                   "error" (nlk:json-object "code" code "message" message)))

(defun initialize-result (request)
  (let* ((params (nlk:json-value request :object "params"))
         (asked (and params (nlk:json-value params :string "protocolVersion"))))
    (nlk:json-object
     "protocolVersion" (or asked +protocol-version+)
     "capabilities" (nlk:json-object "tools" (nlk:json-object))
     "serverInfo" (nlk:json-object "name" +server-name+ "version" +server-version+))))

(defun tools-list-result ()
  (nlk:json-object "tools" (coerce (mapcar #'cdr +tool-schemas+) 'vector)))

(defun tools-call-result (request)
  (let* ((id (gethash "id" request))
         (params (nlk:json-value request :object "params"))
         (name (and params (nlk:json-value params :string "name")))
         (arguments (and params (nlk:json-value params :object "arguments"))))
    (if (member name (tool-names) :test #'string=)
        (result-message id (call-tool name arguments))
        (error-message id -32602 (format nil "no tool ~s; this server has ~{~a~^, ~}"
                                         name (tool-names))))))

(defun dispatch (request)
  "Answer REQUEST, one decoded JSON-RPC message, as the message to send
back -- or NIL for a notification, which takes no answer."
  (let* ((id (gethash "id" request))
         (method (nlk:json-value request :string "method")))
    (cond
      ((null method) (error-message id -32600 "not a JSON-RPC request: no method"))
      ((string= method "initialize") (result-message id (initialize-result request)))
      ((string= method "ping") (result-message id (nlk:json-object)))
      ((string= method "tools/list") (result-message id (tools-list-result)))
      ((string= method "tools/call") (tools-call-result request))
      ((eql 0 (search "notifications/" method)) nil)
      (t (error-message id -32601 (format nil "no method ~a" method))))))

;;; --- the transport -------------------------------------------------------

(defun read-body (env)
  (let ((stream (getf env :raw-body))
        (length (or (getf env :content-length) 0)))
    (when (and stream (plusp length))
      (let ((text (make-string length)))
        (read-sequence text stream)
        text))))

(defun json-response (status object)
  (list status
        (list "content-type" "application/json")
        (list (nlk:encode-json-object object))))

(defun answer (message)
  "MESSAGE through the wire: one JSON body, or 202 for a notification."
  (let ((reply (dispatch message)))
    (if reply
        (json-response 200 reply)
        (list 202 '("content-type" "application/json") '()))))

(defun parse-body (text)
  "TEXT as one decoded JSON-RPC message => (values MESSAGE PARSED-P)."
  (if (and text (plusp (length text)))
      (handler-case (values (nlk:decode-json text) t)
        (error () (values nil nil)))
      (values nil nil)))

(defun handle-http (env)
  "The route's answer: JSON-RPC in the body, JSON out."
  (handler-case
      (if (eq (getf env :request-method) :post)
          (multiple-value-bind (message parsed-p) (parse-body (read-body env))
            (cond
              ((not parsed-p)
               (json-response 400 (error-message nil -32700 "the body is not JSON")))
              ((vectorp message)
               (let ((replies (remove nil (map 'list #'dispatch message))))
                 (if replies
                     (json-response 200 (coerce replies 'vector))
                     (list 202 '("content-type" "application/json") '()))))
              (t (answer message))))
          (json-response 405 (error-message nil -32600 "MCP here is a POST; a GET starts no stream")))
    (error (condition)
      (json-response 200 (error-message nil -32603 (one-line condition))))))

;;; --- /mcp-serve ----------------------------------------------------------

(defun client-config ()
  "The client snippet, with the port this gateway answers on."
  (multiple-value-bind (url token) (funcall *endpoint*)
    (declare (ignore token))
    (if (null url)
        (nlk:json-object
         "listening" nil
         "hint" "no gateway is listening: start one (/gateway, or nodecode serve) and ask again")
        (let ((target (format nil "~a~a"
                              (string-right-trim "/" url)
                              (or (setting :path) "/mcp-serve"))))
          (nlk:json-object
           "listening" t
           "url" target
           "client" (nlk:json-object
                     "mcpServers"
                     (nlk:json-object
                      "nodecode" (nlk:json-object "type" "http" "url" target))))))))

(defun run-slash (arguments session-id)
  "Answer /mcp-serve: how to point a client here, and what it gets."
  (declare (ignore session-id))
  (let ((config (client-config)))
    (cond
      ((nlk:json-value config :boolean "listening")
       (format nil "Point an MCP client at this Nodecode, then restart it:~%~a~%It gets ~{~a~^, ~}.~@[ read_only is on, so messages_send refuses.~]"
               (nlk:encode-json-object (nlk:json-value config :object "client"))
               (tool-names)
               (setting :read-only)))
      (arguments (format nil "mcp-serve: ~a (~a)" (nlk:json-value config :string "hint")
                         (one-line arguments 40)))
      (t (format nil "mcp-serve: ~a" (nlk:json-value config :string "hint"))))))

;;; --- the cell ------------------------------------------------------------

(nlk:define-peripheral mcp-serve :not-running t)

(defparameter +manual+
  "mcp-serve: this Nodecode is an MCP server, so other agents read and send its conversations.
The route sits on the gateway's port; (mcp-serve:client-config) answers the client line, /mcp-serve prints it.
Tools another client gets: conversations_list, conversation_get, messages_read, events_poll, messages_send, channels_list.
Reads come from the event log and need no gateway; messages_send goes through nle:submit and needs one.
mcp-serve.path, mcp-serve.public, mcp-serve.read_only and mcp-serve.max_messages are the settings.")

(defun start-server ()
  (let ((path (or (setting :path) "")))
    (when (string= path "")
      (nlk:config-error "mcp-serve.path is empty: name the route, e.g. /mcp-serve"))
    (unless (nle:gateway-endpoint)
      (nle:notice "mcp-serve: no gateway is listening, so no client can reach this Nodecode yet"
                  :level :warning :key "nodecode-mcp-serve"))
    (nle:route path #'handle-http :public (setting :public))
    (setf *route* path)
    (nle:on-stop (lambda ()
                   (nle:route path nil)
                   (setf *route* nil)))
    path))

(nle:define-cell mcp-serve
  (:section ("mcp-serve")
            (:guide "path is where an MCP client POSTs, on the gateway's port; public lets a client in without the operator token; read_only keeps messages_send off; max_messages bounds one read")
            ("path" :string :default "/mcp-serve"
                    :doc "the route on the gateway's port that answers MCP")
            ("public" :boolean :default t
                      :doc "answer a local MCP client that carries no operator token")
            ("read_only" :boolean :default nil
                         :doc "list, read and poll only; messages_send refuses")
            ("max_messages" :integer :default 50 :min 1
                            :doc "the most messages one read answers"))
  (:command "mcp-serve" 'run-slash
            :description "How to point an MCP client at this Nodecode"
            :argument-hint "nothing")
  (:help :mcp-serve "mcp-serve: other MCP clients read and send this Nodecode's conversations" +manual+)
  (:start #'start-server))
