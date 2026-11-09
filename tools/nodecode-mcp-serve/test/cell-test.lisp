;;;; cell-test.lisp --- the wire, the six tools, and the client line.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here is offline: a request body goes into HANDLE-HTTP as a
;;;; string stream, and the three reads that would touch a store, a session
;;;; or a gateway are function variables the tests bind. Nothing opens a
;;;; socket, and nothing needs a running Nodecode.

(in-package #:nodecode.test)

(define-test-slice "mcp-serve" "MCP-SERVE-CELL-" :start nodecode-mcp-serve:start-cell)

;;; --- the world a test stands in -------------------------------------------

(defparameter *mcp-serve-messages*
  (list (make-instance 'nlk:message-event :role "user" :content "hello there"
                                          :session-id "s1" :kind "message" :id 1
                                          :log-position 1 :occurred-at 1)
        (make-instance 'nlk:message-event :role "assistant" :content "hi, how can I help"
                                          :session-id "s1" :kind "message" :id 2
                                          :log-position 2 :occurred-at 2))
  "Two messages, oldest first: what the stubbed event log holds.")

(defun mcp-serve-stub-events (&rest arguments)
  "NLK:EVENTS as the tools call it: a position, a window, or a range."
  (cond
    ((getf arguments :as)
     (reduce #'max *mcp-serve-messages* :key #'nlk:event-log-position :initial-value 0))
    ((getf arguments :after)
     (remove-if (lambda (event)
                  (<= (nlk:event-log-position event) (getf arguments :after)))
                *mcp-serve-messages*))
    (t *mcp-serve-messages*)))

(defun mcp-serve-stub-sessions ()
  (list (list :id "s1" :title "a conversation" :prompt "hello there" :source "tui"
              :working-directory "/tmp/" :message-count 2 :updated-at 1700000000)
        (list :id "s2" :title "a discord thread" :prompt "from the room" :source "discord"
              :working-directory "/tmp/" :message-count 1 :updated-at 1700000100)))

(defvar *mcp-serve-sent* nil
  "What a stubbed send was handed, newest first, as (SESSION-ID TEXT).")

(defun mcp-serve-install-stubs ()
  (setf nodecode-mcp-serve::*sessions* #'mcp-serve-stub-sessions
        nodecode-mcp-serve::*events* #'mcp-serve-stub-events
        nodecode-mcp-serve::*submit* (lambda (session-id text)
                                        (push (list session-id text) *mcp-serve-sent*)
                                        :admitted)
        nodecode-mcp-serve::*endpoint* (lambda () (values "http://127.0.0.1:4242" "token"))
        *mcp-serve-sent* nil))

(defmacro with-mcp-serve-stubs (&body body)
  "Run BODY against the stub world, putting every seam back afterwards."
  `(let ((sessions nodecode-mcp-serve::*sessions*)
         (events nodecode-mcp-serve::*events*)
         (submit nodecode-mcp-serve::*submit*)
         (endpoint nodecode-mcp-serve::*endpoint*))
     (unwind-protect (progn (mcp-serve-install-stubs) ,@body)
       (setf nodecode-mcp-serve::*sessions* sessions
             nodecode-mcp-serve::*events* events
             nodecode-mcp-serve::*submit* submit
             nodecode-mcp-serve::*endpoint* endpoint))))

;;; --- driving the route ----------------------------------------------------

(defun mcp-serve-post (text &key (method :post))
  "TEXT through the route => (values STATUS BODY-OBJECT)."
  (destructuring-bind (status headers body)
      (nodecode-mcp-serve:handle-http
       (list :request-method method
             :content-length (length text)
             :raw-body (make-string-input-stream text)))
    (declare (ignore headers))
    (values status (and body (plusp (length (first body)))
                        (nlk:decode-json (first body))))))

(defun mcp-serve-rpc (method &optional params (id 1))
  "One JSON-RPC request as a string."
  (nlk:encode-json-object
   (if params
       (nlk:json-object "jsonrpc" "2.0" "id" id "method" method "params" params)
       (nlk:json-object "jsonrpc" "2.0" "id" id "method" method))))

(defun mcp-serve-tool-text (name &optional arguments)
  "NAME's answer text and failure flag, through the whole JSON-RPC path."
  (multiple-value-bind (status answer)
      (mcp-serve-post (mcp-serve-rpc "tools/call"
                                      (if arguments
                                          (nlk:json-object "name" name "arguments" arguments)
                                          (nlk:json-object "name" name))))
    (is (= 200 status) "a tools/call is answered 200")
    (let ((result (nlk:json-value answer :object "result")))
      (values (nlk:json-value (aref (nlk:json-value result :array "content") 0) :string "text")
              (nlk:json-value result :boolean "isError")))))

;;; --- lifecycle ------------------------------------------------------------

(define-cell-lifecycle-tests "mcp-serve"
  (:running (is (equal "/mcp-serve" (nodecode-mcp-serve::server-running-p))
                "the route is up on the path the section named")
            (is (equal "/mcp-serve" (nodecode-mcp-serve::setting :path)))
            (is (nodecode-mcp-serve::setting :public) "a local client is let in by default"))
  (:stopped (is (null (nodecode-mcp-serve::server-running-p)) "stop takes the route back"))
  (:command "mcp-serve")
  (:refused ("max_messages" 0) ("path" 42)))

(deftest mcp-serve-cell-a-path-is-a-setting-not-a-constant ()
  (with-mcp-serve-stubs
    (with-cell-stop ((mcp-serve-start "path" "/agent-mcp"))
      (is (equal "/agent-mcp" (nodecode-mcp-serve::server-running-p))
          "the route is where the section says, not where the source does")
      (let ((text (nodecode-mcp-serve::run-slash nil nil)))
        (is (search "http://127.0.0.1:4242/agent-mcp" text)
            "and the client line the operator is given carries it")))))

;;; --- the protocol ---------------------------------------------------------

(deftest mcp-serve-cell-initialize-answers-the-client ()
  (with-mcp-serve-stubs
    (multiple-value-bind (status answer)
        (mcp-serve-post (mcp-serve-rpc "initialize"
                                        (nlk:json-object "protocolVersion" "2025-03-26")))
      (is (= 200 status) "initialize is answered")
      (is (equal "2.0" (nlk:json-value answer :string "jsonrpc")))
      (is (equal "2025-03-26" (nlk:json-value answer :string "result" "protocolVersion"))
          "the revision the client asked for is the one answered")
      (is (equal "nodecode-mcp-serve"
                 (nlk:json-value answer :string "result" "serverInfo" "name")))
      (is (nlk:json-value answer :object "result" "capabilities" "tools")
          "tools is what it offers"))))

(deftest mcp-serve-cell-lists-its-six-tools ()
  (with-mcp-serve-stubs
    (multiple-value-bind (status answer) (mcp-serve-post (mcp-serve-rpc "tools/list"))
      (is (= 200 status))
      (let ((tools (nlk:json-value answer :array "result" "tools"))
            (names (nodecode-mcp-serve:tool-names)))
        (is (= 6 (length tools)) "six tools, one schema each")
        (is (equal names (map 'list (lambda (tool) (nlk:json-value tool :string "name")) tools))
            "every tool the cell names is advertised, in order")
        (dolist (tool (coerce tools 'list))
          (is (equal "object" (nlk:json-value tool :string "inputSchema" "type"))
              "each tool's schema is an object"))))))

(deftest mcp-serve-cell-a-bad-method-is-a-json-rpc-error ()
  (with-mcp-serve-stubs
    (multiple-value-bind (status answer)
        (mcp-serve-post (mcp-serve-rpc "tools/nonsense"))
      (is (= 200 status) "a JSON-RPC error is still a 200")
      (is (= -32601 (nlk:json-value answer :integer "error" "code")))
      (multiple-value-bind (status empty)
          (mcp-serve-post (nlk:encode-json-object
                            (nlk:json-object "jsonrpc" "2.0"
                                             "method" "notifications/initialized")))
        (is (= 202 status) "a notification is taken, and answered nothing")
        (is (null empty))))
    (multiple-value-bind (status answer) (mcp-serve-post "not json at all")
      (is (= 400 status) "a body that is not JSON is refused as one")
      (is (= -32700 (nlk:json-value answer :integer "error" "code"))))
    (multiple-value-bind (status answer) (mcp-serve-post "" :method :get)
      (is (= 405 status) "a GET starts no stream here")
      (is (stringp (nlk:json-value answer :string "error" "message"))))))

;;; --- the tools ------------------------------------------------------------

(deftest mcp-serve-cell-reads-conversations-and-messages ()
  (with-cell-stop ((mcp-serve-start))
   (with-mcp-serve-stubs
    (multiple-value-bind (text failed) (mcp-serve-tool-text "conversations_list")
      (is (not failed))
      (is (search "s1" text) "a conversation's id is in the answer")
      (is (search "a discord thread" text) "and the platform it began on rides along"))
    (multiple-value-bind (text failed)
        (mcp-serve-tool-text "messages_read" (nlk:json-object "session_id" "s1"))
      (is (not failed))
      (is (search "hi, how can I help" text) "a message's text is in the answer")
      (is (search "\"cursor\":2" text) "and the position events_poll resumes from"))
    (multiple-value-bind (text failed) (mcp-serve-tool-text "channels_list")
      (is (not failed))
      (is (search "discord" text) "the channels are the sources the directory names")))))

(deftest mcp-serve-cell-polls-from-a-cursor ()
  (with-mcp-serve-stubs
    (multiple-value-bind (text failed) (mcp-serve-tool-text "events_poll")
      (is (not failed))
      (is (search "\"count\":0" text) "with no cursor, nothing is replayed")
      (is (search "\"cursor\":2" text) "and the cursor to start from is the newest fact"))
    (multiple-value-bind (text failed)
        (mcp-serve-tool-text "events_poll" (nlk:json-object "session_id" "s1" "cursor" 1))
      (is (not failed))
      (is (search "hi, how can I help" text) "what arrived after the cursor comes back")
      (is (search "\"count\":1" text) "one message, not the whole history")
      (is (not (search "hello there" text)) "and what the cursor already covered is not repeated"))))

(deftest mcp-serve-cell-sends-through-the-ingress ()
  (with-mcp-serve-stubs
    (with-cell-stop ((mcp-serve-start))
     (multiple-value-bind (text failed)
        (mcp-serve-tool-text "messages_send"
                              (nlk:json-object "session_id" "s1" "text" "are you there"))
      (is (not failed) "a send is not a failure")
      (is (search "sent to s1" text))
      (is (equal '(("s1" "are you there")) *mcp-serve-sent*)
          "the text reached the ingress once, for that session")))
    (with-cell-stop ((mcp-serve-start "read_only" t))
      (multiple-value-bind (text failed)
          (mcp-serve-tool-text "messages_send"
                                (nlk:json-object "session_id" "s1" "text" "still there"))
        (is failed "read_only makes a send a refusal")
        (is (search "read_only" text) "and names the setting that refused it")
        (is (equal '(("s1" "are you there")) *mcp-serve-sent*)
            "nothing more reached the ingress")))))

;;; --- the client line ------------------------------------------------------

(deftest mcp-serve-cell-slash-prints-the-client-line ()
  (with-mcp-serve-stubs
    (with-cell-stop ((mcp-serve-start))
      (let ((text (nodecode-mcp-serve::run-slash nil nil)))
        (is (search "http://127.0.0.1:4242/mcp-serve" text)
            "the line carries the port this gateway answers on and the route")
        (is (search "mcpServers" text) "and is a client snippet, not a sentence")
        (is (search "messages_send" text) "with what the client gets")
        (is (not (search "read_only" text)) "and says nothing about a setting that is off"))))
  (let ((nodecode-mcp-serve::*endpoint* (lambda () (values nil nil))))
    (let ((text (nodecode-mcp-serve::run-slash nil nil)))
      (is (search "no gateway is listening" text) "with no gateway, it says so"))))
