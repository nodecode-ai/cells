;;;; cell-test.lisp --- the mermail cell, end to end over a scripted MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a socket: the cell's one network seam
;;;; (NODECODE-MERMAIL::*HTTP*) is bound to a fake server that answers the
;;;; JSON-RPC methods each test scripts and records every request it took, so
;;;; "the key never left" and "the envelope carried the ids" are facts a test
;;;; can read. The tools are called through the real dispatch seam
;;;; (EXECUTE-WIRE-CALL), so the schema the provider is shown and the body
;;;; that runs are the same declaration.

(in-package #:nodecode.test)

(define-test-slice "mermail" "MERMAIL-CELL-" :start nodecode-mermail:start-cell)

;;; --- the fake server ---------------------------------------------------------

(defparameter +endpoint+ "https://mermail.test/mcp"
  "The endpoint every test's section names.")

(defvar *requests* '()
  "Every request the fake server took, newest first: (METHOD URL HEADERS CONTENT).")

(defvar *calls* 0
  "A counter for wire call ids.")

(defvar *show-tool-answers* nil
  "Whether TOOL-TEXT echoes what each tool answered: bind it true to read a
failing test's log.")

(defun mcp-answer (id result)
  "One JSON-RPC answer carrying ID."
  (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun text-result (text &key (is-error nil))
  "A tools/call result whose one content item is TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                    "isError" (and is-error t)))

(defun sse-text (payload)
  "PAYLOAD as one server-sent event, the other way MCP answers."
  (format nil "event: message~%data: ~a~%~%" payload))

(defun fake-server (&key expire-first (sse nil) (answer "Inbox: 2 messages")
                         (tools '("list_emails" "send_email" "get_email")))
  "A stand-in for NLK:HTTP speaking just enough MCP: initialize hands back a
session id, the initialized notification is accepted, tools/list answers
TOOLS, and tools/call answers ANSWER. With EXPIRE-FIRST the first call meets
a session the server has forgotten (404)."
  (setf *requests* '())
  (let ((sessions 0) (calls 0))
    (lambda (method url &key headers content &allow-other-keys)
      (push (list method url headers content) *requests*)
      (let* ((body (and content (plusp (length content)) (nlk:decode-json content :whole t)))
             (name (and (hash-table-p body) (gethash "method" body)))
             (id (and (hash-table-p body) (gethash "id" body))))
        (cond
          ((equal name "initialize")
           (incf sessions)
           (values (mcp-answer id (nlk:json-object "protocolVersion" "2025-06-18"
                                                    "capabilities" (nlk:make-json-object)
                                                    "serverInfo" (nlk:json-object "name" "mermail"
                                                                                   "version" "1.5.5")))
                   200 (list (cons "mcp-session-id" (format nil "sess-~d" sessions))) nil))
          ((equal name "notifications/initialized") (values "" 202 nil nil))
          ((equal name "tools/list")
           (values (mcp-answer id (nlk:json-object
                                   "tools"
                                   (coerce (loop for tool in tools
                                                 collect (nlk:json-object "name" tool))
                                           'vector)))
                   200 (list (cons "mcp-session-id" "sess-1")) nil))
          ((equal name "tools/call")
           (incf calls)
           (cond ((and expire-first (= calls 1))
                  (values "{\"error\":{\"message\":\"session not found\"}}" 404 nil nil))
                 (t (let ((payload (mcp-answer id (text-result answer))))
                      (values (if sse (sse-text payload) payload)
                              200 (list (cons "mcp-session-id" "sess-1")) nil)))))
          (t (values "{}" 400 nil nil)))))))

(defmacro with-fake-mermail (&body body)
  "The cell over a server that answers every method: what the lifecycle tests
start inside."
  `(let ((nodecode-mermail::*http* (fake-server))
         (nodecode-mermail::*session* nil))
     ,@body))

(defmacro with-mermail-server ((&rest options) &body body)
  "The cell over a server scripted by OPTIONS, its session fresh."
  `(let ((nodecode-mermail::*http* (fake-server ,@options))
         (nodecode-mermail::*session* nil))
     ,@body))

(defun start-mermail (&rest pairs)
  "The cell started on the carrier's config plus PAIRS, the endpoint and a
key always present."
  (apply #'mermail-start "base_url" +endpoint+ "api_key" "test-key" pairs))

(defun json-answer (&rest pairs)
  "PAIRS as the JSON text of a tool call's arguments."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun tool-text (name &rest arguments)
  "The text the tool called NAME answers, called as the provider's call is."
  (let ((text (princ-to-string
               (nle::message-content
                (execute-wire-call (format nil "mermail-call-~d" (incf *calls*)) name
                                    (if arguments (apply #'json-answer arguments) "{}"))))))
    (when *show-tool-answers*
      (format t "~&; ~a => ~a~%" name text))
    text))

(defun request-body (request)
  "The decoded JSON body of REQUEST, or NIL."
  (let ((content (fourth request)))
    (and content (plusp (length content)) (nlk:decode-json content :whole t))))

(defun methods ()
  "The JSON-RPC method of every request the server took, oldest first."
  (loop for request in (reverse *requests*)
        for body = (request-body request)
        collect (and (hash-table-p body) (gethash "method" body))))

(defun tool-calls ()
  "The tools/call requests the server took, oldest first, as (NAME ARGUMENTS)."
  (loop for request in (reverse *requests*)
        for body = (request-body request)
        when (and (hash-table-p body) (equal (gethash "method" body) "tools/call"))
          collect (let ((params (gethash "params" body)))
                    (list (nlk:json-value params :string "name")
                          (nlk:json-value params :object "arguments")))))

(defun header-value (request name)
  "The value REQUEST carried for NAME."
  (cdr (assoc name (third request) :test #'string-equal)))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "mermail"
  (:fixture with-fake-mermail)
  (:config "base_url" "https://mermail.test/mcp" "api_key" "test-key")
  (:help :mermail)
  (:command "mermail")
  (:running
   (loop for name in '("mermail_mailboxes" "mermail_emails" "mermail_search" "mermail_read"
                       "mermail_send" "mermail_draft" "mermail_call")
         do (is (nle::find-tool name) name)))
  (:stopped
   (is (null (nle::find-tool "mermail_mailboxes")) "stop takes every tool out of the catalog")
   (is (null nodecode-mermail::*mermail*) "and the settings with it"))
  (:refused ("timeout_seconds" 0) ("profile" "everything")))

;;; --- a read -------------------------------------------------------------------

(deftest mermail-cell-a-listing-initializes-then-calls-the-server ()
  (with-mermail-server ()
    (with-cell-stop ((start-mermail))
      (let ((text (tool-text "mermail_emails" "mailbox_id" "mb-1" "limit" 5)))
        (is (search "Inbox: 2 messages" text) "the server's answer reaches the model"))
      (is (equal '("initialize" "notifications/initialized" "tools/call") (methods)) "one session, then the call")
      (destructuring-bind (name arguments) (first (tool-calls))
        (is (equal "list_emails" name) "the cell speaks the server's own tool name")
        (is (equal "mb-1" (nlk:json-value arguments :string "mailboxId")) "the path id stays top-level")
        (let ((query (nlk:json-value arguments :object "query")))
          (is (eql 5 (nlk:json-value query :integer "limit")) "the limit rides in query")
          (is (eql t (nlk:json-value query :boolean "metadata_only")) "and a listing asks for metadata")
          (is (equal "inbox" (nlk:json-value query :string "folder")) "with the defaulted folder")))
      (let ((first-request (first (reverse *requests*))))
        (is (equal "test-key" (header-value first-request "x-api-key")) "the key rides as x-api-key")
        (is (equal "sess-1" (header-value (first *requests*) "mcp-session-id")) "and every later request carries the session the server handed back")))))

(deftest mermail-cell-a-search-and-a-read-shape-their-own-envelopes ()
  (with-mermail-server (:answer "{\"id\":\"e-1\"}")
    (with-cell-stop ((start-mermail))
      (tool-text "mermail_search" "mailbox_id" "mb-1" "text" "invoice" "from" "bank.test")
      (destructuring-bind (name arguments) (first (tool-calls))
        (is (equal "search_emails" name))
        (let ((query (nlk:json-value arguments :object "query")))
          (is (equal "invoice" (nlk:json-value query :string "search")) "the free text rides as search")
          (is (equal "bank.test" (nlk:json-value query :string "from")))
          (is (null (nlk:json-value query :string "subject")) "an absent filter is left out")))
      (tool-text "mermail_read" "mailbox_id" "mb-1" "email_id" "e-1")
      (destructuring-bind (name arguments) (second (tool-calls))
        (is (equal "get_email" name))
        (is (equal "e-1" (nlk:json-value arguments :string "emailId")) "the message id stays top-level")
        (let ((query (nlk:json-value arguments :object "query")))
          (is (eql t (nlk:json-value query :boolean "agent_safe_content")) "a body is read agent-safe")
          (is (equal "clean" (nlk:json-value query :string "require_scan_status")) "and scan-gated")
          (is (eql 10000 (nlk:json-value query :integer "max_body_chars")) "with the defaulted cap"))))))

(deftest mermail-cell-a-send-says-exactly-what-it-will-deliver ()
  (with-mermail-server (:answer "{\"status\":\"queued\"}")
    (with-cell-stop ((start-mermail))
      (let ((text (tool-text "mermail_send" "mailbox_id" "mb-1" "from" "me@mermail.app"
                              "to" "them@example.test" "subject" "Hello" "text" "Hi there")))
        (is (search "queued" text)))
      (destructuring-bind (name arguments) (first (tool-calls))
        (is (equal "send_email" name))
        (let ((body (nlk:json-value arguments :object "body")))
          (is (equal "me@mermail.app" (nlk:json-value body :string "from")))
          (is (equal "them@example.test" (nlk:json-value body :string "to")))
          (is (equal "Hi there" (nlk:json-value body :string "text")) "a send carries text, not body"))))))

;;; --- refusals and recoveries ----------------------------------------------------

(deftest mermail-cell-no-key-refuses-before-anything-is-sent ()
  ;; a machine with no MERMAIL_API_KEY in its environment
  (with-stubbed-fdefinitions ((nle::credential-env (variable) (and variable nil)))
    (with-mermail-server ()
      (with-cell-stop ((mermail-start "base_url" +endpoint+))
        (let ((text (tool-text "mermail_mailboxes")))
          (is (search "MERMAIL_API_KEY" text) "the refusal names the variable")
          (is (search "mermail.api_key" text) "and the section member"))
        (is (null *requests*) "and nothing was sent")))))

(deftest mermail-cell-a-session-the-server-forgot-is-made-again ()
  (with-mermail-server (:expire-first t :answer "second time")
    (with-cell-stop ((start-mermail))
      (let ((text (tool-text "mermail_mailboxes")))
        (is (search "second time" text) "the call was sent once more"))
      (is (equal 2 (count "tools/call" (methods) :test #'equal)) "and was refused first")
      (is (equal 2 (count "initialize" (methods) :test #'equal)) "with a fresh initialize"))))

(deftest mermail-cell-an-event-stream-answer-is-read-too ()
  ;; MCP's Streamable HTTP may answer a POST as server-sent events; the same
  ;; call must read either, or a server that streams answers nothing.
  (with-mermail-server (:sse t :answer "streamed answer")
    (with-cell-stop ((start-mermail))
      (is (search "streamed answer" (tool-text "mermail_mailboxes")) "the event's data reaches the model"))))

;;; --- the command ---------------------------------------------------------------

(deftest mermail-cell-the-command-says-where-the-key-comes-from ()
  (with-mermail-server ()
    (with-cell-stop ((start-mermail))
      (multiple-value-bind (text dialog) (nodecode-mermail::run-slash "" "s1")
        (is (search "mermail.api_key" text) "the member it reads")
        (is (search +endpoint+ text) "and the endpoint it asks")
        (is (null (search "test-key" text)) "never the key itself")
        (is (listp dialog) "a panel comes with it")
        (is (search "Mermail" (prin1-to-string dialog)) "titled after the service"))
      (is (null *requests*) "and nothing was sent"))))

(deftest mermail-cell-check-asks-the-server-for-its-catalog ()
  (with-mermail-server ()
    (with-cell-stop ((start-mermail))
      (let ((text (nodecode-mermail::run-slash "check" "s1")))
        (is (search "3 tools" text) "the catalog size")
        (is (search "list_emails" text) "and the names it carries"))
      (is (equal '("initialize" "notifications/initialized" "tools/list") (methods)) "a read-only check, one session deep"))))
