;;;; cell-test.lisp --- the pushary cell against a stubbed Pushary.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Pushary request is a stubbed NLK:HTTP and the key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches pushary.com, the environment or the
;;;; operator's files. A tool is called the way a model's call reaches it —
;;;; through the real dispatch, contract and all — so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "pushary" "PUSHARY-CELL-" :start nodecode-pushary:start-cell
  :defaults (("api_key_env" "PUSHARY_API_KEY")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter *requests* nil
  "Every exchange the stub answered, oldest first, as (METHOD URL HEADERS CONTENT).
It is a global on purpose: the value a thread sends with is the global one.")

(defmacro with-pushary (&body body)
  "BODY with the key answering and the request log emptied."
  `(let ((saved *requests*))
     (setf *requests* '())
     (nlk:with-cleanup ((setf *requests* saved))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "PUSHARY_API_KEY") "test-key"))
         ,@body))))

(defun envelope (payload)
  "An MCP answer carrying PAYLOAD — one JSON text — as its one content block."
  (nlk:encode-json-object
   (nlk:json-object
    "jsonrpc" "2.0" "id" 1
    "result" (nlk:json-object
              "content" (vector (nlk:json-object "type" "text" "text" payload))))))

(defmacro with-pushary-wire ((&key answer (status 200)) &body body)
  "BODY with NLK:HTTP answering ANSWER — an MCP envelope around {\"ok\":true}
when empty — and STATUS to every Pushary request, each kept in *REQUESTS*."
  `(let ((payload (or ,answer (envelope "{\"ok\":true}"))))
     (with-stubbed-fdefinition (nlk:http (method target &rest options)
                                 (push (list method target (getf options :headers)
                                             (getf options :content))
                                       *requests*)
                                 (values payload ,status))
       ,@body)))

(defun header (request name)
  "NAME's value in REQUEST's header alist, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun sent (request)
  "REQUEST's body, decoded."
  (nlk:decode-json (fourth request)))

(defun tool-name-sent (request)
  "The MCP tool REQUEST called."
  (nlk:json-value (nlk:json-value (sent request) :any "params") :string "name"))

(defun tool-answer (id tool arguments)
  "The tool's answer to ARGUMENTS, as the model reads it: the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun answer-object (id tool arguments)
  (nlk:decode-json (tool-answer id tool arguments)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "pushary"
  (:config)
  (:help :pushary)
  (:command "pushary")
  (:fixture with-pushary)
  (:refused ("api_key_env" "PUSHARY_ABSENT_KEY")))

(deftest pushary-cell-without-a-key-refuses ()
  ;; The one thing no verb can work without: the section names a variable, and
  ;; a variable that is unset stops the start before anything installs.
  (with-stubbed-fdefinition (nle::credential-env (name) (and name nil))
    (let ((condition (signals-error nlk:config-refusal (pushary-start))))
      (let ((text (princ-to-string condition)))
        (is (search "PUSHARY_API_KEY" text) "the refusal names the unset variable"))
      (is (null (nle::find-tool "pushary_notify")) "and installs nothing"))))

;;; --- pushary_notify ------------------------------------------------------------------

(deftest pushary-cell-notify-sends-one-mcp-call (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire ()
      (let ((object (answer-object "p-1"
                                   "pushary_notify"
                                   "{\"title\":\"Deploy\",\"body\":\"the deploy is live\",\"context_type\":\"success\",\"summary\":\"shipped\"}")))
        (is (eq t (nlk:json-value object :boolean "ok")) "the server's answer is what the model reads")
        (is (= 1 (length *requests*)) "one exchange, no retry")
        (let ((request (first *requests*)))
          (is (equal :post (first request)))
          (is (equal "https://pushary.com/api/mcp/mcp" (second request)) "Pushary's one endpoint")
          (is (equal "Bearer test-key" (header request "authorization")) "the account's key rides it")
          (is (equal "tools/call" (nlk:json-value (sent request) :string "method")))
          (is (equal "send_notification" (tool-name-sent request)))
          (let ((arguments (nlk:json-value (nlk:json-value (sent request) :any "params") :any "arguments")))
            (is (equal "the deploy is live" (nlk:json-value arguments :string "body")))
            (is (equal "Deploy" (nlk:json-value arguments :string "title")))
            (let ((context (nlk:json-value arguments :any "context")))
              (is (equal "success" (nlk:json-value context :string "type")) "context_type adds the detail page")
              (is (equal "shipped" (nlk:json-value context :string "summary")) "and its summary with it"))
            (is (equal "shipped" (nlk:json-value (nlk:json-value arguments :any "context") :string "summary")))))))))

(deftest pushary-cell-notify-refuses-an-empty-body (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire ()
      (is (search "ERROR:" (tool-answer "p-2" "pushary_notify" "{\"body\":\"   \"}")) "blank is not a notification")
      (is (null *requests*) "and it goes out before any request"))))

(deftest pushary-cell-notify-says-what-pushary-refused (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire (:answer (nlk:encode-json-object
                                  (nlk:json-object "jsonrpc" "2.0" "id" 1
                                                   "error" (nlk:json-object "message" "api key is invalid")))
                        :status 401)
      (let ((answer (tool-answer "p-3" "pushary_notify" "{\"body\":\"hello\"}")))
        (is (search "ERROR:" answer) "a refused call is a failed call, not a success")
        (is (search "api key is invalid" answer) "in Pushary's own words")))))

(deftest pushary-cell-notify-reads-an-event-stream (with-pushary)
  ;; A blocking question may come back as text/event-stream: the last data:
  ;; line is the answer, and it reads like a plain body.
  (with-cell-stop ((pushary-start))
    (with-pushary-wire (:answer (format nil "event: message~%data: ~a~%~%"
                                        (envelope "{\"ok\":true,\"requestId\":\"req-9\"}")))
      (let ((answer (answer-object "p-4" "pushary_notify" "{\"body\":\"hi\"}")))
        (is (equal "req-9" (nlk:json-value answer :string "requestId")) "an event stream is one answer")))))

;;; --- pushary_ask, pushary_wait, pushary_cancel ----------------------------------------

(deftest pushary-cell-ask-waits-once-for-an-unanswered-question (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire (:answer (envelope "{\"correlationId\":\"c-1\",\"answered\":false}"))
      (let ((object (answer-object "a-1" "pushary_ask" "{\"question\":\"ship it?\",\"tool_name\":\"sh\",\"blocker\":\"deploy\"}")))
        (is (= 2 (length *requests*)) "the ask, then one poll")
        (is (equal "ask_user" (tool-name-sent (second *requests*))) "the question goes first")
        (is (equal "wait_for_answer" (tool-name-sent (first *requests*))) "and the poll after it")
        (let ((arguments (nlk:json-value (nlk:json-value (sent (second *requests*)) :any "params") :any "arguments")))
          (is (equal "ship it?" (nlk:json-value arguments :string "question")))
          (is (eq t (nlk:json-value arguments :boolean "wait")) "the default is to wait")
          (is (equal "sh" (nlk:json-value arguments :string "toolName")) "the approval's tool rides along"))
        (is (equal "c-1" (nlk:json-value object :string "correlationId")) "the merged answer keeps the id")
        (let ((polled (nlk:json-value (nlk:json-value (sent (first *requests*)) :any "params") :any "arguments")))
          (is (equal "c-1" (nlk:json-value polled :string "correlationId")) "and passes it to the poll"))))))

(deftest pushary-cell-ask-with-wait-false-answers-at-once (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire (:answer (envelope "{\"correlationId\":\"c-2\",\"answered\":false}"))
      (let ((object (answer-object "a-2" "pushary_ask" "{\"question\":\"ship it?\",\"wait\":false}")))
        (is (= 1 (length *requests*)) "no poll when the caller asked for none")
        (is (equal "c-2" (nlk:json-value object :string "correlationId")))
        (is (eq nil (nlk:json-value object :boolean "answered")) "and the answer says so")))))

(deftest pushary-cell-wait-and-cancel-name-their-tools (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire ()
      (tool-answer "a-3" "pushary_wait" "{\"correlation_id\":\"c-3\"}")
      (is (equal "wait_for_answer" (tool-name-sent (first *requests*))))
      (tool-answer "a-4" "pushary_cancel" "{\"correlation_id\":\"c-4\"}")
      (is (equal "cancel_question" (tool-name-sent (first *requests*))))
      (let ((answer (tool-answer "a-5" "pushary_wait" "{\"correlation_id\":\"\"}" )))
        (is (search "ERROR:" answer) "an empty id is refused")))))

;;; --- pushary_remind and pushary_propose_scope -------------------------------------------

(deftest pushary-cell-remind-schedules-with-an-operation-id (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire ()
      (tool-answer "r-1" "pushary_remind" "{\"body\":\"water the plants\",\"in_minutes\":30}")
      (let ((request (first *requests*)))
        (is (equal "schedule_reminder" (tool-name-sent request)))
        (let ((arguments (nlk:json-value (nlk:json-value (sent request) :any "params") :any "arguments")))
          (is (equal "water the plants" (nlk:json-value arguments :string "body")))
          (is (= 30 (nlk:json-value arguments :integer "inMinutes")))
          (let ((id (nlk:json-value arguments :string "requestId")))
            (is (plusp (length id)) "a request id makes a retry one reminder, not two")))))))

(deftest pushary-cell-scope-proposes-the-boundary (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire ()
      (tool-answer "s-1" "pushary_propose_scope"
                   "{\"done_when\":\"the tests pass\",\"allowed_paths\":[\"src/**\"]}")
      (is (equal "propose_scope" (tool-name-sent (first *requests*))))
      (let ((arguments (nlk:json-value (nlk:json-value (sent (first *requests*)) :any "params") :any "arguments")))
        (is (equal "the tests pass" (nlk:json-value arguments :string "doneWhen")))
        (is (equal "src/**" (nlk:json-value (aref (nlk:json-value arguments :array "allowedPaths") 0) :string))))
      (is (search "ERROR:" (tool-answer "s-2" "pushary_propose_scope" "{}")) "done_when is required"))))

;;; --- a verb with nothing started ---------------------------------------------------------

(deftest pushary-cell-a-verb-refuses-while-not-running (with-pushary)
  (setf nodecode-pushary::*pushary* nil)
  (with-pushary-wire ()
    (let ((answer (nodecode-pushary:send-notification
                   (nodecode-pushary::decode-notice
                    (nlk:make-json-object "body" "hello")))))
      (is (search "not running" answer) "the refusal says the cell is not on")
      (is (null *requests*) "and nothing left the machine"))))

;;; --- the command and the manual ------------------------------------------------------------

(deftest pushary-cell-the-command-answers-through-stubbed-io (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire (:answer (envelope "{\"ok\":true,\"requestId\":\"req-7\"}"))
      (let ((line (cell-entry "nodecode-pushary" "pushary" "")))
        (is (search "PUSHARY_API_KEY" line) "the status names the variable the key is read from")
        (is (search "pushary_notify" line) "and the tools it reaches")
        (is (null *requests*) "status sends nothing"))
      (is (search "test notification went" (cell-entry "nodecode-pushary" "pushary" "test")))
      (is (= 1 (length *requests*)) "test sends exactly one")
      (is (search "Usage" (cell-entry "nodecode-pushary" "pushary" "wobble")) "an unknown verb says what the verbs are"))))

;;; --- the same push, from eval ----------------------------------------------------------------

(deftest pushary-cell-eval-sends-the-same-notification (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire ()
      (is (search "ok" (nodecode-pushary:notify "the build is green" :title "Build")) "notify is the tool's own body")
      (let ((arguments (nlk:json-value (nlk:json-value (sent (first *requests*)) :any "params") :any "arguments")))
        (is (equal "the build is green" (nlk:json-value arguments :string "body")))
        (is (equal "Build" (nlk:json-value arguments :string "title")) "a keyword argument reaches the call")))))

(deftest pushary-cell-eval-asks-the-same-question (with-pushary)
  (with-cell-stop ((pushary-start))
    (with-pushary-wire (:answer (envelope "{\"correlationId\":\"c-9\",\"answered\":false}"))
      (is (search "c-9" (nodecode-pushary:ask "ship it?")) "ask is the tool's own body")
      (is (= 2 (length *requests*)) "the ask, then its one poll")
      (let ((arguments (nlk:json-value (nlk:json-value (sent (second *requests*)) :any "params") :any "arguments")))
        (is (equal "ship it?" (nlk:json-value arguments :string "question")))
        (is (equal "confirm" (nlk:json-value arguments :string "type")) "the default type reaches the wire")))))
