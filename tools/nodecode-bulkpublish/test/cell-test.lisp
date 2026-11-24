;;;; cell-test.lisp --- lifecycle, the MCP exchange, the guard, /bulkpublish, a refusal's shape.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What reaches outside this image is NODECODE-BULKPUBLISH::*HTTP*, the one
;;;; function every call dials, and WITH-BULKPUBLISH stubs it: each test
;;;; plans its own answers by the JSON-RPC method the frame names, so no test
;;;; touches the network and none depends on what a live server says today.
;;;; What is proved is that the session is opened once and in order, that a
;;;; tool call sends the arguments it was given, that a write answers instead
;;;; of sending until it is confirmed, that an SSE answer and a refused status
;;;; both read, and that a tool, a command and a failure all come out as text
;;;; the model can read.

(in-package #:nodecode.test)

(define-test-slice "bulkpublish" "BULKPUBLISH-CELL-" :start nodecode-bulkpublish:start-cell
  :defaults (("endpoint" "https://mcp.bulkpublish.test/mcp/full")
             ("api_key" "bp_test")))

(define-cell-lifecycle-tests "bulkpublish"
  (:help :bulkpublish)
  (:command "bulkpublish")
  (:running (is (equal nodecode-bulkpublish::+manual+ (nle:help :bulkpublish)) "(help :bulkpublish) is the manual")
            (is (equal "https://mcp.bulkpublish.test/mcp/full" (nodecode-bulkpublish::endpoint)) "the section's endpoint, without its trailing slash")
            (is (third (nle::find-tool "bulkpublish_tools")) "the listing tool is registered")
            (is (third (nle::find-tool "bulkpublish_call")) "and the one that runs a tool"))
  (:refused ("endpoint" 7) ("timeout" 0))
  (:idle bulkpublish:bulkpublish-error
         (nodecode-bulkpublish::list-tools)
         (bulkpublish:call "list_channels")
         (bulkpublish:channels))
  (:stopped (is (null (nle::find-tool "bulkpublish_tools")) "stop unregisters the tools")
            (is (null (nle::find-tool "bulkpublish_call")))))

;;; --- the stubbed wire -----------------------------------------------------------

(defvar *bulk-plan* '()
  "((MATCHER . ANSWER) ...): MATCHER takes a decoded frame, ANSWER takes it too and answers (values TEXT STATUS HEADERS).")

(defvar *bulk-requests* '()
  "((METHOD URL HEADERS FRAME) ...) the stubbed wire received, newest first.")

(defun bulk-headers (&optional (session-id "s-1"))
  "A response's header table, as NLK:HTTP hands one back."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "content-type" table) "application/json")
    (when session-id (setf (gethash "mcp-session-id" table) session-id))
    table))

(defun bulk-answer (frame result &key (content-type "application/json") (session-id "s-1") (status 200))
  "FRAME's own id answered with RESULT, as one 200 -- JSON, or the one data: line SSE carries."
  (let* ((object (nlk:json-object "jsonrpc" "2.0" "id" (gethash "id" frame) "result" result))
         (headers (bulk-headers session-id)))
    (setf (gethash "content-type" headers) content-type)
    (values (if (equal content-type "text/event-stream")
                (format nil "event: message~%data: ~a~%~%" (nlk:encode-json-object object))
                (nlk:encode-json-object object))
            status
            headers)))

(defun bulk-answers (result &key content-type)
  "An answer for any frame: RESULT as its reply."
  (lambda (frame) (bulk-answer frame result :content-type (or content-type "application/json"))))

(defun bulk-method-p (name)
  "A matcher taking the frames whose method is NAME."
  (lambda (frame) (equal name (nlk:json-value frame :string "method"))))

(defun bulk-dial (method url &key headers content &allow-other-keys)
  "The stub standing in for NLK:HTTP: record the exchange, answer the first
plan entry whose matcher takes the frame, and an empty result when none does."
  (let ((frame (and content (handler-case (nlk:decode-json content) (error () nil)))))
    (push (list method url headers frame) *bulk-requests*)
    (loop for (matcher . answer) in *bulk-plan*
          when (funcall matcher frame)
            do (return-from bulk-dial (funcall answer frame)))
    (bulk-answer frame (nlk:make-json-object))))

(defmacro with-bulkpublish ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS, its one dial stubbed, and
nothing the test did not plan answered."
  `(with-saved-globals (nle:*tools* nodecode-bulkpublish::*http* nodecode-bulkpublish::*session*)
     (let ((*bulk-plan* '())
           (*bulk-requests* '()))
       (setf nodecode-bulkpublish::*http* #'bulk-dial
             nodecode-bulkpublish::*session* nil)
       (with-cell-stop ((bulkpublish-start ,@pairs))
         ,@body))))

(defun bulk-plan-add (matcher answer)
  "Answer every frame MATCHER takes with ANSWER."
  (push (cons matcher answer) *bulk-plan*))

(defun bulk-tool (name description)
  (nlk:json-object "name" name "description" description
                    "inputSchema" (nlk:json-object "type" "object")))

(defun bulk-content (text &key (is-error nil))
  "A tools/call result carrying TEXT, as the server's does."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                    "isError" is-error))

(defun bulk-catalogue ()
  "Four tools: two reads, a write, and one more read."
  (nlk:json-object
   "tools" (vector (bulk-tool "list_channels" "List the channels connected to the account")
                   (bulk-tool "create_post" "Draft a post for one or more channels")
                   (bulk-tool "publish_post" "Publish a draft now, to every channel it names")
                   (bulk-tool "get_analytics" "Impressions and engagement between two dates"))))

(defun bulk-standard-plan ()
  "The session a call needs: initialize, the notification, the catalogue."
  (bulk-plan-add (bulk-method-p "initialize")
                 (bulk-answers (nlk:json-object "protocolVersion" "2025-06-18"
                                                 "serverInfo" (nlk:json-object "name" "bulkpublish"))))
  (bulk-plan-add (bulk-method-p "notifications/initialized")
                 (lambda (frame) (declare (ignore frame)) (values "" 202 nil)))
  (bulk-plan-add (bulk-method-p "tools/list") (bulk-answers (bulk-catalogue))))

(defun bulk-sent ()
  "The exchanges the stub received, oldest first."
  (reverse *bulk-requests*))

(defun bulk-methods ()
  "The JSON-RPC methods the stub was sent, oldest first."
  (loop for request in (bulk-sent)
        collect (nlk:json-value (fourth request) :string "method")))

(defun bulk-sent-call ()
  "The params of the last tools/call the stub received."
  (let ((request (find "tools/call" (bulk-sent)
                       :key (lambda (request) (nlk:json-value (fourth request) :string "method"))
                       :test #'equal)))
    (and request (gethash "params" (fourth request)))))

(defun bulk-object (&rest pairs)
  "PAIRS, alternating string keys and values, as one JSON object."
  (let ((object (nlk:make-json-object)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key object) value))
    object))

(defun bulk-tool-call (name &rest arguments)
  "NAME called through the registered tool with ARGUMENTS as alternating strings."
  (funcall (third (nle::find-tool "bulkpublish_call"))
           (nlk:contract-decode (nlk:contract 'nodecode-bulkpublish::bulkpublish-call)
                                (apply #'bulk-object "tool" name arguments))))

(defun bulk-tools-call (&rest pairs)
  "bulkpublish_tools called through the registered tool."
  (funcall (third (nle::find-tool "bulkpublish_tools"))
           (nlk:contract-decode (nlk:contract 'nodecode-bulkpublish::bulkpublish-tools)
                                (apply #'bulk-object pairs))))

;;; --- the exchange ------------------------------------------------------------------

(deftest bulkpublish-cell-the-session-opens-once-and-the-tools-are-listed ()
  (with-bulkpublish ()
    (bulk-standard-plan)
    (let ((answer (bulkpublish:tools)))
      (is (search "list_channels - List the channels connected" answer) "a tool is one line: its name, then what it does")
      (is (search "4 of 4 tools" answer) "the listing says how many of how many")
      (is (search "publish_post - Publish a draft now" answer)))
    (is (equal '("initialize" "notifications/initialized" "tools/list") (bulk-methods)) "the session is opened once, in order")
    (is (search "1 of 4 tools carrying publish" (bulkpublish:tools :query "publish")) "a query keeps what its text carries")
    (is (equal 3 (length (bulk-methods))) "a second listing reads the catalogue the session kept")
    (is (equal "2025-06-18" (getf nodecode-bulkpublish::*session* :protocol)) "the server's revision is the session's")
    (is (equal "s-1" (getf nodecode-bulkpublish::*session* :id)) "and the session id it issued")))

(deftest bulkpublish-cell-a-tool-call-sends-its-arguments-by-name ()
  (with-bulkpublish ()
    (bulk-standard-plan)
    (bulk-plan-add (bulk-method-p "tools/call")
                   (bulk-answers (bulk-content "post 42 is a draft on x and instagram")))
    (let ((answer (bulkpublish:call "get_post" "postId" "42")))
      (is (equal "post 42 is a draft on x and instagram" answer) "the text the result carries is the answer"))
    (let ((params (bulk-sent-call)))
      (is (equal "get_post" (nlk:json-value params :string "name")) "the tool's name crosses as it was given")
      (is (equal "42" (nlk:json-value (gethash "arguments" params) :string "postId")) "and its arguments as one object"))
    (bulk-plan-add (bulk-method-p "tools/call") (bulk-answers (bulk-content "two channels: x, instagram")))
    (is (search "two channels" (bulkpublish:channels)) "list_channels is one call away")))

(deftest bulkpublish-cell-an-sse-answer-and-a-refused-status-both-read ()
  (with-bulkpublish ()
    (bulk-plan-add (bulk-method-p "initialize")
                   (bulk-answers (nlk:json-object "protocolVersion" "2025-06-18")
                                 :content-type "text/event-stream"))
    (bulk-plan-add (bulk-method-p "tools/list") (bulk-answers (bulk-catalogue)))
    (is (search "get_analytics" (bulkpublish:tools)) "a data: line is the reply as much as a JSON body is"))
  (with-bulkpublish ()
    (bulk-plan-add (bulk-method-p "initialize")
                   (lambda (frame) (declare (ignore frame))
                     (values "{\"error\":\"bad key\"}" 401 (bulk-headers nil))))
    (let ((answer (bulk-tools-call)))
      (is (search "ERROR: BULKPUBLISH-ERROR" answer) "a refused status answers a failure, not a signal")
      (is (search "HTTP 401" answer) "with the status")
      (is (search "bad key" answer) "and what the server said"))))

(deftest bulkpublish-cell-a-write-answers-what-it-would-send-until-confirmed ()
  (with-bulkpublish ()
    (bulk-standard-plan)
    (bulk-plan-add (bulk-method-p "tools/call") (bulk-answers (bulk-content "published to x")))
    (let ((answer (bulk-tool-call "publish_post" "arguments" (nlk:json-object "postId" "42"))))
      (is (search "publish_post changes the account" answer) "a write without confirm says so")
      (is (search "{\"postId\":\"42\"}" answer) "and shows the arguments it would send")
      (is (null (bulk-sent-call)) "while nothing reached the server"))
    (is (equal "published to x" (bulk-tool-call "publish_post" "arguments" (nlk:json-object "postId" "42") "confirm" t)) "the same call with confirm sends it")
    (bulk-plan-add (bulk-method-p "tools/call") (bulk-answers (bulk-content "created a draft")))
    (is (equal "created a draft" (bulk-tool-call "create_post" "arguments" (nlk:json-object "content" "hi"))) "a draft is not a write: it goes at once")
    (is (equal "created a draft" (bulk-tool-call "list_channels")) "a read needs no confirm either")))

(deftest bulkpublish-cell-a-tool-result-marked-an-error-is-a-failure ()
  (with-bulkpublish ()
    (bulk-standard-plan)
    (bulk-plan-add (bulk-method-p "tools/call")
                   (bulk-answers (bulk-content "channel 9 is not connected" :is-error t)))
    (let ((answer (bulk-tool-call "get_post" "arguments" (nlk:json-object "postId" "9"))))
      (is (search "ERROR: BULKPUBLISH-ERROR: get_post failed" answer) "an error result is the failure the model repairs from")
      (is (search "channel 9 is not connected" answer) "carrying the server's words"))))

(deftest bulkpublish-cell-a-missing-key-refuses-with-the-way-to-set-it ()
  (with-bulkpublish ("api_key" "")
    (bulk-standard-plan)
    (let ((answer (bulk-tools-call)))
      (is (search "ERROR: BULKPUBLISH-ERROR" answer))
      (is (search "no BulkPublish key" answer) "the refusal names what is missing")
      (is (search "BULKPUBLISH_API_KEY" answer) "and the variable the section names")
      (is (equal 0 (length (bulk-sent))) "nothing was dialled")))
  (with-bulkpublish ("api_key" "" "api_key_env" "HOMEBREW_BULKPUBLISH")
    (let ((message (handler-case (progn (bulkpublish:call "list_channels") "")
                     (error (condition) (princ-to-string condition)))))
      (is (search "HOMEBREW_BULKPUBLISH" message) "another variable's name is the one the verb says"))))

(deftest bulkpublish-cell-an-unanswered-section-refuses-and-names-the-setting ()
  (with-bulkpublish ("endpoint" "")
    (bulk-standard-plan)
    (let ((answer (bulk-tools-call)))
      (is (search "ERROR: BULKPUBLISH-ERROR" answer) "an unanswered endpoint is a failure the model reads")
      (is (search "bulkpublish.endpoint" answer) "naming the setting it misses")
      (is (equal 0 (length (bulk-sent))) "and nothing was dialled"))))

(deftest bulkpublish-cell-the-command-answers-by-hand ()
  (with-bulkpublish ()
    (bulk-standard-plan)
    (bulk-plan-add (bulk-method-p "tools/call")
                   (bulk-answers (bulk-content "2 channels: x, instagram")))
    (multiple-value-bind (text dialog) (nodecode-bulkpublish::run-command "channels" nil)
      (is (search "2 channels: x, instagram" text) "/bulkpublish channels lists the account's")
      (is dialog "and puts its panel beside the answer"))
    (is (search "get_analytics" (nodecode-bulkpublish::run-command "tools" nil)) "/bulkpublish tools lists the catalogue")
    (is (search "post 42" (progn (bulk-plan-add (bulk-method-p "tools/call")
                                                (bulk-answers (bulk-content "post 42 is a draft")))
                                  (nodecode-bulkpublish::run-command "call get_post {\"postId\": \"42\"}" nil))) "/bulkpublish call reads a tool name and a JSON object")
    (is (search "is not a JSON object" (nodecode-bulkpublish::run-command "call get_post nope" nil)))
    (multiple-value-bind (text dialog) (nodecode-bulkpublish::run-command "" nil)
      (is (search "bulkpublish: 15 networks through" text) "bare /bulkpublish says where the server is")
      (is (search "key set" text) "and that a key is set")
      (is dialog))
    (is (search "is not one of its choices" (nodecode-bulkpublish::run-command "spotify x" nil)))))
