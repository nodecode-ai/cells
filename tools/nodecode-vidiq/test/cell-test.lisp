;;;; cell-test.lisp --- the vidiq cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every vidIQ endpoint is a stubbed NLK:HTTP and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches mcp.vidiq.com, the environment or
;;;; the operator's config.

(in-package #:nodecode.test)

(define-test-slice "vidiq" "VIDIQ-CELL-" :start nodecode-vidiq:start-cell)

(define-cell-lifecycle-tests "vidiq"
  (:config "access_token" "vidiq_test")
  (:help :vidiq)
  (:command "vidiq")
  (:running
   (is (equal nodecode-vidiq::+primer+ (nle:help :vidiq)) "(help :vidiq) is the primer")
   (is (search "vidiq:user-channels" nodecode-vidiq::+primer+) "which names the verb channel work starts from")
   (is (search "vidiq:job-poll" nodecode-vidiq::+primer+) "and the one asynchronous work follows"))
  (:refused ("timeout_seconds" "soon"))
  (:idle nodecode-vidiq:vidiq-error (nodecode-vidiq:balance)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro vidiq-answer (form)
  "FORM as the text it answers, or its refusal's report."
  `(handler-case ,form
     (nodecode-vidiq:vidiq-error (condition) (princ-to-string condition))))

(defun vidiq-rpc-answer (result &optional (status 200) headers)
  "What the hosted MCP answers: one JSON-RPC message carrying RESULT, as JSON text and STATUS."
  (values (nlk:encode-json-object
           (nlk:json-object "jsonrpc" "2.0" "id" 1 "result" result))
          status
          headers))

(defun vidiq-session-headers ()
  "What initialize answers with: the session every later call carries."
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) "sess-42")
    headers))

(defun vidiq-tool-result (text &key error)
  "A tools/call result whose one text part is TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                    :when error "isError" t))

(defun vidiq-request (content)
  "The JSON-RPC message CONTENT, a request body's text, decoded."
  (nlk:decode-json content))

(defun vidiq-header (name headers)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun vidiq-method-of (request)
  "The JSON-RPC method REQUEST asked for, or NIL when it recorded no body."
  (let ((content (fourth request)))
    (and (stringp content) (nlk:json-value (vidiq-request content) :string "method"))))

(defun vidiq-tool-of (request)
  "The tool a tools/call REQUEST named, or NIL for a handshake message."
  (let ((content (fourth request)))
    (and (stringp content) (nlk:json-value (vidiq-request content) :string "params" "name"))))

(defmacro vidiq-answer-all (asked)
  "The whole cell's stub, as the body of an NLK:HTTP replacement: initialize, its ready
notification, and every tool call. ASKED collects (METHOD URL HEADERS CONTENT) per POST."
  `(lambda (method url &key headers content timeout connect-timeout)
     (push (list method url headers content) ,asked)
     (cond
       ((search "\"method\":\"initialize\"" content)
        (vidiq-rpc-answer (nlk:json-object "protocolVersion" "2025-06-18"
                                            "serverInfo" (nlk:json-object "name" "vidiq"))
                           200 (vidiq-session-headers)))
       ((search "\"method\":\"notifications/initialized\"" content)
        (values "" 202 nil))
       (t (vidiq-rpc-answer (vidiq-tool-result "{\"ok\":true}"))))))

;;; --- one call, on the wire ---------------------------------------------------------

(deftest vidiq-cell-a-verb-is-a-json-rpc-tool-call ()
  (with-cell-stop ((vidiq-start "access_token" "vidiq_test"))
    (let ((asked '()))
      (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
                                  (funcall (vidiq-answer-all asked) method url :headers headers :content content))
        (is (equal "{\"ok\":true}" (nodecode-vidiq:channel-videos "UC123" :max-results 25)) "a verb answers the tool's own text")
        (is (equal '("initialize" "notifications/initialized" "tools/call") (mapcar #'vidiq-method-of (reverse asked))) "the handshake once, then the one tool call")
        (let ((call (first asked))
              (headers (third (first asked)))
              (body (vidiq-request (fourth (first asked)))))
          (is (equal "POST" (first call)) "one POST per message")
          (is (equal "https://mcp.vidiq.com/mcp" (second call)) "the hosted endpoint the plugin's .mcp.json carries")
          (is (equal "Bearer vidiq_test" (vidiq-header "authorization" headers)) "the token rides as a bearer header")
          (is (equal "sess-42" (vidiq-header "mcp-session-id" headers)) "and the session initialize answered rides every call after it")
          (is (equal "tools/call" (nlk:json-value body :string "method")) "one tool call a message")
          (is (equal "vidiq_channel_videos" (nlk:json-value body :string "params" "name")) "the tool is the verb's")
          (is (equal "UC123" (nlk:json-value body :string "params" "arguments" "channelId")) "a positional argument is the field the service names")
          (is (equal 25 (nlk:json-value body :integer "params" "arguments" "maxResults")) "and a keyword argument is that field in its camelCase spelling"))
        (nodecode-vidiq:outliers :max-results 10)
        (let* ((body (vidiq-request (fourth (first asked))))
               (name (nlk:json-value body :string "params" "name"))
               (max (nlk:json-value body :integer "params" "arguments" "maxResults")))
          (is (equal "vidiq_outliers" name) "a verb with no required field still names its tool")
          (is (eql 10 max) "and sends the one field it was given"))
        (nodecode-vidiq:videos-by-ids '("a" "b"))
        (let ((filters (nlk:json-value (vidiq-request (fourth (first asked))) :string "params" "arguments" "filters")))
          (is (equal "video==a,b" filters) "several ids become the filters field the service wants"))))))

(deftest vidiq-cell-reads-the-sse-path-and-a-tool-error ()
  (with-cell-stop ((vidiq-start "access_token" "vidiq_test"))
    (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
                                (let ((message (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" 1 "result" (vidiq-tool-result "hi from sse")))))
                                  (values (format nil "event: message~%data: ~a~c~c" message #\Newline #\Newline) 200 nil)))
      (is (equal "hi from sse" (nodecode-vidiq:user-channels)) "the last data: event of a stream is the message"))
    (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
                                (vidiq-rpc-answer (vidiq-tool-result "this channel is not authorized" :error t)))
      (is (search "this channel is not authorized" (vidiq-answer (nodecode-vidiq:channel-analytics "UC123"))) "an isError result is a refusal saying what happened"))))

(deftest vidiq-cell-a-refused-token-names-the-server-code ()
  (with-cell-stop ((vidiq-start "access_token" "vidiq_expired"))
    (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
                                (let ((error (nlk:json-object "code" -32001 "message" "invalid token" "data" (nlk:json-object "code" "unauthorized"))))
                                  (values (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" 1 "error" error)) 401 nil)))
      (let ((answer (vidiq-answer (nodecode-vidiq:balance))))
        (is (search "invalid token" answer) "the server's message is the refusal")
        (is (search "unauthorized" answer) "with its own code, not the RPC one")
        (is (search "access_token" answer) "and where the operator fixes it")))))

(deftest vidiq-cell-a-missing-token-refuses-before-any-call ()
  (with-cell-stop ((vidiq-start))
    (let ((called nil))
      (with-stubbed-fdefinitions
          ((nle::credential-env (name) (declare (ignore name)) nil)
           (nlk:http (method url &key headers content timeout connect-timeout)
             (setf called t)
             (values "{}" 200 nil)))
        (let ((answer (vidiq-answer (nodecode-vidiq:user-channels))))
          (is (search "vidiq.access_token" answer) "the refusal names the section member")
          (is (search "VIDIQ_ACCESS_TOKEN" answer) "and the variable a shell can hold it in")
          (is (null called) "nothing was dialled"))))))

(deftest vidiq-cell-the-command-reads-the-balance ()
  (with-cell-stop ((vidiq-start "access_token" "vidiq_test"))
    (let ((asked '()))
      (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
                                  (push (list method url headers content) asked)
                                  (let* ((body (vidiq-request content))
                                         (tool (nlk:json-value body :string "params" "name"))
                                         (text (cond ((equal tool "vidiq_balance") "40 credits left this month")
                                                     ((equal tool "vidiq_user_channels") "UC1 — one authorized channel")
                                                     (t "[]"))))
                                    (vidiq-rpc-answer (vidiq-tool-result text))))
        (let ((answer (cell-entry "nodecode-vidiq" "vidiq" "status")))
          (is (search "40 credits left this month" answer) "the balance is the answer")
          (is (search "UC1" answer) "with the channels this account authorized"))
        (is (equal '("vidiq_balance" "vidiq_user_channels") (remove nil (mapcar #'vidiq-tool-of (reverse asked)))) "one call each")
        (let ((answer (cell-entry "nodecode-vidiq" "vidiq" "nope")))
          (is (search "/vidiq [status|balance|tools]" answer) "an unknown argument teaches what it takes"))))))
