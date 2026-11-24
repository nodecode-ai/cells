;;;; cell-test.lisp --- the unbrowse cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Unbrowse endpoint is a stubbed NLK:HTTP and the key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches unbrowse.ai, the environment or the
;;;; operator's config.

(in-package #:nodecode.test)

(define-test-slice "unbrowse" "UNBROWSE-CELL-" :start nodecode-unbrowse:start-cell)

(define-cell-lifecycle-tests "unbrowse"
  (:config "api_key" "ub_live_test")
  (:help :unbrowse)
  (:command "unbrowse")
  (:running
   (is (equal nodecode-unbrowse::+primer+ (nle:help :unbrowse)) "(help :unbrowse) is the primer")
   (is (search "unbrowse:scrape" nodecode-unbrowse::+primer+) "which names the verb a page read goes through"))
  (:refused ("timeout_seconds" "soon"))
  (:idle nodecode-unbrowse:unbrowse-error (nodecode-unbrowse:credits)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro unbrowse-answer (form)
  "FORM as the text it answers, or its refusal's report."
  `(handler-case ,form
     (nodecode-unbrowse:unbrowse-error (condition) (princ-to-string condition))))

(defun unbrowse-rpc-answer (result &optional (status 200))
  "What the hosted MCP answers: one JSON-RPC message carrying RESULT, as JSON text and STATUS."
  (values (nlk:encode-json-object
           (nlk:json-object "jsonrpc" "2.0" "id" 1 "result" result))
          status))

(defun unbrowse-tool-result (text &key error)
  "A tools/call result whose one text part is TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                    :when error "isError" t))

(defun unbrowse-request (content)
  "The JSON-RPC message CONTENT, a request body's text, decoded."
  (nlk:decode-json content))

(defun unbrowse-header (name headers)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

;;; --- one call, on the wire ---------------------------------------------------------

(deftest unbrowse-cell-verbs-are-json-rpc-tool-calls ()
  (with-cell-stop ((unbrowse-start "api_key" "ub_live_test"))
    (let ((asked '()))
      (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
                                  (push (list method url headers content) asked)
                                  (unbrowse-rpc-answer
                                   (unbrowse-tool-result "{\"markdown\":\"# Hi\"}")))
        (is (equal "{\"markdown\":\"# Hi\"}" (nodecode-unbrowse:scrape "https://example.com")) "scrape answers the tool's own text")
        (let* ((call (first asked))
               (auth (unbrowse-header "authorization" (third call)))
               (version (unbrowse-header "mcp-protocol-version" (third call)))
               (body (unbrowse-request (fourth call))))
          (is (equal "POST" (first call)) "one POST per call")
          (is (equal "https://unbrowse.ai/mcp" (second call)) "the hosted endpoint the plugin defaults to")
          (is (equal "Bearer ub_live_test" auth) "the key rides as a bearer header")
          (is (equal "2025-11-25" version) "and the protocol version the streaming path wants")
          (is (equal "tools/call" (nlk:json-value body :string "method")) "one tool call a message")
          (is (equal "unbrowse.scrape" (nlk:json-value body :string "params" "name")) "the tool is the verb's")
          (is (equal "https://example.com" (nlk:json-value body :string "params" "arguments" "url")) "and its keyword is the argument"))
        (nodecode-unbrowse:discover "book a table")
        (let* ((body (unbrowse-request (fourth (first asked))))
               (name (nlk:json-value body :string "params" "name"))
               (query (nlk:json-value body :string "params" "arguments" "query")))
          (is (equal "unbrowse.discover" name) "a verb is the tool it is named after")
          (is (equal "book a table" query) "with its one required field"))))))

(deftest unbrowse-cell-reads-the-sse-path-and-a-tool-error ()
  (with-cell-stop ((unbrowse-start "api_key" "ub_live_test"))
    (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
                                (let ((message (nlk:encode-json-object
                                                (nlk:json-object
                                                 "jsonrpc" "2.0" "id" 1
                                                 "result" (unbrowse-tool-result "hi from sse")))))
                                  (values (format nil "event: message~%data: ~a~c~c" message
                                                  #\Newline #\Newline)
                                          200)))
      (let ((answer (nodecode-unbrowse:scrape "https://example.com")))
        (is (equal "hi from sse" answer) "the last data: event of a stream is the message")))
    (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
                                (unbrowse-rpc-answer
                                 (unbrowse-tool-result "the site blocked the browser" :error t)))
      (let ((answer (unbrowse-answer (nodecode-unbrowse:scrape "https://example.com"))))
        (is (search "the site blocked the browser" answer) "an isError result is a refusal saying what happened")))))

(deftest unbrowse-cell-a-refused-key-names-the-server-code ()
  (with-cell-stop ((unbrowse-start "api_key" "ub_live_expired"))
    (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
                                (let ((error (nlk:json-object
                                              "code" -32001
                                              "message" "invalid token"
                                              "data" (nlk:json-object "code" "unauthorized"))))
                                  (values (nlk:encode-json-object
                                           (nlk:json-object "jsonrpc" "2.0" "id" 1 "error" error))
                                          401)))
      (let ((answer (unbrowse-answer (nodecode-unbrowse:credits))))
        (is (search "invalid token" answer) "the server's message is the refusal")
        (is (search "unauthorized" answer) "with its own code, not the RPC one")
        (is (search "api_key" answer) "and where the operator fixes it")))))

(deftest unbrowse-cell-a-missing-key-refuses-before-any-call ()
  (with-cell-stop ((unbrowse-start))
    (let ((called nil))
      (with-stubbed-fdefinitions
          ((nle::credential-env (name) (declare (ignore name)) nil)
           (nlk:http (method url &key headers content timeout connect-timeout)
             (declare (ignore url headers content timeout connect-timeout))
             (setf called t)
             (values "{}" 200)))
        (let ((answer (unbrowse-answer (nodecode-unbrowse:credits))))
          (is (search "unbrowse.api_key" answer) "the refusal names the section member")
          (is (search "UNBROWSE_API_KEY" answer) "and the variable the plugin read")
          (is (null called) "nothing was dialled"))))))

(deftest unbrowse-cell-the-command-reads-the-balance ()
  (with-cell-stop ((unbrowse-start "api_key" "ub_live_test"))
    (let ((asked '()))
      (with-stubbed-fdefinition (nlk:http (method url &key headers content timeout connect-timeout)
                                  (let* ((body (unbrowse-request content))
                                         (tool (nlk:json-value body :string "params" "name"))
                                         (text (if (equal tool "unbrowse.credits")
                                                   "40 credits left this month"
                                                   "12 verified calls billed")))
                                    (push tool asked)
                                    (unbrowse-rpc-answer (unbrowse-tool-result text))))
        (let ((answer (cell-entry "nodecode-unbrowse" "unbrowse" "status")))
          (is (search "40 credits left this month" answer) "the balance is the answer")
          (is (search "12 verified calls billed" answer) "with the month's usage")
          (is (equal '("unbrowse.credits" "unbrowse.usage") (reverse asked)) "one call each, in that order"))
        (let ((answer (cell-entry "nodecode-unbrowse" "unbrowse" "nope")))
          (is (search "/unbrowse [status|credits|usage|tools]" answer) "an unknown argument teaches what it takes"))))))
