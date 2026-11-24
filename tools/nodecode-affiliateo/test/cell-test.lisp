;;;; cell-test.lisp --- the affiliateo cell against a stubbed MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP and the API key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Affiliateo, the environment or the
;;;; operator's files. Each verb is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape. The stub answers MCP's
;;;; streamable HTTP the way the server does: an initialize that hands out a
;;;; session id, a notification that answers 202, tools/list and tools/call,
;;;; either as bare JSON or as a one-event stream.

(in-package #:nodecode.test)

(define-test-slice "affiliateo" "AFFILIATEO-CELL-")

(defun affiliateo-config (&rest pairs)
  "A whole shared config holding the `affiliateo' section PAIRS make, with the
API key variable defaulted."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "api_key_env" section "AFFILIATEO_API_KEY")
    (nlk:json-object "affiliateo" section)))

(defun affiliateo-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-affiliateo:start-cell (apply #'affiliateo-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-afk-key (&body body)
  "BODY with the API key environment answering one fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "AFFILIATEO_API_KEY") "afk_test"))
     ,@body))

(defun afk-headers (session)
  "The response headers one MCP answer carries: JSON, and SESSION when it hands one."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "content-type" table) "application/json")
    (when session (setf (gethash "mcp-session-id" table) session))
    table))

(defvar *afk-stream* nil
  "When true, the stub answers tools/list as a one-event stream.")

(defvar *afk-init-status* 200
  "The status the stub's initialize answers: 200, or a refusal to look at.")

(defun afk-rpc (result)
  "One JSON-RPC 2.0 answer carrying RESULT."
  (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" 1 "result" result)))

(defun afk-error (message)
  "One JSON-RPC 2.0 error object carrying MESSAGE."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" 1
                     "error" (nlk:json-object "code" -32602 "message" message))))

(defun afk-tools-answer ()
  "The catalog the stub's tools/list answers: three tools, one of them a write."
  (afk-rpc
   (nlk:json-object
    "tools"
    (vector
     (nlk:json-object "name" "list_businesses"
                       "description" "List every business this key can see, with its apps and connected ad networks."
                       "inputSchema" (nlk:json-object "type" "object"
                                                        "properties" (nlk:make-json-object))
                       "annotations" (nlk:json-object "readOnlyHint" t))
     (nlk:json-object "name" "get_ad_stats"
                       "description" "Ad results for one account and date range."
                       "inputSchema" (nlk:json-object
                                       "type" "object"
                                       "properties" (nlk:json-object
                                                     "slug" (nlk:json-object "type" "string"))
                                       "required" (vector "slug"))
                       "annotations" (nlk:json-object "readOnlyHint" t))
     (nlk:json-object "name" "pay_affiliates"
                       "description" "Pay every affiliate whose money is Payable, from the wallet."
                       "inputSchema" (nlk:json-object
                                       "type" "object"
                                       "properties" (nlk:json-object
                                                     "slug" (nlk:json-object "type" "string")))
                       "annotations" (nlk:json-object "readOnlyHint" nil))))))

(defun afk-result-answer (text &key errorp)
  "One tools/call answer carrying TEXT, marked a tool error when ERRORP."
  (afk-rpc (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                             "isError" (and errorp t))))

(defmacro with-afk-wire ((asked &key (session "sess-1")) &body body)
  "BODY with NLK:HTTP answering as Affiliateo's MCP server does. ASKED collects
(METHOD HEADERS CONTENT), oldest first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '())
           (*afk-stream* nil)
           (*afk-init-status* 200))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (declare (ignore method url))
                                   (let* ((content (getf options :content))
                                          (body (ignore-errors (nlk:decode-json content)))
                                          (rpc (and (hash-table-p body)
                                                    (nlk:json-value body :string "method"))))
                                     (push (list rpc (getf options :headers) content) ,log)
                                     (cond
                                       ((equal rpc "initialize")
                                        (if (equal *afk-init-status* 200)
                                            (values (afk-rpc (nlk:json-object
                                                              "protocolVersion" "2025-06-18"
                                                              "serverInfo" (nlk:json-object "name" "affiliateo")))
                                                    200 (afk-headers ,session))
                                            (values (afk-error "Unauthorized") *afk-init-status*
                                                    (afk-headers nil))))
                                       ((equal rpc "notifications/initialized")
                                        (values "" 202 (afk-headers nil)))
                                       ((equal rpc "tools/list")
                                        (values (if *afk-stream*
                                                    (format nil "event: message~%data: ~a~%~%"
                                                            (afk-tools-answer))
                                                    (afk-tools-answer))
                                                200 (afk-headers nil)))
                                       ((equal rpc "tools/call")
                                        (let* ((params (nlk:json-value body :object "params"))
                                               (name (nlk:json-value params :string "name")))
                                          (cond
                                            ((equal name "list_businesses")
                                             (values (afk-result-answer
                                                      "{\"businesses\":[{\"slug\":\"acme\",\"apps\":[{\"id\":7}]}]}")
                                                     200 (afk-headers nil)))
                                            ((equal name "get_ad_stats")
                                             (values (afk-result-answer "spend 12.50 EUR, revenue 40.00 EUR")
                                                     200 (afk-headers nil)))
                                            ((equal name "pay_affiliates")
                                             (values (afk-result-answer
                                                      "the wallet is short by 250 cents, so nothing was paid"
                                                      :errorp t)
                                                     200 (afk-headers nil)))
                                            (t (values (afk-error "Unknown tool")
                                                       200 (afk-headers nil))))))
                                       (t (error "unexpected MCP method ~a" rpc)))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun afk-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun afk-header (headers name)
  "NAME's value in one recorded request HEADERS, or NIL."
  (cdr (assoc name headers :test #'equal)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "affiliateo"
  (:config "api_key_env" "AFFILIATEO_API_KEY")
  (:help :affiliateo)
  (:fixture with-afk-key)
  (:refused ("url" "http://affiliateo.com/api/mcp")
            ("timeout" 1)))

(deftest affiliateo-cell-without-a-key-refuses ()
  ;; The one setting no verb can work without: the section names an API key, and
  ;; a section that names none is refused at start, before anything installs.
  (with-afk-key
    (let ((condition (signals-error nlk:config-refusal
                                    (affiliateo-start "api_key_env" ""))))
      (is (search "no API key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "affiliateo_call")) "and installs nothing"))))

(deftest affiliateo-cell-refuses-a-foreign-host (with-afk-key)
  ;; The key may only travel to Affiliateo's own origin.
  (is (signals-error nlk:config-refusal
                      (affiliateo-start "url" "https://haystack.example.com/mcp"))))

;;; --- affiliateo_tools -----------------------------------------------------------------

(deftest affiliateo-cell-lists-its-tools (with-afk-key)
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (let ((text (afk-answer "afk-list-1" "affiliateo_tools" "{}")))
        (is (search "list_businesses - List every business" text) "a tool and what it does")
        (is (search "pay_affiliates" text) "and the write tool too")
        (is (equal '("initialize" "notifications/initialized" "tools/list")
                   (mapcar #'first asked)) "signs in, says so, then reads the catalog")
        (is (equal "Bearer afk_test" (afk-header (second (first asked)) "authorization")) "the key rides the Authorization header")
        (is (equal "sess-1" (afk-header (second (third asked)) "mcp-session-id")) "and the session the server handed back rides every request after it")))))

(deftest affiliateo-cell-filters-the-catalog (with-afk-key)
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (let ((text (afk-answer "afk-list-2" "affiliateo_tools" "{\"filter\": \"AD\"}")))
        (is (search "get_ad_stats" text) "the filter finds a name")
        (is (not (search "pay_affiliates" text)) "and hides the rest")))))

(deftest affiliateo-cell-describes-one-tool (with-afk-key)
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (let ((text (afk-answer "afk-list-3" "affiliateo_tools"
                               "{\"tool\": \"get_ad_stats\"}")))
        (is (search "get_ad_stats" text) "the tool answers")
        (is (search "arguments: " text) "with its arguments")
        (is (search "\"slug\"" text) "as the schema the server published")
        (is (search "read only" text) "and says it changes nothing")))))

(deftest affiliateo-cell-reads-a-streamed-answer (with-afk-key)
  ;; A server may answer tools/list as a one-event stream; the object is the data line.
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (setf *afk-stream* t)
      (is (search "list_businesses" (afk-answer "afk-list-4" "affiliateo_tools" "{}"))))))

(deftest affiliateo-cell-says-when-a-tool-is-unknown (with-afk-key)
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (is (search "No Affiliateo tool is named"
                  (afk-answer "afk-list-5" "affiliateo_tools"
                               "{\"tool\": \"list_busineses\"}"))))))

;;; --- affiliateo_call ------------------------------------------------------------------

(deftest affiliateo-cell-calls-a-tool (with-afk-key)
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (let ((text (afk-answer "afk-call-1" "affiliateo_call"
                               "{\"tool\": \"list_businesses\", \"arguments\": {}}")))
        (is (search "acme" text) "the server's answer comes back whole")
        (is (equal "tools/call" (first (car (last asked)))) "one tools/call was made")
        (is (search "\"list_businesses\"" (third (car (last asked)))) "naming the tool the model asked for")))))

(deftest affiliateo-cell-passes-the-arguments (with-afk-key)
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (is (search "spend 12.50 EUR"
                  (afk-answer "afk-call-3" "affiliateo_call"
                               "{\"tool\": \"get_ad_stats\", \"arguments\": {\"slug\": \"acme\"}}")))
      (is (search "\"slug\":\"acme\"" (third (car (last asked)))) "the arguments reach the server"))))

(deftest affiliateo-cell-reports-a-tool-error (with-afk-key)
  ;; A tool that answers isError true is a failed call, not an answer.
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (let ((text (afk-answer "afk-call-4" "affiliateo_call"
                               "{\"tool\": \"pay_affiliates\", \"arguments\": {\"slug\": \"acme\"}}")))
        (is (uiop:string-prefix-p "ERROR: " text) "the call failed")
        (is (search "wallet is short" text) "and the server's words are the answer")))))

(deftest affiliateo-cell-refuses-a-call-without-a-tool (with-afk-key)
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (let ((text (afk-answer "afk-call-5" "affiliateo_call" "{}")))
        (is (uiop:string-prefix-p "ERROR: " text) "a call names its tool or fails")
        (is (search "tool" (string-downcase text)) "the refusal names what is missing")
        (is (null asked) "before anything reaches the server")))))

(deftest affiliateo-cell-refuses-a-dead-key (with-afk-key)
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (setf *afk-init-status* 401)
      (let ((text (afk-answer "afk-call-6" "affiliateo_tools" "{}")))
        (is (uiop:string-prefix-p "ERROR: " text) "a refused key is a failed call")
        (is (search "API tab" text) "naming where a new key is made")
        (is (search "afk_" text) "and what it looks like")))))

;;; --- the operator's command ------------------------------------------------------------

(deftest affiliateo-cell-command-answers-status (with-afk-key)
  (with-cell-stop ((affiliateo-start))
    (with-afk-wire (asked)
      (let ((text (nodecode-affiliateo::run-command "" "s1")))
        (is (search "nodecode-affiliateo" text) "the command names the cell")
        (is (search "https://affiliateo.com/api/mcp" text) "and the server it talks to")
        (is (search "AFFILIATEO_API_KEY" text) "and where the key comes from")
        (is (null asked) "without dialling anything")))))
