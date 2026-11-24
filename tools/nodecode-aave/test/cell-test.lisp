;;;; cell-test.lisp --- the aave cell against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every MCP exchange is stubbed at *HTTP*: nothing leaves the process and
;;;; the tests read the JSON-RPC envelopes the cell builds. The stub answers
;;;; initialize, the initialized notification, tools/list and tools/call, and
;;;; can answer any of them with a server-sent event stream instead of JSON.

(in-package #:nodecode.test)

(define-test-slice "aave" "AAVE-CELL-" :start nodecode-aave:start-cell)

(define-cell-lifecycle-tests "aave"
  (:help :aave)
  (:command "aave")
  (:running
   (is (nle::find-tool "get_markets") "the market tool is registered")
   (is (nle::find-tool "aave_call") "and the generic one")
   (is (equal nodecode-aave::+primer+ (nle:help :aave)) "(help :aave) is the primer"))
  (:refused ("timeout_seconds" 0))
  (:stopped (is (null (nle::find-tool "get_markets")) "stop takes the tools out")))

;;; --- the wire --------------------------------------------------------------------

(defvar *aave-wire* nil
  "What the stub saw, newest first: (METHOD PARAMS HEADERS).")

(defun aave-headers (&rest names-and-values)
  "NAMES-AND-VALUES as the header table nlk:http really answers with: a hash
 table with dexador's lowercased names, not an alist."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (name value) on names-and-values by #'cddr
          do (setf (gethash (string-downcase name) table) value))
    table))

(defun aave-stub (&key (initialize nil) (call nil) (list nil) (session "s1"))
  "A *HTTP* answering each JSON-RPC method: INITIALIZE, CALL and LIST are
the bodies to answer with; SESSION is the Mcp-Session-Id the handshake hands
back."
  (lambda (method url &key headers content &allow-other-keys)
    (declare (ignore method url))
    (let* ((envelope (shasht:read-json content))
           (name (gethash "method" envelope)))
      (push (list name (gethash "params" envelope) headers) *aave-wire*)
      (cond ((equal name "initialize")
             (values (or initialize (nlk:encode-json-object (aave-initialize-answer))) 200
                     (aave-headers "Mcp-Session-Id" session "Content-Type" "application/json")))
            ((equal name "notifications/initialized") (values "" 202 (aave-headers)))
            ((equal name "tools/call")
             (values (or call (nlk:encode-json-object (aave-tool-answer "{\"data\":{\"apy\":\"3.32\"}}")))
                     200 (aave-headers "Content-Type" "application/json")))
            ((equal name "tools/list")
             (values (or list (nlk:encode-json-object (aave-tools-answer))) 200
                     (aave-headers "Content-Type" "application/json")))
            (t (error "unexpected JSON-RPC method ~a" name))))))

(defmacro with-aave ((&rest keys) &body body)
  "BODY with the cell's HTTP call stubbed by AAVE-STUB on KEYS, and ASKED
the requests it saw, newest first."
  `(let ((*aave-wire* '())
         (nodecode-aave::*http* (aave-stub ,@keys)))
     (symbol-macrolet ((asked *aave-wire*))
       ,@body)))

(defun aave-initialize-answer ()
  (nlk:json-object "jsonrpc" "2.0" "id" 1
                    "result" (nlk:json-object
                              "protocolVersion" "2025-06-18"
                              "capabilities" (nlk:json-object)
                              "serverInfo" (nlk:json-object "name" "aave" "version" "1.0.0"))))

(defun aave-tool-answer (text &key error-p)
  "A tools/call answer whose single content part is TEXT."
  (nlk:json-object "jsonrpc" "2.0" "id" 2
                    "result" (nlk:json-object
                              "content" (vector (nlk:json-object "type" "text" "text" text))
                              "isError" (and error-p t))))

(defun aave-tools-answer (&rest names)
  "A tools/list answer naming NAMES, and one tool this cell does not name."
  (nlk:json-object
   "jsonrpc" "2.0" "id" 3
   "result" (nlk:json-object
             "tools" (coerce (loop for name in (or names '("get_markets" "get_something_new" "preview_action"))
                                    collect (nlk:json-object "name" name
                                                             "description" (format nil "~a, as the server describes it" name)))
                             'vector))))

(defun aave-sse (body)
  "BODY as one server-sent event, the framing a streamable-HTTP server may answer with."
  (format nil "event: message~%data: ~a~%~%" body))

;;; --- calling the cell's own verbs ---------------------------------------------------

(defun aave-call-tool (name &rest pairs)
  "Call the cell's registered tool NAME as the engine does: one decoded object."
  (let ((entry (nle::find-tool name)))
    (unless entry (error "no tool named ~a" name))
    (funcall (third entry) (apply #'nlk:copy-json-object nil pairs))))

(defun aave-request (asked name)
  "The newest request NAME in ASKED (newest first), or NIL."
  (find name asked :key #'first :test #'equal))

(defun aave-sent (asked name)
  "The PARAMS of the newest request NAME in ASKED."
  (second (aave-request asked name)))

(defun aave-refusal (thunk)
  "THUNK's AAVE-ERROR as text, or NIL when it did not refuse."
  (handler-case (progn (funcall thunk) nil)
    (nodecode-aave:aave-error (condition) (princ-to-string condition))))

;;; --- the tools --------------------------------------------------------------------

(deftest aave-cell-a-market-call-round-trips-through-the-wire ()
  (with-aave ()
    (with-cell-stop ((aave-start))
      (let ((answer (aave-call-tool "get_markets" "symbols" (vector "USDC") "chainId" 1)))
        (is (search "3.32" answer) "the text the server returns is the answer")
        (is (equal '("initialize" "notifications/initialized" "tools/call")
                   (mapcar #'first (reverse asked))) "one session opened once, then the call")
        (let ((params (aave-sent asked "tools/call")))
          (is (equal "get_markets" (gethash "name" params)) "the server tool keeps its name")
          (is (equal "USDC" (aref (gethash "symbols" (gethash "arguments" params)) 0)) "the arguments go as the model gave them")
          (is (= 1 (gethash "chainId" (gethash "arguments" params)))))
        (is (equal "s1" (cdr (assoc "mcp-session-id" (third (aave-request asked "tools/call"))
                                     :test #'string-equal))) "the session the server handed back rides every later call")))))

(deftest aave-cell-the-session-and-protocol-header-ride-every-call ()
  (with-aave ()
    (with-cell-stop ((aave-start))
      (aave-call-tool "get_markets")
      (aave-call-tool "get_user_summary" "user" "0xabc")
      (let ((headers (third (aave-request asked "tools/call"))))
        (is (equal "s1" (cdr (assoc "mcp-session-id" headers :test #'string-equal))) "the session id is read out of the header table the wire really hands back")
        (is (equal "2025-06-18" (cdr (assoc "mcp-protocol-version" headers :test #'string-equal))) "and the revision the handshake agreed rides the next call")))))

(deftest aave-cell-a-named-tool-carries-what-the-server-requires ()
  (with-aave ()
    (with-cell-stop ((aave-start))
      (let* ((parameters (getf (second (nle::find-tool "preview_action")) :parameters))
             (required (coerce (gethash "required" parameters) 'list)))
        (is (gethash "sender" (gethash "properties" parameters)) "preview_action names the wallet sender, as the server requires")
        (is (equal '("action" "sender") required) "and marks the two it will not take the call without"))
      (let* ((parameters (getf (second (nle::find-tool "get_repay_with_supply_quote")) :parameters))
             (required (coerce (gethash "required" parameters) 'list)))
        (is (equal '("debtPosition" "repayWithReserve" "amount" "user") required) "the repay quote carries all four of its required arguments")))))

(deftest aave-cell-each-named-tool-forwards-its-own-name ()
  (with-aave ()
    (with-cell-stop ((aave-start))
      (dolist (entry nodecode-aave::*server-tools*)
        (let ((name (first entry)))
          (aave-call-tool name)
          (is (equal name (gethash "name" (aave-sent asked "tools/call")))
              (format nil "~a calls the server's ~a" name name))))
      (is (= 1 (count "initialize" asked :key #'first :test #'equal)) "twelve calls open one session, not twelve"))))

(deftest aave-cell-a-null-argument-is-not-sent ()
  (with-aave ()
    (with-cell-stop ((aave-start))
      (aave-call-tool "get_user_summary" "user" "0xabc" "chainId" nil)
      (let ((arguments (gethash "arguments" (aave-sent asked "tools/call"))))
        (is (equal "0xabc" (gethash "user" arguments)))
        (is (not (nth-value 1 (gethash "chainId" arguments))) "a null member reads as absent")))))

(deftest aave-cell-the-generic-verb-reaches-a-tool-the-cell-does-not-name ()
  (with-aave ()
    (with-cell-stop ((aave-start))
      (aave-call-tool "aave_call" "tool" "get_something_new"
                                  "arguments" (nlk:json-object "user" "0xabc"))
      (let ((params (aave-sent asked "tools/call")))
        (is (equal "get_something_new" (gethash "name" params)) "the named tool is what goes out")
        (is (equal "0xabc" (gethash "user" (gethash "arguments" params))) "and its arguments with it"))
      (is (search "tool" (aave-refusal (lambda () (aave-call-tool "aave_call")))) "naming no tool refuses, and says which member it wants"))))

(deftest aave-cell-a-server-error-result-reads-as-an-error ()
  (with-aave (:call (nlk:encode-json-object
                     (aave-tool-answer "reserve not found on that chain" :error-p t)))
    (with-cell-stop ((aave-start))
      (is (search "ERROR: reserve not found"
                  (aave-call-tool "get_reserve_details" "reserveId" "7")) "isError reaches the model as an error, not as data"))))

(deftest aave-cell-a-json-rpc-error-is-a-refusal ()
  (with-aave (:call (nlk:encode-json-object
                     (nlk:json-object "jsonrpc" "2.0" "id" 2
                                      "error" (nlk:json-object "code" -32602
                                                                "message" "Unknown tool: get_zzz"))))
    (with-cell-stop ((aave-start))
      (let ((refusal (aave-refusal (lambda () (aave-call-tool "aave_call" "tool" "get_zzz")))))
        (is (search "Unknown tool: get_zzz" refusal) "the server's own words reach the operator")))))

(deftest aave-cell-an-sse-answer-reads-the-same-as-json ()
  (with-aave (:initialize (aave-sse (nlk:encode-json-object (aave-initialize-answer)))
              :session "s2")
    (with-cell-stop ((aave-start))
      (is (search "3.32" (aave-call-tool "get_markets")) "a streamed handshake still opens the session")
      (is (equal "s2" (cdr (assoc "mcp-session-id" (third (aave-request asked "tools/call"))
                                   :test #'string-equal))) "and its session id is what the next call carries"))))

(deftest aave-cell-an-endpoint-that-is-not-a-url-refuses ()
  (with-aave ()
    (with-cell-stop ((aave-start "endpoint" "mcp.aave.com"))
      (let ((refusal (aave-refusal (lambda () (aave-call-tool "get_markets")))))
        (is (search "endpoint" refusal) "the refusal names the setting to fix")
        (is (search "https://mcp.aave.com" refusal) "and the value it wants"))
      (is (null (aave-request asked "initialize")) "nothing was dialled"))))

(deftest aave-cell-a-slow-failure-refuses-rather-than-hanging ()
  (with-aave (:initialize (nlk:encode-json-object
                           (nlk:json-object "jsonrpc" "2.0" "id" 1
                                            "error" (nlk:json-object "code" -32603
                                                                      "message" "server restarting"))))
    (with-cell-stop ((aave-start))
      (let ((refusal (aave-refusal (lambda () (aave-call-tool "get_markets")))))
        (is (search "server restarting" refusal) "a failed handshake says why")
        (is (null nodecode-aave::*session*) "and leaves no half-open session behind")))))

;;; --- /aave --------------------------------------------------------------------------

(deftest aave-cell-the-command-reports-and-lists-what-the-server-serves ()
  (with-aave ()
    (with-cell-stop ((aave-start))
      (let ((status (cell-entry "nodecode-aave" "aave" "")))
        (is (search "https://mcp.aave.com" status) "the report names the endpoint")
        (is (search "no session open" status) "and says no call has opened one")
        (is (search "get_markets" status) "and lists the named tools"))
      (let ((listing (cell-entry "nodecode-aave" "aave" "tools")))
        (is (search "get_something_new" listing) "the server's own list is read live")
        (is (search "does not name" listing) "a tool this cell does not name is called out")
        (is (search "aave_call" listing) "and says how to reach it"))
      (aave-call-tool "get_markets")
      (is (search "session open" (cell-entry "nodecode-aave" "aave" "")) "a call opens the session")
      (cell-entry "nodecode-aave" "aave" "forget")
      (is (null nodecode-aave::*session*) "/aave forget closes it"))))

(deftest aave-cell-the-primer-teaches-the-chain-and-never-signs ()
  (is (search "discover -> inspect -> simulate -> build" nodecode-aave::+primer+) "the workflow Aave's skills teach survives as the primer")
  (is (search "preview_action" nodecode-aave::+primer+) "simulate before building")
  (is (search "Nothing is ever signed here" nodecode-aave::+primer+) "and the hand-over is unsigned")
  (is (search "enableCollateral" nodecode-aave::+primer+) "collateral is not assumed")
  (is (search "liquidatable" nodecode-aave::+primer+) "a health factor below 1.0 is named"))
