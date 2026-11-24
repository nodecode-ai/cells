;;;; cell-test.lisp --- lifecycle, the six tools, /stalkchain.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, integer fields under their floors refused, a start with no key
;;;; refused), the MCP lifecycle (meet the server once, echo its session,
;;;; meet it again when it forgets), each tool through the seam the model
;;;; calls it by -- the endpoint, the Bearer header, the JSON-RPC frame and
;;;; the answer -- and /stalkchain with and without an argument. Every
;;;; request is a stubbed NLK:HTTP, so nothing reaches StalkChain or the
;;;; network.

(in-package #:nodecode.test)

;;; --- the door: one stubbed server ---------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defmacro with-stalkchain-door (door &body body)
  "BODY with NLK:HTTP stubbed: DOOR, a function of (METHOD URL ARGS REQUEST)
-- REQUEST the decoded JSON-RPC frame -- answers (values TEXT STATUS HEADERS)."
  `(let ((*requests* '()))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (multiple-value-bind (text status headers)
                                    (funcall ,door method url args
                                             (nlk:decode-json (getf args :content)))
                                  (values text status headers)))
       ,@body)))

(defmacro with-a-calm-door (&body body)
  "BODY with a server no test form should reach: any request is an error."
  `(with-stalkchain-door (lambda (method url args request)
                           (declare (ignore method url args request))
                           (error "unexpected StalkChain request"))
     ,@body))

(defun mcp-frame (request result &key sse)
  "REQUEST's answer, carrying RESULT: one JSON message, or the same in one
server-sent event when SSE."
  (let ((text (nlk:encode-json-object
               (nlk:json-object "jsonrpc" "2.0"
                                "id" (gethash "id" request)
                                "result" result))))
    (if sse
        (format nil "event: message~%data: ~a~%~%" text)
        text)))

(defun tool-content (text)
  "A tools/call result whose one text part is TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))))

(defun tool-failure-content (text)
  "A tools/call result the server marked as an error."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                   "isError" t))

(defun stalkchain-door (&key (content (nlk:json-object "content" #())) (sse nil) (session "s-1"))
  "A handler for the whole MCP lifecycle: initialize issues SESSION, the
notification is a 202, and tools/call answers CONTENT -- a result object, or
a function of the request answering one."
  (lambda (method url args request)
    (declare (ignore method url args))
    (let ((name (nlk:json-value request :string "method")))
      (cond ((equal name "initialize")
             (values (mcp-frame request
                                 (nlk:json-object "protocolVersion" "2025-06-18"
                                                  "serverInfo" (nlk:json-object "name" "stalkchain")))
                     200 (list (cons "mcp-session-id" session))))
            ((equal name "notifications/initialized") (values "" 202 nil))
            ((equal name "tools/call")
             (values (mcp-frame request (if (functionp content) (funcall content request) content)
                                 :sse sse)
                     200 (when sse (list (cons "content-type" "text/event-stream")))))
            (t (error "unexpected method ~a" name))))))

;;; --- reading the stub back -----------------------------------------------------

(defun stalkchain-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun stalkchain-calls ()
  "The tools/call frames sent, oldest first."
  (remove-if-not (lambda (request)
                   (equal "tools/call"
                          (nlk:json-value (nlk:decode-json (getf (cddr request) :content))
                                          :string "method")))
                 (stalkchain-asked)))

(defun stalkchain-body (request)
  "REQUEST's JSON-RPC frame, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun stalkchain-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun stalkchain-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "stalkchain" "STALKCHAIN-CELL-" :start nodecode-stalkchain:start-cell)

(define-cell-lifecycle-tests "stalkchain"
  (:fixture with-a-calm-door)
  (:config "api_key" "t-test")
  (:help :stalkchain)
  (:command "stalkchain")
  (:running (is (nle::find-tool "stalkchain_token_check") "the six tools are registered")
            (is (nle::find-tool "stalkchain_trader_report"))
            (is (nle::find-tool "stalkchain_smart_money"))
            (is (nle::find-tool "stalkchain_wallet"))
            (is (nle::find-tool "stalkchain_alerts"))
            (is (nle::find-tool "stalkchain_tool"))
            (is (equal "https://data.stalkchain.com/mcp/directory" (nodecode-stalkchain::endpoint)) "the default server")
            (is (= 60 (nodecode-stalkchain::setting :timeout-seconds)))
            (is (= 12000 (nodecode-stalkchain::setting :max-chars))))
  (:stopped (is (null (nle::find-tool "stalkchain_token_check")) "stop unregisters the tools")
            (is (null (nle::find-tool "stalkchain_tool"))))
  (:refused ("timeout_seconds" 0) ("max_chars" 5))
  (:idle stalkchain:stalkchain-error (nodecode-stalkchain:call-tool "stalkchain_health" nil)))

(deftest stalkchain-cell-a-key-is-required ()
  (with-a-calm-door
    (with-stubbed-fdefinition (uiop:getenv (variable) (progn variable nil))
      (is-carrying (text (refusal-text nlk:config-refusal (stalkchain-start)))
                   ("stalkchain: no key" "STALKCHAIN_API_KEY"))
      (is (null (nle::find-tool "stalkchain_token_check")) "a refused start installed nothing"))))

;;; --- the lifecycle of the wire ------------------------------------------------

(deftest stalkchain-cell-the-server-is-met-once ()
  (with-stalkchain-door (stalkchain-door :content (tool-content "pong"))
    (with-cell-stop ((stalkchain-start "api_key" "k-1"))
      (is-carrying (text (stalkchain-tool "stalkchain_tool"
                                          (nlk:json-object "tool" "stalkchain_health")))
                   ("pong"))
      (is-carrying (text (stalkchain-tool "stalkchain_tool"
                                          (nlk:json-object "tool" "stalkchain_health")))
                   ("pong"))
      (let* ((asked (stalkchain-asked))
             (methods (map 'list (lambda (request)
                                   (nlk:json-value (stalkchain-body request) :string "method"))
                           asked)))
        (is (= 1 (count "initialize" methods :test #'equal)) "one initialize, then reuse")
        (is (= 1 (count "notifications/initialized" methods :test #'equal)))
        (is (= 2 (length (stalkchain-calls))) "two tools/call frames")
        (let ((call (first (stalkchain-calls))))
          (is (eq :post (first call)))
          (is (equal "https://data.stalkchain.com/mcp/directory" (second call)))
          (is (equal "Bearer k-1" (stalkchain-header call "authorization")) "the key rides the header")
          (is (null (search "k-1" (second call))) "never the url")
          (is (equal "s-1" (stalkchain-header call "mcp-session-id")) "the session is echoed"))))))

(deftest stalkchain-cell-a-forgotten-session-is-met-again ()
  (let ((met 0))
    (with-stalkchain-door
        (lambda (method url args request)
          (declare (ignore method url args))
          (let ((name (nlk:json-value request :string "method")))
            (cond ((equal name "initialize")
                   (values (mcp-frame request (nlk:json-object "protocolVersion" "2025-06-18"))
                           200 (list (cons "mcp-session-id" (format nil "s-~d" (incf met))))))
                  ((equal name "notifications/initialized") (values "" 202 nil))
                  ((equal name "tools/call")
                   (if (= met 1)
                       (values "session gone" 404 nil)
                       (values (mcp-frame request (tool-content "pong")) 200 nil)))
                  (t (error "unexpected method ~a" name)))))
      (with-cell-stop ((stalkchain-start "api_key" "k-1"))
        (is-carrying (text (stalkchain-tool "stalkchain_tool"
                                            (nlk:json-object "tool" "stalkchain_health")))
                     ("pong"))
        (is (= 2 met) "the server was met a second time")))))

;;; --- the tools ------------------------------------------------------------------

(deftest stalkchain-cell-a-token-check-is-one-call ()
  (with-stalkchain-door (stalkchain-door :content (tool-content "{\"holders\":42,\"dev_sold\":true}"))
    (with-cell-stop ((stalkchain-start "api_key" "k-1"))
      (is-carrying (text (stalkchain-tool "stalkchain_token_check"
                                          (nlk:json-object "address" "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263")))
                   ("\"holders\":42")
                   ("<<<EXTERNAL_UNTRUSTED_CONTENT>>>" "the answer is marked as data")
                   ("<<<END_EXTERNAL_UNTRUSTED_CONTENT>>>"))
      (let* ((call (first (stalkchain-calls)))
             (frame (stalkchain-body call))
             (arguments (nlk:json-value frame :object "params" "arguments")))
        (is (= 1 (length (stalkchain-calls))) "one call, not three")
        (is (equal "stalkchain_fomo_analyze_token" (nlk:json-value frame :string "params" "name")) "its name")
        (is (equal "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263"
                   (nlk:json-value arguments :string "address")))
        (is (null (nlk:json-value arguments :string "chain")) "an absent chain is absent, not null")))))

(deftest stalkchain-cell-smart-money-sends-its-window ()
  (with-stalkchain-door (stalkchain-door :content (tool-content "{\"tokens\":[]}"))
    (with-cell-stop ((stalkchain-start "api_key" "k-1"))
      (stalkchain-tool "stalkchain_smart_money"
                       (nlk:json-object "window_minutes" 30 "min_traders" 3))
      (let* ((frame (stalkchain-body (first (stalkchain-calls))))
             (arguments (nlk:json-value frame :object "params" "arguments")))
        (is (equal "stalkchain_fomo_multi_trader_entries" (nlk:json-value frame :string "params" "name")))
        (is (= 30 (nlk:json-value arguments :number "windowMinutes")) "snake_case becomes camelCase")
        (is (= 3 (nlk:json-value arguments :number "minTraders")))
        (is (equal "buy" (nlk:json-value arguments :string "side"))))
      (stalkchain-tool "stalkchain_smart_money" (nlk:json-object))
      (let ((arguments (nlk:json-value (stalkchain-body (second (stalkchain-calls)))
                                        :object "params" "arguments")))
        (is (= 60 (nlk:json-value arguments :number "windowMinutes")) "60 minutes by default")
        (is (= 2 (nlk:json-value arguments :number "minTraders")) "two traders by default")))))

(deftest stalkchain-cell-alerts-map-their-arguments ()
  (with-stalkchain-door (stalkchain-door :content (tool-content "{\"id\":\"a-1\",\"status\":\"active\"}"))
    (with-cell-stop ((stalkchain-start "api_key" "k-1"))
      (is-carrying (text (stalkchain-tool "stalkchain_alerts"
                                          (nlk:json-object "action" "create" "kind" "price_above"
                                                           "token" "mint-1" "price_usd" 1.0d-4
                                                           "channel" "email")))
                   ("a-1")
                   ("active"))
      (let* ((call (first (stalkchain-calls)))
             (frame (stalkchain-body call))
             (sent (getf (cddr call) :content))
             (arguments (nlk:json-value frame :object "params" "arguments"))
             (price (nlk:json-value arguments :number "priceUsd")))
        (is (equal "stalkchain_alert_create" (nlk:json-value frame :string "params" "name")))
        (is (equal "price_above" (nlk:json-value arguments :string "kind")))
        (is (search "\"priceUsd\":" sent) "price_usd rides as priceUsd")
        (is (< (abs (- 1e-4 price)) 1e-8) "a number on the wire, not a string")
        (is (null (search "minTraders" sent)) "an absent member is absent, not null")
        (is (equal "email" (nlk:json-value arguments :string "channel"))))
      (is-carrying (text (stalkchain-tool "stalkchain_alerts"
                                          (nlk:json-object "action" "create" "kind" "price_below"
                                                           "token" "mint-1")))
                   ("ERROR: stalkchain:" "price_below needs price_usd"))
      (stalkchain-tool "stalkchain_alerts" (nlk:json-object "action" "list"))
      (let* ((sent (stalkchain-body (second (stalkchain-calls))))
             (arguments (nlk:json-value sent :object "params" "arguments")))
        (is (equal "stalkchain_alert_list" (nlk:json-value sent :string "params" "name")) "the list tool")
        (is (zerop (hash-table-count arguments)) "a list sends an empty arguments object")))))

(deftest stalkchain-cell-the-generic-tool-passes-its-arguments ()
  (with-stalkchain-door (stalkchain-door :content (tool-content "{\"hits\":[]}"))
    (with-cell-stop ((stalkchain-start "api_key" "k-1"))
      (stalkchain-tool "stalkchain_tool"
                       (nlk:json-object "tool" "stalkchain_fomo_search"
                                        "arguments" (nlk:json-object "q" "bonk" "limit" 3)))
      (let* ((frame (stalkchain-body (first (stalkchain-calls))))
             (arguments (nlk:json-value frame :object "params" "arguments")))
        (is (equal "stalkchain_fomo_search" (nlk:json-value frame :string "params" "name")))
        (is (equal "bonk" (nlk:json-value arguments :string "q")))
        (is (= 3 (nlk:json-value arguments :number "limit")) "a nested object rides whole"))
      (is-carrying (text (stalkchain-tool "stalkchain_tool" (nlk:json-object "tool" "not a tool")))
                   ("ERROR: stalkchain:" "is not a tool name"))
      (is-carrying (text (stalkchain-tool "stalkchain_tool" (nlk:json-object)))
                   ("ERROR: stalkchain:" "tool is required")))))

(deftest stalkchain-cell-a-failed-call-reads-as-an-error ()
  (with-stalkchain-door (stalkchain-door :content (tool-failure-content "no credits left"))
    (with-cell-stop ((stalkchain-start "api_key" "k-1"))
      (is-carrying (text (stalkchain-tool "stalkchain_wallet"
                                          (nlk:json-object "wallet" "0xabc")))
                   ("ERROR: no credits left")))))

(deftest stalkchain-cell-a-streamed-answer-is-read ()
  (with-stalkchain-door (stalkchain-door :content (tool-content "streamed data") :sse t)
    (with-cell-stop ((stalkchain-start "api_key" "k-1"))
      (is-carrying (text (stalkchain-tool "stalkchain_wallet"
                                          (nlk:json-object "wallet" "0xabc")))
                   ("streamed data")))))

;;; --- /stalkchain ------------------------------------------------------------------

(deftest stalkchain-cell-the-command-asks-health-or-searches ()
  (with-stalkchain-door (stalkchain-door :content (tool-content "{\"status\":\"up\",\"traders\":123}"))
    (with-cell-stop ((stalkchain-start "api_key" "k-1"))
      (is-carrying (text (nodecode-stalkchain::run-command "" nil))
                   ("stalkchain:") ("up") ("data.stalkchain.com/mcp/directory"))
      (is-carrying (text (nodecode-stalkchain::run-command "  bonk " nil))
                   ("stalkchain: bonk" "123"))
      (let ((arguments (nlk:json-value (stalkchain-body (second (stalkchain-calls)))
                                        :object "params" "arguments")))
        (is (equal "bonk" (nlk:json-value arguments :string "q")) "the argument is trimmed and searched")))))
