;;;; cell-test.lisp --- lifecycle, the four tools, /broadband-deals, the wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an endpoint with no scheme or a timeout under its floor refused,
;;;; a start with no endpoint at all refused), the four tools through the seam
;;;; the model calls them by -- the initialize handshake once per start, the
;;;; JSON-RPC method, the tool name and the arguments each call puts on the
;;;; wire, and the text each answers -- an error answer and a refused status
;;;; turned into a repair, an event-stream answer read to its reply, and
;;;; /broadband-deals with and without a postcode. Every exchange is a
;;;; stubbed NLK:HTTP over a string stream, so nothing reaches FindBroadband.

(in-package #:nodecode.test)

;;; --- the stubbed server ---------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS =>
(values STREAM STATUS HEADERS).")

(defmacro with-broadband-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials FindBroadband."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected FindBroadband request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun broadband-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun broadband-request-body (request)
  "REQUEST's JSON-RPC message, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun broadband-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun broadband-headers (type)
  "A response header table carrying content-type TYPE."
  (let ((headers (make-hash-table :test #'equal)))
    (when type (setf (gethash "content-type" headers) type))
    headers))

(defun broadband-answer (body &key (status 200) (type "application/json"))
  "A stubbed answer carrying BODY (an object, or text as it stands)."
  (values (make-string-input-stream (if (stringp body) body (nlk:encode-json-object body)))
          status
          (broadband-headers type)))

(defun broadband-serve (&key (tool-text "{}") is-error (type "application/json")
                          (status 200) (raw nil) session)
  "A stubbed endpoint: the initialize handshake, then one tools/call answered
with TOOL-TEXT (or RAW), as TYPE at STATUS. SESSION, when given, is the
Mcp-Session-Id its initialize issues."
  (lambda (method url args)
    (declare (ignore method url))
    (let* ((body (nlk:decode-json (getf args :content)))
           (rpc (nlk:json-value body :string "method"))
           (id (gethash "id" body)))
      (cond
        ((equal rpc "initialize")
         (multiple-value-bind (stream code headers)
             (broadband-answer (nlk:json-object "jsonrpc" "2.0" "id" id
                                                 "result" (nlk:json-object
                                                           "protocolVersion" "2025-06-18"
                                                           "capabilities" (nlk:json-object)
                                                           "serverInfo" (nlk:json-object "name" "findbroadband"))))
           (when session
             (setf (gethash "mcp-session-id" headers) session))
           (values stream code headers)))
        ((equal rpc "notifications/initialized")
         (values (make-string-input-stream "") 202 (broadband-headers nil)))
        ((equal rpc "tools/call")
         (let ((reply (nlk:json-object
                       "jsonrpc" "2.0" "id" id
                       "result" (nlk:json-object
                                 "content" (vector (nlk:json-object "type" "text"
                                                                    "text" (or raw tool-text)))
                                 "isError" (and is-error t)))))
           (if (equal type "text/event-stream")
               (broadband-answer (format nil "event: message~%data: ~a~%~%"
                                         (nlk:encode-json-object reply))
                                 :status status :type type)
               (broadband-answer (or raw reply) :status status :type type))))
        (t (error "unexpected MCP method ~a" rpc))))))

(defun broadband-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as the model
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defun broadband-tool-call ()
  "The last tools/call the cell made, its params decoded."
  (let ((request (car (last (broadband-asked)))))
    (nlk:json-value (broadband-request-body request) :object "params")))

(defun broadband-arguments ()
  "The arguments object of the last tools/call."
  (nlk:json-value (broadband-tool-call) :object "arguments"))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "broadband-deals" "BROADBAND-DEALS-CELL-"
  :start nodecode-broadband-deals:start-cell)

(define-cell-lifecycle-tests "broadband-deals"
  (:fixture with-broadband-wire)
  (:config "endpoint" "https://example.test/mcp")
  (:help :broadband-deals)
  (:command "broadband-deals")
  (:running (is (nle::find-tool "findbroadband_addresses") "findbroadband_addresses is registered")
            (is (nle::find-tool "findbroadband_deals") "findbroadband_deals is registered")
            (is (nle::find-tool "findbroadband_overpaying") "findbroadband_overpaying is registered")
            (is (nle::find-tool "findbroadband_deal") "findbroadband_deal is registered")
            (is (equal "https://example.test/mcp" (nodecode-broadband-deals::setting :endpoint)) "the endpoint the section wrote")
            (is (= 45 (nodecode-broadband-deals::setting :timeout-seconds)) "the default ceiling"))
  (:stopped (is (null (nle::find-tool "findbroadband_deals")) "stop unregisters the tools")
            (is (null (nle::find-tool "findbroadband_overpaying")) "all four of them"))
  (:refused ("endpoint" "example.test/mcp") ("endpoint" "ftp://example.test/mcp")
            ("timeout_seconds" 0))
  (:idle nodecode-broadband-deals:broadband-deals-error
         (nodecode-broadband-deals::run-command "" nil)))

(deftest broadband-deals-cell-a-missing-endpoint-refuses-the-start ()
  (with-broadband-wire
    (let ((text (refusal-text nlk:config-refusal
                               (nodecode-broadband-deals:start-cell
                                (nlk:json-object "broadband-deals" (nlk:json-object))))))
      (is (search "endpoint" text) "the refusal names the setting to add"))
    (is (null (nle::find-tool "findbroadband_deals")) "a refused start installed nothing")))

;;; --- the handshake and findbroadband_addresses ---------------------------------

(deftest broadband-deals-cell-addresses-initialises-once-and-passes-the-answer ()
  (with-broadband-wire
    (with-cell-stop ((broadband-deals-start "endpoint" "https://example.test/mcp/"))
      (let ((server-words "{\"postcode\":\"SS13 1HJ\",\"addresses\":[{\"uprn\":\"100090245041\",\"display_address\":\"1 Fyfields, Basildon\"}],\"count\":1}"))
        (setf *answer-fn* (broadband-serve :tool-text server-words))
        (let ((answer (broadband-tool "findbroadband_addresses" (nlk:json-object "postcode" "ss131hj"))))
          (is (equal server-words answer) "the server's own text is the answer"))
        (let ((asked (broadband-asked)))
          (is (= 3 (length asked)) "initialize, the initialized notification, then the call")
          (is (equal "initialize" (nlk:json-value (broadband-request-body (first asked)) :string "method")))
          (is (equal "notifications/initialized"
                     (nlk:json-value (broadband-request-body (second asked)) :string "method")))
          (is (equal "https://example.test/mcp" (second (third asked))) "the endpoint, its trailing slash trimmed")
          (is (equal "application/json, text/event-stream"
                     (broadband-request-header (third asked) "accept")) "the answer may come as JSON or as an event stream"))
        (is (equal "search_addresses" (nlk:json-value (broadband-tool-call) :string "name")) "the server's own tool name")
        (is (equal "SS13 1HJ" (nlk:json-value (broadband-arguments) :string "postcode")) "a postcode typed without its space is written as the server expects")
        (broadband-tool "findbroadband_addresses" (nlk:json-object "postcode" "SS13 1HJ"))
        (is (= 4 (length (broadband-asked))) "a second call does not initialise again")))))

;;; --- findbroadband_deals -------------------------------------------------------

(deftest broadband-deals-cell-deals-sends-only-the-filters-named ()
  (with-broadband-wire
    (with-cell-stop ((broadband-deals-start "endpoint" "https://example.test/mcp"))
      (setf *answer-fn* (broadband-serve :tool-text "{\"precision\":\"address\",\"deals\":[]}"))
      (broadband-tool "findbroadband_deals"
                      (nlk:json-object "postcode" "SS13 1HJ" "uprn" "100090245041"
                                       "address_label" "1 Fyfields, Basildon"
                                       "max_monthly_price" 30 "min_speed_mbps" 150
                                       "exclude_mid_contract_rises" t "include_satellite" t))
      (let ((arguments (broadband-arguments)))
        (is (= 30 (nlk:json-value arguments :number "max_monthly_price")) "the budget cap, as a number")
        (is (= 150 (nlk:json-value arguments :number "min_speed_mbps")) "the speed floor")
        (is (eq t (nlk:json-value arguments :boolean "exclude_mid_contract_rises")) "the rise switch")
        (is (eq t (nlk:json-value arguments :boolean "include_satellite")) "the satellite switch")
        (is (equal "100090245041" (nlk:json-value arguments :string "uprn")) "the property")
        (is (null (nlk:json-value arguments :boolean "include_mobile_broadband")) "a switch the call did not set is off")
        (is (null (nth-value 1 (gethash "provider" arguments))) "a filter the call did not name is left off the wire")))))

;;; --- findbroadband_overpaying and findbroadband_deal ---------------------------

(deftest broadband-deals-cell-overpaying-calls-the-server-tool ()
  (with-broadband-wire
    (with-cell-stop ((broadband-deals-start "endpoint" "https://example.test/mcp"))
      (setf *answer-fn* (broadband-serve :tool-text "{\"verdict\":\"overpaying\"}"))
      (let ((answer (broadband-tool "findbroadband_overpaying"
                                     (nlk:json-object "postcode" "SS13 1HJ"
                                                      "monthly_bill" 44
                                                      "current_speed_mbps" 100
                                                      "current_provider" "Sky"
                                                      "quote_type" "renewal_offer"))))
        (is (search "overpaying" answer) "the server's verdict is the answer"))
      (is (equal "check_if_overpaying" (nlk:json-value (broadband-tool-call) :string "name")))
      (let ((arguments (broadband-arguments)))
        (is (= 44 (nlk:json-value arguments :number "monthly_bill")))
        (is (equal "renewal_offer" (nlk:json-value arguments :string "quote_type"))))
      (setf *answer-fn* (broadband-serve :tool-text "{\"deal_id\":\"65571\",\"provider\":\"Example Fibre\"}"))
      (is (search "65571" (broadband-tool "findbroadband_deal"
                                           (nlk:json-object "deal_id" "65571" "postcode" "SS13 1HJ"))) "the deal's terms are the answer")
      (is (equal "get_deal_details" (nlk:json-value (broadband-tool-call) :string "name")))
      (is (equal "SS13 1HJ" (nlk:json-value (broadband-arguments) :string "postcode")) "the postcode travels with the deal id, for availability"))))

;;; --- what a wire can answer ----------------------------------------------------

(deftest broadband-deals-cell-an-error-answer-is-a-repair-not-a-crash ()
  (with-broadband-wire
    (with-cell-stop ((broadband-deals-start "endpoint" "https://example.test/mcp"))
      (setf *answer-fn* (broadband-serve :tool-text "That postcode is not valid." :is-error t))
      (is-carrying (text (broadband-tool "findbroadband_deals" (nlk:json-object "postcode" "XX1 1XX")))
                   ("ERROR") ("That postcode is not valid.")))))

(deftest broadband-deals-cell-a-refused-status-carries-the-servers-words ()
  (with-broadband-wire
    (with-cell-stop ((broadband-deals-start "endpoint" "https://example.test/mcp"))
      (setf *answer-fn* (broadband-serve :raw "upstream is down" :status 503
                                         :type "text/plain"))
      (is-carrying (text (broadband-tool "findbroadband_deals" (nlk:json-object "postcode" "SS13 1HJ")))
                   ("HTTP 503") ("upstream is down")))))

(deftest broadband-deals-cell-an-event-stream-answer-is-read-to-its-reply ()
  (with-broadband-wire
    (with-cell-stop ((broadband-deals-start "endpoint" "https://example.test/mcp"))
      (setf *answer-fn* (broadband-serve :tool-text "{\"precision\":\"postcode\"}"
                                         :type "text/event-stream"))
      (is (search "precision" (broadband-tool "findbroadband_deals"
                                              (nlk:json-object "postcode" "SS13 1HJ"))) "the reply in the stream is the answer"))))

(deftest broadband-deals-cell-a-session-id-is-echoed-on-every-later-call ()
  (with-broadband-wire
    (with-cell-stop ((broadband-deals-start "endpoint" "https://example.test/mcp"))
      (setf *answer-fn* (broadband-serve :tool-text "{}" :session "s-42"))
      (broadband-tool "findbroadband_deals" (nlk:json-object "postcode" "SS13 1HJ"))
      (broadband-tool "findbroadband_deals" (nlk:json-object "postcode" "SS13 1HJ"))
      (let ((asked (broadband-asked)))
        (is (null (broadband-request-header (first asked) "mcp-session-id")) "the first request has no session yet")
        (is (equal "s-42" (broadband-request-header (third asked) "mcp-session-id")) "the call after the handshake carries the session it issued")
        (is (equal "s-42" (broadband-request-header (fourth asked) "mcp-session-id")))))))

;;; --- /broadband-deals ----------------------------------------------------------

(deftest broadband-deals-cell-the-command-answers-and-looks-up ()
  (with-broadband-wire
    (with-cell-stop ((broadband-deals-start "endpoint" "https://example.test/mcp"))
      (is-carrying (status (nodecode-broadband-deals::run-command "" nil))
                   ("https://example.test/mcp") ("no key"))
      (setf *answer-fn* (broadband-serve :tool-text "{\"precision\":\"postcode\"}"))
      (is (search "precision" (nodecode-broadband-deals::run-command "SS13 1HJ" nil)) "a postcode alone looks the deals up")
      (is (equal "find_broadband_deals" (nlk:json-value (broadband-tool-call) :string "name")))
      (nodecode-broadband-deals::run-command "addresses SS13 1HJ" nil)
      (is (equal "search_addresses" (nlk:json-value (broadband-tool-call) :string "name")) "the addresses word asks for the address list instead"))))
