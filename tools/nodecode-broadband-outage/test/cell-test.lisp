;;;; cell-test.lisp --- lifecycle, the three tools, /broadband-outage, the wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an endpoint with no scheme or a timeout under its floor refused,
;;;; a start with no endpoint at all refused), the three tools through the
;;;; seam the model calls them by -- the initialize handshake once per start,
;;;; the JSON-RPC method, the tool name and the arguments each call puts on
;;;; the wire, and the text each answers -- the day range and the date shape
;;;; held before a call, an error answer and a refused status turned into a
;;;; repair, an event-stream answer read to its reply, and /broadband-outage
;;;; with and without a provider. Every exchange is a stubbed NLK:HTTP over a
;;;; string stream, so nothing reaches FindBroadband.

(in-package #:nodecode.test)

;;; --- the stubbed server ---------------------------------------------------------

(defvar *outage-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *outage-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS =>
(values STREAM STATUS HEADERS).")

(defmacro with-outage-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *OUTAGE-REQUESTS* and
answered by *OUTAGE-ANSWER-FN*, so no test dials FindBroadband."
  `(let ((*outage-requests* '())
         (*outage-answer-fn* (lambda (method url args)
                                (declare (ignore method url args))
                                (error "unexpected FindBroadband request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *outage-requests*)
                                (funcall *outage-answer-fn* method url args))
       ,@body)))

(defun outage-asked ()
  "The stubbed requests, oldest first."
  (reverse *outage-requests*))

(defun outage-request-body (request)
  "REQUEST's JSON-RPC message, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun outage-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun outage-headers (type)
  "A response header table carrying content-type TYPE."
  (let ((headers (make-hash-table :test #'equal)))
    (when type (setf (gethash "content-type" headers) type))
    headers))

(defun outage-answer (body &key (status 200) (type "application/json"))
  "A stubbed answer carrying BODY (an object, or text as it stands)."
  (values (make-string-input-stream (if (stringp body) body (nlk:encode-json-object body)))
          status
          (outage-headers type)))

(defun outage-serve (&key (tool-text "{}") is-error (type "application/json")
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
             (outage-answer (nlk:json-object "jsonrpc" "2.0" "id" id
                                              "result" (nlk:json-object
                                                        "protocolVersion" "2025-06-18"
                                                        "capabilities" (nlk:json-object)
                                                        "serverInfo" (nlk:json-object "name" "findbroadband-outages"))))
           (when session
             (setf (gethash "mcp-session-id" headers) session))
           (values stream code headers)))
        ((equal rpc "notifications/initialized")
         (values (make-string-input-stream "") 202 (outage-headers nil)))
        ((equal rpc "tools/call")
         (let ((reply (nlk:json-object
                       "jsonrpc" "2.0" "id" id
                       "result" (nlk:json-object
                                 "content" (vector (nlk:json-object "type" "text"
                                                                    "text" (or raw tool-text)))
                                 "isError" (and is-error t)))))
           (if (equal type "text/event-stream")
               (outage-answer (format nil "event: message~%data: ~a~%~%"
                                      (nlk:encode-json-object reply))
                              :status status :type type)
               (outage-answer (or raw reply) :status status :type type))))
        (t (error "unexpected MCP method ~a" rpc))))))

(defun outage-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as the model
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defun outage-tool-call ()
  "The last tools/call the cell made, its params decoded."
  (let ((request (car (last (outage-asked)))))
    (nlk:json-value (outage-request-body request) :object "params")))

(defun outage-arguments ()
  "The arguments object of the last tools/call."
  (nlk:json-value (outage-tool-call) :object "arguments"))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "broadband-outage" "BROADBAND-OUTAGE-CELL-"
  :start nodecode-broadband-outage:start-cell)

(define-cell-lifecycle-tests "broadband-outage"
  (:fixture with-outage-wire)
  (:config "endpoint" "https://example.test/mcp")
  (:help :broadband-outage)
  (:command "broadband-outage")
  (:running (is (nle::find-tool "findbroadband_outage_overview") "findbroadband_outage_overview is registered")
            (is (nle::find-tool "findbroadband_outage_status") "findbroadband_outage_status is registered")
            (is (nle::find-tool "findbroadband_outage_history") "findbroadband_outage_history is registered")
            (is (equal "https://example.test/mcp" (nodecode-broadband-outage::setting :endpoint)) "the endpoint the section wrote")
            (is (= 45 (nodecode-broadband-outage::setting :timeout-seconds)) "the default ceiling"))
  (:stopped (is (null (nle::find-tool "findbroadband_outage_status")) "stop unregisters the tools")
            (is (null (nle::find-tool "findbroadband_outage_history")) "all three of them"))
  (:refused ("endpoint" "example.test/mcp") ("endpoint" "ftp://example.test/mcp")
            ("timeout_seconds" 0))
  (:idle nodecode-broadband-outage:broadband-outage-error
         (nodecode-broadband-outage::run-command "" nil)))

(deftest broadband-outage-cell-a-missing-endpoint-refuses-the-start ()
  (with-outage-wire
    (let ((text (refusal-text nlk:config-refusal
                               (nodecode-broadband-outage:start-cell
                                (nlk:json-object "broadband-outage" (nlk:json-object))))))
      (is (search "endpoint" text) "the refusal names the setting to add"))
    (is (null (nle::find-tool "findbroadband_outage_status")) "a refused start installed nothing")))

;;; --- the handshake and findbroadband_outage_overview ---------------------------

(deftest broadband-outage-cell-overview-initialises-once-and-passes-the-answer ()
  (with-outage-wire
    (with-cell-stop ((broadband-outage-start "endpoint" "https://example.test/mcp/"))
      (let ((server-words "{\"headline\":\"8 UK providers are down for some users.\",\"down_count\":8}"))
        (setf *outage-answer-fn* (outage-serve :tool-text server-words))
        (let ((answer (outage-tool "findbroadband_outage_overview" (nlk:json-object))))
          (is (equal server-words answer) "the server's own text is the answer"))
        (let ((asked (outage-asked)))
          (is (= 3 (length asked)) "initialize, the initialized notification, then the call")
          (is (equal "initialize" (nlk:json-value (outage-request-body (first asked)) :string "method")))
          (is (equal "notifications/initialized"
                     (nlk:json-value (outage-request-body (second asked)) :string "method")))
          (is (equal "https://example.test/mcp" (second (third asked))) "the endpoint, its trailing slash trimmed")
          (is (equal "application/json, text/event-stream"
                     (outage-request-header (third asked) "accept")) "the answer may come as JSON or as an event stream"))
        (is (equal "get_outage_overview" (nlk:json-value (outage-tool-call) :string "name")) "the server's own tool name")
        (is (zerop (hash-table-count (outage-arguments))) "no arguments, as the server's own schema declares")
        (outage-tool "findbroadband_outage_overview" (nlk:json-object))
        (is (= 4 (length (outage-asked))) "a second call does not initialise again")))))

;;; --- findbroadband_outage_status -----------------------------------------------

(deftest broadband-outage-cell-status-asks-for-the-provider-as-said ()
  (with-outage-wire
    (with-cell-stop ((broadband-outage-start "endpoint" "https://example.test/mcp"))
      (setf *outage-answer-fn*
            (outage-serve :tool-text "{\"provider\":\"bt\",\"status\":\"Down\",\"headline\":\"BT is down for some users right now\"}"))
      (let ((answer (outage-tool "findbroadband_outage_status"
                                  (nlk:json-object "provider" "  BT  "))))
        (is (search "BT is down" answer) "the server's own words are the answer"))
      (is (equal "get_provider_outage_status" (nlk:json-value (outage-tool-call) :string "name")))
      (is (equal "BT" (nlk:json-value (outage-arguments) :string "provider")) "the name, trimmed")
      (let ((asked (length (outage-asked))))
        (is-carrying (text (outage-tool "findbroadband_outage_status"
                                         (nlk:json-object "provider" (make-string 61 :initial-element #\x))))
                     ("ERROR") ("longer than the 60"))
        (is (= asked (length (outage-asked))) "a name past the server's own length never leaves the machine")))))

;;; --- findbroadband_outage_history -----------------------------------------------

(deftest broadband-outage-cell-history-sends-only-the-window-named ()
  (with-outage-wire
    (with-cell-stop ((broadband-outage-start "endpoint" "https://example.test/mcp"))
      (setf *outage-answer-fn* (outage-serve :tool-text "{\"entry_count\":2,\"entries\":[]}"))
      (outage-tool "findbroadband_outage_history" (nlk:json-object))
      (is (equal "get_outage_history" (nlk:json-value (outage-tool-call) :string "name")))
      (is (zerop (hash-table-count (outage-arguments))) "no window named sends no argument")
      (outage-tool "findbroadband_outage_history" (nlk:json-object "provider" "Virgin Media" "days" 7))
      (let ((arguments (outage-arguments)))
        (is (equal "Virgin Media" (nlk:json-value arguments :string "provider")))
        (is (= 7 (nlk:json-value arguments :number "days")))
        (is (null (nth-value 1 (gethash "date" arguments))) "a date the call did not name is left off the wire"))
      (outage-tool "findbroadband_outage_history" (nlk:json-object "date" "2026-11-03"))
      (let ((arguments (outage-arguments)))
        (is (equal "2026-11-03" (nlk:json-value arguments :string "date")))
        (is (null (nth-value 1 (gethash "days" arguments))) "a date overrides days, and days was not named")))))

(deftest broadband-outage-cell-history-holds-the-servers-range-and-date-shape ()
  (with-outage-wire
    (with-cell-stop ((broadband-outage-start "endpoint" "https://example.test/mcp"))
      (handler-case (progn (nodecode-broadband-outage:history :days 31)
                           (is nil "31 days is out of the server's range"))
        (nodecode-broadband-outage:broadband-outage-error (condition)
          (is (search "1 to 30" (nodecode-broadband-outage:broadband-outage-error-detail condition)) "the refusal names the range")))
      (handler-case (progn (nodecode-broadband-outage:history :date "03/11/2026")
                           (is nil "another date shape is refused"))
        (nodecode-broadband-outage:broadband-outage-error (condition)
          (is (search "YYYY-MM-DD" (nodecode-broadband-outage:broadband-outage-error-detail condition)) "the refusal names the shape")))
      (is (null (outage-asked)) "neither refusal sent a request"))))

;;; --- what a wire can answer ----------------------------------------------------

(deftest broadband-outage-cell-an-error-answer-is-a-repair-not-a-crash ()
  (with-outage-wire
    (with-cell-stop ((broadband-outage-start "endpoint" "https://example.test/mcp"))
      (setf *outage-answer-fn* (outage-serve :tool-text "That provider is not tracked." :is-error t))
      (is-carrying (text (outage-tool "findbroadband_outage_status" (nlk:json-object "provider" "Nope Telecom")))
                   ("ERROR") ("That provider is not tracked.")))))

(deftest broadband-outage-cell-a-refused-status-carries-the-servers-words ()
  (with-outage-wire
    (with-cell-stop ((broadband-outage-start "endpoint" "https://example.test/mcp"))
      (setf *outage-answer-fn* (outage-serve :raw "upstream is down" :status 503
                                             :type "text/plain"))
      (is-carrying (text (outage-tool "findbroadband_outage_overview" (nlk:json-object)))
                   ("HTTP 503") ("upstream is down")))))

(deftest broadband-outage-cell-an-event-stream-answer-is-read-to-its-reply ()
  (with-outage-wire
    (with-cell-stop ((broadband-outage-start "endpoint" "https://example.test/mcp"))
      (setf *outage-answer-fn* (outage-serve :tool-text "{\"entry_count\":0}"
                                             :type "text/event-stream"))
      (is (search "entry_count" (outage-tool "findbroadband_outage_history" (nlk:json-object "days" 1))) "the reply in the stream is the answer"))))

(deftest broadband-outage-cell-a-session-id-is-echoed-on-every-later-call ()
  (with-outage-wire
    (with-cell-stop ((broadband-outage-start "endpoint" "https://example.test/mcp"))
      (setf *outage-answer-fn* (outage-serve :tool-text "{}" :session "s-42"))
      (outage-tool "findbroadband_outage_overview" (nlk:json-object))
      (outage-tool "findbroadband_outage_overview" (nlk:json-object))
      (let ((asked (outage-asked)))
        (is (null (outage-request-header (first asked) "mcp-session-id")) "the first request has no session yet")
        (is (equal "s-42" (outage-request-header (third asked) "mcp-session-id")) "the call after the handshake carries the session it issued")
        (is (equal "s-42" (outage-request-header (fourth asked) "mcp-session-id")))))))

;;; --- /broadband-outage ---------------------------------------------------------

(deftest broadband-outage-cell-the-command-answers-and-looks-up ()
  (with-outage-wire
    (with-cell-stop ((broadband-outage-start "endpoint" "https://example.test/mcp"))
      (is-carrying (status (nodecode-broadband-outage::run-command "" nil))
                   ("https://example.test/mcp") ("no key"))
      (setf *outage-answer-fn* (outage-serve :tool-text "{\"status\":\"Working fine\"}"))
      (is (search "Working fine" (nodecode-broadband-outage::run-command "Virgin Media" nil)) "a provider alone asks for its status")
      (is (equal "get_provider_outage_status" (nlk:json-value (outage-tool-call) :string "name")))
      (is (equal "Virgin Media" (nlk:json-value (outage-arguments) :string "provider")))
      (nodecode-broadband-outage::run-command "overview" nil)
      (is (equal "get_outage_overview" (nlk:json-value (outage-tool-call) :string "name")) "the overview word asks for every provider")
      (nodecode-broadband-outage::run-command "history BT 7" nil)
      (is (equal "get_outage_history" (nlk:json-value (outage-tool-call) :string "name")) "history asks for the incidents")
      (let ((arguments (outage-arguments)))
        (is (equal "BT" (nlk:json-value arguments :string "provider")) "the provider word")
        (is (= 7 (nlk:json-value arguments :number "days")) "the trailing number is the window")))))
