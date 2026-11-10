;;;; cell-test.lisp --- lifecycle, the four tools, /lastscanned, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start with no key, a timeout under its floor or an endpoint without a
;;;; scheme refused, an api_key_env naming an empty variable refused), the
;;;; four tools through the seam the model calls them by -- the MCP handshake
;;;; once per start, the session id echoed back, the JSON-RPC body the call
;;;; carries and the answer it builds -- the SSE framing the endpoint may
;;;; answer with, the JSON-RPC and isError refusals, and /lastscanned with
;;;; and without an argument. Every request is a stubbed NLK:HTTP, so nothing
;;;; reaches lastscanned or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *lastscanned-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *lastscanned-tool-reply* nil
  "What a tools/call answers: (TEXT STATUS . HEADERS), or NIL for an empty 204.")

(defvar *lastscanned-session* "s-1"
  "The session id the stubbed initialize issues, or NIL for a server that
keeps no session.")

(defparameter +lastscanned-initialize+
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"serverInfo\":{\"name\":\"stub\"}}}"
  "The handshake's answer, as the endpoint words it.")

(defun lastscanned-headers (&rest pairs)
  "A response header table, as dexador hands one back: lowercase names."
  (let ((headers (make-hash-table :test #'equal)))
    (loop for (name value) on pairs by #'cddr
          do (setf (gethash name headers) value))
    headers))

(defun lastscanned-rpc-method (args)
  "The JSON-RPC method ARGS carries, or NIL."
  (nlk:json-value (nlk:decode-json (getf args :content)) :string "method"))

(defun lastscanned-answer (args)
  "The stubbed endpoint's answer to ARGS: the handshake to initialize, 202 to
the notification, and *LASTSCANNED-TOOL-REPLY* to everything else."
  (let ((method (lastscanned-rpc-method args)))
    (cond ((equal method "initialize")
           (values +lastscanned-initialize+ 200
                   (and *lastscanned-session*
                        (lastscanned-headers "mcp-session-id" *lastscanned-session*))))
          ((equal method "notifications/initialized") (values "" 202))
          (*lastscanned-tool-reply*
           (values (first *lastscanned-tool-reply*)
                   (second *lastscanned-tool-reply*)
                   (third *lastscanned-tool-reply*)))
          (t (values "" 204)))))

(defmacro with-lastscanned-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *LASTSCANNED-REQUESTS*
and answered as the stubbed endpoint answers, so no test dials lastscanned.
UIOP:GETENV answers nothing either, so a key in the machine's environment
cannot reach a test that did not ask for one."
  `(let ((*lastscanned-requests* '())
         (*lastscanned-tool-reply* nil)
         (*lastscanned-session* "s-1"))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *lastscanned-requests*)
                                (lastscanned-answer args))
       (with-stubbed-fdefinition (uiop:getenv (name)
                                  (declare (ignore name))
                                  nil)
         ,@body))))

(defun lastscanned-asked ()
  "The stubbed requests, oldest first."
  (reverse *lastscanned-requests*))

(defun lastscanned-call-request ()
  "The test's tools/call request -- what the handshake came before."
  (find "tools/call" (lastscanned-asked) :key #'lastscanned-method-of :test #'equal))

(defun lastscanned-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun lastscanned-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun lastscanned-method-of (request)
  "REQUEST's JSON-RPC method."
  (nlk:json-value (lastscanned-request-body request) :string "method"))

(defun lastscanned-arguments-of (request)
  "REQUEST's tools/call arguments, as the object it sent."
  (nlk:json-value (nlk:json-value (lastscanned-request-body request) :object "params")
                   :object "arguments"))

(defun lastscanned-result (text &key is-error)
  "An MCP tools/call reply carrying TEXT as one text block."
  (let ((result (nlk:make-json-object
                 "content" (vector (nlk:make-json-object "type" "text" "text" text)))))
    (when is-error (setf (gethash "isError" result) t))
    (nlk:encode-json-object (nlk:make-json-object "jsonrpc" "2.0" "id" 3 "result" result))))

(defun lastscanned-answer-text (text &optional (status 200))
  "Have every tools/call answer TEXT at STATUS."
  (setf *lastscanned-tool-reply* (list text status)))

(defun lastscanned-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "lastscanned" "LASTSCANNED-CELL-")

(defun lastscanned-config (&rest pairs)
  "A whole shared config with one `lastscanned' section built from PAIRS."
  (nlk:json-object "lastscanned" (apply #'nlk:make-json-object pairs)))

(defun lastscanned-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-lastscanned:start-cell (apply #'lastscanned-config pairs)))

(define-cell-lifecycle-tests "lastscanned"
  (:fixture with-lastscanned-wire)
  (:help :lastscanned)
  (:command "lastscanned")
  (:running (is (nle::find-tool "track_package") "track_package is registered")
            (is (nle::find-tool "track_packages") "track_packages is registered")
            (is (nle::find-tool "get_rates") "get_rates is registered")
            (is (nle::find-tool "list_carriers") "list_carriers is registered")
            (is (equal "https://mcp.lastscanned.com/mcp" (nodecode-lastscanned::setting :base-url)) "the section's default endpoint")
            (is (null (nodecode-lastscanned::setting :key)) "no key: the free allowance answers")
            (is (= 30 (nodecode-lastscanned::setting :timeout-seconds)) "the default ceiling"))
  (:stopped (is (null (nle::find-tool "track_package")) "stop unregisters the tools")
            (is (null (nle::find-tool "list_carriers")) "all of them"))
  (:refused ("timeout_seconds" 0) ("base_url" "mcp.lastscanned.com"))
  (:idle nodecode-lastscanned:lastscanned-error (nodecode-lastscanned::run-command "" nil)))

(deftest lastscanned-cell-a-named-but-empty-key-variable-refuses ()
  (with-lastscanned-wire
    (is-carrying (answer (refusal-text nlk:config-refusal
                                       (lastscanned-start "api_key_env" "LASTSCANNED_TEST_KEY")))
                 ("api_key_env names LASTSCANNED_TEST_KEY" "the refusal names the variable")
                 ("free daily allowance" "and says what to do instead"))
    (is (null (nle::find-tool "track_package")) "a refused start installed nothing")))

;;; --- the wire ------------------------------------------------------------------

(deftest lastscanned-cell-track-package-speaks-mcp ()
  (with-lastscanned-wire
    (with-cell-stop ((lastscanned-start))
      (lastscanned-answer-text (lastscanned-result "{\"status\":\"in_transit\",\"eta\":\"2026-11-12\"}"))
      (let* ((text (lastscanned-tool "track_package" (nlk:json-object "tracking_number" "1Z999AA1")))
             (requests (lastscanned-asked))
             (call (third requests)))
        (is (equal "{\"status\":\"in_transit\",\"eta\":\"2026-11-12\"}" text) "the server's own text")
        (is (= 3 (length requests)) "the handshake once, then the call")
        (is (equal "initialize" (lastscanned-method-of (first requests))))
        (is (equal "notifications/initialized" (lastscanned-method-of (second requests))))
        (is (equal "tools/call" (lastscanned-method-of call)))
        (is (equal "https://mcp.lastscanned.com/mcp" (second call)) "the section's endpoint")
        (is (equal "2025-06-18" (lastscanned-request-header call "mcp-protocol-version")))
        (is (equal "s-1" (lastscanned-request-header call "mcp-session-id")) "the session the handshake issued, echoed back")
        (is (equal "application/json, text/event-stream"
                   (lastscanned-request-header call "accept")))
        (let ((arguments (lastscanned-arguments-of call)))
          (is (equal "1Z999AA1" (nlk:json-value arguments :string "tracking_number")))
          (is (null (nlk:json-value arguments :string "carrier")) "no carrier: detection decides"))))))

(deftest lastscanned-cell-a-key-is-sent-and-a-stateless-server-still-answers ()
  (with-lastscanned-wire
    (with-cell-stop ((lastscanned-start "api_key" "k-test"))
      (lastscanned-answer-text (lastscanned-result "{\"carriers\":[]}"))
      (lastscanned-tool "list_carriers" (nlk:json-object))
      (is (equal "Bearer k-test"
                 (lastscanned-request-header (lastscanned-call-request) "authorization")) "every message carries the section's key"))
  (with-lastscanned-wire
    (let ((*lastscanned-session* nil))
      (with-cell-stop ((lastscanned-start))
        (lastscanned-answer-text (lastscanned-result "ok"))
        (is (equal "ok" (lastscanned-tool "list_carriers" (nlk:json-object))) "a server that issues no session id is answered all the same")
        (let ((call (lastscanned-call-request)))
          (is (equal "tools/call" (lastscanned-method-of call)) "one call, no session")
          (is (null (lastscanned-request-header call "mcp-session-id")))
          (is (null (lastscanned-request-header call "authorization")))
          (is (null (nlk:json-value (lastscanned-arguments-of call) :string "carrier")))))))))

(deftest lastscanned-cell-an-sse-answer-decodes ()
  (with-lastscanned-wire
    (with-cell-stop ((lastscanned-start))
      (lastscanned-answer-text
       (format nil "event: message~%data: ~a~%~%" (lastscanned-result "{\"ok\":true}")))
      (is (equal "{\"ok\":true}" (lastscanned-tool "list_carriers" (nlk:json-object))) "a data: framed body reads like a plain one"))))

(deftest lastscanned-cell-a-refused-answer-is-a-repair-not-a-crash ()
  (with-lastscanned-wire
    (with-cell-stop ((lastscanned-start))
      (lastscanned-answer-text "{\"jsonrpc\":\"2.0\",\"id\":3,\"error\":{\"code\":-32602,\"message\":\"unknown carrier: xyz\"}}")
      (is-carrying (answer (lastscanned-tool "track_package" (nlk:json-object "tracking_number" "1Z")))
                   ("ERROR: track_package refused") ("unknown carrier: xyz"))
      (lastscanned-answer-text (lastscanned-result "free_lookup_used" :is-error t))
      (is-carrying (answer (lastscanned-tool "track_package" (nlk:json-object "tracking_number" "1Z")))
                   ("ERROR: track_package said:") ("free_lookup_used"))
      (lastscanned-answer-text "" 503)
      (is-carrying (answer (lastscanned-tool "track_package" (nlk:json-object "tracking_number" "1Z")))
                   ("ERROR: track_package answered HTTP 503")))))

;;; --- the other three tools ---------------------------------------------------------

(deftest lastscanned-cell-get-rates-names-only-what-the-call-named ()
  (with-lastscanned-wire
    (with-cell-stop ((lastscanned-start))
      (lastscanned-answer-text (lastscanned-result "{\"quotes\":[]}"))
      (lastscanned-tool "get_rates" (nlk:json-object "from" "10001" "to" "90210"
                                                      "weight" 2
                                                      "weight_unit" "lb"
                                                      "length" 10 "width" 8 "height" 6))
      (let ((arguments (lastscanned-arguments-of (lastscanned-call-request))))
        (is (equal "10001" (nlk:json-value arguments :string "from")))
        (is (equal "90210" (nlk:json-value arguments :string "to")))
        (is (= 2 (nlk:json-value arguments :number "weight")))
        (is (equal "lb" (nlk:json-value arguments :string "weight_unit")))
        (is (= 10 (nlk:json-value arguments :number "length")))
        (is (null (nlk:json-value arguments :string "dimension_unit")) "unset: the server's default")
        (is (null (nlk:json-value arguments :string "residential")))
        (is (null (nlk:json-value arguments :string "carriers")))))))

(deftest lastscanned-cell-a-unit-outside-the-set-is-refused-before-the-wire ()
  (with-lastscanned-wire
    (with-cell-stop ((lastscanned-start))
      (lastscanned-answer-text (lastscanned-result "{}"))
      (is-carrying (answer (lastscanned-tool "get_rates"
                                             (nlk:json-object "from" "1" "to" "2"
                                                               "weight" 1
                                                               "weight_unit" "stone")))
                   ("weight_unit")))))

(deftest lastscanned-cell-track-packages-takes-a-hundred-and-says-so ()
  (with-lastscanned-wire
    (with-cell-stop ((lastscanned-start))
      (lastscanned-answer-text (lastscanned-result "{\"results\":[]}"))
      (let* ((numbers (coerce (loop for index from 1 to 102
                                     collect (format nil "1Z~3,'0d" index))
                               'vector))
             (text (lastscanned-tool "track_packages" (nlk:json-object "tracking_numbers" numbers
                                                                        "carrier" "ups")))
             (sent (nlk:json-value (lastscanned-arguments-of (lastscanned-call-request))
                                   :any "tracking_numbers")))
        (is (search "102 numbers named; the first 100 looked up" text))
        (is (= 100 (length sent)) "the server's cap, applied here")
        (is (equal "ups" (nlk:json-value (lastscanned-arguments-of (lastscanned-call-request))
                                          :string "carrier")))))))

;;; --- /lastscanned ------------------------------------------------------------------

(deftest lastscanned-cell-the-command-tracks-or-says-where-it-points ()
  (with-lastscanned-wire
    (with-cell-stop ((lastscanned-start))
      (is-carrying (answer (nodecode-lastscanned::run-command "" nil))
                   ("https://mcp.lastscanned.com/mcp") ("free daily allowance"))
      (lastscanned-answer-text (lastscanned-result "{\"status\":\"delivered\"}"))
      (let ((text (nodecode-lastscanned::run-command "1Z999 maersk" nil)))
        (is (equal "{\"status\":\"delivered\"}" text))
        (let ((arguments (lastscanned-arguments-of (lastscanned-call-request))))
          (is (equal "1Z999" (nlk:json-value arguments :string "tracking_number")))
          (is (equal "maersk" (nlk:json-value arguments :string "carrier")) "the second word is the carrier when it is given"))))))
