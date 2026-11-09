;;;; cell-test.lisp --- lifecycle, the two tools, and /maqami, against a stubbed endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches MAQAMI and nothing
;;;; needs the network. What is proved is the cell's own wiring: the section (a
;;;; healthy start, wrong values and a missing nationality refused), the session
;;;; an initialize opens and the headers the requests after it carry, both the
;;;; JSON and the server-sent-event shape of an answer, the two tools through the
;;;; seam the model calls them by, a failed call, and the operator's command.

(in-package #:nodecode.test)

;;; --- the stubbed server ---------------------------------------------------------

(defvar *maqami-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *maqami-tools-answer* nil
  "When set, a function of ID answering tools/list instead of the default.")

(defvar *maqami-call-answer* nil
  "When set, a function of ID and the request message answering tools/call.")

(defparameter +maqami-initialize-result+
  "{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{\"tools\":{}},\"serverInfo\":{\"name\":\"maqami\",\"version\":\"1.0.7\"}}"
  "The result an initialize is answered with.")

(defparameter +maqami-tools-json+
  (concatenate 'string
               "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":["
               "{\"name\":\"post_hotels_rates\","
               "\"description\":\"Search live hotel rates by city and dates\","
               "\"inputSchema\":{\"type\":\"object\","
               "\"properties\":{\"cityName\":{},\"checkin\":{},\"checkout\":{}},"
               "\"required\":[\"cityName\",\"checkin\"]}},"
               "{\"name\":\"post_rates_prebook\","
               "\"description\":\"Hold a rate and answer a checkout link\","
               "\"inputSchema\":{\"type\":\"object\","
               "\"properties\":{\"offerId\":{}},\"required\":[\"offerId\"]}}"
               "]}}")
  "A tools/list answer, plain JSON: the other shape a server may answer with.")

(defun maqami-sse (message)
  "MESSAGE as the one server-sent event the endpoint usually answers with."
  (format nil "event: message~%data: ~a~%~%" message))

(defun maqami-headers (&optional session)
  "Answer headers, carrying the session the server opened when SESSION is given."
  (let ((headers (make-hash-table :test #'equal)))
    (when session (setf (gethash "mcp-session-id" headers) session))
    headers))

(defun maqami-answer (method url args)
  "The stubbed endpoint: what the request in ARGS is answered with."
  (declare (ignore method url))
  (let* ((message (nlk:decode-json (getf args :content)))
         (rpc (nlk:json-value message :text "method"))
         (id (nlk:json-value message :integer "id")))
    (cond
      ((equal rpc "initialize")
       (values (maqami-sse (format nil "{\"jsonrpc\":\"2.0\",\"id\":~a,\"result\":~a}"
                                    id +maqami-initialize-result+))
               200 (maqami-headers "sess-1") nil))
      ((equal rpc "notifications/initialized")
       (values "" 202 (maqami-headers) nil))
      ((equal rpc "tools/list")
       (if *maqami-tools-answer*
           (funcall *maqami-tools-answer* id)
           (values +maqami-tools-json+ 200 (maqami-headers) nil)))
      ((equal rpc "tools/call")
       (if *maqami-call-answer*
           (funcall *maqami-call-answer* id message)
           (values (maqami-sse
                    (format nil "{\"jsonrpc\":\"2.0\",\"id\":~a,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"2 hotel rates; the prebook link is https://book.maqami.co/booking?prebookId=1\"}]}}"
                            id))
                   200 (maqami-headers) nil)))
      (t (values "{}" 400 (maqami-headers) nil)))))

(defmacro with-maqami-wire (&body body)
  "BODY with NLK:HTTP stubbed by MAQAMI-ANSWER: a request is logged, an answer
is the stubbed server's, and no test dials MAQAMI."
  `(let ((*maqami-requests* '())
         (*maqami-tools-answer* nil)
         (*maqami-call-answer* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *maqami-requests*)
                                (maqami-answer method url args))
       ,@body)))

(defun maqami-asked ()
  "The stubbed requests, oldest first."
  (reverse *maqami-requests*))

(defun maqami-rpc (request)
  "REQUEST's JSON-RPC message, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun maqami-rpc-method (request)
  "The JSON-RPC method REQUEST asks for."
  (nlk:json-value (maqami-rpc request) :text "method"))

(defun maqami-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun maqami-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "maqami-call" name arguments)))

;;; --- lifecycle -------------------------------------------------------------------

(define-test-slice "maqami-travel" "MAQAMI-TRAVEL-CELL-" :start nodecode-maqami-travel:start-cell)

(define-cell-lifecycle-tests "maqami-travel"
  (:fixture with-maqami-wire)
  (:config "guest_nationality" "EG")
  (:help :maqami)
  (:command "maqami")
  (:running
   (is (nle::find-tool "maqami_tools") "tools is registered")
   (is (nle::find-tool "maqami_call") "and call")
   (is (null (nle::find-tool "maqami_hotels")) "and nothing the server did not ask for"))
  (:stopped
   (is (null (nle::find-tool "maqami_tools")) "and stop takes both back")
   (is (null (nle::find-tool "maqami_call"))))
  (:refused ("guest_nationality" nil)
            ("timeout_seconds" 0)
            ("max_chars" 10)))

(deftest maqami-travel-cell-a-start-with-no-nationality-refuses (with-maqami-wire)
  (let ((text (refusal-text nlk:config-refusal (maqami-travel-start))))
    (is (search "guest_nationality" text) "the refusal names the member to set")
    (is (search "EG" text) "and shows the shape of an answer"))
  (is (null (nle::find-tool "maqami_tools")) "nothing was installed")
  (is (null (maqami-asked)) "and nothing was dialled"))

;;; --- the session, and the two shapes of answer ------------------------------------

(deftest maqami-travel-cell-tools-joins-a-session-and-lists-the-server-s (with-maqami-wire)
  (with-cell-stop ((maqami-travel-start "guest_nationality" "EG"))
    (let ((answer (maqami-call "maqami_tools" "{}"))
          (asked (maqami-asked)))
      (is (= 3 (length asked)) "initialize, initialized, then the list")
      (is (equal "initialize" (maqami-rpc-method (first asked))) "the session is joined first")
      (is (equal "notifications/initialized" (maqami-rpc-method (second asked))) "and announced ready")
      (is (equal "tools/list" (maqami-rpc-method (third asked))) "then the list is asked for")
      (is (equal "sess-1" (maqami-header (third asked) "mcp-session-id")) "with the session the server opened")
      (is (equal "2025-06-18" (maqami-header (third asked) "mcp-protocol-version")) "and the revision agreed")
      (is (search "https://mcp.maqami.co/" (second (first asked))) "to the endpoint the plugin names")
      (is (search "post_hotels_rates" answer) "each tool by name")
      (is (search "checkin*" answer) "with its arguments, a * for a required one")
      (is (search "checkout" answer) "and the optional ones too")
      (is (search "post_rates_prebook" answer) "and the next one"))))

(deftest maqami-travel-cell-tools-keeps-only-what-the-word-names (with-maqami-wire)
  (with-cell-stop ((maqami-travel-start "guest_nationality" "EG"))
    (let ((answer (maqami-call "maqami_tools" "{\"match\": \"prebook\"}")))
      (is (search "post_rates_prebook" answer) "the tool the word names")
      (is (null (search "post_hotels_rates" answer)) "and not the rest"))
    (let ((answer (maqami-call "maqami_tools" "{\"match\": \"volcano\"}")))
      (is (search "no tool" answer) "a word nothing matches says so"))))

(deftest maqami-travel-cell-call-forwards-the-tool-and-its-arguments (with-maqami-wire)
  (with-cell-stop ((maqami-travel-start "guest_nationality" "EG"))
    (let ((answer (maqami-call "maqami_call"
                                "{\"tool\": \"post_hotels_rates\", \"arguments\": {\"cityName\": \"Cairo\", \"checkin\": \"2026-12-01\"}}"))
          (request (car (last (maqami-asked)))))
      (let* ((message (maqami-rpc request))
             (params (nlk:json-value message :object "params")))
        (is (equal "tools/call" (nlk:json-value message :text "method")) "one tools/call")
        (is (equal "post_hotels_rates" (nlk:json-value params :text "name")) "naming the tool")
        (is (equal "Cairo" (nlk:json-value (nlk:json-value params :object "arguments") :text "cityName"))
            "with the arguments the model wrote"))
      (is (search "2 hotel rates" answer) "and the server's text is the answer")
      (is (search "https://book.maqami.co/booking" answer) "the checkout link included"))))

(deftest maqami-travel-cell-a-call-with-no-tool-refuses-before-any-request (with-maqami-wire)
  (with-cell-stop ((maqami-travel-start "guest_nationality" "EG"))
    (let ((answer (maqami-call "maqami_call" "{}")))
      (is (search "tool" answer) "the refusal names the member the call left out")
      (is (null (maqami-asked)) "and nothing was sent"))))

(deftest maqami-travel-cell-a-failed-call-is-the-models-repair (with-maqami-wire)
  (with-cell-stop ((maqami-travel-start "guest_nationality" "EG"))
    (setf *maqami-call-answer*
          (lambda (id message)
            (declare (ignore id message))
            (values "{\"jsonrpc\":\"2.0\",\"id\":3,\"error\":{\"code\":-32603,\"message\":\"the rate session expired, search again\"}}"
                    500 (maqami-headers) nil)))
    (let ((answer (maqami-call "maqami_call" "{\"tool\": \"post_rates_prebook\"}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "HTTP 500" answer) "naming the status")
      (is (search "search again" answer) "and the server's own reason"))))

;;; --- the operator's command -------------------------------------------------------

(deftest maqami-travel-cell-the-command-reports-lists-and-calls (with-maqami-wire)
  (with-cell-stop ((maqami-travel-start "guest_nationality" "EG"))
    (let ((status (nodecode-maqami-travel::run-command "" nil)))
      (is (search "https://mcp.maqami.co/" status) "no argument reports the endpoint")
      (is (search "EG" status) "and the nationality rates are priced for")
      (is (null (maqami-asked)) "without dialling anything"))
    (let ((listing (nodecode-maqami-travel::run-command "tools hotel" nil)))
      (is (search "post_hotels_rates" listing) "tools lists what the server offers"))
    (let ((answer (nodecode-maqami-travel::run-command
                   "call post_hotels_rates {\"cityName\": \"Cairo\"}" nil)))
          (is (search "2 hotel rates" answer) "call runs one tool")
          (is (equal "post_hotels_rates"
                     (nlk:json-value (nlk:json-value (maqami-rpc (car (last (maqami-asked))))
                                                      :object "params")
                                     :text "name"))
              "by the name the operator typed"))))
