;;;; cell-test.lisp --- lifecycle, the two tools, and /foresee, against a stubbed server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP that behaves like a small MCP
;;;; streamable-HTTP server: it opens a session, acknowledges the
;;;; notification, and answers a tools/call with the result the test set. So
;;;; nothing reaches Foresee, no key is needed and the run needs no network.
;;;; What is proved is the cell's own wiring: the section (a healthy start,
;;;; wrong values and an endpoint it will not dial refused), the two tools
;;;; through the seam the model calls them by -- their JSON-RPC bodies, the
;;;; session header, and the text they answer -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS HEADERS).")

(defmacro with-foresee-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials Foresee. Both are globals (saved and restored),
not bindings: the cell's body may answer on a thread of its own, and a
dynamic binding would not reach it."
  `(with-saved-globals (*requests* *reply*
                         nodecode-foresee::*session* nle:*tools*)
     (setf *requests* '()
           *reply* (lambda (method url args)
                     (declare (ignore method url args))
                     (error "unexpected foresee request")))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun foresee-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun foresee-body (request)
  "REQUEST's JSON-RPC body, decoded, or NIL when it carries none."
  (let ((content (getf (cddr request) :content)))
    (and (stringp content) (nlk:decode-json content))))

(defun foresee-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (let ((headers (getf (cddr request) :headers)))
    (cdr (assoc name headers :test #'string-equal))))

(defun foresee-method-of (request)
  (nlk:json-value (foresee-body request) :text "method"))

(defun foresee-result (text &key (is-error nil) structured)
  "A tools/call result object carrying TEXT."
  (nlk:json-object
   "content" (vector (nlk:json-object "type" "text" "text" text))
   "isError" (and is-error t)
   :opt "structuredContent" structured))

(defun foresee-server (&key (answer "estimate") (tools '()) (session "sess-1")
                            (call-status 200) (content-type "application/json"))
  "Make the stubbed wire behave like Foresee's server: an initialize that
issues SESSION, the notification's 202, a tools/list carrying TOOLS, and a
tools/call answering ANSWER -- a result object, or a string body sent with
CALL-STATUS and CONTENT-TYPE."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((body (nlk:decode-json (getf args :content)))
                 (name (nlk:json-value body :text "method"))
                 (id (gethash "id" body)))
            (cond
              ((equal name "initialize")
               (values (nlk:encode-json-object
                        (nlk:json-object "jsonrpc" "2.0" "id" id
                                         "result" (nlk:json-object
                                                   "protocolVersion" "2025-06-18"
                                                   "serverInfo" (nlk:json-object "name" "foresee"))))
                       200
                       (let ((headers (make-hash-table :test #'equal)))
                         (setf (gethash "mcp-session-id" headers) session)
                         headers)))
              ((equal name "notifications/initialized")
               (values "" 202 nil))
              ((equal name "tools/list")
               (values (nlk:encode-json-object
                        (nlk:json-object "jsonrpc" "2.0" "id" id
                                         "result" (nlk:json-object
                                                   "tools" (coerce tools 'vector))))
                       200 nil))
              ((equal name "tools/call")
               (if (stringp answer)
                   (values answer call-status
                           (let ((headers (make-hash-table :test #'equal)))
                             (setf (gethash "content-type" headers) content-type)
                             headers))
                   (values (nlk:encode-json-object
                            (nlk:json-object "jsonrpc" "2.0" "id" id "result" answer))
                           200 nil)))
              (t (values "{}" 200 nil)))))))

(defun foresee-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "foresee-call" name arguments)))

(defun foresee-tool-call (index)
  "The INDEXth tools/call of the stubbed requests, oldest first."
  (let ((calls (remove-if-not (lambda (request)
                                 (equal "tools/call" (foresee-method-of request)))
                               (foresee-asked))))
    (nth index calls)))

(defun foresee-arguments (request)
  "REQUEST's tools/call arguments, decoded."
  (nlk:json-value (nlk:json-value (foresee-body request) :object "params") :object "arguments"))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "foresee" "FORESEE-CELL-" :start nodecode-foresee:start-cell)

(define-cell-lifecycle-tests "foresee"
  (:fixture with-foresee-wire)
  (:help :foresee)
  (:command "foresee")
  (:running
   (is (nle::find-tool "quote_insurance") "the instant quote tool is registered")
   (is (nle::find-tool "live_carrier_quotes") "and the live walk, the original's two")
   (is (= 2 (count-if (lambda (entry)
                        (member (first entry) '("quote_insurance" "live_carrier_quotes")
                                :test #'string=))
                      nle::*tools*)) "two of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "quote_insurance")) "and stop takes every one back")
   (is (null (nle::find-tool "live_carrier_quotes"))))
  (:refused ("timeout_seconds" 0)
            ("max_chars" 10)
            ("endpoint" "ftp://quotes.test/mcp")))

(deftest foresee-cell-a-start-with-no-endpoint-dials-foresee-s-own ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (is (equal "https://agents.go-foresee.com/mcp" (nodecode-foresee::endpoint)) "the hosted server is the default")
      (is (search "foresee" (nodecode-foresee::status-text)) "and the cell is running"))))

(deftest foresee-cell-an-endpoint-this-client-will-not-dial-refuses ()
  (with-foresee-wire ()
    (let ((text (refusal-text nlk:config-refusal (foresee-start "endpoint" "http://quotes.test/mcp"))))
      (is (search "endpoint" text) "the refusal names the member")
      (is (search "http" text) "and says what it will not dial"))))

;;; --- quote_insurance -------------------------------------------------------------

(deftest foresee-cell-quote-opens-a-session-and-posts-the-two-objects ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (foresee-server :answer (foresee-result "C Geico $92/mo ci 88-96"))
      (let ((answer (foresee-call
                      "quote_insurance"
                      "{\"profile\": {\"zip_code\": \"95814\", \"age\": 35},
                        \"lines\": {\"auto\": {\"bi\": \"100/300\", \"coll_deductible\": 500}}}")))
        (is (equal "C Geico $92/mo ci 88-96" answer) "the server's text is the answer")
        (let ((calls (remove-if-not (lambda (request)
                                       (equal "tools/call" (foresee-method-of request)))
                                     (foresee-asked))))
          (is (= 1 (length calls)) "one tools/call")
          (let ((request (first calls)))
            (is (eq :post (first request)) "a POST")
            (is (equal "https://agents.go-foresee.com/mcp" (second request)) "to Foresee's endpoint")
            (is (equal "sess-1" (foresee-header request "mcp-session-id")) "carrying the session the initialize issued")
            (is (equal "tools/call" (nlk:json-value (foresee-body request) :text "method")) "the body names tools/call")
            (let ((arguments (foresee-arguments request))
                  (params (nlk:json-value (foresee-body request) :object "params")))
              (is (equal "quote_insurance" (nlk:json-value params :text "name")) "naming the tool")
              (is (equal "95814" (nlk:json-value (nlk:json-value arguments :object "profile")
                                                  :text "zip_code")) "the profile as the model wrote it")
              (is (equal "100/300" (nlk:json-value
                                     (nlk:json-value (nlk:json-value arguments :object "lines")
                                                     :object "auto")
                                     :text "bi")) "and the coverage selection, untouched"))))))))

(deftest foresee-cell-one-session-serves-every-call ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (foresee-server :answer (foresee-result "answer"))
      (foresee-call "quote_insurance" "{\"profile\": {}, \"lines\": {\"auto\": {}}}")
      (foresee-call "quote_insurance" "{\"profile\": {}, \"lines\": {\"auto\": {}}}")
      (is (= 1 (count "initialize" (foresee-asked) :key #'foresee-method-of :test #'equal)) "both calls share the one session")
      (is (= 2 (count "tools/call" (foresee-asked) :key #'foresee-method-of :test #'equal)) "and each made its own call"))))

(deftest foresee-cell-a-quote-with-no-profile-is-refused-before-the-wire ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (foresee-server :answer (foresee-result "answer"))
      (let ((answer (foresee-call "quote_insurance" "{}")))
        (is (search "profile" answer) "the refusal names the member the call left out")
        (is (null (foresee-asked)) "and nothing was sent")))))

;;; --- live_carrier_quotes ---------------------------------------------------------

(deftest foresee-cell-live-quotes-carries-the-consent-verbatim ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (foresee-server :answer (foresee-result "status: in_progress"))
      (let ((answer (foresee-call
                      "live_carrier_quotes"
                      "{\"profile\": {\"zip_code\": \"95814\"},
                        \"lines\": {\"auto\": {\"bi\": \"100/300\"}},
                        \"user_authorization\": \"yes, go ahead\",
                        \"identity\": {\"first_name\": \"A\", \"email\": \"a@example.test\"},
                        \"carriers\": [\"geico\"]}")))
        (is (equal "status: in_progress" answer) "the collection state is the answer")
        (let ((arguments (foresee-arguments (foresee-tool-call 0))))
          (is (equal "yes, go ahead" (nlk:json-value arguments :text "user_authorization")) "the go-ahead is the user's own words")
          (is (equal "geico" (aref (nlk:json-value arguments :array "carriers") 0)) "and the carriers asked for are narrowed")
          (is (equal "a@example.test"
                     (nlk:json-value (nlk:json-value arguments :object "identity") :text "email")) "the identity the carriers send the quote to"))))))

(deftest foresee-cell-a-live-call-with-no-authorization-is-refused ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (let ((answer (foresee-call "live_carrier_quotes"
                                   "{\"profile\": {}, \"lines\": {\"auto\": {}}}")))
        (is (search "user_authorization" answer) "consent is not optional")
        (is (null (foresee-asked)) "and nothing reached a carrier")))))

;;; --- what comes back ---------------------------------------------------------------

(deftest foresee-cell-a-refused-answer-is-the-model-s-repair ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (foresee-server :answer (foresee-result
                                "{\"error\": \"authorization_required\", \"detail\": \"give the disclosure, then the yes\"}"
                                :is-error t))
      (let ((answer (foresee-call "quote_insurance" "{\"profile\": {}, \"lines\": {}}")))
        (is (search "ERROR: " answer) "a refused answer is the tool result, not a condition up the stack")
        (is (search "authorization_required" answer) "and carries the server's own word for it")))))

(deftest foresee-cell-a-json-rpc-error-names-what-the-server-said ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (setf *reply* (lambda (method url args)
                      (declare (ignore method url))
                      (let* ((body (nlk:decode-json (getf args :content)))
                             (name (nlk:json-value body :text "method")))
                        (cond
                          ((equal name "initialize")
                           (values "{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":{}}" 200 nil))
                          ((equal name "notifications/initialized") (values "" 202 nil))
                          (t (values "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"no_carriers: nothing is priced for that line\"}}"
                                     200 nil))))))
      (let ((answer (foresee-call "quote_insurance" "{\"profile\": {}, \"lines\": {}}")))
        (is (search "ERROR: " answer) "it is a tool result the model can read")
        (is (search "no_carriers" answer) "naming the server's reason")))))

(deftest foresee-cell-an-sse-answer-reads-like-a-json-one ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (let* ((result (foresee-result "answer: from an event stream"))
             (frame (format nil "event: message~%data: ~a~%~%"
                            (nlk:encode-json-object
                             (nlk:json-object "jsonrpc" "2.0" "id" 1 "result" result)))))
        (foresee-server :answer frame :content-type "text/event-stream")
        (is (equal "answer: from an event stream"
                   (foresee-call "quote_insurance" "{\"profile\": {}, \"lines\": {}}")) "the data line's message is the answer")))))

(deftest foresee-cell-a-forgotten-session-is-opened-again ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (foresee-server :answer (foresee-result "second time"))
      (foresee-call "quote_insurance" "{\"profile\": {}, \"lines\": {}}")
      (let ((serve *reply*) (calls 0))
        (setf *reply* (lambda (method url args)
                        (let ((name (foresee-method-of (list* method url args))))
                          (if (equal name "tools/call")
                              (progn (incf calls)
                                     (if (= calls 1)
                                         (values "session not found" 404 nil)
                                         (funcall serve method url args)))
                              (funcall serve method url args)))))
        (let ((answer (foresee-call "quote_insurance" "{\"profile\": {}, \"lines\": {}}")))
          (is (equal "second time" answer)
              (format nil "a recycled session is rebuilt, not surfaced as an error [~a]" answer)))))))

(deftest foresee-cell-structured-content-is-the-answer-when-there-is-no-text ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (foresee-server :answer (nlk:json-object
                                "content" (vector)
                                "structuredContent" (nlk:json-object
                                                     "by_line" (nlk:json-object "auto" "priced"))))
      (let ((answer (foresee-call "quote_insurance" "{\"profile\": {}, \"lines\": {}}")))
        (is (search "by_line" answer) "the structured headline reaches the model")
        (is (search "priced" answer) "with its numbers")))))

(deftest foresee-cell-a-long-answer-is-cut-at-max-chars ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start "max_chars" 200))
      (foresee-server :answer (foresee-result (make-string 400 :initial-element #\x)))
      (let ((answer (foresee-call "quote_insurance" "{\"profile\": {}, \"lines\": {}}")))
        (is (search "more characters" answer) "the answer says what was left")
        (is (< (length answer) 260) "and stays small")))))

;;; --- the operator's command ---------------------------------------------------------

(deftest foresee-cell-the-command-shows-the-connection-and-the-server-s-tools ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (foresee-server :tools (list (nlk:json-object "name" "quote_insurance"
                                                     "description" "Instant quote estimates")
                                    (nlk:json-object "name" "live_carrier_quotes"
                                                     "description" "Foresee's agents on the carriers' forms")))
      (let ((status (nodecode-foresee::run-command "" nil)))
        (is (search "https://agents.go-foresee.com/mcp" status) "no argument reports the endpoint")
        (is (search "quote_insurance" status) "and the tools the server advertises")
        (is (search "live_carrier_quotes" status) "both of them"))
      (let ((one (nodecode-foresee::run-command "  live  " nil)))
        (is (search "live_carrier_quotes" one) "an argument narrows the list")
        (is (null (search "quote_insurance" one)) "to what it names")))))

(deftest foresee-cell-the-command-reports-a-server-it-cannot-reach ()
  (with-foresee-wire ()
    (with-cell-stop ((foresee-start))
      (setf *reply* (lambda (method url args)
                      (declare (ignore method url args))
                      (values "{\"jsonrpc\":\"2.0\",\"id\":0,\"error\":{\"message\":\"service unavailable\"}}"
                              503 nil)))
      (let ((status (nodecode-foresee::run-command "" nil)))
        (is (search "foresee:" status) "the failure is reported, not raised")
        (is (search "503" status) "with the status the server answered")))))


