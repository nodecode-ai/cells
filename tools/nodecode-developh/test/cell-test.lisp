;;;; cell-test.lisp --- lifecycle, the four tools, and /developh, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches the studio, needs
;;;; a key or touches the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start and wrong values refused), the four tools through
;;;; the seam the model calls them by -- their endpoint, their JSON-RPC
;;;; envelope, their arguments and their answer -- the SSE and plain-JSON
;;;; shapes the server answers with, the one writing tool's confirmation gate,
;;;; and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-developh-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials the studio."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected developh request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun developh-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun developh-reply (text &optional (status 200))
  "The server answers TEXT with STATUS, as a plain JSON body."
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun developh-sse-reply (text &key is-error)
  "The server answers TEXT as the streamable-HTTP SSE frame it sends: an
`event: message' line, the JSON-RPC result as one `data:' line."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url args))
          (values (format nil "event: message~%data: {\"jsonrpc\": \"2.0\", \"id\": 1, \"result\": {\"content\": [{\"type\": \"text\", \"text\": \"~a\"}]~@[, \"isError\": true~]}}~%~%"
                          text (and is-error t))
                  200))))

(defun developh-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun developh-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun developh-arguments (request)
  "REQUEST's tools/call params, as the JSON object the server receives."
  (nlk:json-value (nlk:json-value (developh-request-body request) :object "params")
                   :object "arguments"))

(defun developh-called (request)
  "REQUEST's tool name."
  (nlk:json-value (nlk:json-value (developh-request-body request) :object "params")
                   :text "name"))

(defun developh-tool (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "developh-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "developh" "DEVELOPH-CELL-" :start nodecode-developh:start-cell)

(define-cell-lifecycle-tests "developh"
  (:fixture with-developh-wire)
  (:config "locale" "en")
  (:help :developh)
  (:command "developh")
  (:running
   (is (nle::find-tool "run_website_audit") "the audit is registered")
   (is (nle::find-tool "developh_studio_info") "and the studio profile")
   (is (nle::find-tool "developh_case_studies") "and the case studies")
   (is (nle::find-tool "developh_contact") "and the inquiry, the original's four")
   (is (= 4 (count-if (lambda (entry)
                        (member (first entry)
                                '("run_website_audit" "developh_studio_info"
                                  "developh_case_studies" "developh_contact")
                                :test #'equal))
                      nle::*tools*)) "four of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "run_website_audit")) "and stop takes every one back")
   (is (null (nle::find-tool "developh_contact"))))
  (:refused ("timeout_seconds" 0)
            ("audit_timeout_seconds" 10)
            ("max_chars" 10)
            ("locale" "fr")
            ("endpoint" "ftp://example.test/mcp")))

(deftest developh-cell-a-refused-endpoint-names-the-member-and-installs-nothing (with-developh-wire)
  (let ((text (refusal-text nlk:config-refusal (developh-start "endpoint" "not-an-address"))))
    (is-carrying text "endpoint" "http")
    (is (null (nle::find-tool "run_website_audit")) "nothing was installed")))

;;; --- the audit ------------------------------------------------------------------

(deftest developh-cell-the-audit-posts-one-rpc-call-and-reads-the-sse-report (with-developh-wire)
  (with-cell-stop ((developh-start "locale" "en"))
    (developh-sse-reply "Performance 96, SEO 100. Fix the hero image.")
    (let ((answer (developh-tool "run_website_audit" "{\"url\": \"example.com\"}")))
      (is (equal 1 (length (developh-asked))) "one request, as the plugin's skill sends")
      (let ((request (first (developh-asked))))
        (is (eq :post (first request)) "a POST")
        (is (equal "https://www.developh.co/mcp" (second request)) "to the served endpoint")
        (is (search "text/event-stream" (developh-request-header request "accept")) "asking for the stream the server answers in")
        (is (equal "2.0" (nlk:json-value (developh-request-body request) :text "jsonrpc")) "a JSON-RPC envelope")
        (is (equal "tools/call" (nlk:json-value (developh-request-body request) :text "method")) "carrying one tool call")
        (is (equal "run_website_audit" (developh-called request)) "named as the plugin names it")
        (is (equal "example.com" (nlk:json-value (developh-arguments request) :text "url")) "with the address the person gave")
        (is (equal "en" (nlk:json-value (developh-arguments request) :text "locale")) "and the section's answer language"))
      (is (search "Performance 96" answer) "the report reaches the model")
      (is (search "Fix the hero image" answer) "whole"))))

(deftest developh-cell-the-audit-refuses-a-local-address-without-a-request (with-developh-wire)
  (with-cell-stop ((developh-start "locale" "en"))
    (let ((answer (developh-tool "run_website_audit" "{\"url\": \"http://localhost:3000\"}")))
      (is (search "ERROR: " answer) "the refusal is the tool result")
      (is (search "local address" answer) "naming what the audit cannot reach")
      (is (search "public http(s) addresses" answer) "and what it needs")
      (is (null (developh-asked)) "and nothing was sent"))
    (let ((answer (developh-tool "run_website_audit" "{\"url\": \"   \"}")))
      (is (search "url is empty" answer) "an empty address is refused too"))))

(deftest developh-cell-a-service-error-is-what-the-model-repairs-from (with-developh-wire)
  (with-cell-stop ((developh-start "locale" "en"))
    (developh-reply
     "{\"jsonrpc\": \"2.0\", \"id\": 1, \"error\": {\"code\": -32602, \"message\": \"Tool nope not found\"}}")
    (let ((answer (developh-tool "run_website_audit" "{\"url\": \"example.com\"}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "the service refused the call" answer) "the RPC error is named")
      (is (search "Tool nope not found" answer) "with the service's own words")
      (is (search "-32602" answer) "and its code"))))

(deftest developh-cell-a-tool-result-marked-as-an-error-is-still-the-answer (with-developh-wire)
  (with-cell-stop ((developh-start "locale" "en"))
    (developh-sse-reply "only public addresses are audited" :is-error t)
    (let ((answer (developh-tool "run_website_audit" "{\"url\": \"example.com\"}")))
      (is (search "ERROR: " answer) "an isError result is not read as a report")
      (is (search "only public addresses" answer) "its text is kept for the model"))))

(deftest developh-cell-a-report-longer-than-max-chars-is-cut-and-counted (with-developh-wire)
  (with-cell-stop ((developh-start "locale" "en" "max_chars" 500))
    (developh-sse-reply (make-string 700 :initial-element #\x))
    (let ((answer (developh-tool "run_website_audit" "{\"url\": \"example.com\"}")))
      (is (search "200 more characters" answer) "the cut is stated")
      (is (= (+ 500 (length (format nil "~%[... 200 more characters]"))) (length answer)) "and exactly the section's worth of report is read"))))

;;; --- the studio's other two read tools --------------------------------------------

(deftest developh-cell-studio-and-case-studies-ask-their-own-tools (with-developh-wire)
  (with-cell-stop ((developh-start "locale" "en"))
    (developh-sse-reply "Design and development studio.")
    (let ((answer (developh-tool "developh_studio_info" "{\"locale\": \"pt\"}")))
      (is (equal "studio_info" (developh-called (first (developh-asked)))) "the server's own tool name")
      (is (equal "pt" (nlk:json-value (developh-arguments (first (developh-asked))) :text "locale")) "and the language the call asked for")
      (is (search "Design and development studio" answer) "its answer reaches the model"))
    (setf *requests* '())
    (developh-sse-reply "Restaurants: one case.")
    (let ((answer (developh-tool "developh_case_studies" "{\"sector\": \"restaurants\"}")))
      (let ((request (first (developh-asked))))
        (is (equal "list_case_studies" (developh-called request)) "the case-study tool")
        (is (equal "restaurants" (nlk:json-value (developh-arguments request) :text "sector")) "filtered by the sector the call named")
        (is (null (nlk:json-value (developh-arguments request) :text "service")) "and no filter the call did not name"))
      (is (search "Restaurants: one case" answer) "the filtered work is the answer"))))

;;; --- the one writing tool ----------------------------------------------------------

(deftest developh-cell-an-inquiry-needs-confirmation-and-then-carries-it (with-developh-wire)
  (with-cell-stop ((developh-start "locale" "en"))
    (let ((answer (developh-tool "developh_contact"
                                  "{\"name\": \"Ana\", \"email\": \"ana@example.test\", \"message\": \"A new store\"}")))
      (is (search "ERROR: " answer) "an unconfirmed inquiry is refused")
      (is (search "confirmed" answer) "naming the member it needs")
      (is (null (developh-asked)) "and nothing is sent"))
    (developh-sse-reply "Inquiry sent. You will get a confirmation email.")
    (let ((answer (developh-tool "developh_contact"
                                  "{\"name\": \"Ana\", \"email\": \"ana@example.test\", \"message\": \"A new store\", \"confirmed\": true}")))
      (let* ((request (first (developh-asked)))
             (arguments (developh-arguments request)))
        (is (equal "request_contact" (developh-called request)) "the server's contact tool")
        (is (eq t (nlk:json-value arguments :boolean "confirmed")) "which records the confirmation")
        (is (equal "Ana" (nlk:json-value arguments :text "name")) "with the person's name")
        (is (equal "A new store" (nlk:json-value arguments :text "message")) "and their message")
        (is (null (nlk:json-value arguments :text "company")) "and nothing they did not give"))
      (is (search "You will get a confirmation email" answer) "the studio's answer is the tool result"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest developh-cell-the-command-audits-or-says-what-it-is-connected-to (with-developh-wire)
  (with-cell-stop ((developh-start "locale" "en"))
    (let ((status (nodecode-developh::run-command "" nil)))
      (is (search "https://www.developh.co/mcp" status) "no argument reports the endpoint")
      (is (search "no key needed" status) "and that there is nothing to sign up for")
      (is (null (developh-asked)) "and sends nothing"))
    (developh-sse-reply "Performance 96")
    (let ((answer (nodecode-developh::run-command "  example.com  " nil)))
      (is (search "Performance 96" answer) "an address runs one audit")
      (is (equal "example.com"
                 (nlk:json-value (developh-arguments (first (developh-asked))) :text "url")) "trimmed, as typed"))
    (setf *requests* '())
    (developh-sse-reply "Design and development studio.")
    (nodecode-developh::run-command "studio" nil)
    (is (equal "studio_info" (developh-called (first (developh-asked)))) "studio asks the profile")
    (setf *requests* '())
    (developh-sse-reply "Restaurants: one case.")
    (nodecode-developh::run-command "work" nil)
    (is (equal "list_case_studies" (developh-called (first (developh-asked)))) "work asks the case studies")))
