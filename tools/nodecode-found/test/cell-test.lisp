;;;; cell-test.lisp --- lifecycle, the six tools, /found, and the MCP wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a value under its floor refused, a section with no key refused),
;;;; the six tools through the seam the model calls them by -- the JSON-RPC
;;;; exchange, the session, the remote tool's name and arguments, and the
;;;; answer -- and /found with and without a query. Every request is a
;;;; stubbed NLK:HTTP, so nothing reaches FOUND or the network.

(in-package #:nodecode.test)

;;; --- fixtures ----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS HEADERS).")

(defmacro with-found-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials FOUND."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected FOUND request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun found-asked () (reverse *requests*))
(defun found-call-body (request) (nlk:decode-json (getf (cddr request) :content)))
(defun found-call-method (request) (nlk:json-value (found-call-body request) :string "method"))
(defun found-call-arguments (request)
  (nlk:json-value (found-call-body request) :object "params" "arguments"))
(defun found-call-tool (request)
  (nlk:json-value (found-call-body request) :string "params" "name"))
(defun found-header (request name)
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))
(defun found-tool-calls () (remove "tools/call" (found-asked) :test-not #'equal :key #'found-call-method))

(defun found-answer (text &optional (status 200) headers)
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status headers))))

(defun found-rpc (id result)
  "One JSON-RPC answer carrying RESULT, as the wire writes it."
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":~d,\"result\":~a}" id result))

(defun found-content (text)
  "A tools/call RESULT carrying TEXT as its one content block."
  (format nil "{\"content\":[{\"type\":\"text\",\"text\":~a}]}" (nlk:encode-json-object text)))

(defun found-server (&key (text "") (session "s-1") (tool-result nil))
  "A whole MCP session answered from the stub: initialize names SESSION, the
notification is accepted, and a tools/call answers TOOL-RESULT or TEXT."
  (setf *answer-fn*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((body (nlk:decode-json (getf args :content)))
                 (name (nlk:json-value body :string "method"))
                 (id (nlk:json-value body :integer "id")))
            (cond
              ((equal name "initialize")
               (values (found-rpc id "{\"protocolVersion\":\"2025-06-18\"}") 200
                       (list (cons "content-type" "application/json")
                             (cons "mcp-session-id" session))))
              ((equal name "notifications/initialized") (values "" 202 nil))
              (t (values (found-rpc id (or tool-result (found-content text))) 200 nil)))))))

(defun found-tool (name arguments)
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ---------------------------------------------------------------

(define-test-slice "found" "FOUND-CELL-" :start nodecode-found:start-cell)

(define-cell-lifecycle-tests "found"
  (:fixture with-found-wire)
  (:config "base_url" "https://found.test/mcp" "api_key" "k-test")
  (:help :found)
  (:command "found")
  (:running (is (nle::find-tool "found_status") "found_status is registered")
            (is (nle::find-tool "found_leads"))
            (is (nle::find-tool "found_numbers"))
            (is (nle::find-tool "found_account"))
            (is (nle::find-tool "found_draft"))
            (is (nle::find-tool "found_find"))
            (is (equal "https://found.test/mcp" (nodecode-found::endpoint)))
            (is (equal "k-test" (nodecode-found::api-key)))
            (is (= 60 (nodecode-found::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "found_leads")) "stop unregisters the tools")
            (is (null nodecode-found::*session*) "and forgets the session"))
  (:refused ("timeout_seconds" 0))
  (:idle nodecode-found:found-error (nodecode-found::run-command "" nil)))

(deftest found-cell-a-section-with-no-key-is-refused ()
  (with-found-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (found-start "base_url" "https://found.test/mcp")))
                   ("needs a key") ("api_key"))
      (is (null (nle::find-tool "found_status")) "a refused start installed nothing"))))

(deftest found-cell-the-key-may-come-from-the-environment ()
  (with-found-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (if (equal name "FOUND_API_KEY") "env-key" nil))
      (with-cell-stop ((found-start "base_url" "https://found.test/mcp"))
        (is (equal "env-key" (nodecode-found::api-key)) "FOUND_API_KEY is read when the section names none")))))

;;; --- the wire ------------------------------------------------------------------

(deftest found-cell-status-speaks-the-mcp-wire ()
  (with-found-wire
    (with-cell-stop ((found-start "base_url" "https://found.test/mcp" "api_key" "k-test"))
      (found-server :text "Plan: growth. LinkedIn: healthy. Credits: 240.")
      (let ((answer-text (found-tool "found_status" (nlk:json-object))))
        (is (search "Plan: growth" answer-text) "the content block's text is the answer"))
      (let ((requests (found-asked)))
        (is (= 3 (length requests)) "initialize, the notification, then the call")
        (is (equal "initialize" (found-call-method (first requests))))
        (is (equal "notifications/initialized" (found-call-method (second requests))))
        (is (equal "tools/call" (found-call-method (third requests))))
        (is (equal "connect_status" (found-call-tool (third requests))) "the original's own tool name")
        (dolist (request requests)
          (is (equal :post (first request)) "every message is one POST")
          (is (equal "https://found.test/mcp" (second request)))
          (is (equal "Bearer k-test" (found-header request "authorization")) "the section's key")
          (is (search "text/event-stream" (found-header request "accept")) "an SSE answer is accepted"))
        (is (null (found-header (first requests) "mcp-session-id")) "initialize opens the session")
        (is (equal "s-1" (found-header (second requests) "mcp-session-id")) "the id it answered is kept")
        (is (equal "s-1" (found-header (third requests) "mcp-session-id")))))))

(deftest found-cell-a-second-call-keeps-one-session ()
  (with-found-wire
    (with-cell-stop ((found-start "base_url" "https://found.test/mcp" "api_key" "k-test"))
      (found-server :text "nothing new")
      (found-tool "found_leads" (nlk:json-object))
      (found-tool "found_leads" (nlk:json-object))
      (is (= 4 (length (found-asked))) "one initialize, one notification, two calls")
      (is (= 2 (length (found-tool-calls)))))))

(deftest found-cell-an-sse-answer-is-read ()
  (with-found-wire
    (with-cell-stop ((found-start "base_url" "https://found.test/mcp" "api_key" "k-test"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore method url))
              (let* ((body (nlk:decode-json (getf args :content)))
                     (name (nlk:json-value body :string "method"))
                     (id (nlk:json-value body :integer "id")))
                (cond
                  ((equal name "initialize")
                   (values (found-rpc id "{}") 200 (list (cons "mcp-session-id" "s-2"))))
                  ((equal name "notifications/initialized") (values "" 202 nil))
                  (t (values (format nil "event: message~%data: ~a~%~%"
                                     (found-rpc id (found-content "6 people, top 3 listed")))
                             200 (list (cons "content-type" "text/event-stream"))))))))
      (is (search "6 people" (found-tool "found_status" (nlk:json-object))) "a text/event-stream body answers like a JSON one"))))

(deftest found-cell-a-forgotten-session-is-opened-again ()
  (with-found-wire
    (with-cell-stop ((found-start "base_url" "https://found.test/mcp" "api_key" "k-test"))
      (let ((calls 0))
        (setf *answer-fn*
              (lambda (method url args)
                (declare (ignore method url))
                (let* ((body (nlk:decode-json (getf args :content)))
                       (name (nlk:json-value body :string "method"))
                       (id (nlk:json-value body :integer "id")))
                  (cond
                    ((equal name "initialize")
                     (incf calls)
                     (values (found-rpc id "{}") 200 (list (cons "mcp-session-id" "s-3"))))
                    ((equal name "notifications/initialized") (values "" 202 nil))
                    ((= calls 1) (values "{\"error\":\"session expired\"}" 404 nil))
                    (t (values (found-rpc id (found-content "answered on the new session")) 200 nil))))))
        (is (search "answered on the new session" (found-tool "found_status" (nlk:json-object))) "a 404 re-opens the session and repeats the call once")
        (is (= 2 calls) "one new initialize")))))

;;; --- the six tools ---------------------------------------------------------------

(deftest found-cell-leads-carry-their-filters ()
  (with-found-wire
    (with-cell-stop ((found-start "base_url" "https://found.test/mcp" "api_key" "k-test"))
      (found-server :text "6 people. The top 3: ...")
      (found-tool "found_leads" (nlk:json-object "min_flames" 2 "query" "Acme"))
      (let* ((call (first (found-tool-calls)))
             (arguments (found-call-arguments call)))
        (is (equal "find_leads" (found-call-tool call)))
        (is (= 2 (nlk:json-value arguments :integer "min_flames")) "the call's filter")
        (is (equal "Acme" (nlk:json-value arguments :string "query")))
        (is (= 7 (nlk:json-value arguments :integer "found_within_days")) "this week by default")
        (is (= 10 (nlk:json-value arguments :integer "per_page")) "a small page by default"))
      (setf *requests* '())
      (found-tool "found_leads" (nlk:json-object "found_within_days" 30 "per_page" 50))
      (let ((arguments (found-call-arguments (first (found-tool-calls)))))
        (is (= 30 (nlk:json-value arguments :integer "found_within_days")) "the call's own window")
        (is (= 50 (nlk:json-value arguments :integer "per_page")))))))

(deftest found-cell-numbers-reach-their-view ()
  (with-found-wire
    (with-cell-stop ((found-start "base_url" "https://found.test/mcp" "api_key" "k-test"))
      (found-server :text "Reply rate 6.1%")
      (found-tool "found_numbers" (nlk:json-object "view" "stats" "last_7_days" t))
      (let* ((call (first (found-tool-calls)))
             (arguments (found-call-arguments call)))
        (is (equal "pipeline_stats" (found-call-tool call)) "stats is the Dashboard's numbers")
        (is (eq t (nlk:json-value arguments :boolean "last_7_days")) "as the call asked"))
      (setf *requests* '())
      (found-tool "found_numbers" (nlk:json-object "view" "accounts"))
      (is (equal "prioritise_accounts" (found-call-tool (first (found-tool-calls)))) "accounts is the heated-accounts view")
      (setf *requests* '())
      (is-carrying (answer (found-tool "found_numbers" (nlk:json-object "view" "report")))
                   ("campaign") ("required"))
      (is (null (found-tool-calls)) "a report with no campaign asks first")
      (is-carrying (answer (found-tool "found_numbers" (nlk:json-object "view" "vibes")))
                   ("view is one of")))))

(deftest found-cell-account-and-draft-reach-their-tools ()
  (with-found-wire
    (with-cell-stop ((found-start "base_url" "https://found.test/mcp" "api_key" "k-test"))
      (found-server :text "Acme: 40 people, SaaS, raised $4M last week")
      (is (search "raised $4M" (found-tool "found_account" (nlk:json-object "company" "Acme"))))
      (let* ((call (first (found-tool-calls)))
             (arguments (found-call-arguments call)))
        (is (equal "research_account" (found-call-tool call)))
        (is (equal "Acme" (nlk:json-value arguments :string "company"))))
      (setf *requests* '())
      (found-tool "found_draft" (nlk:json-object "lead_id" "lead-7"))
      (let* ((call (first (found-tool-calls)))
             (arguments (found-call-arguments call)))
        (is (equal "draft_message" (found-call-tool call)))
        (is (equal "lead-7" (nlk:json-value arguments :string "lead_id"))))
      (is-carrying (answer (found-tool "found_account" (nlk:json-object))) ("company")))))

(deftest found-cell-a-credit-search-asks-its-price-first ()
  (with-found-wire
    (with-cell-stop ((found-start "base_url" "https://found.test/mcp" "api_key" "k-test"))
      (found-server :text "Price: 12 credits for 50 companies. quote_id q-9")
      (is-carrying (answer (found-tool "found_find" (nlk:json-object "action" "companies"
                                                                    "query" "Series A fintechs")))
                   ("spends credits") ("quote_id"))
      (is (null (found-tool-calls)) "the first call spends nothing until it is sent")
      (is (search "quote_id q-9"
                  (found-tool "found_find" (nlk:json-object "action" "companies"
                                                             "query" "Series A fintechs"
                                                             "quote_id" "q-9" "max_credits" 12))))
      (let* ((call (first (found-tool-calls)))
             (arguments (found-call-arguments call)))
        (is (equal "find_new_leads" (found-call-tool call)))
        (is (equal "q-9" (nlk:json-value arguments :string "quote_id")))
        (is (= 12 (nlk:json-value arguments :integer "max_credits")))
        (is (equal "Series A fintechs" (nlk:json-value arguments :string "query"))))
      (setf *requests* '())
      (found-tool "found_find" (nlk:json-object "action" "results"))
      (is (equal "get_new_leads" (found-call-tool (first (found-tool-calls)))) "reading a finished search's rows needs no quote")
      (is-carrying (answer (found-tool "found_find" (nlk:json-object "action" "call")))
                   ("action is one of")))))

;;; --- refusals, and /found --------------------------------------------------------

(deftest found-cell-a-refusal-is-a-repair-not-a-crash ()
  (with-found-wire
    (with-cell-stop ((found-start "base_url" "https://found.test/mcp" "api_key" "k-test"))
      (found-answer "{\"error\":\"invalid_token\",\"error_description\":\"the key is not valid\"}" 401)
      (is-carrying (answer (found-tool "found_status" (nlk:json-object)))
                   ("ERROR: found:") ("HTTP 401") ("the key is not valid"))
      (setf *requests* '())
      (found-server :tool-result "{\"content\":[{\"type\":\"text\",\"text\":\"a plan limit refused this\"}],\"isError\":true}")
      (is-carrying (answer (found-tool "found_status" (nlk:json-object)))
                   ("ERROR: found:") ("a plan limit refused this")))))

(deftest found-cell-the-command-answers ()
  (with-found-wire
    (with-cell-stop ((found-start "base_url" "https://found.test/mcp" "api_key" "k-test"))
      (let ((status (cell-entry "nodecode-found" "found" "")))
        (is (search "FOUND: https://found.test/mcp" status))
        (is (search "key set" status))
        (is (null (found-asked)) "no argument asks the network nothing"))
      (found-server :text "6 people this week")
      (is (search "6 people this week" (cell-entry "nodecode-found" "found" "Acme")))
      (let* ((call (first (found-tool-calls)))
             (arguments (found-call-arguments call)))
        (is (equal "find_leads" (found-call-tool call)))
        (is (equal "Acme" (nlk:json-value arguments :string "query")))
        (is (= 7 (nlk:json-value arguments :integer "found_within_days")))))))
