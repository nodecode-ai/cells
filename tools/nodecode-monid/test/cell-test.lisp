;;;; cell-test.lisp --- lifecycle, the six tools, /monid, and the price ceiling.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a value under its floor refused, a start with no key refused),
;;;; the tools through the seam the model calls them by -- their URL, their
;;;; body, the authorization header and their answer -- and /monid with and
;;;; without a query. Every request is a stubbed NLK:HTTP, so nothing
;;;; reaches Monid or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-monid-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Monid."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected monid request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun monid-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun monid-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun monid-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun monid-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun monid-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "monid" "MONID-CELL-" :start nodecode-monid:start-cell)

(define-cell-lifecycle-tests "monid"
  (:fixture with-monid-wire)
  (:config "api_key" "k-test" "base_url" "https://monid.test")
  (:help :monid)
  (:command "monid")
  (:running (is (nle::find-tool "monid_discover") "monid_discover is registered")
            (is (nle::find-tool "monid_inspect"))
            (is (nle::find-tool "monid_run"))
            (is (nle::find-tool "monid_run_status"))
            (is (nle::find-tool "monid_stop_run"))
            (is (nle::find-tool "monid_balance"))
            (is (equal "https://monid.test" (nodecode-monid::root)))
            (is (= 5 (nodecode-monid::setting :max-cost-cents)) "the default ceiling")
            (is (equal "0.0500" (format nil "~,4f" (nodecode-monid::run-ceiling))) "five cents"))
  (:stopped (is (null (nle::find-tool "monid_discover")) "stop unregisters the tools"))
  (:refused ("max_cost_cents" -1) ("discover_limit" 0) ("timeout_seconds" 0))
  (:idle nodecode-monid:monid-error (nodecode-monid::run-command "" nil)))

(deftest monid-cell-a-missing-key-refuses ()
  (with-monid-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (monid-start "base_url" "https://monid.test")))
                   ("monid: no API key" "MONID_API_KEY"))
      (is (null (nle::find-tool "monid_discover")) "a refused start installed nothing"))))

;;; --- the tools ------------------------------------------------------------------

(deftest monid-cell-discover-answers-cards-with-prices ()
  (with-monid-wire
    (with-cell-stop ((monid-start "api_key" "k-test" "base_url" "https://monid.test"))
      (monid-answer "{\"items\":[{\"provider\":\"apify\",\"endpoint\":\"/apidojo/tweet-scraper\",\"summary\":\"Scrape tweets by search term.\",\"score\":0.81,\"price\":{\"amount\":{\"value\":0.004,\"currency\":\"USD\"}},\"metrics\":{\"status\":\"healthy\",\"runTimeMs\":{\"p50\":4400}}}],\"total\":1}")
      (let ((answer (monid-tool "monid_discover" (nlk:json-object "query" "twitter posts" "limit" 3))))
        (is (search "1 Monid endpoints for \"twitter posts\"" answer))
        (is (search "apify /apidojo/tweet-scraper" answer))
        (is (search "0.0040 USD" answer))
        (is (search "healthy 4.4s" answer)))
      (let* ((request (first (monid-asked)))
             (body (monid-request-body request)))
        (is (= 1 (length (monid-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://monid.test/v1/discover/endpoints" (second request)))
        (is (equal "Bearer k-test" (monid-request-header request "authorization")))
        (is (equal "twitter posts" (nlk:json-value body :string "q")))
        (is (= 3 (nlk:json-value body :integer "limit")) "the call's limit")))))

(deftest monid-cell-run-sends-the-ceiling-and-the-input ()
  (with-monid-wire
    (with-cell-stop ((monid-start "api_key" "k-test" "base_url" "https://monid.test"))
      (monid-answer "{\"runId\":\"r-1\",\"status\":\"RUNNING\"}")
      (let ((answer (monid-tool "monid_run"
                                 (nlk:json-object "provider" "exa" "endpoint" "/search"
                                                  "body" (nlk:json-object "query" "oslo")))))
        (is (search "run r-1: RUNNING" answer))
        (is (search "poll monid_run_status with run_id r-1" answer)))
      (let* ((request (first (monid-asked)))
             (body (monid-request-body request)))
        (is (equal "https://monid.test/v1/run" (second request)))
        (is (equal "exa" (nlk:json-value body :string "provider")))
        (is (< (abs (- (nlk:json-value body :number "maxCost") 0.05)) 1d-6)
            "the section's ceiling rides every run")
        (is (equal "oslo" (nlk:json-value body :string "input" "body" "query")))))))

(deftest monid-cell-balance-and-a-refused-answer ()
  (with-monid-wire
    (with-cell-stop ((monid-start "api_key" "k-test" "base_url" "https://monid.test"))
      (monid-answer "{\"balance\":{\"value\":12.5,\"currency\":\"USD\"},\"held\":{\"value\":0.5,\"currency\":\"USD\"}}")
      (is (equal "Monid balance: 12.5000 USD available, 0.5000 USD held by runs in progress."
                 (monid-tool "monid_balance" (nlk:json-object))))
      (monid-answer "{\"message\":\"Invalid API key\"}" 401)
      (is-carrying (answer (monid-tool "monid_inspect"
                                        (nlk:json-object "provider" "exa" "endpoint" "/search")))
                   ("ERROR: monid:" "HTTP 401") ("Invalid API key")))))

(deftest monid-cell-command-searches-the-catalog ()
  (with-monid-wire
    (with-cell-stop ((monid-start "api_key" "k-test" "base_url" "https://monid.test"))
      (monid-answer "{\"items\":[]}")
      (is (search "no endpoint for" (nodecode-monid::run-command "amazon prices" nil)))
      (is (equal "https://monid.test/v1/discover/endpoints" (second (first (monid-asked)))))
      (is (search "Monid at https://monid.test" (nodecode-monid::run-command "" nil))))))
