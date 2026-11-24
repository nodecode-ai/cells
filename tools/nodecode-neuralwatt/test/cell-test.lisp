;;;; cell-test.lisp --- lifecycle, the views over stubbed exchanges, a refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over stubbed answers: that a
;;;; start installs the nine tools, /neuralwatt and the manual and a stop takes
;;;; every one of them back, that a section with no key refuses, that prices are
;;;; read out of the catalog with their flex note, that the dashboard reads its
;;;; three endpoints and reports a part that failed alone, that a comparison
;;;; groups the scanned rows and says when the window is incomplete, that the
;;;; costliest requests come back ranked, and that a session id is required for
;;;; a session's detail. Nothing dials: every exchange goes through the cell's
;;;; own *HTTP*.

(require :sb-posix)

(in-package #:nodecode.test)

(define-test-slice "neuralwatt" "NEURALWATT-CELL-"
  :start nodecode-neuralwatt:start-cell)

;;; --- the seam ----------------------------------------------------------------

(defvar *asked* ()
  "Every url the stubbed exchange was asked for, newest first.")

(defvar *answers* '()
  "(URL-SUBSTRING STATUS BODY) rows; the longest matching substring answers.")

(defun stub-http (method url &rest args)
  (declare (ignore method args))
  (push url *asked*)
  (let* ((rows (sort (copy-list *answers*)
                      (lambda (a b) (> (length (first a)) (length (first b))))))
         (row (find-if (lambda (row) (search (first row) url)) rows)))
    (if row
        (values (third row) (second row) '())
        (values "{}" 404 '()))))

(defun answer (substring status body)
  (push (list substring status body) *answers*))

(defmacro with-env ((name value) &body body)
  "Run BODY with the environment variable NAME set to VALUE, put back after."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (uiop:getenv ,name)))
       (unwind-protect
            (progn (sb-posix:setenv ,name ,value 1) ,@body)
         (if ,saved (sb-posix:setenv ,name ,saved 1) (sb-posix:unsetenv ,name))))))

(defmacro with-neuralwatt (&body body)
  "Run BODY with an account key in the environment and every exchange stubbed."
  `(with-env ("NEURALWATT_API_KEY" "test-neuralwatt-key")
     (let ((nodecode-neuralwatt::*http* #'stub-http)
           (*asked* '())
           (*answers* '()))
       ,@body)))

(defun answer-text (id tool json)
  "The tool's answer to one call, as the model reads it."
  (nle::message-content (execute-wire-call id tool json)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "neuralwatt"
  (:fixture with-neuralwatt)
  (:help :neuralwatt)
  (:command "neuralwatt")
  (:running (is (nle::find-tool "neuralwatt_health") "the health tool is registered")
            (is (nle::find-tool "neuralwatt_sessions") "and the sessions tool")
            (is (nle::find-tool "neuralwatt_dashboard") "and the dashboard tool")
            (is (equal "test-neuralwatt-key" (nodecode-neuralwatt::api-key)) "the key comes from the variable"))
  (:refused ("api_key_env" "NODECODE_NEURALWATT_TEST_UNSET"))
  (:stopped (is (null (nle::find-tool "neuralwatt_health")) "stop unregisters the tools")
            (is (null (nle::find-tool "neuralwatt_usage")) "every one of them")))

;;; --- the catalog and its prices --------------------------------------------------

(deftest neuralwatt-cell-prices-come-out-of-the-catalog ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (answer "v1/models" 200
              "{\"scope\":\"public\",\"data\":[{\"id\":\"nw-large\",\"metadata\":{\"display_name\":\"NW Large\",\"pricing\":{\"input_per_million\":0.5,\"output_per_million\":1.5,\"cached_input_per_million\":0.25,\"currency\":\"USD\",\"flex_discount_multiplier\":0.5}}},{\"id\":\"nw-small\",\"metadata\":{\"pricing\":{\"pricing_tbd\":true}}}]}")
      (let ((text (answer-text "p1" "neuralwatt_prices" "{}")))
        (is (search "USD per million tokens" text) "the unit is said")
        (is (search "nw-large -- NW Large: in $0.5, out $1.5, cached $0.25" text) "a priced model reads as a row")
        (is (search "flex x0.5" text) "its flex multiplier rides along")
        (is (search "nw-small" text) "a model whose price is not fixed is still listed")
        (is (search "tbd" text))
        (is (search "do not apply the multiplier again" text) "and the note the original gave")))))

(deftest neuralwatt-cell-the-catalog-lists-ids ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (answer "v1/models" 200
              "{\"scope\":\"account\",\"data\":[{\"id\":\"nw-large\",\"metadata\":{\"display_name\":\"NW Large\"}}]}")
      (let ((text (answer-text "m1" "neuralwatt_models" "{}")))
        (is (search "NeuralWatt models (account)" text) "the scope is named")
        (is (search "nw-large -- NW Large" text) "and every model with its display name")))))

;;; --- the dashboard ----------------------------------------------------------------

(deftest neuralwatt-cell-the-dashboard-reads-three-endpoints ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (answer "v1/quota" 200
              "{\"snapshot_at\":\"2026-01-01T00:00:00Z\",\"balance\":12.5,\"subscription\":{\"kwh_allowance\":100},\"key\":{\"allowance\":{\"credits\":5}},\"limits\":{\"rate_limit_tier\":\"pro\"}}")
      (answer "v1/usage/summary" 200
              "{\"totals\":{\"cost_usd\":1.25,\"requests\":12}}")
      (answer "v1/usage/energy" 200
              "{\"totals\":{\"kwh\":0.5},\"daily\":[{\"date\":\"2026-01-01\",\"kwh\":0.5}]}")
      (let ((text (answer-text "d1" "neuralwatt_dashboard" "{\"days\":3}")))
        (is (search "balance: 12.5" text) "the balance is reported")
        (is (search "rate-limit tier:" text) "and the rate-limit tier")
        (is (search "today (" text) "today's usage has its own block")
        (is (search "cost_usd: 1.25" text) "with the totals the API answered")
        (is (search "recent energy (" text) "and the energy window is named")
        (is (search "kwh: 0.5" text) "with its daily breakdown")
        (is (= 3 (length *asked*)) "three exchanges, one per endpoint")
        (is (= 2 (count-if (lambda (url) (search "start_date=" url)) *asked*)) "the two windows are sent")))))

(deftest neuralwatt-cell-one-part-of-the-dashboard-may-fail ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (answer "v1/quota" 500 "upstream is down")
      (answer "v1/usage/summary" 200 "{\"totals\":{\"requests\":2}}")
      (answer "v1/usage/energy" 200 "{\"totals\":{\"kwh\":0.1}}")
      (let ((text (answer-text "d2" "neuralwatt_dashboard" "{}")))
        (is (search "not read: /v1/quota answered HTTP 500" text) "the part that failed says so")
        (is (search "requests: 2" text) "and the parts that answered still arrive")))))

(deftest neuralwatt-cell-a-day-window-is-checked ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (let ((text (answer-text "d3" "neuralwatt_dashboard" "{\"days\":40}")))
        (is (search "ERROR: NEURALWATT-ERROR" text) "a window past thirty days is refused")
        (is (search "days must be an integer from 1 to 30" text) "in the original's words")
        (is (null *asked*) "before anything is dialed")))))

;;; --- usage: a comparison over scanned pages -------------------------------------------

(deftest neuralwatt-cell-compare-groups-the-scanned-rows ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (answer "v1/usage/requests" 200
              "{\"period\":{\"start\":\"2026-01-01\"},\"accounting_method\":\"actual\",\"requests\":[{\"requested_model\":\"nw-large\",\"model\":\"nw-large-2026\",\"cost_usd\":0.25,\"duration\":2.0,\"ttft\":0.5,\"total_tokens\":1000},{\"requested_model\":\"nw-large\",\"model\":\"nw-large-2026\",\"cost_usd\":0.75,\"duration\":4.0,\"ttft\":1.5,\"total_tokens\":3000},{\"requested_model\":\"nw-small\",\"cost_usd\":0.1,\"duration\":1.0,\"ttft\":0.25,\"total_tokens\":500}]}")
      (let ((text (answer-text "u1" "neuralwatt_usage" "{\"action\":\"compare\"}")))
        (is (search "3 requests over 1 page, the window complete" text) "the scan says what it read")
        (is (search "nw-large" text) "the requested model is the row")
        (is (search "$1" text) "its cost is summed")
        (is (< (search "nw-large" text) (search "nw-small" text)) "the costlier model is listed first")
        (is (search "nw-large-2026" text) "the served model is named beside the one asked for")
        (is (search "3" text) "its average duration is computed")))))

(deftest neuralwatt-cell-an-incomplete-scan-says-so ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (answer "v1/usage/requests?" 200
              "{\"requests\":[{\"requested_model\":\"nw-large\",\"cost_usd\":0.25}],\"next_cursor\":\"page-2\"}")
      (let ((text (answer-text "u2" "neuralwatt_usage"
                               "{\"action\":\"compare\",\"max_pages\":1}")))
        (is (search "INCOMPLETE" text) "a window only partly read is marked incomplete")
        (is (search "page-2" text) "and the cursor to read on is kept")))))

(deftest neuralwatt-cell-an-unknown-action-is-refused ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (is (eq :refused (handler-case (progn (nodecode-neuralwatt:usage-view "nope") :answered)
                          (error () :refused))) "a view the cell does not read is refused")
      (is (eq :refused (handler-case (progn (nodecode-neuralwatt:requests-view "nope") :answered)
                          (error () :refused))) "and so is a request view it does not read")
      (is (null *asked*) "nothing was dialed"))))

;;; --- requests: ranked by cost ----------------------------------------------------------

(deftest neuralwatt-cell-the-costliest-requests-come-back-ranked ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (answer "v1/usage/requests" 200
              "{\"requests\":[{\"cost_usd\":0.1,\"model\":\"nw-small\",\"total_tokens\":100},{\"cost_usd\":2.5,\"model\":\"nw-large\",\"total_tokens\":9000,\"duration\":7.0,\"ttft\":2.0},{\"model\":\"nw-small\",\"total_tokens\":10}]}")
      (let ((text (answer-text "r1" "neuralwatt_requests"
                               "{\"action\":\"top_expensive\",\"limit\":2}")))
        (is (search "2 requests carried a cost" text) "only priced rows are ranked")
        (is (< (search "$2.5" text) (search "$0.1" text)) "costliest first")
        (is (search "cost $2.5 -- model nw-large" text) "a row says its cost and model")
        (is (search "9000 tokens" text) "and what it spent")
        (is (search "ttft 2s" text) "and how long it waited")
        (is (search "cost $0.1 -- model nw-small -- 100 tokens" text) "the second priced row is there")))))

;;; --- sessions, health and a rejected key ------------------------------------------------

(deftest neuralwatt-cell-a-session-detail-needs-its-id ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (let ((text (answer-text "s1" "neuralwatt_sessions" "{\"action\":\"detail\"}")))
        (is (search "session_id is required" text) "the refusal says what to name")
        (is (null *asked*) "and nothing is dialed"))
      (answer "v1/usage/sessions/sess-7" 200 "{\"turns\":[{\"cost_usd\":0.5}]}")
      (let ((text (answer-text "s2" "neuralwatt_sessions"
                               "{\"action\":\"detail\",\"session_id\":\"sess-7\"}")))
        (is (search "NeuralWatt session sess-7" text) "a named session is read")
        (is (search "cost_usd: 0.5" text) "and its turns arrive")))))

(deftest neuralwatt-cell-health-reads-both-endpoints ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (answer "/health/detailed" 200 "{\"components\":{\"database\":\"ok\",\"redis\":\"ok\"}}")
      (answer "com/health" 200 "{\"status\":\"ok\",\"uptime\":3600}")
      (let ((text (answer-text "h1" "neuralwatt_health" "{}")))
        (is (search "status: ok" text) "the basic status")
        (is (search "database: ok" text) "and the component checks")
        (is (= 2 (length *asked*)) "two exchanges, no more")))))

(deftest neuralwatt-cell-a-rejected-key-is-said-plainly ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (answer "v1/quota" 401 "nope")
      (let ((text (answer-text "q1" "neuralwatt_quota" "{}")))
        (is (search "answered HTTP 401" text))
        (is (search "key is unset, expired" text) "with the hint the original gave")
        (is (search "nope" text) "and what the API said")))))

;;; --- the command ---------------------------------------------------------------------

(deftest neuralwatt-cell-the-command-answers-its-views ()
  (with-neuralwatt
    (with-cell-stop ((neuralwatt-start))
      (answer "v1/quota" 200 "{\"balance\":3}")
      (answer "v1/usage/summary" 200 "{\"totals\":{\"requests\":1}}")
      (answer "v1/usage/energy" 200 "{\"totals\":{\"kwh\":0.2}}")
      (is (search "NeuralWatt dashboard" (nodecode-neuralwatt::run-command "" nil)) "no argument is the dashboard")
      (is (search "balance: 3" (nodecode-neuralwatt::run-command "quota" nil)) "a word names its view")
      (is (search "not a view this cell reads" (nodecode-neuralwatt::run-command "weather" nil)) "a word it does not read is refused"))))
