;;;; cell-test.lisp --- lifecycle, all 28 tools, the command, and each refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Financial Datasets'
;;;; data: the section (a value out of range refuses the start; no key refuses
;;;; before the first request and names the member to write), every one of the
;;;; 28 tools through the real dispatch seam against a stubbed API, the key's
;;;; header, the query the API reads, paging, the screener's POST, the macro
;;;; routes, and each refusal the original's handlers wrote out by hand.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "financial-datasets"
  (:fixture with-api)
  (:config "api_key" "test-key" "api_key_env" +key-env+ "base_url" +endpoint+)
  (:help :financial-datasets)
  (:command "financial")
  (:running
   (is (= 28 (length (nodecode-financial-datasets::registered-tools))) "the original's 28 tools, no more and no fewer")
   (dolist (name (nodecode-financial-datasets::tool-names))
     (is (nle::find-tool name) (format nil "the ~a tool is registered" name))))
  (:stopped
   (dolist (name '("fd_get_news" "fd_get_income_statement" "fd_screen_stocks"))
     (is (null (nle::find-tool name)) (format nil "stop unregisters ~a" name))))
  (:refused
   ("timeout" 0)))

;;; --- every tool ----------------------------------------------------------------

(defparameter +minimal-arguments+
  '(("fd_get_beneficial_owners" . "{}")
    ("fd_get_beneficial_ownership" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_company_facts" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_earnings" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_filing_items" . "{\"ticker\":\"AAPL\",\"filing_type\":\"10-K\",\"year\":2024}")
    ("fd_list_filing_item_types" . "{}")
    ("fd_get_filings" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_financial_metrics" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_financial_metrics_snapshot" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_balance_sheet" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_income_statement" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_cash_flow_statement" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_index_fund" . "{\"ticker\":\"SPY\"}")
    ("fd_get_insider_ownership" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_insider_trades" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_institutional_investors" . "{}")
    ("fd_get_institutional_holdings" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_interest_rates" . "{}")
    ("fd_get_kpi_guidance" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_kpi_metrics" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_kpi_non_gaap" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_macro_data" . "{\"dataset\":\"yield_curve\"}")
    ("fd_get_news" . "{}")
    ("fd_get_segmented_financials" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_stock_price" . "{\"ticker\":\"AAPL\"}")
    ("fd_get_stock_prices" . "{\"ticker\":\"AAPL\"}")
    ("fd_screen_stocks" . "{\"filters\":[{\"field\":\"revenue\",\"operator\":\"gt\",\"value\":1000000000}]}")
    ("fd_list_stock_screener_filters" . "{}"))
  "One minimal call per tool: the arguments a model would send first.")

(deftest financial-datasets-cell-every-tool-answers-through-the-wire
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (dolist (name (nodecode-financial-datasets::tool-names))
        (setf *calls* '())
        (let ((text (call name (or (cdr (assoc name +minimal-arguments+ :test #'string=)) "{}"))))
          (is (not (search "ERROR" text)) (format nil "~a answers without an error: ~a" name text))
          (is (plusp (length text)) (format nil "~a answers text" name))))))

;;; --- the request the API reads ---------------------------------------------------

(deftest financial-datasets-cell-the-key-rides-a-header-and-never-the-url
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (call "fd_get_news" "{\"limit\":3}")
      (is (equal "test-key" (header "X-API-KEY")) "the key rides the X-API-KEY header")
      (is (null (search "test-key" (newest-url))) "and never the URL")
      (is (search "https://api.test/fd/news/" (newest-url)) "the configured base URL and the route")
      (is (equal "3" (cdr (assoc "limit" (query-of (newest-url)) :test #'string=))) "and the arguments as the query")))

(deftest financial-datasets-cell-a-ticker-is-upper-cased-and-a-cik-padded
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (call "fd_get_company_facts" "{\"ticker\":\"aapl\"}")
      (is (equal "AAPL" (cdr (assoc "ticker" (query-of (newest-url)) :test #'string=))) "a ticker reaches the API upper-cased")
      (call "fd_get_beneficial_ownership" "{\"filer_cik\":\"1510281\"}")
      (is (equal "0001510281" (cdr (assoc "filer_cik" (query-of (newest-url)) :test #'string=))) "a CIK is zero-padded to ten digits")))

(deftest financial-datasets-cell-a-false-flag-and-a-blank-value-are-dropped
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (call "fd_get_beneficial_ownership" "{\"ticker\":\"AAPL\",\"history\":false,\"type\":\"\"}")
      (let ((query (query-of (newest-url))))
        (is (null (assoc "history" query :test #'string=)) "a false flag is not sent")
        (is (null (assoc "type" query :test #'string=)) "nor a blank enumeration")
        (is (equal "AAPL" (cdr (assoc "ticker" query :test #'string=))) "and the real filter still is"))))

(deftest financial-datasets-cell-a-statement-takes-its-period-and-its-as-reported-route
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (let ((text (call "fd_get_income_statement" "{\"ticker\":\"AAPL\",\"period\":\"annual\",\"limit\":4}")))
        (is (search "391035000000" text) "the statement's own numbers reach the model")
        (is (search "financials/income-statements/" (newest-url)))
        (is (equal "annual" (cdr (assoc "period" (query-of (newest-url)) :test #'string=)))))
      (call "fd_get_balance_sheet" "{\"ticker\":\"AAPL\",\"as_reported\":true,\"period\":\"ttm\"}")
      (is (search "financials/balance-sheets/as-reported/" (newest-url)) "the as-reported route")
      (is (equal "annual" (cdr (assoc "period" (query-of (newest-url)) :test #'string=))) "which serves annual or quarterly only, so ttm reads annual")
      (is (null (assoc "as_reported" (query-of (newest-url)) :test #'string=)) "and as_reported is not a query parameter")))

(deftest financial-datasets-cell-a-page-link-is-followed-and-its-rows-merged
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (let ((text (call "fd_get_income_statement" "{\"ticker\":\"AAPL\",\"limit\":2}")))
        (is (search "\"revenue\": 1" text) "the first page")
        (is (search "\"revenue\": 2" text) "and the page the link named")
        (is (= 2 (length *calls*)) "two requests, not a loop")
        (is (search "cursor=two" (newest-url))))))

(deftest financial-datasets-cell-the-screener-posts-its-filters
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (let ((text (call "fd_screen_stocks"
                        "{\"filters\":[{\"field\":\"pe_ratio\",\"operator\":\"lt\",\"value\":20}]}")))
        (is (equal :post (first (newest-call))) "the screener is the one POST")
        (is (search "MSFT" text) "and its results reach the model")
        (let ((body (getf (third (newest-call)) :content)))
          (is (search "pe_ratio" body) "the filters go as they came")
          (is (search "\"currency\":\"USD\"" body) "with the default currency")
          (is (search "\"limit\":10" body) "and the default limit")))))

(deftest financial-datasets-cell-macro-routes-each-dataset
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (call "fd_get_macro_data" "{\"dataset\":\"inflation\"}")
      (is (search "macro/inflation/snapshot/" (newest-url)) "no dates means the snapshot route")
      (call "fd_get_macro_data" "{\"dataset\":\"interest_rates\",\"start_date\":\"2025-01-01\",\"bank\":\"fed\"}")
      (is (search "macro/interest-rates/?start_date" (newest-url)) "a date means the history route")
      (is (equal "FED" (cdr (assoc "bank" (query-of (newest-url)) :test #'string=))) "and a bank code is upper-cased")
      (is (search "bank code" (call "fd_get_macro_data"
                                    "{\"dataset\":\"interest_rates\",\"start_date\":\"2025-01-01\"}")) "interest-rate history names the bank it needs")
      (is (search "series id" (call "fd_get_macro_data"
                                    "{\"dataset\":\"inflation\",\"end_date\":\"2025-01-01\"}")) "and inflation history names the series it needs")))

(deftest financial-datasets-cell-a-filing-shape-that-cannot-work-refuses-before-any-request
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (let ((eight-k (call "fd_get_filing_items" "{\"ticker\":\"AAPL\",\"filing_type\":\"8-K\"}"))
            (ten-q (call "fd_get_filing_items" "{\"ticker\":\"AAPL\",\"filing_type\":\"10-Q\",\"year\":2025}")))
        (is (search "accession_number is required for 8-K" eight-k))
        (is (search "quarter is required for 10-Q" ten-q)))
      (is (null *calls*) "neither reached the API")))

(deftest financial-datasets-cell-a-missing-key-refuses-before-any-request
    (with-fd ("api_key_env" +no-key-env+ "base_url" +endpoint+)
      (let ((text (call "fd_get_news" "{}")))
        (is (search "no Financial Datasets key" text))
        (is (search "api_key" text) "the refusal names the member to write")
        (is (search +no-key-env+ text) "and the variable it would read"))
      (is (null *calls*) "and nothing was dialled")))

(deftest financial-datasets-cell-an-http-refusal-carries-the-status-and-never-the-key
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (script "news/" (lambda (method url)
                        (declare (ignore method url))
                        (values "{\"message\":\"invalid api key\"}" 401)))
      (let ((text (call "fd_get_news" "{}")))
        (is (search "HTTP 401" text) "the status")
        (is (search "invalid api key" text) "and the API's own sentence")
        (is (null (search "test-key" text)) "never the key"))
      (is (= 1 (length *calls*)) "a refusal is not retried")))

;;; --- the command ----------------------------------------------------------------

(deftest financial-datasets-cell-the-command-says-what-this-cell-is
    (with-fd ("api_key" "test-key" "base_url" +endpoint+)
      (let ((status (nodecode-financial-datasets::run-command "" "s1"))
            (bogus (nodecode-financial-datasets::run-command "not a ticker!" "s1")))
        (is (search "financial-datasets: https://api.test/fd/" status))
        (is (search "key: set in the section" status))
        (is (search "fd_get_income_statement" status) "it names what this start registered")
        (is (search "not a ticker" bogus)))
      (is (null *calls*) "the status command dials nothing")
      (is (search "\"price\": 190" (nodecode-financial-datasets::run-command "aapl" "s1")) "and a ticker answers its price snapshot")))
