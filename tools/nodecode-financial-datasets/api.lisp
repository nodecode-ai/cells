;;;; api.lisp --- the Financial Datasets REST API, and the 28 tools over it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is one HTTP client and one handler per tool, written against
;;;; the same REST API this file speaks: the same routes, the same query
;;;; parameters, the same `next_page_url' paging, the same POST for the
;;;; screener. What changes is the language and the seam: NLK:HTTP instead of
;;;; urllib, and a table of tools instead of 28 near-identical functions.
;;;;
;;;; The key rides an X-API-KEY request header and never the URL, so it cannot
;;;; appear in a log line or in a refusal this cell writes. It is read from the
;;;; section, else from the environment variable the section names, on every
;;;; request.

(in-package #:nodecode-financial-datasets)

;;; --- where it calls, and with what -------------------------------------------------

(defparameter +default-base-url+ "https://api.financialdatasets.ai/"
  "The one host this cell dials, as the original hard-coded it. The trailing
slash is part of the address: routes are appended to it, and a paging link is
followed only when it starts with it.")

(defparameter +default-key-env+ "FINANCIAL_DATASETS_API_KEY"
  "The variable the original's manifest asked for, and this cell's default.")

(defparameter +user-agent+ "nodecode-financial-datasets/0.1.0"
  "What the API sees this cell as.")

(defparameter +page-limit+ 100
  "The most pages one call follows. The API caps every list at a fixed page
size and links the rest through `next_page_url'; the model cannot be trusted
to loop over cursors, so a call follows them, and this bounds a pathological
one (months of minute bars) by returning the rows gathered so far.")

(defun base-url ()
  "The address this start dials, ending in a slash."
  (let ((url (or (setting :base-url) +default-base-url+)))
    (if (char= #\/ (char url (1- (length url))))
        url
        (concatenate 'string url "/"))))

(defun api-key ()
  "The key: the secret in the section, else the environment variable the
section names. => a string, or NIL when neither holds one."
  (or (setting :api-key)
      (let ((name (setting :api-key-env)))
        (and (stringp name)
             (plusp (length name))
             (let ((value (nlk:trimmed (or (uiop:getenv name) ""))))
               (and (plusp (length value)) value))))))

(defun request-headers ()
  "The headers one request carries: the key, and JSON in and out."
  (let ((key (api-key)))
    (append (when key (list (cons "X-API-KEY" key)))
            (list (cons "User-Agent" +user-agent+)
                  (cons "Accept" "application/json")))))

;;; --- one request -------------------------------------------------------------------

(defun error-sentence (decoded text)
  "The API's own `message' or `error', else the body's first 200 characters."
  (or (and (hash-table-p decoded)
           (or (let ((message (nlk:json-value decoded :text "message")))
                 (and message (nlk:trimmed message)))
               (let ((error (nlk:json-value decoded :text "error")))
                 (and error (nlk:trimmed error)))))
      (let ((body (and (stringp text) (nlk:trimmed text))))
        (and body (plusp (length body))
             (subseq body 0 (min 200 (length body)))))
      "no detail"))

(defun request (method url &key content)
  "One call to the API => its decoded body. A refused status, an answer that
is not JSON and a transport failure are one condition, FAIL."
  (unless (api-key)
    (fail "no Financial Datasets key: write api_key in the financial-datasets ~
section, or export ~a" (or (setting :api-key-env) +default-key-env+)))
  (multiple-value-bind (body status)
      (nlk:http method url
                :headers (request-headers)
                :content content
                :timeout (or (setting :timeout) 30))
    (let ((decoded (and body (plusp (length (nlk:trimmed body)))
                        (ignore-errors (nlk:decode-json body)))))
      (when (>= status 400)
        (fail "Financial Datasets answered HTTP ~d: ~a" status (error-sentence decoded body)))
      (or decoded
          (fail "Financial Datasets answered HTTP ~d with a body that is not JSON" status)))))

(defun quoted (value)
  "VALUE as one URL-encoded query string piece."
  (quri:url-encode (princ-to-string value) :encoding :utf-8))

(defun api-url (path &optional parameters)
  "The absolute URL for PATH with PARAMETERS, an alist of (NAME . VALUE); a
VALUE that is a list is one repetition of NAME per element, as the API reads a
repeated `item'."
  (let ((url (concatenate 'string (base-url) path))
        (pieces '()))
    (dolist (pair parameters)
      (destructuring-bind (name . value) pair
        (if (listp value)
            (dolist (one value)
              (push (format nil "~a=~a" name (quoted one)) pieces))
            (push (format nil "~a=~a" name (quoted value)) pieces))))
    (if pieces
        (format nil "~a?~{~a~^&~}" url (nreverse pieces))
        url)))

(defun api-host-page-p (url)
  "Whether URL is a follow-up page on the configured host. The key never goes
anywhere else, so a link off the host is not followed."
  (and (stringp url)
       (uiop:string-prefix-p (base-url) url)))

(defun merge-page (accumulated page)
  "Merge a follow-up PAGE: record lists are concatenated (recursively, in case
an envelope nests them one level down), and scalars keep the first page's."
  (when (hash-table-p page)
    (maphash (lambda (key value)
               (unless (string= key "next_page_url")
                 (multiple-value-bind (existing present) (gethash key accumulated)
                   (cond ((and present (hash-table-p existing) (hash-table-p value))
                          (merge-page existing value))
                         ((and present (vectorp existing) (vectorp value)
                               (not (stringp existing)))
                          (setf (gethash key accumulated)
                                (concatenate 'vector existing value)))
                         ((not present) (setf (gethash key accumulated) value))))))
             page))
  accumulated)

(defun get-json (path &optional parameters &key paginated)
  "One GET, following `next_page_url' when PAGINATED => the decoded body."
  (let ((decoded (request :get (api-url path parameters))))
    (when paginated
      (let ((next (nlk:json-value decoded :string "next_page_url"))
            (pages 1))
        (loop while (and (api-host-page-p next) (< pages +page-limit+))
              do (let ((page (request :get next)))
                   (merge-page decoded page)
                   (setf next (nlk:json-value page :string "next_page_url"))
                   (incf pages)))
        (remhash "next_page_url" decoded)))
    decoded))

(defun post-json (path body)
  "One POST of BODY, a JSON object => the decoded answer."
  (request :post (api-url path)
           :content (nlk:encode-json-object body)))

;;; --- the call's arguments as the API's query ---------------------------------------

(defun argument (arguments name)
  "The call's NAME argument, or NIL when it is absent, JSON null or JSON
false — the three the API reads the same way, as no filter."
  (nlk:json-value arguments :any name))

(defun empty-argument-p (value)
  "Whether VALUE is one the API should not be sent: absent, false, or a blank
string."
  (or (null value)
      (and (stringp value) (zerop (length (nlk:trimmed value))))))

(defun zero-padded (text width)
  "TEXT padded on the left with zeros to WIDTH characters, as a CIK is."
  (let ((text (princ-to-string text)))
    (if (>= (length text) width)
        text
        (concatenate 'string (make-string (- width (length text)) :initial-element #\0) text))))

(defun query-value (name value)
  "VALUE as the API reads it: a ticker upper-cased, a CIK zero-padded, a true
flag spelled `true', an array as its elements."
  (cond ((eq value t) "true")
        ((and (vectorp value) (not (stringp value))) (coerce value 'list))
        ((stringp value)
         (cond ((string= name "ticker") (string-upcase value))
               ((string= name "filer_cik") (zero-padded (nlk:trimmed value) 10))
               (t (nlk:trimmed value))))
        (t value)))

(defun query-parameters (tool arguments)
  "The call's arguments as the API's query parameters: only the members the
tool declares, blanks and false flags dropped, and each spelled the way the
route reads it. The order is the tool's own, so a test can read the URL."
  (loop for spec in (getf tool :params)
        for name = (first spec)
        for value = (argument arguments name)
        unless (empty-argument-p value)
          collect (cons name (query-value name value))))

;;; --- the fetchers -----------------------------------------------------------------

(defun fetch-route (arguments tool)
  "A tool whose arguments are the query string of one route: the GET, and the
member of its answer the tool promises."
  (let ((data (get-json (getf tool :path) (query-parameters tool arguments)
                         :paginated (getf tool :paginated))))
    (if (getf tool :key)
        (gethash (getf tool :key) data)
        data)))

(defun require-one-of (tool arguments first second)
  "Exactly one of FIRST and SECOND, as the two 13D/13G and 13F tools need."
  (declare (ignore tool))
  (let ((a (argument arguments first))
        (b (argument arguments second)))
    (when (and (empty-argument-p a) (empty-argument-p b))
      (fail "either ~a or ~a is required" first second))
    (when (and (not (empty-argument-p a)) (not (empty-argument-p b)))
      (fail "give either ~a or ~a, not both" first second))))

(defun fetch-beneficial-owners (arguments tool)
  "The 13D/13G filer list: the whole answer, `total' and the page of filers."
  (get-json (getf tool :path) (query-parameters tool arguments)))

(defun fetch-beneficial-ownership (arguments tool)
  (require-one-of tool arguments "ticker" "filer_cik")
  (fetch-route arguments tool))

(defun fetch-company-facts (arguments tool)
  (require-one-of tool arguments "ticker" "cik")
  (fetch-route arguments tool))

(defun fetch-institutional-holdings (arguments tool)
  (require-one-of tool arguments "ticker" "filer_cik")
  (fetch-route arguments tool))

(defun fetch-filings (arguments tool)
  "A ticker wins over a CIK, as the original and the MCP server read it."
  (when (and (empty-argument-p (argument arguments "ticker"))
             (empty-argument-p (argument arguments "cik")))
    (fail "either ticker or cik is required"))
  (let ((parameters (query-parameters tool arguments)))
    (when (argument arguments "ticker")
      (setf parameters (remove "cik" parameters :key #'car :test #'string=)))
    (let ((data (get-json (getf tool :path) parameters :paginated t)))
      (gethash (getf tool :key) data))))

(defparameter +items-10-k+
  '("Item-1" "Item-1A" "Item-1B" "Item-2" "Item-3" "Item-4" "Item-5" "Item-6" "Item-7"
    "Item-7A" "Item-8" "Item-9" "Item-9A" "Item-9B" "Item-10" "Item-11" "Item-12"
    "Item-13" "Item-14" "Item-15" "Item-16"))
(defparameter +items-10-q+ '("Item-1" "Item-2" "Item-3" "Item-4"))
(defparameter +items-8-k+
  '("Item-1.01" "Item-1.02" "Item-1.03" "Item-1.04" "Item-2.01" "Item-2.02" "Item-2.03"
    "Item-2.04" "Item-2.05" "Item-2.06" "Item-3.01" "Item-3.02" "Item-3.03" "Item-4.01"
    "Item-4.02" "Item-5.01" "Item-5.02" "Item-5.03" "Item-5.04" "Item-5.05" "Item-5.06"
    "Item-5.07" "Item-5.08" "Item-6.01" "Item-6.02" "Item-6.03" "Item-6.04" "Item-6.05"
    "Item-7.01" "Item-8.01" "Item-9.01"))

(defun item-types ()
  "The item names each filing type serves, which the original answers from a
table rather than the API."
  (nlk:json-object "10-K" (coerce +items-10-k+ 'vector)
                    "10-Q" (coerce +items-10-q+ 'vector)
                    "8-K" (coerce +items-8-k+ 'vector)))

(defun fetch-filing-item-types (arguments tool)
  (declare (ignore arguments tool))
  (item-types))

(defun fetch-filing-items (arguments tool)
  "10-K and 10-Q are addressed by year (and quarter), 8-K by accession
number; the API refuses the other shapes, and this says so first."
  (let ((filing-type (argument arguments "filing_type")))
    (if (and (stringp filing-type) (string= filing-type "8-K"))
        (when (empty-argument-p (argument arguments "accession_number"))
          (fail "accession_number is required for 8-K filings"))
        (progn
          (when (empty-argument-p (argument arguments "year"))
            (fail "year is required for 10-K and 10-Q filings"))
          (when (and (stringp filing-type) (string= filing-type "10-Q")
                     (empty-argument-p (argument arguments "quarter")))
            (fail "quarter is required for 10-Q filings"))))
    (get-json (getf tool :path) (query-parameters tool arguments))))

(defun financial-statement (arguments tool)
  "One of the three statements, as reported or as standardized. The
as-reported routes live at `<route>/as-reported' and serve only `annual' or
`quarterly', so `ttm' becomes `annual' there."
  (let* ((as-reported (eq (argument arguments "as_reported") t))
         (path (if as-reported
                   (format nil "financials/~a/as-reported/" (getf tool :route))
                   (format nil "financials/~a/" (getf tool :route))))
         (parameters (query-parameters tool arguments))
         (data nil))
    (setf parameters (remove "as_reported" parameters :key #'car :test #'string=))
    (when (and as-reported (equal "ttm" (cdr (assoc "period" parameters :test #'string=))))
      (setf (cdr (assoc "period" parameters :test #'string=)) "annual"))
    (setf data (get-json path parameters :paginated t))
    (gethash (getf tool :key) data)))

(defparameter +macro-paths+
  '(("yield_curve" . "macro/yield-curve")
    ("interest_rates" . "macro/interest-rates")
    ("inflation" . "macro/inflation"))
  "One REST route per macro dataset.")

(defun fetch-macro (arguments tool)
  "The macro datasets: no dates means the snapshot route (the latest values);
  any date means the history route, which serves one bank or one series per
  request."
  (declare (ignore tool))
  (let* ((dataset (argument arguments "dataset"))
         (path (and (stringp dataset) (cdr (assoc dataset +macro-paths+ :test #'string=)))))
    (unless path
      (fail "dataset must be one of: ~{~a~^, ~}" (mapcar #'car +macro-paths+)))
    (let ((snapshot (and (empty-argument-p (argument arguments "start_date"))
                          (empty-argument-p (argument arguments "end_date")))))
      (when (and (not snapshot) (string= dataset "interest_rates")
                 (empty-argument-p (argument arguments "bank")))
        (fail "interest_rates history is served per central bank: give a bank code ~
(e.g. 'FED', 'ECB', 'BOJ'), or omit the dates for every bank's current rate"))
      (when (and (not snapshot) (string= dataset "inflation")
                 (empty-argument-p (argument arguments "series")))
        (fail "inflation history is served per series: give a series id (e.g. ~
'cpi_all_sa'), or omit the dates for the latest month of every series"))
      (let* ((parameters (macro-parameters arguments))
             (data (get-json (if snapshot
                                 (format nil "~a/snapshot/" path)
                                 (format nil "~a/" path))
                             parameters
                             :paginated (not snapshot))))
        (gethash dataset data)))))

(defun macro-parameters (arguments)
  "The macro tools' four optional members, with the bank upper-cased and the
series lower-cased, as the original spelled them before the request."
  (loop for (name . transform) in (list (cons "start_date" #'identity)
                                         (cons "end_date" #'identity)
                                         (cons "bank" #'string-upcase)
                                         (cons "series" #'string-downcase))
        for value = (argument arguments name)
        unless (empty-argument-p value)
          collect (cons name (funcall transform (query-value name value)))))

;;; --- today, in the market's own time zone -------------------------------------------

(defun nth-weekday-of-month (year month weekday n)
  "The day-of-month of the N-th WEEKDAY (0 is Sunday) of MONTH, which is how
both ends of US daylight time are named."
  (let* ((first (encode-universal-time 0 0 0 1 month year 0))
         (first-weekday (nth-value 6 (decode-universal-time first 0)))
         (day (+ 1 (mod (- weekday first-weekday) 7) (* 7 (1- n)))))
    day))

(defun eastern-today ()
  "Today's date in US Eastern time, YYYY-MM-DD: the calendar the market
keeps, from the UTC clock this process has. Daylight time runs from the
second Sunday of March to the first Sunday of November."
  (let* ((now (get-universal-time))
         (year (nth-value 5 (decode-universal-time now 0)))
         (dst-start (encode-universal-time 0 0 7 (nth-weekday-of-month year 3 0 2) 3 year))
         (dst-end (encode-universal-time 0 0 6 (nth-weekday-of-month year 11 0 1) 11 year))
         (offset (if (and (>= now dst-start) (< now dst-end)) -4 -5)))
    (multiple-value-bind (second minute hour day month year) (decode-universal-time (+ now (* offset 3600)) 0)
      (declare (ignore second minute hour))
      (format nil "~4,'0d-~2,'0d-~2,'0d" year month day))))

(defun one-year-before (day)
  "DAY, a YYYY-MM-DD string, one year earlier: February 29 becomes
February 28 in a common year, as the original's replace() did."
  (let* ((year (parse-integer day :start 0 :end 4))
         (month (parse-integer day :start 5 :end 7))
         (date (parse-integer day :start 8 :end 10))
         (earlier (1- year)))
    (when (and (= month 2) (= date 29)
               (not (and (zerop (mod earlier 4))
                         (or (plusp (mod earlier 100)) (zerop (mod earlier 400))))))
      (setf date 28))
    (format nil "~4,'0d-~2,'0d-~2,'0d" earlier month date)))

(defun fetch-stock-prices (arguments tool)
  "The window ends today in Eastern time unless the call names a day, and
starts a year before it ends unless the call names a start."
  (let* ((parameters (query-parameters tool arguments))
         (end (or (cdr (assoc "end_date" parameters :test #'string=)) (eastern-today)))
         (start (or (cdr (assoc "start_date" parameters :test #'string=))
                    (one-year-before end))))
    (setf parameters (remove-if (lambda (pair) (member (car pair) '("start_date" "end_date")
                                                       :test #'string=))
                                parameters))
    (let ((data (get-json (getf tool :path)
                           (append parameters
                                   (list (cons "start_date" start) (cons "end_date" end)))
                           :paginated t)))
      (gethash (getf tool :key) data))))

(defun fetch-screen-stocks (arguments tool)
  "The screener is the one POST: the filters go as they came, with the
currency and the limit beside them."
  (declare (ignore tool))
  (let ((filters (argument arguments "filters")))
    (unless (and (vectorp filters) (plusp (length filters)))
      (fail "at least one filter is required: call fd_list_stock_screener_filters ~
to see the fields"))
    (let ((body (nlk:json-object "filters" filters
                                  "currency" (or (argument arguments "currency") "USD")
                                  "limit" (or (argument arguments "limit") 10))))
      (gethash "results" (post-json "financials/search/screener/" body)))))

;;; --- the 28 tools ------------------------------------------------------------------
;;; One table, read by START-CELL: the same 28 tools, the same names (the
;;; original's `fd_' prefix, so they cannot collide with another cell's), the
;;; same routes, and the same arguments. A member carries the JSON type the
;;; provider is shown, a line of documentation, and where the original had one,
;;; a default, a range or an enumeration.

(defun tool (name description parameters &key path key paginated route fetch)
  "One row of the table below: what the provider is shown, and where the call goes."
  (list :name name :description description :params parameters :path path :key key
        :paginated paginated :route route :fetch (or fetch 'fetch-route)))

(defparameter +ticker+ '("ticker" "string" "The stock ticker symbol (e.g. 'AAPL', 'MSFT', 'TSLA')."
                          :required t))
(defparameter +ticker-optional+
  '("ticker" "string" "The stock ticker symbol (e.g. 'AAPL', 'MSFT', 'TSLA')."))
(defparameter +period-ttm+
  '("period" "string" "annual, quarterly or ttm (default: ttm)."
    :enum ("annual" "quarterly" "ttm") :default "ttm"))
(defparameter +period-kpi+
  '("period" "string" "quarterly or annual (default: quarterly)."
    :enum ("quarterly" "annual") :default "quarterly"))
(defparameter +limit-4+ '("limit" "integer" "Number of periods to return (default: 4)." :default 4 :min 1))
(defparameter +report-periods+
  '(("report_period" "string" "Exact report period (YYYY-MM-DD).")
    ("report_period_gte" "string" "Report period greater than or equal to (YYYY-MM-DD).")
    ("report_period_lte" "string" "Report period less than or equal to (YYYY-MM-DD).")
    ("report_period_gt" "string" "Report period strictly greater than (YYYY-MM-DD).")
    ("report_period_lt" "string" "Report period strictly less than (YYYY-MM-DD).")))
(defparameter +filing-dates+
  '(("filing_date" "string" "Exact filing date (YYYY-MM-DD).")
    ("filing_date_gte" "string" "Filing date greater than or equal to (YYYY-MM-DD).")
    ("filing_date_lte" "string" "Filing date less than or equal to (YYYY-MM-DD).")))
(defparameter +kpi-metric+
  '("metric_name" "string" "Filter by metric name, when only one is wanted."))

(defparameter +tools+
  (list
   (tool "fd_get_beneficial_owners"
         "Lists beneficial owners (holders of more than 5% of a company's shares, from SEC Schedules 13D/13G) with their filer CIK and reporting-person name. Optionally filter by case-insensitive name prefix. The answer's `total' is the full match count; when it exceeds the page, narrow the search with `name'. Use it to find the filer_cik for fd_get_beneficial_ownership."
         '(("name" "string" "Optional case-insensitive name prefix to filter the list (e.g. 'Saba').")
           ("limit" "integer" "Number of owners to return (default: 100, max: 1000)." :default 100 :min 1))
         :path "beneficial-ownership/filers/" :fetch 'fetch-beneficial-owners)
   (tool "fd_get_beneficial_ownership"
         "Retrieves beneficial-ownership stakes (holders of more than 5% of a class of shares) from SEC Schedules 13D and 13G, monthly since January 2025. 13D stakes are actvist (intent to influence control), 13G passive. Query by ticker (who owns this company) OR by filer_cik (what does this owner hold): exactly one is required. Each row is one reporting person with percent_of_class and, for 13D, purpose_of_transaction."
         (list +ticker-optional+
               '("filer_cik" "string" "SEC CIK of the beneficial owner (e.g. '0001510281'); zero-padded to 10 digits.")
               '("type" "string" "Filter by stake type: activist (13D) or passive (13G). Omit for both."
                 :enum ("activist" "passive"))
               '("history" "boolean" "When true, the full amendment history of each stake instead of only its current state.")
               '("filing_date" "string" "Exact filing date (YYYY-MM-DD).")
               '("filing_date_gte" "string" "Filing date greater than or equal to (YYYY-MM-DD).")
               '("filing_date_lte" "string" "Filing date less than or equal to (YYYY-MM-DD).")
               '("limit" "integer" "Number of rows to return (default: 10, max: 1000)." :default 10 :min 1))
         :path "beneficial-ownership/" :key "beneficial_owners" :paginated t
         :fetch 'fetch-beneficial-ownership)
   (tool "fd_get_company_facts"
         "Company facts for a ticker or a CIK: market cap, employees, sector and industry, exchange, location, website, SIC codes, weighted average shares, and events such as ticker changes."
         '(("ticker" "string" "The stock ticker symbol (e.g. 'AAPL'). Either ticker or cik is required.")
           ("cik" "string" "The CIK (Central Index Key). Either ticker or cik is required."))
         :path "company/facts/" :key "company_facts" :fetch 'fetch-company-facts)
   (tool "fd_get_earnings"
         "Earnings from SEC filings, one flat list either way. Give a `ticker' for that company's most recent filings (8-K/10-Q/10-K/20-F), sorted by report_period then filing_date; omit it for the real-time feed of the most recently filed earnings across every covered company. Each entry carries ticker, report_period, fiscal_period, filing_date, accession_number and the quarterly and annual financial blocks."
         (list '("ticker" "string" "The company's ticker; omit for the real-time feed of every company's newest filing."))
         :path "earnings/" :key "earnings" :paginated t)
   (tool "fd_get_filing_items"
         "Retrieves specific items from a company's SEC filings: risk factors, business, MD&A, financial statements and the rest, by item name (fd_list_filing_item_types names them). 10-K and 10-Q are addressed by year (and quarter for 10-Q), 8-K by accession number."
         '(("ticker" "string" "The ticker symbol." :required t)
           ("filing_type" "string" "The type of filing." :required t :enum ("10-K" "10-Q" "8-K"))
           ("year" "integer" "The year of the filing. Required for 10-K and 10-Q.")
           ("quarter" "integer" "The quarter of a 10-Q filing.")
           ("accession_number" "string" "The accession number. Required for 8-K filings.")
           ("item" "array" "The item or items to get; the 8-K and 10-K/10-Q names are each served by their own filing types.")
           ("limit" "integer" "Number of items to return." :min 1))
         :path "filings/items/" :fetch 'fetch-filing-items)
   (tool "fd_list_filing_item_types"
         "The item names that can be extracted from 10-K, 10-Q and 8-K filings, grouped by filing type. Call it before fd_get_filing_items when the item name is not certain."
         '()
         :fetch 'fetch-filing-item-types)
   (tool "fd_get_filings"
         "SEC filings for a ticker or a CIK: the accession number, the filing type, the report date, and the URLs of the documents. A ticker wins over a CIK when both are given."
         '(("ticker" "string" "The stock ticker symbol (e.g. 'AAPL'). Either ticker or cik is required.")
           ("cik" "string" "The CIK (Central Index Key). Either ticker or cik is required.")
           ("limit" "integer" "Number of filings to retrieve (default: 100, max: 1000)." :default 100 :min 1)
           ("filing_type" "string" "Filter by filing type (e.g. '10-K', '10-Q', '8-K', 'DEF 14A')."))
         :path "filings/" :key "filings" :paginated t :fetch 'fetch-filings)
   (tool "fd_get_financial_metrics"
         "Financial metrics and valuation ratios over time, one row per report period: what a screen or a valuation question needs, by ticker and period."
         (append (list +ticker+ +period-ttm+ +limit-4+) +report-periods+)
         :path "financial-metrics/" :key "financial_metrics" :paginated t)
   (tool "fd_get_financial_metrics_snapshot"
         "The latest financial metrics and valuation ratios for one ticker, as a single object."
         (list +ticker+)
         :path "financial-metrics/snapshot/" :key "snapshot")
   (tool "fd_get_balance_sheet"
         "The balance sheet as standardized (or as reported, with as_reported), one row per period: assets, liabilities and equity lines, by ticker."
         (append (list +ticker+ +period-ttm+ +limit-4+
                       '("as_reported" "boolean" "When true, the as-reported statement from the filing rather than the standardized one."))
                 +report-periods+)
         :path "balance-sheets" :key "balance_sheets" :route "balance-sheets"
         :fetch 'financial-statement)
   (tool "fd_get_income_statement"
         "The income statement as standardized (or as reported, with as_reported), one row per period: revenue, cost of revenue, gross and operating profit, net income, EPS."
         (append (list +ticker+ +period-ttm+ +limit-4+
                       '("as_reported" "boolean" "When true, the as-reported statement from the filing rather than the standardized one."))
                 +report-periods+)
         :path "income-statements" :key "income_statements" :route "income-statements"
         :fetch 'financial-statement)
   (tool "fd_get_cash_flow_statement"
         "The cash flow statement as standardized (or as reported, with as_reported), one row per period: operating, investing and financing flows, capex, free cash flow."
         (append (list +ticker+ +period-ttm+ +limit-4+
                       '("as_reported" "boolean" "When true, the as-reported statement from the filing rather than the standardized one."))
                 +report-periods+)
         :path "cash-flow-statements" :key "cash_flow_statements" :route "cash-flow-statements"
         :fetch 'financial-statement)
   (tool "fd_get_index_fund"
         "An ETF or index fund's holdings and each position's weight (percent of net assets), from SEC fund-holdings filings: the latest filing by default, or the composition in effect on a past date, sorted by weight."
         '(("ticker" "string" "The fund ticker symbol (e.g. 'SPY', 'QQQ', 'VTI')." :required t)
           ("as_of" "string" "The composition in effect on or before this date (YYYY-MM-DD); the latest filing by default.")
           ("asset_class" "string" "Filter holdings by instrument type." :enum ("equity" "bond"))
           ("limit" "integer" "Number of holdings to return (default: 50, max: 1000)." :default 50 :min 1)
           ("offset" "integer" "Number of holdings to skip, for paging (default: 0)." :default 0 :min 0))
         :path "index-funds/" :paginated t)
   (tool "fd_get_insider_ownership"
         "What a company's officers, directors and 10% owners actually hold (common shares, options, RSUs), from SEC Form 3 and Form 5: the state, where fd_get_insider_trades is the events. Positions are returned as reported per filing, newest first."
         (append (list +ticker+
                       '("name" "string" "Filter by insider name (case-insensitive contains, e.g. 'cook').")
                       '("form_type" "string" "Filter by SEC form: 3 and 5, or their amendments."
                         :enum ("3" "3/A" "5" "5/A")))
                 +filing-dates+
                 (list '("filing_date_gt" "string" "Filing date strictly greater than (YYYY-MM-DD).")
                       '("filing_date_lt" "string" "Filing date strictly less than (YYYY-MM-DD).")
                       '("limit" "integer" "Number of rows (default: 10, max: 1000)." :default 10 :min 1)))
         :path "insider-ownership/" :key "insider_ownership" :paginated t)
   (tool "fd_get_insider_trades"
         "Insider trading transactions (Form 4) by a company's officers, directors and major shareholders: purchases, sales and other transactions, newest first. For insider sentiment and ownership changes."
         (append (list +ticker+
                       '("name" "string" "Filter by insider name (e.g. 'Timothy D Cook')."))
                 +filing-dates+
                 (list '("limit" "integer" "Number of rows (default: 100, max: 1000)." :default 100 :min 1)))
         :path "insider-trades/" :key "insider_trades" :paginated t)
   (tool "fd_get_institutional_investors"
         "Institutional investors (SEC 13F filers) with their CIK and most recent reported name, optionally filtered by name prefix. Use it to find the filer_cik for fd_get_institutional_holdings."
         '(("name" "string" "Optional case-insensitive name prefix (e.g. 'Berkshire')."))
         :path "institutional-holdings/investors" :key "investors")
   (tool "fd_get_institutional_holdings"
         "SEC 13F institutional holdings, straight from EDGAR. Ask by filer_cik (what positions a filer holds) OR by ticker (which institutions hold this security): exactly one is required. Without a report_period filter it answers the latest quarter."
         '(("filer_cik" "string" "Zero-padded 10-digit SEC CIK of the institutional filer (e.g. '0001067983').")
           ("ticker" "string" "The held security's ticker (e.g. 'AAPL').")
           ("report_period" "string" "Exact 13F report period, the quarter-end date (YYYY-MM-DD).")
           ("report_period_gte" "string" "Report period greater than or equal to (YYYY-MM-DD).")
           ("report_period_lte" "string" "Report period less than or equal to (YYYY-MM-DD).")
           ("limit" "integer" "Number of holdings (default: 10, max: 200)." :default 10 :min 1))
         :path "institutional-holdings/" :key "institutional_holdings" :paginated t
         :fetch 'fetch-institutional-holdings)
   (tool "fd_get_interest_rates"
         "The latest policy interest rate of every major central bank (FED, ECB, BOE, BOJ and the rest), as a snapshot. fd_get_macro_data with dataset='interest_rates' also serves rate history."
         '()
         :path "macro/interest-rates/snapshot/" :key "interest_rates")
   (tool "fd_get_kpi_guidance"
         "The company's own guidance for the KPIs it reports, by ticker and period, with optional metric filter."
         (append (list +ticker+ +period-kpi+ +kpi-metric+
                       '("report_period_gte" "string" "Report period greater than or equal to (YYYY-MM-DD).")
                       '("report_period_lte" "string" "Report period less than or equal to (YYYY-MM-DD).")
                       +limit-4+))
         :path "kpi/guidance/" :key "kpi_guidance" :paginated t)
   (tool "fd_get_kpi_metrics"
         "The operating KPIs a company reports each period (subscribers, units, ARPU, stores and the like), by ticker and period."
         (append (list +ticker+ +period-kpi+ +kpi-metric+
                       '("report_period_gte" "string" "Report period greater than or equal to (YYYY-MM-DD).")
                       '("report_period_lte" "string" "Report period less than or equal to (YYYY-MM-DD).")
                       +limit-4+))
         :path "kpi/metrics/" :key "kpi_metrics" :paginated t)
   (tool "fd_get_kpi_non_gaap"
         "The non-GAAP measures a company reports (adjusted EBITDA, adjusted EPS, free cash flow and its own definitions), by ticker and period."
         (append (list +ticker+ +period-kpi+ +kpi-metric+
                       '("report_period_gte" "string" "Report period greater than or equal to (YYYY-MM-DD).")
                       '("report_period_lte" "string" "Report period less than or equal to (YYYY-MM-DD).")
                       +limit-4+))
         :path "kpi/non-gaap/" :key "kpi_non_gaap" :paginated t)
   (tool "fd_get_macro_data"
         "Macroeconomic datasets. dataset='yield_curve': the daily US Treasury par yield curve, one object per business day with one key per tenor (1_month to 30_year), a null tenor meaning unpublished, not zero. dataset='interest_rates': central bank policy rates. dataset='inflation': monthly US CPI, twelve series, with change_1m_pct and change_12m_pct. Omit both dates for the latest values; give a date for history."
         '(("dataset" "string" "Which dataset: yield_curve, interest_rates or inflation." :required t
            :enum ("yield_curve" "interest_rates" "inflation"))
           ("start_date" "string" "First date (YYYY-MM-DD, inclusive). Omit both dates for the latest values.")
           ("end_date" "string" "Last date (YYYY-MM-DD, inclusive; default today).")
           ("bank" "string" "Central bank code for interest_rates history, e.g. 'FED', 'ECB', 'BOE', 'BOJ'.")
           ("series" "string" "Inflation series id, e.g. cpi_all_sa, cpi_core_sa, cpi_shelter_sa, each also as _nsa."))
         :fetch 'fetch-macro)
   (tool "fd_get_news"
         "Recent news for a company (give a ticker) or for the broad market (omit it, for the latest market-moving news: AI, macro, rates, earnings, geopolitics, energy, crypto, IPOs, housing). Also useful for explaining a broad price move."
         '(("ticker" "string" "The stock ticker symbol (e.g. 'AAPL'); omit for market-wide news.")
           ("limit" "integer" "Number of articles (default: 5, max: 10)." :default 5 :min 1))
         :path "news/" :key "news" :paginated t)
   (tool "fd_get_segmented_financials"
         "Revenue and profit by segment and by geography, one row per report period, by ticker."
         (append (list +ticker+
                       '("period" "string" "annual or quarterly (default: annual)."
                         :enum ("annual" "quarterly") :default "annual")
                       +limit-4+)
                 +report-periods+)
         :path "financials/segments/" :key "segmented_financials" :paginated t)
   (tool "fd_get_stock_price"
         "The most recent price snapshot for one stock: latest price, volume, and the open, high, low and close of the session."
         (list +ticker+)
         :path "prices/snapshot/" :key "snapshot")
   (tool "fd_get_stock_prices"
         "Historical prices for a stock over a date range: open, high, low, close and volume, at a chosen interval. The window ends today in Eastern time and starts a year before it ends unless the call says otherwise."
         '(("ticker" "string" "The stock ticker symbol (e.g. 'AAPL')." :required t)
           ("interval" "string" "The interval between rows (default: day)."
            :enum ("second" "minute" "day" "week" "month" "year") :default "day")
           ("interval_multiplier" "integer" "Multiplier for the interval (default: 1)." :default 1 :min 1)
           ("start_date" "string" "Start date (YYYY-MM-DD; default: one year before end_date).")
           ("end_date" "string" "End date (YYYY-MM-DD; default: today in Eastern time)."))
         :path "prices/" :key "prices" :paginated t :fetch 'fetch-stock-prices)
   (tool "fd_screen_stocks"
         "Screens and filters stocks by financial metrics, valuation ratios and company attributes: combine conditions to find what matches a criterion (e.g. revenue above $1B and a P/E under 20). Call fd_list_stock_screener_filters first for the fields and operators."
         '(("filters" "array" "The filter conditions, at least one: each an object with field, operator (gt, lt, gte, lte, eq, in) and value." :required t)
           ("currency" "string" "The currency financial data is read in (default: USD)." :default "USD")
           ("limit" "integer" "Maximum number of results (default: 10, max: 100)." :default 10 :min 1))
         :fetch 'fetch-screen-stocks)
   (tool "fd_list_stock_screener_filters"
         "Every field and operator the stock screener takes, grouped by category (income statement, balance sheet, cash flow, metrics, company attributes). Call it before fd_screen_stocks."
         '()
         :path "financials/search/screener/filters/" :fetch 'fetch-route))
  "The 28 tools, each with the JSON arguments the provider is shown (see
PARAMETER-SCHEMA) and the fetch function behind it.")

(defun tool-names ()
  "The 28 tool names, in the order the original listed them."
  (mapcar (lambda (tool) (getf tool :name)) +tools+))

(defun find-entry (name)
  (or (find name +tools+ :key (lambda (tool) (getf tool :name)) :test #'string=)
      (fail "no tool named ~a" name)))

;;; --- what the provider is shown ------------------------------------------------------

(defun parameter-schema (spec)
  "One member's JSON-Schema object from its SPEC."
  (destructuring-bind (name type doc &key required default enum min max items) spec
    (declare (ignore required))
    (let ((object (nlk:json-object "type" type "description" doc)))
      (when (member :default spec) (setf (gethash "default" object) default))
      (when enum (setf (gethash "enum" object) (coerce enum 'vector)))
      (when (member :min spec) (setf (gethash "minimum" object) min))
      (when (member :max spec) (setf (gethash "maximum" object) max))
      (when items (setf (gethash "items" object) (nlk:decode-json items)))
      (values name object))))

(defun tool-schema (tool)
  "The plist NLE:REGISTER-TOOL is given: the description, and the JSON
object naming every member the call may carry."
  (let ((properties (nlk:json-object))
        (required '()))
    (dolist (spec (getf tool :params))
      (multiple-value-bind (name object) (parameter-schema spec)
        (setf (gethash name properties) object)
        (when (getf (cdddr spec) :required) (push name required))))
    (list :description (getf tool :description)
          :parameters (nlk:json-object "type" "object"
                                        "properties" properties
                                        "required" (coerce (nreverse required) 'vector)))))

;;; --- one call -----------------------------------------------------------------------

(defun run-tool (name arguments)
  "NAME called with ARGUMENTS, the decoded hash table the engine hands a
handler => the tool's text."
  (let* ((tool (find-entry name))
         (arguments (or arguments (nlk:json-object))))
    (nlk:pretty-json (funcall (getf tool :fetch) arguments tool))))

(defun handler (name)
  "The function NLE:REGISTER-TOOL takes for NAME: the text, never a signal."
  (lambda (arguments)
    (handler-case (run-tool name arguments)
      (financial-datasets-error (condition)
        (format nil "ERROR: ~a" (financial-datasets-error-detail condition)))
      (error (condition)
        (format nil "ERROR: ~a failed: ~a" name condition)))))

(defun fetch (name &rest arguments)
  "NAME called from eval with ARGUMENTS as a plist: the same text the model
reads. (financial-datasets:fetch \"fd_get_news\" :ticker \"AAPL\" :limit 3)"
  (let ((object (nlk:json-object)))
    (loop for (key value) on arguments by #'cddr
          do (setf (gethash (string-downcase (substitute #\_ #\- (string key))) object)
                   value))
    (run-tool name object)))
