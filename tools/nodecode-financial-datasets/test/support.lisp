;;;; support.lisp --- the Financial Datasets test slice: a stubbed API, no network.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the same nodecode.test registry every cell's do
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; FINANCIAL-DATASETS-CELL- name prefix; RUN-FINANCIAL-DATASETS-TESTS runs
;;;; exactly that slice.
;;;;
;;;; The one seam that reaches outside is NLK:HTTP, the only way this cell
;;;; touches the network. WITH-API replaces it with a small stand-in for the
;;;; REST API -- the routes these tools call, and a record of every exchange --
;;;; so a test asserts on the request and nothing dials.

(in-package #:nodecode.test)

(define-test-slice "financial-datasets" "FINANCIAL-DATASETS-CELL-"
    :start nodecode-financial-datasets:start-cell)

;;; --- the canned API ---------------------------------------------------------------

(defparameter +endpoint+ "https://api.test/fd/"
  "The base URL a test's section names, so a paging link is one this cell may follow.")

(defparameter +key-env+ "NODECODE_FINANCIAL_DATASETS_TEST_KEY"
  "A variable nothing sets: the key in these tests is the one the section carries.")

(defparameter +no-key-env+ "NODECODE_FINANCIAL_DATASETS_TEST_ABSENT"
  "The variable a test names when the key must be missing.")

(defvar *calls* '()
  "Every (METHOD URL ARGUMENTS) the stubbed API took, newest first.")

(defvar *answers* '()
  "((MARKER . FUNCTION) ...) a test sets to script the answer whose URL holds
MARKER; the newest entry wins, and FUNCTION answers (values TEXT STATUS).")

(defun script (marker function)
  "Answer every request whose URL holds MARKER with FUNCTION for this test."
  (push (cons marker function) *answers*))

(defun path-of (url)
  "The part of URL after the endpoint, without the query."
  (let* ((start (length +endpoint+))
         (path (if (and (>= (length url) start) (string= +endpoint+ url :end2 start))
                   (subseq url start)
                   url))
         (query (position #\? path)))
    (if query (subseq path 0 query) path)))

(defun query-of (url)
  "URL's query string as an alist of (NAME . VALUE), as the API reads it."
  (let ((query (position #\? url)))
    (when query
      (loop for pair in (uiop:split-string (subseq url (1+ query)) :separator "&")
            for equals = (position #\= pair)
            collect (cons (subseq pair 0 equals) (subseq pair (1+ equals)))))))

(defun json-body (object)
  (nlk:encode-json-object object))

(defun answer-for (method path-query)
  "What the stand-in API says to one exchange."
  (cond
    ((and (equal method :post) (search "financials/search/screener/" path-query))
     (json-body (nlk:json-object
                 "results" (vector (nlk:json-object "ticker" "MSFT" "pe_ratio" 18.2)))))
    ((search "financials/search/screener/filters/" path-query)
     (json-body (nlk:json-object
                 "filters" (nlk:json-object
                            "income_statement" (vector "revenue" "net_income")
                            "operators" (vector "gt" "lt" "eq")))))
    ((search "cursor=two" path-query)
     (json-body (nlk:json-object
                 "income_statements" (vector (nlk:json-object "ticker" "AAPL" "revenue" 2)))))
    ((search "financials/income-statements/" path-query)
     (let ((page (json-body (nlk:json-object
                             "income_statements"
                             (vector (nlk:json-object "ticker" "AAPL"
                                                       "report_period" "2025-09-30"
                                                       "revenue" 391035000000
                                                       "net_income" 93736000000))))))
       (if (search "limit=2" path-query)
           (json-body (nlk:json-object
                       "income_statements" (vector (nlk:json-object "ticker" "AAPL" "revenue" 1))
                       "next_page_url" (format nil "~afinancials/income-statements/?cursor=two"
                                                +endpoint+)))
           page)))
    ((search "cursor=two" path-query)
     (json-body (nlk:json-object
                 "income_statements" (vector (nlk:json-object "ticker" "AAPL" "revenue" 2)))))
    ((search "prices/snapshot/" path-query)
     (json-body (nlk:json-object "snapshot" (nlk:json-object "ticker" "AAPL" "price" 190))))
    ((search "prices/" path-query)
     (json-body (nlk:json-object "prices" (vector (nlk:json-object "close" 191 "volume" 42)))))
    ((search "news/" path-query)
     (json-body (nlk:json-object "news" (vector (nlk:json-object "title" "Market rallies")))))
    ((search "company/facts/" path-query)
     (json-body (nlk:json-object "company_facts"
                                  (nlk:json-object "ticker" "AAPL" "sector" "Technology"))))
    ((search "macro/interest-rates/snapshot/" path-query)
     (json-body (nlk:json-object
                 "interest_rates" (vector (nlk:json-object "bank" "FED" "rate" 4.5)))))
    ((search "macro/yield-curve" path-query)
     (json-body (nlk:json-object
                 "yield_curve" (vector (nlk:json-object "date" "2026-01-02" "10_year" 4.2)))))
    ((search "beneficial-ownership/filers/" path-query)
     (json-body (nlk:json-object "total" 1
                                  "filers" (vector (nlk:json-object "filer_cik" "0001510281"
                                                                     "name" "Saba Capital")))))
    ((search "institutional-holdings/investors" path-query)
     (json-body (nlk:json-object "investors" (vector (nlk:json-object "filer_cik" "0001067983")))))
    ((search "filings/items/" path-query)
     (json-body (nlk:json-object
                 "filing_items" (vector (nlk:json-object "item" "Item-1A"
                                                          "text" "Risk factors.")))))
    ((search "financial-metrics/snapshot/" path-query)
     (json-body (nlk:json-object "snapshot" (nlk:json-object "ticker" "AAPL" "pe_ratio" 31.5))))
    (t (json-body (nlk:json-object "ok" t)))))

(defun stub-http (method url &rest arguments)
  "The stubbed NLK:HTTP: the stand-in API, and a record of every exchange."
  (push (list method url arguments) *calls*)
  (let ((scripted (find-if (lambda (entry) (search (car entry) url)) *answers*)))
    (if scripted
        (multiple-value-bind (text status) (funcall (cdr scripted) method url)
          (values text status (make-hash-table :test #'equal)))
        (values (answer-for method (subseq url (length +endpoint+)))
                200 (make-hash-table :test #'equal)))))

(defmacro with-api (&body body)
  "Run BODY with NLK:HTTP stubbed and the stub's own state cleared."
  `(with-saved-globals (*calls* *answers*)
     (setf *calls* '() *answers* '())
     (with-stubbed-fdefinition (nlk:http (method url &rest arguments)
                                 (apply #'stub-http method url arguments))
       ,@body)))

(defmacro with-fd ((&rest pairs) &body body)
  "Run BODY with the cell started under PAIRS as the `financial-datasets'
section, its wire stubbed, and stopped on unwind."
  `(with-api
     (with-cell-stop ((financial-datasets-start ,@pairs))
       ,@body)))

;;; --- asking the cell what a model would ---------------------------------------------

(defun call (name &optional (arguments "{}"))
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun newest-call ()
  (first *calls*))

(defun newest-url ()
  (second (newest-call)))

(defun newest-headers ()
  (getf (third (newest-call)) :headers))

(defun header (name)
  (cdr (assoc name (newest-headers) :test #'string-equal)))
