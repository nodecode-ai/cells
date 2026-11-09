;;;; cell.lisp --- the section, the 28 tools, /financial, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes plugin: a manifest naming 28 tools, a handler
;;;; each, and the key asked for at install time. Here the same 28 tools are
;;;; one NLE:DEFINE-CELL declaration -- the `financial-datasets' section they
;;;; run on, a command the operator types, a topic on the manual, and a stop
;;;; derived from them all.
;;;;
;;;; An unanswered section still starts: the tools are there, and the first
;;;; call refuses naming the member to write. That is the way in, rather than a
;;;; cell that silently disappears when the key is missing.

(in-package #:nodecode-financial-datasets)

;;; --- the words ----------------------------------------------------------------

(defparameter +primer+
  "financial-datasets: Financial Datasets' stock-market API as 28 read-only tools, every
name starting fd_. Each answers JSON text, and one call reaches one dataset:

  company      fd_get_company_facts
  statements   fd_get_income_statement, fd_get_balance_sheet, fd_get_cash_flow_statement,
               fd_get_segmented_financials -- ticker plus period (annual, quarterly, ttm) and
               a limit (default 4); as_reported gives the filing's own numbers
  metrics      fd_get_financial_metrics, fd_get_financial_metrics_snapshot
  earnings     fd_get_earnings                 (a ticker, or none for the whole feed)
  KPIs         fd_get_kpi_metrics, fd_get_kpi_guidance, fd_get_kpi_non_gaap
  prices       fd_get_stock_price (latest), fd_get_stock_prices (a window, default the
               year to today)
  SEC filings  fd_get_filings, fd_get_filing_items, fd_list_filing_item_types
  insiders     fd_get_insider_trades, fd_get_insider_ownership
  institutions fd_get_institutional_investors, fd_get_institutional_holdings (13F)
  5% owners    fd_get_beneficial_owners, fd_get_beneficial_ownership (13D/13G)
  funds        fd_get_index_fund
  macro        fd_get_macro_data, fd_get_interest_rates
  news         fd_get_news
  screener     fd_screen_stocks, fd_list_stock_screener_filters

How to work: a ticker is enough for most questions; give a period and a small limit rather
than pulling every row. Amounts are in the currency each row names, and a null figure means
the API has no value for that period -- say so, never read it as zero. For a valuation or a
screen, call fd_list_stock_screener_filters first, then fd_screen_stocks with the fields it
names. For ownership, resolve a name to a filer_cik with fd_get_beneficial_owners or
fd_get_institutional_investors before asking which stakes a filer holds.
Setup: a key from financialdatasets.ai in the financial-datasets section's api_key, or the
environment variable api_key_env names (FINANCIAL_DATASETS_API_KEY); usage is billed to that
key. From eval the same calls are (financial-datasets:fetch \"fd_get_news\" :ticker \"AAPL\").
(help :financial-datasets) has this while the cell runs."
  "What (help :financial-datasets) answers while the cell runs.")

;;; --- the tools ---------------------------------------------------------------

(defun install-tools ()
  "Register the 28 tools, each with its own inverse in this start's scope."
  (dolist (tool +tools+)
    (let ((name (getf tool :name)))
      (nle:register-tool name (tool-schema tool) (handler name))
      (nle:on-stop (lambda () (nle:unregister-tool name))))))

(defun registered-tools ()
  "The tool names this start registered."
  (tool-names))

;;; --- /financial ----------------------------------------------------------------

(defun ticker-argument-p (text)
  "Whether TEXT reads as one ticker: letters, digits, dots and dashes."
  (and (plusp (length text))
       (every (lambda (char) (or (alphanumericp char) (member char '(#\. #\-))))
              text)))

(defun key-state ()
  "One line saying whether a key answers, and where from."
  (cond ((setting :api-key) "set in the section")
        ((api-key) (format nil "set: the environment variable ~a" (setting :api-key-env)))
        (t (format nil "missing: write api_key in the financial-datasets section, or export ~a"
                   (or (setting :api-key-env) +default-key-env+)))))

(defun run-command (args session-id)
  "(/financial) says which service this cell talks to, whether a key answers and what
it registered; (/financial AAPL) answers that stock's price snapshot."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (cond
      ((zerop (length text))
       (format nil "financial-datasets: ~a~%key: ~a~%28 tools: ~{~a~^, ~}~%(/financial TICKER answers one stock's price snapshot; (help :financial-datasets) has the rest)"
               (base-url) (key-state) (registered-tools)))
      ((ticker-argument-p text)
       (handler-case
           (run-tool "fd_get_stock_price"
                     (nlk:json-object "ticker" (string-upcase text)))
         (financial-datasets-error (condition)
           (format nil "~a" (financial-datasets-error-detail condition)))))
      (t (format nil "~s is not a ticker: /financial takes one, or nothing for the status" text)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell financial-datasets
  ;; The key is the whole setup: the original's manifest named the same one
  ;; variable, and its README pointed at the same page for it.
  (:section ("financial-datasets")
    (:guide "api_key is the one step left: create a key at financialdatasets.ai and paste it, or export the variable api_key_env names; every call is billed to that key")
    ("api_key" :secret
               :doc "your Financial Datasets key, sent in an X-API-KEY header and never in the URL")
    ("api_key_env" :string :default "FINANCIAL_DATASETS_API_KEY"
                   :doc "the environment variable read when api_key is unset")
    ("base_url" :string :default "https://api.financialdatasets.ai/"
                :doc "where the API answers; the one host this cell dials")
    ("timeout" :integer :default 30 :min 1
               :doc "seconds one request may take"))
  (:command "financial" 'run-command
            :description "Financial Datasets: the key, the tools, or one stock's snapshot"
            :argument-hint "[TICKER]")
  (:help :financial-datasets "Financial Datasets: statements, prices, filings, ownership, KPIs, news and macro data as 28 tools"
         +primer+)
  (:start (lambda () (install-tools))))
