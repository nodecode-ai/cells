;;;; cell-test.lisp --- the lifecycle, the five tools, /money, and the ledger file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops, each tool answers through the
;;;; seam the model calls it by, rows add up and filter, the budget counts the
;;;; UTC month, the portfolio prices holdings against a stubbed CoinGecko, the
;;;; spread scan keeps only the differences over its threshold, and the ledger
;;;; outlives a restart because the file in the folder holds it. Both market
;;;; tools run against a stubbed NLK:HTTP, so the slice needs no network, and
;;;; every call reads a ledger in the test's own home, so nothing here reads
;;;; the operator's money.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defun money-magnet-folder ()
  "The folder one test's ledger is kept in, inside the test's own home."
  (namestring (nlk:home "money-magnet-test/")))

(defun money-magnet-ledger-file ()
  (merge-pathnames "ledger.json" (nlk:home "money-magnet-test/")))

(defun money-magnet-empty ()
  "One test's ledger: the file removed, so the test starts from none."
  (when (probe-file (money-magnet-ledger-file)) (delete-file (money-magnet-ledger-file))))

(defun money-magnet-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun money-magnet-json (text)
  "TEXT -- a tool's answer -- decoded, as the model reads it."
  (nlk:decode-json text))

(defun money-magnet-number (answer key)
  (nlk:json-value answer :number key))

(defun money-magnet-call (name &rest arguments)
  "NAME answered through the tool seam with one JSON object of ARGUMENTS."
  (money-magnet-json (money-magnet-tool name (apply #'nlk:make-json-object arguments))))

(defun money-magnet-command (args)
  "The text /money answers ARGS with, the slash command's first value."
  (nth-value 0 (cell-entry "nodecode-money-magnet" "money" args)))

(defun money-magnet-holding (symbol amount buy-price)
  (nlk:json-object "symbol" symbol "amount" amount "buy_price" buy-price))

;;; --- the stubbed wire ------------------------------------------------------------

(defparameter *money-magnet-urls* '()
  "Every URL the stubbed NLK:HTTP was asked for, newest first.")

(defmacro with-money-magnet-wire ((&rest answers) &body body)
  "Run BODY with NLK:HTTP answering the JSON text in ANSWERS -- one (MARKER
TEXT STATUS) triple per answer, the first whose MARKER the URL carries -- and
recording every URL. No answer matches: a 500 with an empty body."
  `(with-saved-globals (*money-magnet-urls*)
     (setf *money-magnet-urls* '())
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                 (push url *money-magnet-urls*)
                                 (let ((scripted (find-if (lambda (entry)
                                                            (search (first entry) url))
                                                          (list ,@(loop for answer in answers
                                                                        collect `(list ,@answer))))))
                                   (if scripted
                                       (values (second scripted) (third scripted))
                                       (values "" 500))))
       ,@body)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "money-magnet" "MONEY-MAGNET-CELL-" :start nodecode-money-magnet:start-cell)

(define-cell-lifecycle-tests "money-magnet"
  (:config "data_dir" (money-magnet-folder))
  (:help :money)
  (:command "money")
  (:running (is (nle::find-tool "finance_add") "finance_add is registered")
            (is (nle::find-tool "finance_list"))
            (is (nle::find-tool "finance_budget"))
            (is (nle::find-tool "finance_portfolio"))
            (is (nle::find-tool "finance_arbitrage"))
            (is (pathnamep (nodecode-money-magnet::ledger-file)) "the ledger file is resolved")
            (is (equal "USD" (nodecode-money-magnet::currency)) "the currency is a label with a default"))
  (:stopped (is (null (nle::find-tool "finance_add")) "stop unregisters the tools"))
  (:refused ("currency" "EUR"))
  (:idle nodecode-money-magnet:money-magnet-error
         (nodecode-money-magnet::run-command "status" nil)))

(deftest money-magnet-cell-a-missing-data-dir-refuses-by-name ()
  (is-carrying (line (refusal-text nlk:config-refusal (money-magnet-start "currency" "EUR")))
               "data_dir"
               "the folder the ledger is kept in")
  (is (null (nle::find-tool "finance_add")) "a refused start installed nothing"))

;;; --- the ledger ------------------------------------------------------------------

(deftest money-magnet-cell-rows-add-up-and-filter ()
  (money-magnet-empty)
  (with-cell-stop ((money-magnet-start "data_dir" (money-magnet-folder)))
    (let ((added (money-magnet-call "finance_add" "tx_type" "expense" "amount" 12.5
                                     "category" "food" "note" "lunch")))
      (is (= 1 (nlk:json-value added :integer "total_tx")) "the ledger counts the row")
      (is (equal "expense" (nlk:json-value (nlk:json-value added :object "entry") :string "type")))
      (is (stringp (nlk:json-value (nlk:json-value added :object "entry")
                                           :string "ts"))))  ; the moment it was recorded
    (money-magnet-call "finance_add" "tx_type" "income" "amount" 1000 "category" "salary")
    (money-magnet-call "finance_add" "tx_type" "expense" "amount" 7.5 "category" "transport")
    (let ((listed (money-magnet-call "finance_list")))
      (is (= 3 (nlk:json-value listed :integer "matched")) "the ledger holds three rows")
      (is (= 1000 (money-magnet-number listed "income")) "the income is summed")
      (is (= 20 (money-magnet-number listed "expense")) "and so are the expenses")
      (is (= 980 (money-magnet-number listed "net")) "the net is the difference")
      (is (= 3 (length (nlk:json-value listed :array "transactions"))) "every row is answered"))
    (let ((expenses (money-magnet-call "finance_list" "tx_type" "expense")))
      (is (= 2 (nlk:json-value expenses :integer "matched")) "the filter counts the expenses")
      (is (= 20 (money-magnet-number expenses "expense")))
      (is (= -20 (money-magnet-number expenses "net")) "two expenses net to their negative sum"))
    (let ((food (money-magnet-call "finance_list" "category" "food")))
      (is (= 1 (nlk:json-value food :integer "matched")) "the category filter matches without case")
      (is (equal "food" (nlk:json-value (aref (nlk:json-value food :array "transactions") 0)
                                         :string "category"))))
    (let ((page (money-magnet-call "finance_list" "limit" 1)))
      (is (= 1 (nlk:json-value page :integer "count")) "a limit pages the answer")
      (is (= 3 (nlk:json-value page :integer "matched")) "while the totals still count every match"))
    (is-carrying (line (money-magnet-tool "finance_add"
                                           (nlk:json-object "tx_type" "expense" "amount" -3
                                                             "category" "food")))
                 "ERROR: money-magnet:"
                 ("amount must be a number greater than zero" "a negative amount writes nothing"))
    (is (= 3 (nlk:json-value (money-magnet-call "finance_list")
                              :integer "matched")))))

(deftest money-magnet-cell-the-budget-counts-this-utc-month ()
  (money-magnet-empty)
  (with-cell-stop ((money-magnet-start "data_dir" (money-magnet-folder)))
    (money-magnet-call "finance_add" "tx_type" "expense" "amount" 120 "category" "rent")
    (money-magnet-call "finance_add" "tx_type" "expense" "amount" 30 "category" "food")
    (money-magnet-call "finance_add" "tx_type" "income" "amount" 500 "category" "salary")
    (let ((budget (money-magnet-call "finance_budget" "monthly_limit" 400)))
      (is (= 400 (money-magnet-number budget "monthly_limit")) "the limit is set")
      (is (= 150 (money-magnet-number budget "spent")) "only this month's expenses count")
      (is (= 250 (money-magnet-number budget "remaining")) "what is left of the limit")
      (is (= 37.5 (money-magnet-number budget "percent_used")) "the percentage used")
      (is (= 2 (nlk:json-value budget :integer "transactions_this_month")))
      (is (equal (nodecode-money-magnet::month-key)
                     (nlk:json-value budget :string "month"))))

    (let ((read-only (money-magnet-call "finance_budget")))
      (is (= 400 (money-magnet-number read-only "monthly_limit")) "a call without a limit changes nothing"))
    (let ((ledger (money-magnet-json (uiop:read-file-string (money-magnet-ledger-file)))))
      (is (= 3 (length (nlk:json-array ledger "transactions"))) "the ledger file holds every row"))))

(deftest money-magnet-cell-the-ledger-outlives-a-restart ()
  (money-magnet-empty)
  (with-cell-stop ((money-magnet-start "data_dir" (money-magnet-folder)))
    (money-magnet-call "finance_add" "tx_type" "expense" "amount" 9 "category" "coffee"))
  (with-cell-stop ((money-magnet-start "data_dir" (money-magnet-folder)))
    (let ((listed (money-magnet-call "finance_list")))
      (is (= 1 (nlk:json-value listed :integer "matched")) "the row is still there")
      (is (= 9 (money-magnet-number listed "expense")) "and still adds up"))))

;;; --- the market tools ------------------------------------------------------------

(deftest money-magnet-cell-the-portfolio-prices-holdings ()
  (money-magnet-empty)
  (with-cell-stop ((money-magnet-start "data_dir" (money-magnet-folder)))
    (with-money-magnet-wire (("api.coingecko.com" "{\"bitcoin\":{\"usd\":50000},\"ethereum\":{\"usd\":2000}}" 200))
      (let ((answer (money-magnet-call "finance_portfolio"
                                       "holdings" (vector (money-magnet-holding "btc" 0.5 30000)
                                                          (money-magnet-holding "ETH" 2 1000)))))
        (is (search "ids=bitcoin,ethereum" (first *money-magnet-urls*)) "the coin ids, and nothing the user wrote, are what goes out")

        (is (= 29000 (money-magnet-number answer "total_value")) "value at the quoted prices")
        (is (= 17000 (money-magnet-number answer "total_cost")) "against what they cost")
        (is (= 12000 (money-magnet-number answer "total_pnl")) "the profit is the difference")
        (let ((rows (nlk:json-value answer :array "holdings")))
          (is (= 2 (length rows)))
          (is (equal "BTC" (nlk:json-value (aref rows 0) :string "symbol")) "a lowercase symbol reads back upper")

          (is (= 25000 (nlk:json-value (aref rows 0) :number "value")))
          (is (= 100 (nlk:json-value (aref rows 1) :number "pnl_pct")) "and its percentage: twice the cost is 100% up"))))

    (with-money-magnet-wire (("api.coingecko.com" "" 429))
      (is-carrying (line (money-magnet-tool "finance_portfolio"
                                             (nlk:json-object "holdings"
                                                               (vector (money-magnet-holding "BTC" 1 1)))))
                   "ERROR: money-magnet:"
                   ("CoinGecko answered HTTP 429" "a refused status is a repair, not a wrong number")))
    (is-carrying (line (money-magnet-tool "finance_portfolio"
                                           (nlk:json-object "holdings"
                                                             (vector (nlk:json-object "amount" 1
                                                                                       "buy_price" 1)))))
                 ("every holding needs a symbol" "a holding without a symbol is refused"))))

(deftest money-magnet-cell-the-spread-scan-keeps-the-gaps-over-its-threshold ()
  (money-magnet-empty)
  (with-cell-stop ((money-magnet-start "data_dir" (money-magnet-folder)))
    (with-money-magnet-wire
        (("api.binance.com" "[{\"symbol\":\"BTCUSDT\",\"price\":\"51000.00\"},{\"symbol\":\"ETHUSDT\",\"price\":\"2005.00\"},{\"symbol\":\"SOLUSDT\",\"price\":\"900.00\"}]" 200)
         ("api.coingecko.com" "[{\"symbol\":\"btc\",\"current_price\":50000},{\"symbol\":\"eth\",\"current_price\":2000},{\"symbol\":\"sol\",\"current_price\":100}]" 200))
      (let ((answer (money-magnet-call "finance_arbitrage")))
        (is (= 2 (length *money-magnet-urls*)) "one ticker and one market list are read")
        (let ((rows (nlk:json-value answer :array "opportunities")))
          (is (= 2 (length rows)) "the pair under the spread is left out")
          (is (equal "SOL" (nlk:json-value (aref rows 0) :string "symbol")) "the widest gap comes first")

          (is (= 800 (nlk:json-value (aref rows 0) :number "diff_pct")))
          (is (equal "BTC" (nlk:json-value (aref rows 1) :string "symbol"))))
        (is (= 0.5 (money-magnet-number answer "spread_percent")) "the threshold is answered with the scan")))
    (is-carrying (line (money-magnet-command "arbitrage"))
                 ("the spread scan failed" "the slash command reports a dead wire instead of raising"))))

;;; --- /money ----------------------------------------------------------------------

(deftest money-magnet-cell-the-command-shows-the-month-and-adds-a-row ()
  (money-magnet-empty)
  (with-cell-stop ((money-magnet-start "data_dir" (money-magnet-folder)))
    (is-carrying (line (money-magnet-command ""))
                 ("Money Magnet (USD)" "the ledger and the month")
                 "0 transactions")
    (is-carrying (line (money-magnet-command "add expense 4.25 coffee"))
                 ("Added #1: expense 4.25 coffee" "a typed amount becomes a row"))
    (is-carrying (line (money-magnet-command "budget 100"))
                 "limit 100.00 USD"
                 "spent 4.25"
                 ("4.2% used" "the month against a limit"))
    (is-carrying (line (money-magnet-command "budget 3"))
                 ("over by 1.25" "a spent month over its limit says so"))
    (is-carrying (line (money-magnet-command "status"))
                 "1 transaction"
                 "Expense: 4.25")
    (is-carrying (line (money-magnet-command "add expense lots food"))
                 ("lots is not an amount" "a word where an amount goes is refused, never read as code"))
    (is-carrying (line (money-magnet-command "nonsense"))
                 "Unknown command: nonsense")))
