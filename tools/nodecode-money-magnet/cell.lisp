;;;; cell.lisp --- Money Magnet: a ledger, a monthly budget, a portfolio, a
;;;; spread scan, and /money.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Whydiriki700/money-magnet (see NOTICE). The library, the five
;;;; tool names with their arguments and JSON answers, the monthly budget
;;;; arithmetic over UTC months, the coin-id map and the CoinGecko-versus-Binance
;;;; spread scan are the original's. The harness seams are Nodecode's: the
;;;; section is the config, a tool is one (:tool ...) clause of NLE:DEFINE-CELL,
;;;; and the original's /money command is a slash command.
;;;;
;;;; A transaction is one row of the ledger file, written whole under the
;;;; cell's lock: where the original kept two files under the harness home
;;;; (transactions.json and budget.json), this keeps one ledger.json in the
;;;; folder the section names, so a read-modify-write can never split the
;;;; budget from the rows it is measured against. Only the two market tools
;;;; dial: the price APIs are public, need no key, and carry coin symbols,
;;;; never anything the user wrote.

(defpackage #:nodecode-money-magnet
  (:use #:cl)
  (:export #:money-magnet-error))

(in-package #:nodecode-money-magnet)

(nlk:define-peripheral money-magnet :not-running t :lock "money-magnet")

(defconstant +format-version+ 1 "The ledger file's own format, so a later one can tell.")
(defparameter *spread-percent* 0.5
  "How large a price difference the arbitrage scan reports, in percent.
A defparameter, so a layer can rebind it.")
(defparameter *default-limit* 20 "How many rows finance_list answers when the call names none.")
(defparameter *max-limit* 500 "The most rows one finance_list call may ask for.")
(defparameter *top-spreads* 10 "The most opportunities one finance_arbitrage call answers.")
(defparameter *http-timeout* 15 "One price lookup's ceiling, in seconds.")
(defparameter +coingecko-simple-price+
  "https://api.coingecko.com/api/v3/simple/price?vs_currencies=usd&ids="
  "CoinGecko's current price of the coins named after it, comma-joined.")
(defparameter +coingecko-markets+
  "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&order=market_cap_desc&per_page=20&page=1"
  "CoinGecko's twenty largest coins, which the spread scan compares.")
(defparameter +binance-ticker+ "https://api.binance.com/api/v3/ticker/price"
  "Binance's spot price of every pair, which the spread scan compares.")

;;; --- where the ledger is ---------------------------------------------------------

(defun data-folder ()
  "The folder the ledger file lives in, as the section names it."
  (uiop:ensure-directory-pathname (or (setting :data-dir) "")))

(defun ledger-file ()
  (merge-pathnames "ledger.json" (data-folder)))

(defun currency ()
  "The label the amounts are kept in."
  (let ((named (nlk:trimmed (or (setting :currency) ""))))
    (if (plusp (length named)) named "USD")))

(defun now-text ()
  "Now, as UTC ISO-8601 to the second: what a row's timestamp carries."
  (nlk:iso-time (nlk:unix-now) :millis nil))

(defun month-key (&optional (stamp (now-text)))
  "The UTC month STAMP falls in, as YYYY-MM: how a budget month is counted."
  (subseq stamp 0 (min 7 (length stamp))))

;;; --- the ledger file -------------------------------------------------------------

(defun transaction-of (object)
  "One row read back from the ledger file."
  (list :id (or (nlk:json-value object :integer "id") 0)
        :ts (or (nlk:json-value object :string "ts") "")
        :type (or (nlk:json-value object :string "type") "expense")
        :amount (or (nlk:json-value object :number "amount") 0)
        :category (or (nlk:json-value object :string "category") "")
        :note (or (nlk:json-value object :string "note") "")))

(defun read-ledger ()
  "The ledger as a plist (:limit NUMBER :transactions (ROW ...)), or an empty
one the first time. A file that does not decode is an error the model reads
and a person fixes, never silently an empty ledger."
  (let ((file (ledger-file)))
    (if (probe-file file)
        (let ((object (nlk:decode-json (uiop:read-file-string file))))
          (list :limit (or (nlk:json-value object :number "monthly_limit") 0)
                :transactions (loop for row across (nlk:json-array object "transactions")
                                    collect (transaction-of row))))
        (list :limit 0 :transactions '()))))

(defun transaction-json (row)
  (nlk:json-object
   "id" (getf row :id)
   "ts" (getf row :ts)
   "type" (getf row :type)
   "amount" (getf row :amount)
   "category" (getf row :category)
   "note" (getf row :note)))

(defun ledger-json (ledger)
  (nlk:json-object
   "format" +format-version+
   "currency" (currency)
   "monthly_limit" (getf ledger :limit)
   "transactions" (coerce (mapcar #'transaction-json (getf ledger :transactions)) 'vector)))

(defun write-ledger (ledger)
  "LEDGER to its file atomically, under the folder's lock, so two processes
sharing the folder cannot write one ledger from two stale reads."
  (let ((file (ledger-file)))
    (ensure-directories-exist file)
    (nlk:with-folder-lock ((data-folder))
      (nlk:write-file-atomically file (format nil "~a~%" (nlk:encode-json-object (ledger-json ledger)))
                                  :mode :private))
    ledger))

(defmacro with-ledger ((ledger) &body body)
  "BODY with LEDGER read from the file and written back when it answers."
  `(with-money-magnet-lock
     (let ((,ledger (read-ledger)))
       (multiple-value-prog1 (progn ,@body)
         (write-ledger ,ledger)))))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model
repairs from -- the shape every tool of the original answers a refusal in."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: money-magnet: ~a" condition)))))

;;; --- arguments -------------------------------------------------------------------

(defun argument (args name)
  "ARGS' NAME, or NIL; ARGS is NIL when a call carries no arguments at all."
  (and (hash-table-p args) (gethash name args)))

(defun argument-text (args name)
  (let ((value (argument args name)))
    (and (stringp value) value)))

(defun required-text (args name)
  (let ((value (argument-text args name)))
    (if (and value (plusp (length (nlk:trimmed value))))
        (nlk:trimmed value)
        (fail "~a is required" name))))

(defun bounded-integer (args name default minimum &optional (maximum most-positive-fixnum))
  "NAME, an integer between MINIMUM and MAXIMUM, DEFAULT when absent."
  (let ((value (argument args name)))
    (cond ((null value) default)
          ((and (integerp value) (<= minimum value maximum)) value)
          (t (fail "~a must be an integer between ~d and ~d." name minimum maximum)))))

(defun positive-number (args name)
  "NAME, a number greater than zero, as the amount a row or a holding carries."
  (let ((value (argument args name)))
    (cond ((and (numberp value) (plusp value)) (coerce value 'double-float))
          ((null value) (fail "~a is required, a number greater than zero" name))
          (t (fail "~a must be a number greater than zero" name)))))

(defun non-negative-number (args name)
  (let ((value (argument args name)))
    (cond ((and (numberp value) (not (minusp value))) (coerce value 'double-float))
          ((null value) (fail "~a is required, a number zero or more" name))
          (t (fail "~a must be a number zero or more" name)))))

(defun transaction-type (args name)
  "NAME read as \"income\" or \"expense\"."
  (let ((value (string-downcase (or (argument-text args name) ""))))
    (cond ((string= value "income") "income")
          ((string= value "expense") "expense")
          ((zerop (length value)) (fail "~a is required: \"income\" or \"expense\"" name))
          (t (fail "~a must be \"income\" or \"expense\", not ~s" name value)))))

;;; --- numbers ---------------------------------------------------------------------

(defun read-amount (text)
  "TEXT as a positive number, or NIL when it is not one. Only digits and one
decimal point are read, with READ-EVAL off, so an amount typed at /money can
never be a form the image evaluates."
  (let ((typed (nlk:trimmed (or text ""))))
    (when (and (plusp (length typed))
               (digit-char-p (char typed 0))
               (<= (count #\. typed) 1)
               (every (lambda (character)
                        (or (digit-char-p character) (char= character #\.)))
                      typed))
      (let ((*read-eval* nil)
            (*read-default-float-format* 'double-float))
        (multiple-value-bind (value position)
            (read-from-string typed nil nil)
          (when (and (numberp value) (plusp value) (= position (length typed)))
            (coerce value 'double-float)))))))

(defun rounded (value)
  "VALUE to two decimal places, as an amount is quoted."
  (/ (round (* (coerce value 'double-float) 100)) 100.0d0))

(defun one-decimal (value)
  (/ (round (* (coerce value 'double-float) 10)) 10.0d0))

(defun percentage (part whole)
  "PART as a percentage of WHOLE, or 0 when WHOLE is zero."
  (if (plusp whole) (one-decimal (* 100 (/ (coerce part 'double-float) whole))) 0.0d0))

(defun money-line (amount)
  (format nil "~,2f ~a" (coerce amount 'double-float) (currency)))

;;; --- the market reads ------------------------------------------------------------

(defun get-json (url what)
  "URL's decoded answer. A refused status, an empty body and a body that is
not JSON are one condition, FAIL."
  (multiple-value-bind (body status)
      (nlk:http :get url :timeout *http-timeout*)
    (unless (eql status 200)
      (fail "~a answered HTTP ~a" what status))
    (let ((text (and body (nlk:trimmed body))))
      (when (or (null text) (zerop (length text)))
        (fail "~a answered an empty body" what))
      (or (ignore-errors (nlk:decode-json text))
          (fail "~a answered a body that is not JSON" what)))))

(defparameter +coingecko-ids+
  '(("BTC" . "bitcoin") ("ETH" . "ethereum") ("SOL" . "solana") ("BNB" . "binancecoin")
    ("XRP" . "ripple") ("ADA" . "cardano") ("DOGE" . "dogecoin") ("AVAX" . "avalanche-2")
    ("DOT" . "polkadot") ("MATIC" . "matic-network") ("LINK" . "chainlink") ("UNI" . "uniswap")
    ("LTC" . "litecoin") ("ATOM" . "cosmos") ("NEAR" . "near") ("APT" . "aptos"))
  "The coin symbols of the original's map, and the CoinGecko id each prices under.
Any other symbol is priced by its lowercase name, as the original falls back.")

(defun coin-id (symbol)
  (or (cdr (assoc (string-upcase symbol) +coingecko-ids+ :test #'string=))
      (string-downcase symbol)))

(defun coingecko-prices (symbols)
  "SYMBOLS's current price in USD, as a hash of coin id to price."
  (let* ((ids (format nil "~{~a~^,~}" (mapcar #'coin-id symbols)))
         (object (get-json (concatenate 'string +coingecko-simple-price+ ids) "CoinGecko"))
         (prices (make-hash-table :test #'equal)))
    (unless (hash-table-p object)
      (fail "CoinGecko answered ~s, not an object of prices" object))
    (maphash (lambda (id entry)
               (let ((price (nlk:json-value entry :number "usd")))
                 (when price (setf (gethash id prices) price))))
             object)
    prices))

(defun checked-holdings (value)
  "VALUE -- the call's holdings -- as a vector of {symbol, amount, buy_price},
each a hash table, refused when any is not one."
  (unless (and (vectorp value) (not (stringp value)) (plusp (length value)))
    (fail "holdings must be an array of at least one {symbol, amount, buy_price}"))
  (loop for holding across value
        do (unless (hash-table-p holding)
             (fail "every holding must be a {symbol, amount, buy_price} object"))
           (unless (and (stringp (nlk:json-value holding :string "symbol"))
                        (plusp (length (nlk:trimmed (nlk:json-value holding :string "symbol")))))
             (fail "every holding needs a symbol"))
           (unless (and (numberp (nlk:json-value holding :number "amount"))
                        (plusp (nlk:json-value holding :number "amount")))
             (fail "every holding needs an amount greater than zero"))
           (unless (and (numberp (nlk:json-value holding :number "buy_price"))
                        (not (minusp (nlk:json-value holding :number "buy_price"))))
             (fail "every holding needs a buy_price, zero or more"))
        collect (list :symbol (string-upcase (nlk:trimmed (nlk:json-value holding :string "symbol")))
                      :amount (coerce (nlk:json-value holding :number "amount") 'double-float)
                      :buy-price (coerce (nlk:json-value holding :number "buy_price") 'double-float))
        into holdings
        finally (return (coerce holdings 'vector))))

(defun wire-number (object key)
  "OBJECT's KEY as a number, whether the peer spelled it as a JSON number or
as a decimal string, which is how Binance quotes a price."
  (or (nlk:json-value object :number key)
      (read-amount (nlk:json-value object :string key))))

(defun binance-prices ()
  "Every Binance spot pair's price, as a hash of symbol to price."
  (let ((object (get-json +binance-ticker+ "Binance"))
        (prices (make-hash-table :test #'equal)))
    (unless (vectorp object)
      (fail "Binance answered a ticker that is not an array"))
    (loop for row across object
          for symbol = (nlk:json-value row :string "symbol")
          for price = (wire-number row "price")
          when (and symbol price) do (setf (gethash symbol prices) price))
    prices))

;;; --- the tools -------------------------------------------------------------------

(defun next-id (rows)
  (1+ (reduce #'max rows :key (lambda (row) (getf row :id)) :initial-value 0)))

(defun tool-add (args)
  "finance_add: one income or expense row in the ledger."
  (answering
   (with-ledger (ledger)
     (let* ((entry (list :id (next-id (getf ledger :transactions))
                         :ts (now-text)
                         :type (transaction-type args "tx_type")
                         :amount (positive-number args "amount")
                         :category (required-text args "category")
                         :note (or (argument-text args "note") ""))))
       (setf (getf ledger :transactions) (append (getf ledger :transactions) (list entry)))
       (nlk:encode-json-object
        (nlk:json-object
         "success" t
         "entry" (transaction-json entry)
         "total_tx" (length (getf ledger :transactions))))))))

(defun matching-rows (ledger tx-type category)
  "LEDGER's rows, the ones of TX-TYPE (\"all\" for every one) in CATEGORY
(every category when empty)."
  (remove-if-not (lambda (row)
                   (and (or (string= tx-type "all") (string= (getf row :type) tx-type))
                        (or (zerop (length category))
                            (string-equal (getf row :category) category))))
                 (getf ledger :transactions)))

(defun totals (rows)
  "ROWS' income, expense and net, and how many there are."
  (let ((income 0.0d0) (expense 0.0d0))
    (dolist (row rows)
      (if (string= (getf row :type) "income")
          (incf income (getf row :amount))
          (incf expense (getf row :amount))))
    (values (rounded income) (rounded expense) (rounded (- income expense)) (length rows))))

(defun tool-list (args)
  "finance_list: the ledger's newest rows, with the totals of what matched."
  (answering
   (let* ((ledger (read-ledger))
          (limit (bounded-integer args "limit" *default-limit* 1 *max-limit*))
          (tx-type (string-downcase (or (argument-text args "tx_type") "all")))
          (category (or (argument-text args "category") ""))
          (rows (matching-rows ledger tx-type category))
          (page (last rows (min limit (length rows)))))
     (multiple-value-bind (income expense net count) (totals rows)
       (nlk:encode-json-object
        (nlk:json-object
         "count" (length page)
         "matched" count
         "currency" (currency)
         "income" income
         "expense" expense
         "net" net
         "transactions" (coerce (mapcar #'transaction-json page) 'vector)))))))

(defun budget-answer (ledger)
  "LEDGER's monthly budget as the model and /money both read it."
  (let* ((limit (or (getf ledger :limit) 0))
         (month (month-key))
         (spent 0.0d0)
         (count 0))
    (dolist (row (getf ledger :transactions))
      (when (and (string= (getf row :type) "expense")
                 (string= (month-key (getf row :ts)) month))
        (incf spent (getf row :amount))
        (incf count)))
    (nlk:json-object
     "month" month
     "currency" (currency)
     "monthly_limit" (rounded limit)
     "spent" (rounded spent)
     "remaining" (if (plusp limit) (rounded (- limit spent)) 0.0d0)
     "percent_used" (percentage spent limit)
     "transactions_this_month" count)))

(defun tool-budget (args)
  "finance_budget: set the monthly expense limit, and always report the month."
  (answering
   (with-ledger (ledger)
     (let ((limit (argument args "monthly_limit")))
       (when (and limit (not (and (numberp limit) (not (minusp limit)))))
         (fail "monthly_limit must be a number zero or more"))
       (when (and limit (plusp limit))
         (setf (getf ledger :limit) (coerce limit 'double-float))))
     (nlk:encode-json-object (budget-answer ledger)))))

(defun holding-answer (holding prices)
  "One holding priced against the market: value, cost, PnL and PnL percent."
  (let* ((symbol (getf holding :symbol))
         (amount (getf holding :amount))
         (buy-price (getf holding :buy-price))
         (price (or (gethash (coin-id symbol) prices) 0))
         (value (* price amount))
         (cost (* buy-price amount))
         (pnl (- value cost)))
    (nlk:json-object
     "symbol" symbol
     "amount" amount
     "buy_price" buy-price
     "current_price" price
     "value" (rounded value)
     "cost" (rounded cost)
     "pnl" (rounded pnl)
     "pnl_pct" (percentage pnl cost))))

(defun tool-portfolio (args)
  "finance_portfolio: what the holdings are worth, against what they cost."
  (answering
   (let* ((holdings (checked-holdings (argument args "holdings")))
          (prices (coingecko-prices (loop for holding across holdings
                                          collect (getf holding :symbol))))
          (rows (loop for holding across holdings
                      collect (holding-answer holding prices) into rows
                      finally (return rows)))
          (value (reduce #'+ rows :key (lambda (row) (gethash "value" row)) :initial-value 0))
          (cost (reduce #'+ rows :key (lambda (row) (gethash "cost" row)) :initial-value 0)))
     (nlk:encode-json-object
      (nlk:json-object
       "currency" "USD"
       "holdings" (coerce rows 'vector)
       "total_value" (rounded value)
       "total_cost" (rounded cost)
       "total_pnl" (rounded (- value cost))
       "total_pnl_pct" (percentage (- value cost) cost))))))

(defun opportunity-json (symbol coingecko binance diff)
  (nlk:json-object
   "symbol" symbol
   "coingecko" coingecko
   "binance" binance
   "diff_pct" (rounded diff)))

(defun arbitrage-scan ()
  "The spread opportunities among CoinGecko's largest coins and Binance's spot
pairs, largest difference first => ((SYMBOL COINGECKO BINANCE DIFF) ...)."
  (let ((binance (binance-prices))
        (markets (get-json +coingecko-markets+ "CoinGecko"))
        (opportunities '()))
    (unless (vectorp markets)
      (fail "CoinGecko answered a market list that is not an array"))
    (loop for coin across markets
          for symbol = (nlk:json-value coin :string "symbol")
          for price = (nlk:json-value coin :number "current_price")
          for pair = (and symbol (concatenate 'string (string-upcase symbol) "USDT"))
          for spot = (and pair (gethash pair binance))
          when (and spot price (plusp price))
            do (let ((diff (* 100 (/ (- spot price) price))))
                 (when (> (abs diff) *spread-percent*)
                   (push (list (string-upcase symbol) price spot diff) opportunities))))
    (subseq (stable-sort opportunities #'> :key (lambda (one) (abs (fourth one))))
            0 (min *top-spreads* (length opportunities)))))

(defun tool-arbitrage (args)
  "finance_arbitrage: CoinGecko's largest coins against Binance's spot pairs."
  (declare (ignore args))
  (answering
   (nlk:encode-json-object
    (nlk:json-object
     "opportunities" (coerce (loop for (symbol coingecko binance diff) in (arbitrage-scan)
                                    collect (opportunity-json symbol coingecko binance diff))
                              'vector)
     "spread_percent" (one-decimal *spread-percent*)
     "note" "diff > spread_percent flagged; account for fees before trading"))))

(defun arbitrage-text ()
  "The spread scan as /money arbitrage reads it."
  (let ((opportunities (arbitrage-scan)))
    (if (null opportunities)
        (format nil "No pair differs by more than ~,2f% right now.~%" (one-decimal *spread-percent*))
        (with-output-to-string (out)
          (format out "CoinGecko against Binance, differences over ~,2f% (fees not counted):~%"
                  (one-decimal *spread-percent*))
          (loop for (symbol coingecko binance diff) in opportunities
                do (format out "  ~a: ~,2f vs ~,2f - ~,2f%~%" symbol coingecko binance diff))))))


;;; --- /money and the manual -------------------------------------------------------

(defun status-text (ledger)
  "The ledger and this month's budget, as /money status reads them."
  (multiple-value-bind (income expense net count) (totals (getf ledger :transactions))
    (let ((budget (budget-answer ledger))
          (limit (or (getf ledger :limit) 0)))
      (format nil "Money Magnet (~a): ~d transaction~:p.~%Income: ~,2f · Expense: ~,2f · Net: ~,2f~%Month ~a: spent ~,2f~@[ of ~,2f~]~@[, ~,2f left~]; ~,1f% used~%"
              (currency) count income expense net
              (gethash "month" budget)
              (gethash "spent" budget)
              (and (plusp limit) (gethash "monthly_limit" budget))
              (let ((left (gethash "remaining" budget)))
                (and (plusp left) left))
              (gethash "percent_used" budget)))))


(defun add-via-command (argv)
  "`/money add TYPE AMOUNT CATEGORY [NOTE]` out of ARGV's words."
  (when (< (length argv) 4)
    (return-from add-via-command
      "Usage: /money add income|expense AMOUNT CATEGORY [NOTE]"))
  (let ((amount (read-amount (third argv))))
    (unless amount
      (return-from add-via-command
        (format nil "~a is not an amount: /money add expense 12.50 food" (third argv))))
    (with-ledger (ledger)
      (let ((entry (list :id (next-id (getf ledger :transactions))
                         :ts (now-text)
                         :type (if (string-equal (second argv) "income") "income" "expense")
                         :amount amount
                         :category (fourth argv)
                         :note (format nil "~{~a~^ ~}" (cddddr argv)))))
        (setf (getf ledger :transactions) (append (getf ledger :transactions) (list entry)))
        (format nil "Added #~d: ~a ~,2f ~a~@[ (~a)~]"
                (getf entry :id) (getf entry :type) (getf entry :amount)
                (getf entry :category)
                (let ((note (getf entry :note)))
                  (and (plusp (length note)) note)))))))

(defun run-command (args session-id)
  "One /money invocation: the ledger and the month, one row added, the
budget, or the spread scan."
  (declare (ignore session-id))
  ;; Not wrapped in ANSWERING: while the cell is not running the command must
  ;; refuse as MONEY-MAGNET-ERROR, the way every other verb does.
  (let* ((typed (nlk:trimmed (or args "")))
         (argv (nlk:split-words typed))
         (head (string-downcase (or (first argv) ""))))
    (cond
      ((member head '("" "status") :test #'string=)
       (status-text (read-ledger)))
      ((string= head "add") (add-via-command argv))
      ((string= head "budget")
       (with-ledger (ledger)
         (let ((limit (read-amount (second argv))))
           (when limit (setf (getf ledger :limit) limit))
           (let ((budget (budget-answer ledger)))
             (format nil "Month ~a: limit ~,2f ~a; spent ~,2f (~,1f% used)~@[, ~,2f left~]~@[, over by ~,2f~].~%"
                     (gethash "month" budget)
                     (gethash "monthly_limit" budget)
                     (currency)
                     (gethash "spent" budget)
                     (gethash "percent_used" budget)
                     (let ((left (gethash "remaining" budget)))
                       (and (plusp left) left))
                     (let ((left (gethash "remaining" budget)))
                       (and (minusp left) (- left))))))))
      ((string= head "portfolio")
       "finance_portfolio prices your holdings: call it with holdings as [{symbol, amount, buy_price}].")
      ((string= head "arbitrage")
       (handler-case (arbitrage-text)
         ((or error sb-sys:deadline-timeout) (condition)
           (format nil "the spread scan failed: ~a" condition))))
      ((member head '("help" "-h" "--help") :test #'string=)
       (format nil "Money Magnet commands:~%  /money status          - the ledger and this month~%  ~
                    /money add TYPE AMOUNT CATEGORY [NOTE]~%  /money budget [LIMIT]  - set or read the ~
                    monthly expense limit~%  /money portfolio       - how to price holdings~%  ~
                    /money arbitrage       - CoinGecko against Binance, now"))
      (t (format nil "Unknown command: ~a. Try /money help." head)))))

;;; --- the schemas -----------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "additionalProperties" :false
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *add-schema*
  (tool-schema
   "Add one transaction -- income or expense -- to the user's ledger, in the currency the ledger keeps. The amount is a positive number; the category is the user's own word (food, rent, salary)."
   '(("tx_type" "string" "\"income\" or \"expense\"." t)
     ("amount" "number" "A positive number, in the ledger's currency." t)
     ("category" "string" "What it was for or where it came from." t)
     ("note" "string" "Optional short note.")))
  "The finance_add tool, as the original's schema has it.")

(defparameter *list-schema*
  (tool-schema
   "List the ledger's newest transactions, with the income, expense and net of everything the filters match. Nothing is sent anywhere: this answers from the ledger file on this computer."
   '(("limit" "integer" "How many rows to answer, 1-500, default 20." t)
     ("tx_type" "string" "\"income\", \"expense\" or \"all\" (default).")
     ("category" "string" "Only this category, matched without case.")))
  "The finance_list tool.")

(defparameter *budget-schema*
  (tool-schema
   "Set the monthly expense limit, and always answer this month's spending, what is left of the limit and the percentage used. Months are UTC calendar months."
   '(("monthly_limit" "number" "The monthly expense limit; leave it out to only read the month.")))
  "The finance_budget tool.")

(defparameter *portfolio-schema*
  (tool-schema
   "Price a crypto portfolio at CoinGecko's current USD prices: value, cost, profit or loss and its percentage, per holding and in total. Only the coin symbols are sent."
   '(("holdings" "array" "At least one {symbol, amount, buy_price} object." t)))
  "The finance_portfolio tool.")

(defparameter *arbitrage-schema*
  (tool-schema
   "Scan for arbitrage: CoinGecko's twenty largest coins against Binance's spot pairs, reporting the pairs whose prices differ by more than the configured spread. Fees are not accounted for."
   '())
  "The finance_arbitrage tool.")

(defparameter +primer+
  "Money Magnet keeps the user's money picture on this computer: income and expense rows, a
monthly budget, a crypto portfolio and a spread scan. (help :money) is this page.
  finance_add       {tx_type, amount, category, note?}    one income or expense row
  finance_list      {limit?, tx_type?, category?}         newest rows, with income, expense and net
  finance_budget    {monthly_limit?}                      set the monthly limit; answer this month
  finance_portfolio {holdings: [{symbol, amount, buy_price}]}  value, cost and PnL per holding
  finance_arbitrage {}                                    CoinGecko against Binance, now
The ledger is one file, ledger.json, in the folder the money-magnet section's data_dir names; a
row's timestamp and the budget month are UTC. Amounts are in the section's currency, a label the
answers carry. Only finance_portfolio and finance_arbitrage dial anything: the public CoinGecko
and Binance price APIs, which need no key and receive coin symbols, never a row. /money status,
/money add TYPE AMOUNT CATEGORY [NOTE], /money budget [LIMIT], /money arbitrage. A refusal is
ERROR: money-magnet with the reason, for the model to repair."
  "What (help :money) answers while the cell runs.")

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell money-magnet
  (:section ("money-magnet")
    (:guide "data_dir is the folder the ledger file is kept in; next step: /money shows the month")
    ("data_dir" :path :required t
     :doc "the folder the ledger file is kept in, one ledger per folder")
    ("currency" :string :default "USD"
     :doc "the label the amounts are kept in; the market tools price in USD"))
  (:help :money "money-magnet: a ledger, a monthly budget, a crypto portfolio and a spread scan" +primer+)
  (:command "money" 'run-command
            :description "The ledger and this month, one row added, the budget, or the spread scan"
            :argument-hint "status · add TYPE AMOUNT CATEGORY [NOTE] · budget [LIMIT] · arbitrage")
  (:tool "finance_add" *add-schema* 'tool-add)
  (:tool "finance_list" *list-schema* 'tool-list)
  (:tool "finance_budget" *budget-schema* 'tool-budget)
  (:tool "finance_portfolio" *portfolio-schema* 'tool-portfolio)
  (:tool "finance_arbitrage" *arbitrage-schema* 'tool-arbitrage)
  (:start (lambda ()
            ;; A ledger needs a folder before it can have a file: a section
            ;; that names none refuses at start, naming the member.
            (unless (plusp (length (or (setting :data-dir) "")))
              (error 'nlk:config-refusal
                     :detail "money-magnet: data_dir names the folder the ledger is kept in; set it in the money-magnet section of config.jsonc or in /settings"))
            (ensure-directories-exist (ledger-file)))))
