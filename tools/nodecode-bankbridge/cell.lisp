;;;; cell.lisp --- BankBridge's read-only bank data, as tools and slash commands.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; BankBridge is a remote MCP server: one Streamable-HTTP endpoint, JSON-RPC
;;;; in, JSON-RPC out, and a bearer key from the operator's dashboard. The cell
;;;; is that client with no SDK under it -- nlk:http posts the three messages a
;;;; call takes (initialize, notifications/initialized, tools/call) and reads
;;;; back whichever shape the server answers, one JSON body or an SSE stream.
;;;; Each tool here is one BankBridge tool, its arguments passed through whole,
;;;; so the server owns the schema and the cell owns none of it.
;;;;
;;;; Twelve tools, one /bankbridge command that says what the section holds,
;;;; and the original's 19 slash commands as prompts (commands.lisp) submitted
;;;; to the session that ran them.
;;;;
;;;; Ported from bankbridge-money's Hermes Agent plugin (see NOTICE).

(in-package #:nodecode-bankbridge)

;;; --- the manual ---------------------------------------------------------------

(defparameter *summary*
  "Your own bank, card and brokerage accounts, read-only, through BankBridge: balances, spending, subscriptions, cashflow, holdings."
  "The one line every request's help section carries.")

(defparameter *manual*
  "BankBridge answers questions about the operator's own money with live, read-only data from the US
banks, credit cards and brokerages they connected to it. Nothing can move money, pay a bill or place a
trade. The cell is the client of BankBridge's remote MCP server; each tool below is one BankBridge
tool and its arguments go to the server as given.
  bankbridge_list_accounts {account_id}        every connected account with its live balance;
      account_id narrows it to one. Start a money question here: account ids change when a bank is
      reconnected, so never reuse an old one.
  bankbridge_get_account {account_id}          one account in full.
  bankbridge_get_spending_summary {month, start_date, end_date, group_by, account_id}
      how much went where; group_by is category, merchant, month or week.
  bankbridge_search_transactions {query, merchant, account_id, start_date, end_date, min_amount,
      max_amount, limit, offset}                find a charge by name or amount.
  bankbridge_get_merchant_history {merchant, start_date, end_date}   one merchant over time: its
      first charge, its price now, whether a 'duplicate' is one subscription under two spellings.
  bankbridge_get_recurring_charges {account_id}    subscriptions and bills BankBridge detected.
  bankbridge_get_monthly_cashflow {month}      income against spending for one month, YYYY-MM.
  bankbridge_list_transactions {start_date, end_date, category, account_id, min_amount, max_amount,
      limit, offset}                            the raw rows; offset pages them.
  bankbridge_list_categories {}                the category names the filters take.
  bankbridge_list_holdings {account_id}        portfolio, cost basis, gains.
  bankbridge_list_investment_transactions {account_id, start_date, end_date}
      buys, sells and dividends.
  bankbridge_connect_bank {}                   a link the operator opens to connect another bank.
Amounts are dollars: positive is money leaving the account (spending), negative is money coming in
(income, refunds). Card payments from checking and transfers between the operator's own accounts are
not spending; transfers to other people are. Check a merchant's history before calling two charges
duplicates, and do not project a year from one month. Every answer carries a warnings array: if it is
not empty, tell the operator (a bank may need reconnecting) and say the totals cover only the banks
that answered. Setup, if a call says the key is missing or refused: a BankBridge account, a key from
the dashboard named in the section's api_key_env, and at least one bank connected. Pricing is per
connected bank, from BankBridge.
The /bankbridge command says what the section holds; its check subcommand proves the key with one
call. The original plugin's 19 money reviews are slash commands too: /balances, /subscriptions,
/monthly-report and the rest submit their prompt to this session."
  "What (help :bankbridge) answers while the cell runs.")

;;; --- the endpoints and the twelve tools --------------------------------------------

(defparameter *default-url* "https://bankbridge.money/api/mcp"
  "BankBridge's one endpoint: the only host a call leaves for.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks.")

(defparameter *tools*
  '(("list_accounts" "List every connected account with its live balance, optionally one account."
     ("account_id"))
    ("get_account" "One account in full: its balance, its type, its mask and its limits."
     ("account_id"))
    ("get_spending_summary" "How much was spent, grouped by category, merchant, month or week."
     ("month" "start_date" "end_date" "group_by" "account_id"))
    ("search_transactions" "Find a charge by merchant, description, or amount range."
     ("query" "merchant" "account_id" "start_date" "end_date" "min_amount" "max_amount" "limit" "offset"))
    ("get_merchant_history" "One merchant over time: first charge, latest charge, price drift."
     ("merchant" "start_date" "end_date"))
    ("get_recurring_charges" "The subscriptions and bills BankBridge detected, with their cadence."
     ("account_id"))
    ("get_monthly_cashflow" "Income against spending for one month, given as YYYY-MM."
     ("month"))
    ("list_transactions" "The raw transactions in a date range, filtered and paged with offset."
     ("start_date" "end_date" "category" "account_id" "min_amount" "max_amount" "limit" "offset"))
    ("list_categories" "The category names the spending and transaction filters take."
     ())
    ("list_holdings" "Portfolio holdings with cost basis and gains."
     ("account_id"))
    ("list_investment_transactions" "Buys, sells and dividends in a date range."
     ("account_id" "start_date" "end_date"))
    ("connect_bank" "A link the operator opens in a browser to connect another bank."
     ()))
  "Each BankBridge tool this cell offers: its name, its description, its named arguments.")

(defparameter *argument-docs*
  '(("account_id" . "an account id from bankbridge_list_accounts")
    ("month" . "one month, YYYY-MM")
    ("start_date" . "the first day of the range, YYYY-MM-DD")
    ("end_date" . "the last day of the range, YYYY-MM-DD")
    ("group_by" . "category, merchant, month or week")
    ("query" . "text to find in a merchant name or description")
    ("merchant" . "a merchant name, as a charge shows it")
    ("category" . "a category name from bankbridge_list_categories")
    ("min_amount" . "the smallest amount in dollars, inclusive")
    ("max_amount" . "the largest amount in dollars, inclusive")
    ("limit" . "how many rows at most")
    ("offset" . "how many rows to skip, to page"))
  "What each argument means, as the tool schema says it.")

(defun tool-schema (name)
  "The NAME tool's schema: the server owns the arguments, so every member the
original plugin's skill names is here, documented, and anything else the server
takes passes through too."
  (let ((entry (assoc name *tools* :test #'string=)))
    (unless entry
      (fail "~s is not one of BankBridge's tools" name))
    (list :description (second entry)
          :parameters
          (nlk:json-object
           "type" "object"
           "properties"
           (nlk:make-json-object
            (loop for (member . doc) in *argument-docs*
                  append (list member (nlk:json-object "type" "string" "description" doc))))
           "additionalProperties" t
           "required" #()))))

;;; --- the one exchange ---------------------------------------------------------------

(defun nonblank (text)
  (and (stringp text) (plusp (length (nlk:trimmed text))) (nlk:trimmed text)))

(defun api-key ()
  "The key the section's api_key_env names: read once, at start."
  (setting :api-key))

(defun request-headers ()
  `(("content-type" . "application/json")
    ("accept" . "application/json, text/event-stream")
    ("authorization" . ,(format nil "Bearer ~a" (api-key)))
    ("mcp-protocol-version" . ,+protocol-version+)))

(defun header (headers name)
  "NAME's value in the response headers (the HTTP client lowercases them), or NIL."
  (and (hash-table-p headers) (gethash name headers)))

(defun decode-body (text)
  "TEXT as a JSON value, or NIL when it is not JSON."
  (and (stringp text) (plusp (length text))
       (handler-case (nlk:decode-json text) (error () nil))))

(defun read-all (stream)
  (with-output-to-string (out)
    (loop for char = (read-char stream nil nil)
          while char do (write-char char out))))

(defun reply-p (message)
  (and (hash-table-p message)
       (nth-value 1 (gethash "id" message))
       (null (nlk:json-value message :string "method"))))

(defun read-message (body headers status)
  "The one JSON-RPC message a response's BODY carries, reading an SSE stream to
its reply and a JSON body whole. => the message, or NIL for an empty answer."
  (cond
    ((member status '(202 204)) nil)
    ((null body) nil)
    ((stringp body) (decode-body body))
    ((streamp body)
     (let ((content-type (or (header headers "content-type") "")))
       (if (uiop:string-prefix-p "text/event-stream" content-type)
           (let ((found nil))
             (nlk:read-events
              (if (subtypep (stream-element-type body) 'character)
                  body
                  (flexi-streams:make-flexi-stream body :external-format :utf-8))
              (lambda (text &aux (message (decode-body text)))
                (when (reply-p message) (setf found message) t))
              :idle 30 :limit (* 2 1024 1024))
             found)
           (decode-body (read-all body)))))
    (t nil)))

(defun failure-text (status message)
  "What a refused exchange says, in BankBridge's own words when it gave any."
  (let ((detail (and (hash-table-p message)
                     (let ((error (gethash "error" message)))
                       (cond ((hash-table-p error) (nlk:json-value error :string "message"))
                             ((stringp error) error))))))
    (cond ((and (>= status 400) (= status 401))
           "ERROR: BankBridge refused the key (HTTP 401): set the variable the section's api_key_env names to a key from the BankBridge dashboard, then (restart-cells \"nodecode-bankbridge\")")
          ((>= status 400)
           (format nil "ERROR: BankBridge answered HTTP ~a~@[: ~a~]" status detail))
          (detail (format nil "ERROR: BankBridge: ~a" detail))
          (t "ERROR: BankBridge answered something this cell could not read"))))

(defun post (message &key (idle 0))
  "One JSON-RPC POST to the endpoint => (values MESSAGE STATUS HEADERS)."
  (declare (ignore idle))
  (multiple-value-bind (body status headers)
      (nlk:http :post (setting :url)
                :headers (request-headers)
                :content (nlk:encode-json-object message)
                :timeout (setting :timeout))
    (values (handler-case (read-message body headers status)
              (error (condition)
                (fail "BankBridge's answer could not be read: ~a"
                      (nlk:one-line (princ-to-string condition)))))
            status headers)))

(defun handshake ()
  "Initialize a session and tell the server it may go on. => the session id, or NIL."
  (multiple-value-bind (message status headers)
      (post (nlk:json-object "jsonrpc" "2.0" "id" 1 "method" "initialize"
                              "params" (nlk:json-object
                                        "protocolVersion" +protocol-version+
                                        "capabilities" (nlk:json-object)
                                        "clientInfo" (nlk:json-object
                                                      "name" "nodecode" "version" "0.1.0"))))
    (when (>= status 400)
      (fail "~a" (failure-text status message)))
    (when (and (hash-table-p message) (gethash "error" message))
      (fail "~a" (failure-text 200 message)))
    (post (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized"))
    (header headers "mcp-session-id")))

(defun rpc (method params &key (id 2))
  "One request inside a fresh session => (values RESULT STATUS MESSAGE)."
  (handshake)
  (multiple-value-bind (message status)
      (post (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                              "params" (or params (nlk:json-object))))
    (values (result-of message status) status message)))

(defun result-of (message status)
  "MESSAGE's result, or a refusal in its own words."
  (when (>= status 400)
    (fail "~a" (failure-text status message)))
  (let ((error (and (hash-table-p message) (gethash "error" message))))
    (when error
      (fail "~a" (failure-text 200 message))))
  (unless (hash-table-p message)
    (fail "BankBridge answered no result; /bankbridge check tells whether the key works"))
  (or (gethash "result" message)
      (fail "BankBridge answered no result")))

;;; --- the answers -------------------------------------------------------------------

(defun content-text (result)
  "RESULT's text, every content part joined, and whether the server called it an error."
  (let ((parts (nlk:json-array result "content"))
        (pieces '()))
    (loop for part across (or parts #())
          when (equal "text" (nlk:json-value part :string "type"))
            do (push (or (nlk:json-value part :string "text") "") pieces))
    (values (format nil "~{~a~^~%~}" (nreverse pieces))
            (and (eq t (nlk:json-value result :boolean "isError")) t))))

(defun clipped (text)
  "TEXT under the section's result ceiling, saying what was cut."
  (let ((cap (or (ignore-errors (setting :result-chars)) 20000)))
    (if (<= (length text) cap)
        text
        (format nil "~a~%[BankBridge: the rest of this answer was cut at ~:d characters; ask for less, or page with offset]"
                (subseq text 0 cap) cap))))

(defun call (name &optional arguments)
  "Call the BankBridge tool NAME with ARGUMENTS, a JSON object or an alist.
=> its text; a refusal is the text of a failed call, never a signal."
  (handler-case
      (progn
        (unless (nonblank name) (fail "a call needs a tool name"))
        (when (and arguments (hash-table-p arguments) (zerop (hash-table-count arguments)))
          (setf arguments nil))
        (multiple-value-bind (result status) (rpc "tools/call" (nlk:json-object "name" name "arguments" (or arguments (nlk:json-object))))
          (declare (ignore status))
          (multiple-value-bind (text error-p) (content-text result)
            (clipped (if error-p (format nil "ERROR: ~a" text) text)))))
    (bankbridge-error (condition)
      (format nil "ERROR: ~a" (bankbridge-error-detail condition)))
    (error (condition)
      (format nil "ERROR: BankBridge: ~a" (nlk:one-line (princ-to-string condition))))))

(defun tool-answer (name arguments)
  "A tool's body: NAME's arguments, a decoded JSON hash table, passed through."
  (call name (and arguments (plusp (hash-table-count arguments)) arguments)))

(defun tool-names ()
  "The tools the server offers, from one tools/list."
  (let ((result (rpc "tools/list" nil)))
    (loop for tool across (or (nlk:json-array result "tools") #())
          collect (nlk:json-value tool :string "name"))))

;;; --- the commands ------------------------------------------------------------------

(defun prompt-command (name)
  "The prompt of the original's slash command NAME."
  (third (assoc name *prompts* :test #'string=)))

(defun run-prompt (name args session-id)
  "The original's slash command NAME: its prompt, $ARGUMENTS filled in, to the session."
  (let* ((text (prompt-command name))
         (filled (string-replace-all (or text "") "$ARGUMENTS" (nlk:trimmed (or args "")))))
    (nle:submit session-id filled :command-id (format nil "bankbridge-~a-~a" name (get-universal-time))
                 :source "bankbridge" :source-id name)
    (format nil "/~a: asked BankBridge -- ~a" name
            (let ((head (first (nlk:lines (prompt-command-description name)))))
              (or head "the money review runs in this session")))))

(defun prompt-command-description (name)
  (or (second (assoc name *prompts* :test #'string=)) ""))

(defun string-replace-all (text old new)
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search old text :start2 start)
          do (if at
                 (progn (write-string text out :start start :end at)
                        (write-string new out)
                        (setf start (+ at (length old))))
                 (progn (write-string text out :start start)
                        (return))))))

(defun register-prompt-commands ()
  "The original's 19 money reviews, each its own slash command."
  (dolist (entry *prompts*)
    (destructuring-bind (name description &rest prompt) entry
      (declare (ignore prompt))
      (nle:register-command "nodecode-bankbridge" name
                            (let ((name name))
                              (lambda (args session-id) (run-prompt name args session-id)))
                            :description (format nil "BankBridge: ~a" description)))))

(defun unregister-prompt-commands ()
  (nle:unregister-commands "nodecode-bankbridge"))

(defun prompt-command-names ()
  (mapcar #'first *prompts*))

(defun status-line ()
  "What /bankbridge says: where it points, which variable holds the key, what it offers."
  (format nil "BankBridge: ~a; key from ~a~:[ (unset)~;~]; ~d tools; ~d money reviews (~{~a~^, ~})"
          (setting :url) (setting :api-key-env)
          (nonblank (api-key)) (length *tools*) (length *prompts*)
          (mapcar (lambda (entry) (concatenate 'string "/" (first entry))) *prompts*)))

(defun run-slash (args session-id)
  "The /bankbridge command: its status, a check that the key works, or help."
  (let ((words (nlk:split-words (or args "")))
        (verb (string-downcase (or (first (nlk:split-words (or args ""))) ""))))
    (cond
      ((string= verb "")
       (values (status-line)
               (nle:list-dialog
                :title "BankBridge" :summary (status-line)
                :rows (list (nle:run-row "Check the key" "/bankbridge check"
                                         :detail "one call: which accounts the key reaches")
                            (nle:run-row "Balances" "/balances"
                                         :detail "every account, a rolling total, credit utilization")))))
      ((string= verb "check")
       (handler-case
           (let ((names (tool-names)))
             (format nil "BankBridge answered: ~d tools (~{~a~^, ~})"
                     (length names) (subseq names 0 (min 6 (length names)))))
         (bankbridge-error (condition)
           (format nil "BankBridge: ~a" (bankbridge-error-detail condition)))))
      ((string= verb "help")
       *manual*)
      ((member verb (prompt-command-names) :test #'string=)
       (run-prompt verb (format nil "~{~a~^ ~}" (rest words)) session-id))
      (t (format nil "BankBridge: ~a is not one of its words; /bankbridge alone says where it points, and check proves the key"
                 verb)))))

;;; --- reading the section ---------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the one secret the section names: read once, here."
  (declare (ignore table))
  (let* ((key-env (or (nonblank (getf values :api-key-env)) "BANKBRIDGE_API_KEY"))
         (url (or (nonblank (getf values :url)) *default-url*))
         (plist (copy-list values)))
    (setf (getf plist :api-key-env) key-env
          (getf plist :url) url
          (getf plist :api-key)
          (or (nonblank (nle::credential-env key-env))
              (nlk:config-error
               "~a is unset or empty: put a BankBridge key from https://bankbridge.money in it, or name another variable in api_key_env"
               key-env))
          (getf plist :timeout) (or (getf values :timeout) 60)
          (getf plist :result-chars) (or (getf values :result-chars) 20000))
    plist))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell bankbridge
  (:section ("bankbridge")
    (:guide "where to go: a key from bankbridge.money's dashboard, exported in the shell that starts Nodecode; a bank connected on the dashboard; then ask about your money")
    ("api_key_env" :env :default "BANKBRIDGE_API_KEY"
     :doc "environment variable holding a BankBridge key (bbk_...), from the dashboard")
    ("url" :string :default "https://bankbridge.money/api/mcp"
     :doc "the BankBridge MCP endpoint; leave it as it is")
    ("timeout" :integer :default 60 :min 5 :doc "seconds one call may take")
    ("result_chars" :integer :default 20000 :min 500 :doc "the most characters one answer carries"))
  (:settings #'read-settings)
  (:help :bankbridge *summary* *manual*)
  (:command "bankbridge" 'run-slash
            :description "BankBridge: where it points, a check that the key works, the money reviews")
  (:start (lambda ()
            (register-prompt-commands)
            (nle:on-stop (lambda () (unregister-prompt-commands)))))
  (:tool "bankbridge_list_accounts" (tool-schema "list_accounts")
         (lambda (arguments) (tool-answer "list_accounts" arguments)))
  (:tool "bankbridge_get_account" (tool-schema "get_account")
         (lambda (arguments) (tool-answer "get_account" arguments)))
  (:tool "bankbridge_get_spending_summary" (tool-schema "get_spending_summary")
         (lambda (arguments) (tool-answer "get_spending_summary" arguments)))
  (:tool "bankbridge_search_transactions" (tool-schema "search_transactions")
         (lambda (arguments) (tool-answer "search_transactions" arguments)))
  (:tool "bankbridge_get_merchant_history" (tool-schema "get_merchant_history")
         (lambda (arguments) (tool-answer "get_merchant_history" arguments)))
  (:tool "bankbridge_get_recurring_charges" (tool-schema "get_recurring_charges")
         (lambda (arguments) (tool-answer "get_recurring_charges" arguments)))
  (:tool "bankbridge_get_monthly_cashflow" (tool-schema "get_monthly_cashflow")
         (lambda (arguments) (tool-answer "get_monthly_cashflow" arguments)))
  (:tool "bankbridge_list_transactions" (tool-schema "list_transactions")
         (lambda (arguments) (tool-answer "list_transactions" arguments)))
  (:tool "bankbridge_list_categories" (tool-schema "list_categories")
         (lambda (arguments) (tool-answer "list_categories" arguments)))
  (:tool "bankbridge_list_holdings" (tool-schema "list_holdings")
         (lambda (arguments) (tool-answer "list_holdings" arguments)))
  (:tool "bankbridge_list_investment_transactions" (tool-schema "list_investment_transactions")
         (lambda (arguments) (tool-answer "list_investment_transactions" arguments)))
  (:tool "bankbridge_connect_bank" (tool-schema "connect_bank")
         (lambda (arguments) (tool-answer "connect_bank" arguments))))
