;;;; cell.lisp --- Paystack transactions, reconciliation, /payops and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :payops) answers it, and every request's help section carries one
;;;; line naming it. A person drives the same verbs from eval in the PAYOPS
;;;; package, and the /payops command answers in chat.
;;;;
;;;; Read-only by construction: the only two endpoints reached are Paystack's
;;;; transaction and settlement listers, both GET, both under one page cap.
;;;; There is no charge, refund, transfer or payout call anywhere in here.
;;;;
;;;; Rewritten from BigWave45's Hermes Agent plugin (see NOTICE): the two
;;;; model-facing tools and their argument schemas, the normalization of
;;;; Paystack's rows, the paged read with its cap warning, and the
;;;; deterministic reconciliation report (per-currency summaries, duplicate
;;;; references, and the original's stated limitations).

(in-package #:nodecode-payops)

;;; --- the manual ----------------------------------------------------------------

(defparameter *manual*
  "Paystack data through the nodecode-payops cell: read-only, two tools, and the
same two verbs from eval in the payops package. Nothing here moves money.
  payops_list_transactions {from_date, to_date, max_pages, providers}
      A Paystack account's transactions for an inclusive date range, one object
      each: provider, transaction_id, reference, amount_minor, currency, status,
      paid_at. Amounts are in minor units (kobo for NGN), as Paystack gives them.
  payops_reconcile {from_date, to_date, max_pages, providers}
      The same transactions plus the account's settlements, summarized per
      provider and currency, with the data-quality exceptions found: duplicate
      reference, the same reference twice in the range.
Dates are YYYY-MM-DD. max_pages is 1 to 20 pages of 100 records (default: the
payops section's max_pages, 5); a range longer than that is read up to the cap
and the answer says so in provider_warnings. providers takes paystack only.
From eval: (payops:transactions \"2026-10-01\" \"2026-10-31\") and
(payops:reconcile \"2026-10-01\" \"2026-10-31\"), or /payops in the shell.
The key is read from the environment variable the section's secret_key_env names
(PAYSTACK_SECRET_KEY by default) and sent only to api.paystack.co, over https,
as a Bearer header. Use a test key while you are not sure.
Reconciliation reports transaction totals and settlement totals SEPARATELY:
settlement timing lags transaction activity, so they are not treated as equal.
It also does not reach a bank statement or a merchant order ledger."
  "What (help :payops) answers while the cell runs.")

;;; --- amounts -------------------------------------------------------------------

(defparameter +zero-decimal+
  '("BIF" "CLP" "DJF" "GNF" "ISK" "JPY" "KMF" "KRW" "PYG" "RWF" "UGX" "VND" "VUV"
    "XAF" "XOF" "XPF")
  "Currencies whose minor unit is the unit (money.py's table, kept).")

(defparameter +three-decimal+
  '("BHD" "IQD" "JOD" "KWD" "LYD" "OMR" "TND")
  "Currencies with three decimal places (money.py's table, kept).")

(defun currency-decimals (currency)
  "How many decimal places CURRENCY's minor unit implies: 0, 3, or 2."
  (let ((code (string-upcase (or currency ""))))
    (cond ((member code +zero-decimal+ :test #'string=) 0)
          ((member code +three-decimal+ :test #'string=) 3)
          (t 2))))

(defun format-amount (minor currency)
  "MINOR units of CURRENCY as a readable amount: (format-amount 123456 \"NGN\") => \"NGN 1,234.56\"."
  (let* ((code (string-upcase (or currency "NGN")))
         (decimals (currency-decimals code))
         (factor (expt 10 decimals))
         (magnitude (abs (or minor 0)))
         (sign (if (minusp (or minor 0)) "-" "")))
    (if (zerop decimals)
        (format nil "~a ~a~:d" code sign magnitude)
        (format nil "~a ~a~:d.~v,'0d" code sign (floor magnitude factor) decimals (mod magnitude factor)))))

;;; --- the records, as the original's dataclasses had them --------------------------

(defstruct (transaction (:constructor make-transaction))
  (provider "paystack") (transaction-id "") (reference "") (amount-minor 0)
  (currency "NGN") (status "unknown") (paid-at nil))

(defstruct (settlement (:constructor make-settlement))
  (provider "paystack") (settlement-id "") (currency "NGN") (amount-minor 0)
  (status "unknown") (settled-at nil))

(defun wire-text (value)
  "VALUE from a wire row as text: NIL is empty, a number its own spelling."
  (cond ((null value) "") ((stringp value) value) (t (princ-to-string value))))

(defun wire-text-or (value fallback)
  "WIRE-TEXT, or FALLBACK when it is empty."
  (let ((text (wire-text value)))
    (if (plusp (length text)) text fallback)))

(defun row-transaction (row)
  "One Paystack transaction row as a TRANSACTION, as the original normalized it."
  (make-transaction
   :transaction-id (wire-text (nlk:json-value row :any "id"))
   :reference (wire-text (nlk:json-value row :any "reference"))
   :amount-minor (or (nlk:json-value row :integer "amount") 0)
   :currency (string-upcase (wire-text-or (nlk:json-value row :any "currency") "NGN"))
   :status (string-downcase (wire-text-or (nlk:json-value row :any "status") "unknown"))
   :paid-at (let ((paid (nlk:json-value row :string "paid_at")))
              (or paid (nlk:json-value row :string "transaction_date")))))

(defun row-settlement (row)
  "One Paystack settlement row as a SETTLEMENT; total_amount wins over amount."
  (make-settlement
   :settlement-id (wire-text (nlk:json-value row :any "id"))
   :currency (string-upcase (wire-text-or (nlk:json-value row :any "currency") "NGN"))
   :amount-minor (or (nlk:json-value row :integer "total_amount")
                     (nlk:json-value row :integer "amount")
                     0)
   :status (string-downcase (wire-text-or (nlk:json-value row :any "status") "unknown"))
   :settled-at (let ((settled (nlk:json-value row :string "settlement_date")))
                 (or settled (nlk:json-value row :string "createdAt")))))

(defun transaction-json (item)
  "ITEM as the JSON object a tool answer carries."
  (nlk:json-object "provider" (transaction-provider item)
                    "transaction_id" (transaction-transaction-id item)
                    "reference" (transaction-reference item)
                    "amount_minor" (transaction-amount-minor item)
                    "currency" (transaction-currency item)
                    "status" (transaction-status item)
                    :opt "paid_at" (transaction-paid-at item)))

;;; --- the reconciliation, as reconcile.py computed it -------------------------------

(defstruct (tx-tally (:constructor make-tx-tally))
  (transaction-count 0) (successful-count 0) (failed-count 0) (transaction-amount-minor 0))

(defstruct (st-tally (:constructor make-st-tally))
  (settlement-count 0) (settled-amount-minor 0) (non-final-count 0))

(defparameter +successful+ '("success" "successful" "succeeded")
  "The statuses reconcile.py counted as money in.")

(defparameter +failed+ '("failed" "failure")
  "The statuses reconcile.py counted as a failed attempt.")

(defparameter +settled+ '("success" "successful" "settled" "completed")
  "The statuses reconcile.py counted as settled; anything else is not final.")

(defun group-lessp (a b)
  "Order two (PROVIDER CURRENCY) groups by provider, then currency."
  (or (string-lessp (first a) (first b))
      (and (string= (first a) (first b)) (string-lessp (second a) (second b)))))

(defun sorted-groups (table)
  "TABLE's keys, in a stable order, so one range always answers one report."
  (sort (loop for key being the hash-keys of table collect key) #'group-lessp))

(defun tally (transactions settlements)
  "=> (values TRANSACTION-SUMMARIES SETTLEMENT-SUMMARIES EXCEPTIONS), each a
list of plists in the original's order and names."
  (let ((groups (make-hash-table :test #'equal))
        (settled (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (exceptions '()))
    (dolist (item transactions)
      (let ((reference (transaction-reference item)))
        (when (plusp (length reference))
          (let ((key (list (transaction-provider item) reference)))
            (when (gethash key seen)
              (push (list :type "duplicate_reference"
                          :provider (transaction-provider item)
                          :reference reference)
                    exceptions))
            (setf (gethash key seen) t))))
      (let* ((key (list (transaction-provider item) (transaction-currency item)))
             (tally (or (gethash key groups) (setf (gethash key groups) (make-tx-tally)))))
        (incf (tx-tally-transaction-count tally))
        (cond ((member (transaction-status item) +successful+ :test #'string=)
               (incf (tx-tally-successful-count tally))
               (incf (tx-tally-transaction-amount-minor tally) (transaction-amount-minor item)))
              ((member (transaction-status item) +failed+ :test #'string=)
               (incf (tx-tally-failed-count tally))))))
    (dolist (item settlements)
      (let* ((key (list (settlement-provider item) (settlement-currency item)))
             (tally (or (gethash key settled) (setf (gethash key settled) (make-st-tally)))))
        (incf (st-tally-settlement-count tally))
        (if (member (settlement-status item) +settled+ :test #'string=)
            (incf (st-tally-settled-amount-minor tally) (settlement-amount-minor item))
            (incf (st-tally-non-final-count tally)))))
    (values (loop for (provider currency) in (sorted-groups groups)
                  for tally = (gethash (list provider currency) groups)
                  collect (list :provider provider :currency currency
                                :transaction-count (tx-tally-transaction-count tally)
                                :successful-count (tx-tally-successful-count tally)
                                :failed-count (tx-tally-failed-count tally)
                                :transaction-amount-minor (tx-tally-transaction-amount-minor tally)))
            (loop for (provider currency) in (sorted-groups settled)
                  for tally = (gethash (list provider currency) settled)
                  collect (list :provider provider :currency currency
                                :settlement-count (st-tally-settlement-count tally)
                                :settled-amount-minor (st-tally-settled-amount-minor tally)
                                :non-final-count (st-tally-non-final-count tally)))
            (nreverse exceptions))))

(defparameter +limitations+
  (vector "Transaction totals and settlement totals are reported separately; settlement timing can lag transaction activity."
          "Duplicate references are detected within one provider only."
          "This report does not establish bank-statement or merchant-order-ledger equivalence.")
  "The original's own statement of what the report does not claim.")

;;; --- the two answers -----------------------------------------------------------

(defun check-providers (providers)
  "This release reads one provider; anything else is a refusal in words."
  (let ((names (coerce (or providers '()) 'list)))
    (dolist (name names)
      (unless (string= "paystack" (string-downcase (nlk:trimmed (princ-to-string name))))
        (fail "this cell reads Paystack only; ~s is not a provider it knows" name)))
    (when (/= (length names) (length (remove-duplicates names :test #'string-equal)))
      (fail "providers must not repeat: ~{~a~^, ~}" names))))

(defun iso-date-p (text)
  "TEXT is a YYYY-MM-DD date."
  (and (stringp text)
       (= 10 (length text))
       (loop for index below 10
             for character = (char text index)
             always (if (member index '(4 7))
                        (char= character #\-)
                        (digit-char-p character)))
       (let ((month (parse-integer text :start 5 :end 7))
             (day (parse-integer text :start 8 :end 10)))
         (and (<= 1 month 12) (<= 1 day 31)))))

(defun checked-range (from to)
  "FROM and TO as ISO dates with FROM on or before TO, or a refusal."
  (let ((from (nlk:trimmed (or from "")))
        (to (nlk:trimmed (or to ""))))
    (unless (iso-date-p from) (fail "from_date must be a date as YYYY-MM-DD, not ~s" from))
    (unless (iso-date-p to) (fail "to_date must be a date as YYYY-MM-DD, not ~s" to))
    (when (string-lessp to from)
      (fail "to_date (~a) must be on or after from_date (~a)" to from))
    (values from to)))

(defun warnings-with-omitted (warnings shown total what)
  "WARNINGS, plus one line when an answer leaves rows out."
  (let ((omitted (- total shown)))
    (if (plusp omitted)
        (append warnings
                (list (format nil "listed ~d of ~d ~a (max_rows=~d); a narrower date range lists the rest"
                              shown total what (setting :max-rows))))
        warnings)))

(defun list-text (from to max-pages)
  "The transactions between FROM and TO, as the tool's JSON text."
  (multiple-value-bind (rows warnings) (fetch-rows "transaction" from to max-pages)
    (let* ((items (mapcar #'row-transaction rows))
           (limit (setting :max-rows))
           (shown (if (> (length items) limit) (subseq items 0 limit) items))
           (warnings (warnings-with-omitted warnings (length shown) (length items) "transactions")))
      (nlk:encode-json-object
       (nlk:json-object
        "transactions" (coerce (mapcar #'transaction-json shown) 'vector)
        "count" (length items)
        "provider_errors" (vector)
        "provider_warnings" (coerce warnings 'vector))))))

(defun reconcile-data (from to max-pages)
  "=> (values TRANSACTIONS SETTLEMENTS TX-SUMMARIES ST-SUMMARIES EXCEPTIONS WARNINGS)."
  (multiple-value-bind (transaction-rows transaction-warnings)
      (fetch-rows "transaction" from to max-pages)
    (multiple-value-bind (settlement-rows settlement-warnings)
        (fetch-rows "settlement" from to max-pages)
      (let ((transactions (mapcar #'row-transaction transaction-rows))
            (settlements (mapcar #'row-settlement settlement-rows)))
        (multiple-value-bind (transaction-summaries settlement-summaries exceptions)
            (tally transactions settlements)
          (values transactions settlements transaction-summaries settlement-summaries exceptions
                  (append transaction-warnings settlement-warnings)))))))

(defun summary-json (summary)
  "One transaction summary plist as the JSON object the report carries."
  (nlk:json-object "provider" (getf summary :provider)
                    "currency" (getf summary :currency)
                    "transaction_count" (getf summary :transaction-count)
                    "successful_count" (getf summary :successful-count)
                    "failed_count" (getf summary :failed-count)
                    "transaction_amount_minor" (getf summary :transaction-amount-minor)))

(defun settlement-json (summary)
  "One settlement summary plist as the JSON object the report carries."
  (nlk:json-object "provider" (getf summary :provider)
                    "currency" (getf summary :currency)
                    "settlement_count" (getf summary :settlement-count)
                    "settled_amount_minor" (getf summary :settled-amount-minor)
                    "non_final_count" (getf summary :non-final-count)))

(defun exception-json (exception)
  "One exception plist as the JSON object the report carries."
  (nlk:json-object "type" (getf exception :type)
                    "provider" (getf exception :provider)
                    "reference" (getf exception :reference)))

(defun reconcile-text (from to max-pages)
  "The reconciliation report between FROM and TO, as the tool's JSON text."
  (multiple-value-bind (transactions settlements transaction-summaries settlement-summaries
                        exceptions warnings)
      (reconcile-data from to max-pages)
    (declare (ignore transactions settlements))
    (nlk:encode-json-object
     (nlk:json-object
      "transaction_summaries" (coerce (mapcar #'summary-json transaction-summaries) 'vector)
      "settlement_summaries" (coerce (mapcar #'settlement-json settlement-summaries) 'vector)
      "exceptions" (coerce (mapcar #'exception-json exceptions) 'vector)
      "limitations" +limitations+
      "transaction_count" (loop for summary in transaction-summaries
                                 sum (getf summary :transaction-count))
      "settlement_count" (loop for summary in settlement-summaries
                                sum (getf summary :settlement-count))
      "provider_errors" (vector)
      "provider_warnings" (coerce warnings 'vector)))))

;;; --- refusals become the call's text ----------------------------------------------

(defmacro answering (&body body)
  "BODY's text; every refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (payops-error (condition)
       (nle:failure (format nil "ERROR: ~a" (payops-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Paystack: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

;;; --- the tools' arguments, one declaration each -------------------------------------

(nlk:define-contract transactions-query
  (:doc "List a Paystack account's transactions for an inclusive date range. Read-only: no charge, refund, transfer or payout is ever made. Amounts are in minor currency units, as Paystack returns them (kobo for NGN).")
  ("from_date" :text :required t
   :doc "inclusive start date, YYYY-MM-DD")
  ("to_date" :text :required t
   :doc "inclusive end date, YYYY-MM-DD")
  ("max_pages" :integer
   :doc "pages of 100 records to read, 1 to 20; left out, the payops section's max_pages (5)")
  ("providers" :array :of :string
   :doc "the payment providers to read; this cell supports paystack only"))

(nlk:define-contract reconcile-query
  (:doc "Reconcile a Paystack account's transactions against its settlements for an inclusive date range: per-currency summaries, duplicate references, and the stated limitations. Read-only, and deterministic: one range always answers the same report. Amounts stay in minor currency units.")
  ("from_date" :text :required t
   :doc "inclusive start date, YYYY-MM-DD")
  ("to_date" :text :required t
   :doc "inclusive end date, YYYY-MM-DD")
  ("max_pages" :integer
   :doc "pages of 100 records to read, 1 to 20; left out, the payops section's max_pages (5)")
  ("providers" :array :of :string
   :doc "the payment providers to read; this cell supports paystack only"))

;;; --- the verbs a person calls from eval, and the tools the model calls -------------

(defun transactions (from to &key max-pages)
  "A Paystack account's transactions between FROM and TO, as JSON text."
  (answering
    (multiple-value-bind (from to) (checked-range from to)
      (list-text from to (page-cap max-pages)))))

(defun reconcile (from to &key max-pages)
  "Paystack transactions and settlements between FROM and TO, reconciled, as JSON text."
  (answering
    (multiple-value-bind (from to) (checked-range from to)
      (reconcile-text from to (page-cap max-pages)))))

(defun run-list-transactions (query)
  "The payops_list_transactions tool body: QUERY is its decoded contract."
  (answering
    (progn
      (check-providers (transactions-query-providers query))
      (multiple-value-bind (from to) (checked-range (transactions-query-from-date query)
                                                     (transactions-query-to-date query))
        (list-text from to (page-cap (transactions-query-max-pages query)))))))

(defun run-reconcile (query)
  "The payops_reconcile tool body: QUERY is its decoded contract."
  (answering
    (progn
      (check-providers (reconcile-query-providers query))
      (multiple-value-bind (from to) (checked-range (reconcile-query-from-date query)
                                                     (reconcile-query-to-date query))
        (reconcile-text from to (page-cap (reconcile-query-max-pages query)))))))

;;; --- the /payops command ----------------------------------------------------------

(defun iso-day (universal)
  "UNIVERSAL time as a local YYYY-MM-DD date."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun parse-range (args)
  "FROM and TO out of ARGS: \"2026-10-01..2026-10-31\", \"2026-10-01 2026-10-31\",
or, with no argument, the last seven days."
  (let ((text (nlk:trimmed (or args ""))))
    (cond
      ((zerop (length text))
       (values (iso-day (- (get-universal-time) (* 6 86400)))
               (iso-day (get-universal-time))))
      ((search ".." text)
       (let ((split (search ".." text)))
         (values (nlk:trimmed (subseq text 0 split))
                 (nlk:trimmed (subseq text (+ split 2))))))
      (t
       (let ((space (position #\Space text)))
         (if space
             (values (nlk:trimmed (subseq text 0 space))
                     (nlk:trimmed (subseq text (1+ space))))
             (fail "give one date range, as /payops 2026-10-01..2026-10-31, or no argument for the last seven days; ~s is not one" text)))))))

(defun transaction-phrase (summaries)
  "The transaction side of the command's line, one group per currency."
  (when summaries
    (format nil "~{~a~^; ~}"
            (loop for summary in summaries
                  collect (format nil "~a ~d of ~d successful~@[, ~a successful total~]"
                                  (getf summary :currency)
                                  (getf summary :successful-count)
                                  (getf summary :transaction-count)
                                  (let ((amount (getf summary :transaction-amount-minor)))
                                    (and (plusp amount)
                                         (format-amount amount (getf summary :currency)))))))))

(defun settlement-phrase (summaries)
  "The settlement side of the command's line, one group per currency."
  (when summaries
    (format nil "~{~a~^; ~}"
            (loop for summary in summaries
                  collect (format nil "~a ~d settlement~:p~@[ ~a settled~]~@[, ~d not final~]"
                                  (getf summary :currency)
                                  (getf summary :settlement-count)
                                  (let ((amount (getf summary :settled-amount-minor)))
                                    (and (plusp amount)
                                         (format-amount amount (getf summary :currency))))
                                  (let ((pending (getf summary :non-final-count)))
                                    (and (plusp pending) pending)))))))

(defun summary-text (from to)
  "One line for the shell: what the range holds, and every exception found."
  (multiple-value-bind (transactions settlements transaction-summaries settlement-summaries
                        exceptions warnings)
      (reconcile-data from to (setting :max-pages))
    (declare (ignore settlements))
    (format nil "payops ~a..~a: ~d transaction~:p~@[ (~a)~]~@[; settlement~:p: ~a~]~@[; ~d exception~:p~]~@[~%note~:p: ~{~a~^; ~}~]"
            from to (length transactions)
            (transaction-phrase transaction-summaries)
            (settlement-phrase settlement-summaries)
            (let ((count (length exceptions)))
              (and (plusp count) count))
            (and warnings (mapcar (lambda (warning) (nlk:one-line warning :cap 120)) warnings)))))

(defun run-command (args session-id)
  "The /payops command: a range's reconciliation in one line, or a refusal."
  (declare (ignore session-id))
  (handler-case
      (multiple-value-bind (from to) (parse-range args)
        (multiple-value-bind (from to) (checked-range from to)
          (summary-text from to)))
    (payops-error (condition)
      (nle:failure (format nil "payops: ~a" (payops-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "payops: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- the section, and the one declaration ---------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the key: the environment variable secret_key_env
names first, a literal secret_key second. No key at all is fine at start — a
call refuses in words — and the primer is how the operator finds out."
  (declare (ignore table))
  (let* ((env (nlk:trimmed (or (getf values :secret-key-env) "")))
         (literal (nlk:trimmed (or (getf values :secret-key) "")))
         (from-env (and (plusp (length env))
                        (let ((value (nle::credential-env env)))
                          (and value
                               (plusp (length (nlk:trimmed value)))
                               (nlk:trimmed value))))))
    (list* :secret-key (or from-env (and (plusp (length literal)) literal))
           :secret-key-env env
           values)))

(nle:define-cell payops
  (:section ("payops")
    (:guide "paste a Paystack secret key into the environment variable secret_key_env names (PAYSTACK_SECRET_KEY), or write it in secret_key; next step: ask for a month of transactions, or run /payops")
    ("secret_key_env" :env :default "PAYSTACK_SECRET_KEY"
     :doc "the environment variable holding the Paystack secret key (dashboard, Settings, API Keys and Webhooks)")
    ("secret_key" :secret
     :doc "a key written here instead of named in secret_key_env; the environment variable wins")
    ("base_url" :string :default "https://api.paystack.co"
     :doc "the API base; https only")
    ("max_pages" :integer :default 5 :min 1
     :doc "pages of 100 records a call reads when it asks for none; 20 is the ceiling")
    ("max_rows" :integer :default 100 :min 1
     :doc "how many transactions one answer lists; the rest is counted and said")
    ("timeout" :integer :default 20 :min 1
     :doc "how long one Paystack request may take, in seconds"))
  (:settings #'read-settings)
  (:help :payops
         "payops: read-only Paystack transactions and reconciliation reports"
         *manual*)
  (:command "payops" 'run-command
            :description "Paystack transactions and settlements for a date range"
            :argument-hint "2026-10-01..2026-10-31")
  (:tool "payops_list_transactions" (nlk:contract 'transactions-query) #'run-list-transactions)
  (:tool "payops_reconcile" (nlk:contract 'reconcile-query) #'run-reconcile)
  (:start (lambda ()
            (unless (setting :secret-key)
              (nle:notice (format nil "payops: no Paystack secret key yet — set ~a (or secret_key in the payops section) and (restart-cells \"nodecode-payops\")"
                                  (let ((env (nlk:trimmed (or (setting :secret-key-env) ""))))
                                    (if (plusp (length env)) env "PAYSTACK_SECRET_KEY")))
                          :level :warning :key "nodecode-payops")
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-payops"))))))
)
