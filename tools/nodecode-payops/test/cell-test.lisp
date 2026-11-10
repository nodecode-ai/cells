;;;; cell-test.lisp --- the payops cell against a stubbed Paystack.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Paystack endpoint is a stubbed NLK:HTTP and the key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Paystack, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it --
;;;; through the real dispatch, contract and all -- so the schema the model is
;;;; shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "payops" "PAYOPS-CELL-" :start nodecode-payops:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-payops-key (&body body)
  "BODY with PAYSTACK_SECRET_KEY set to a fixed test key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "PAYSTACK_SECRET_KEY") "sk_test_payops"))
     ,@body))

(defmacro with-payops-keyless (&body body)
  "BODY with no key anywhere, as a fresh install before setup."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              nil)
     ,@body))

(defmacro with-paystack-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with nothing bound and answers (values TEXT STATUS), as
NLK:HTTP does. ASKED collects (METHOD URL HEADERS), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :headers)) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Paystack request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun payops-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun payops-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (payops-answer id tool arguments)))

(defun payops-request-header (asked name)
  "The NAME header of the first request in ASKED."
  (cdr (assoc name (third (first asked)) :test #'string=)))

(defparameter +page-one+
  "{\"status\":true,\"message\":\"Transactions retrieved\",\"data\":[{\"id\":101,\"reference\":\"ref_a\",\"amount\":150000,\"currency\":\"NGN\",\"status\":\"success\",\"paid_at\":\"2026-10-02T10:00:00.000Z\"},{\"id\":102,\"reference\":\"ref_b\",\"amount\":50000,\"currency\":\"ngn\",\"status\":\"failed\",\"paid_at\":null,\"transaction_date\":\"2026-10-03T09:00:00.000Z\"}],\"meta\":{\"total\":4,\"pageCount\":2,\"page\":1}}"
  "Page one of a range: a success, a failure, and a lowercased currency.")

(defparameter +page-two+
  "{\"status\":true,\"message\":\"Transactions retrieved\",\"data\":[{\"id\":103,\"reference\":\"ref_a\",\"amount\":120000,\"currency\":\"NGN\",\"status\":\"success\",\"paid_at\":\"2026-10-04T11:00:00.000Z\"},{\"id\":104,\"reference\":\"ref_c\",\"amount\":30000,\"currency\":\"NGN\",\"status\":\"abandoned\",\"paid_at\":null}],\"meta\":{\"total\":4,\"pageCount\":2,\"page\":2}}"
  "Page two: ref_a again, which is the duplicate the report finds.")

(defparameter +settlements+
  "{\"status\":true,\"message\":\"Settlements retrieved\",\"data\":[{\"id\":9001,\"total_amount\":180000,\"currency\":\"NGN\",\"status\":\"success\",\"settlement_date\":\"2026-10-05T00:00:00.000Z\"},{\"id\":9002,\"amount\":20000,\"currency\":\"NGN\",\"status\":\"pending\",\"createdAt\":\"2026-10-06T00:00:00.000Z\"}],\"meta\":{\"pageCount\":1}}"
  "One settled payout and one not final.")

(defparameter +routes+
  '(("page=2" (values +page-two+ 200))
    ("/settlement" (values +settlements+ 200))
    ("/transaction" (values +page-one+ 200)))
  "The whole stubbed range, the pages told apart by their query.")

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "payops"
  (:config "secret_key_env" "PAYSTACK_SECRET_KEY")
  (:help :payops)
  (:command "payops")
  (:refused ("max_pages" 0) ("timeout" 0))
  (:fixture with-payops-key))

;;; --- keep list ----------------------------------------------------------------------

(deftest payops-cell-lists-transactions-across-pages ()
  (with-payops-key
    (with-cell-stop ((payops-start))
      (with-paystack-wire (asked ("page=2" (values +page-two+ 200))
                                 ("/transaction" (values +page-one+ 200)))
        (let* ((object (payops-object "p1" "payops_list_transactions"
                                       "{\"from_date\":\"2026-10-01\",\"to_date\":\"2026-10-31\"}"))
               (rows (nlk:json-array object "transactions"))
               (second-row (aref rows 1)))
          (is (= 4 (nlk:json-value object :integer "count")))
          (is (= 4 (length rows)))
          (is (= 2 (length asked)) "two pages, because the meta said pageCount 2")
          (is (eq :get (first (first asked))) "read-only: a GET")
          (is (search "api.paystack.co/transaction" (second (first asked))))
          (is (search "perPage=100" (second (first asked))))
          (let ((authorization (payops-request-header asked "authorization")))
            (is (equal "Bearer sk_test_payops" authorization) "one header, as the original"))
          (is (equal "NGN" (nlk:json-value (aref rows 0) :string "currency")) "currency upcased")
          (is (equal "failed" (nlk:json-value second-row :string "status")))
          (is (= 50000 (nlk:json-value second-row :integer "amount_minor")) "minor units, unchanged")
          (let ((paid-at (nlk:json-value second-row :string "paid_at")))
            (is (equal "2026-10-03T09:00:00.000Z" paid-at) "else transaction_date is read"))
          (is (zerop (length (nlk:json-array object "provider_warnings"))) "no warning: it read the range"))))))

(deftest payops-cell-says-when-the-page-cap-stopped-it ()
  (with-payops-key
    (with-cell-stop ((payops-start))
      (with-paystack-wire (asked ("/transaction" (values +page-one+ 200)))
        (let* ((object (payops-object "p2" "payops_list_transactions"
                                       "{\"from_date\":\"2026-10-01\",\"to_date\":\"2026-10-31\",\"max_pages\":1}"))
               (warnings (nlk:json-array object "provider_warnings")))
          (is (= 1 (length asked)) "one page, as asked")
          (is (= 1 (length warnings)))
          (is (search "read 1 of 2 pages" (aref warnings 0)))
          (is (= 2 (nlk:json-value object :integer "count")) "what it did read is still answered"))))))

(deftest payops-cell-reconciles-and-finds-a-duplicate-reference ()
  (with-payops-key
    (with-cell-stop ((payops-start))
      (with-paystack-wire (asked ("page=2" (values +page-two+ 200))
                                 ("/settlement" (values +settlements+ 200))
                                 ("/transaction" (values +page-one+ 200)))
        (let* ((report (payops-object "p3" "payops_reconcile"
                                       "{\"from_date\":\"2026-10-01\",\"to_date\":\"2026-10-31\"}"))
               (transactions (nlk:json-array report "transaction_summaries"))
               (settlements (nlk:json-array report "settlement_summaries"))
               (exceptions (nlk:json-array report "exceptions")))
          (is (= 4 (nlk:json-value report :integer "transaction_count")))
          (is (= 1 (length transactions)) "one currency, one summary")
          (let ((summary (aref transactions 0))
                (payouts (aref settlements 0)))
            (is (= 4 (nlk:json-value summary :integer "transaction_count")))
            (is (= 2 (nlk:json-value summary :integer "successful_count")))
            (is (= 1 (nlk:json-value summary :integer "failed_count")) "abandoned counts as neither")
            (is (= 270000 (nlk:json-value summary :integer "transaction_amount_minor")))
            (is (= 2 (nlk:json-value payouts :integer "settlement_count")))
            (is (= 180000 (nlk:json-value payouts :integer "settled_amount_minor")))
            (is (= 1 (nlk:json-value payouts :integer "non_final_count"))))
          (is (= 1 (length exceptions)))
          (is (equal "duplicate_reference" (nlk:json-value (aref exceptions 0) :string "type")))
          (is (equal "ref_a" (nlk:json-value (aref exceptions 0) :string "reference")))
          (is (= 3 (length (nlk:json-array report "limitations"))) "its limits travel too")
          (is (= 3 (length asked)) "two pages of transactions and one of settlements"))))))

(deftest payops-cell-refuses-without-a-key ()
  (with-payops-keyless
    (with-cell-stop ((payops-start))
      (with-paystack-wire (asked)
        (is-carrying (text (payops-answer "p4" "payops_list_transactions"
                                           "{\"from_date\":\"2026-10-01\",\"to_date\":\"2026-10-31\"}"))
          ("no Paystack secret key" "it says what is missing")
          ("PAYSTACK_SECRET_KEY" "and names the environment variable to set")
          ("restart-cells" "and how to pick it up"))
        (is (null asked) "nothing was dialled")))))

(deftest payops-cell-refuses-a-key-paystack-will-not-take ()
  (with-payops-key
    (with-cell-stop ((payops-start))
      (with-paystack-wire (asked ("/transaction"
                                   (values "{\"status\":false,\"message\":\"Invalid key\"}" 401)))
        (is-carrying (text (payops-answer "p5" "payops_list_transactions"
                                           "{\"from_date\":\"2026-10-01\",\"to_date\":\"2026-10-31\"}"))
          ("ERROR" "an HTTP refusal is the call's text")
          ("HTTP 401")
          ("Invalid key" "with Paystack's own words")
          ("the key is not accepted"))))))

(deftest payops-cell-refuses-a-range-it-cannot-read ()
  (with-payops-key
    (with-cell-stop ((payops-start))
      (with-paystack-wire (asked)
        (is-carrying (text (payops-answer "p6" "payops_list_transactions"
                                           "{\"from_date\":\"2026-11-01\",\"to_date\":\"2026-10-01\"}"))
          ("to_date" "backwards dates say which one is wrong"))
        (is-carrying (text (payops-answer "p7" "payops_list_transactions"
                                           "{\"from_date\":\"2026-10-01\",\"to_date\":\"2026-10-31\",\"providers\":[\"flutterwave\"]}"))
          ("Paystack only" "the one provider this release reads"))
        (is (null asked) "neither dialled")))))

;;; --- the shell ------------------------------------------------------------------

(deftest payops-cell-command-answers-one-range-in-a-line ()
  (with-payops-key
    (with-cell-stop ((payops-start))
      (with-paystack-wire (asked ("page=2" (values +page-two+ 200))
                                 ("/settlement" (values +settlements+ 200))
                                 ("/transaction" (values +page-one+ 200)))
        (is-carrying (text (cell-entry "nodecode-payops" "payops" "2026-10-01..2026-10-31"))
          ("payops 2026-10-01..2026-10-31:" "the range it read")
          ("4 transactions" "the count")
          ("NGN 2,700.00" "the successful total, formatted for a person")
          ("2 of 4 successful")
          ("1 exception" "the duplicate is named in the count"))))))

(deftest payops-cell-command-answers-a-refusal-in-a-line ()
  (with-payops-key
    (with-cell-stop ((payops-start))
      (is-carrying (text (cell-entry "nodecode-payops" "payops" "yesterday"))
        ("payops:" "the command never signals at the operator")
        ("one date range" "it says what a range looks like")))))
