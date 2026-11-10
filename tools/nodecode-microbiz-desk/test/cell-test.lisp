;;;; cell-test.lisp --- the lifecycle, the five tools, /microbiz-desk, and the desk file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops, each tool answers through the
;;;; seam the model calls it by, the command records the same sale, a customer
;;;; is created by the first sale and found by a partial name afterwards, an
;;;; ambiguous name and an overpayment are refused, the money sums exactly,
;;;; the report counts only its period, and the desk outlives a restart
;;;; because the file in the folder holds it. Every call reads a desk in the
;;;; test's own folder, so nothing here reads the operator's business.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defun microbiz-desk-folder ()
  "The folder one test's desk is kept in, inside the test's own home."
  (namestring (nlk:home "microbiz-desk-test/")))

(defun microbiz-desk-file ()
  (merge-pathnames "desk.json" (nlk:home "microbiz-desk-test/")))

(defun microbiz-desk-empty ()
  "One test's desk: the file removed, so the test starts from none."
  (when (probe-file (microbiz-desk-file)) (delete-file (microbiz-desk-file))))

(defun microbiz-desk-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun microbiz-desk-call (name &rest arguments)
  "NAME answered through the tool seam with one JSON object of ARGUMENTS,
decoded as the model reads it."
  (let ((answer (microbiz-desk-tool name (apply #'nlk:make-json-object arguments))))
    (if (hash-table-p answer) answer (ignore-errors (nlk:decode-json answer)))))

(defun microbiz-desk-text (name &rest arguments)
  "NAME's answer as its text, for a refusal that is not JSON at all."
  (microbiz-desk-tool name (apply #'nlk:make-json-object arguments)))

(defun microbiz-desk-number (answer key)
  (nlk:json-value answer :number key))

(defun microbiz-desk-command (args)
  "The text /microbiz-desk answers ARGS with, the slash command's first value."
  (nth-value 0 (cell-entry "nodecode-microbiz-desk" "microbiz-desk" args)))

(defmacro with-microbiz-desk-cell (&body body)
  "BODY with one test's empty desk started in the test's own folder."
  `(progn
     (microbiz-desk-empty)
     (with-cell-stop ((microbiz-desk-start "data_dir" (microbiz-desk-folder)))
       ,@body)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "microbiz-desk" "MICROBIZ-DESK-CELL-" :start nodecode-microbiz-desk:start-cell)

(define-cell-lifecycle-tests "microbiz-desk"
  (:config "data_dir" (microbiz-desk-folder))
  (:help :microbiz-desk)
  (:command "microbiz-desk")
  (:running (is (nle::find-tool "microbiz_add_sale") "microbiz_add_sale is registered")
            (is (nle::find-tool "microbiz_record_payment"))
            (is (nle::find-tool "microbiz_who_owes_me"))
            (is (nle::find-tool "microbiz_customer_statement"))
            (is (nle::find-tool "microbiz_report"))
            (is (pathnamep (nodecode-microbiz-desk::desk-path)) "the desk file is resolved")
            (is (equal "NGN" (nodecode-microbiz-desk::currency)) "the currency is a label with a default"))
  (:stopped (is (null (nle::find-tool "microbiz_add_sale")) "stop unregisters the tools"))
  (:refused ("currency" "NGN"))
  (:idle nodecode-microbiz-desk:microbiz-desk-error
         (nodecode-microbiz-desk::run-command "owing" nil)))

(deftest microbiz-desk-cell-a-missing-data-dir-refuses-by-name ()
  (is-carrying (text (refusal-text nlk:config-refusal (microbiz-desk-start "currency" "NGN")))
               "data_dir"
               "the folder the desk's file is kept in")
  (is (null (nle::find-tool "microbiz_add_sale")) "a refused start installed nothing"))

;;; --- a sale, a debt, a payment -------------------------------------------------

(deftest microbiz-desk-cell-a-sale-creates-the-customer-and-a-debt ()
  (with-microbiz-desk-cell
    (let ((answer (microbiz-desk-call "microbiz_add_sale" "customer" "Mama Ngozi"
                                      "item" "2 bags of rice" "amount" 45000
                                      "amount_paid" 20000)))
      (is (equal "Mama Ngozi" (nlk:json-value answer :string "customer")) "the customer is named")
      (is (= 45000 (microbiz-desk-number answer "sale_amount")) "the sale is the whole price")
      (is (= 20000 (microbiz-desk-number answer "paid_now")) "what was paid now is recorded")
      (is (= 25000 (microbiz-desk-number answer "customer_now_owes")) "and the rest is a debt")
      (is-carrying (summary (nlk:json-value answer :string "summary"))
                   "Recorded NGN 45,000 sale of 2 bags of rice to Mama Ngozi"
                   "Mama Ngozi now owes NGN 25,000"))
    (let* ((who (microbiz-desk-call "microbiz_who_owes_me"))
           (debtors (nlk:json-value who :array "debtors")))
      (is (= 1 (nlk:json-value who :integer "count")) "one customer owes")
      (is (= 25000 (microbiz-desk-number who "total_owed")) "the debt is the total owed")
      (is (= 1 (length debtors)))
      (is (equal "Mama Ngozi" (nlk:json-value (aref debtors 0) :string "customer")))
      (is (= 25000 (microbiz-desk-number (aref debtors 0) "owes")))
      (is (integerp (nlk:json-value (aref debtors 0) :integer "days_since_last_sale")) "the days since the last sale are a whole number"))
    (is (probe-file (microbiz-desk-file)) "the desk is a file in the folder the section names")))

(deftest microbiz-desk-cell-a-payment-clears-the-debt-and-an-overpayment-is-refused ()
  (with-microbiz-desk-cell
    (microbiz-desk-call "microbiz_add_sale" "customer" "Mama Ngozi" "item" "2 bags of rice"
                         "amount" 45000 "amount_paid" 20000)
    (let ((answer (microbiz-desk-call "microbiz_record_payment" "customer" "mama ngozi"
                                      "amount" "10000" "note" "bank transfer")))
      (is (= 10000 (microbiz-desk-number answer "paid")) "a string amount reads as money")
      (is (= 15000 (microbiz-desk-number answer "balance_left")) "the balance falls by what was paid")
      (is-carrying (summary (nlk:json-value answer :string "summary"))
                   "Recorded NGN 10,000 from Mama Ngozi" "Still owes NGN 15,000"))
    (is-carrying (too-much (microbiz-desk-text "microbiz_record_payment" "customer" "Mama Ngozi"
                                               "amount" 99999))
                 "ERROR: microbiz-desk:" "only owes NGN 15,000" "is too much")
    (is (= 15000 (microbiz-desk-number (microbiz-desk-call "microbiz_who_owes_me") "total_owed")) "the refused payment wrote nothing")))

(deftest microbiz-desk-cell-an-ambiguous-name-is-refused ()
  (with-microbiz-desk-cell
    (microbiz-desk-call "microbiz_add_sale" "customer" "Mama Ngozi" "item" "rice" "amount" 45000)
    (microbiz-desk-call "microbiz_add_sale" "customer" "Mama Ada" "item" "oil" "amount" 5000)
    (is-carrying (refused (microbiz-desk-text "microbiz_record_payment" "customer" "Mama"
                                               "amount" 1000))
                 "ERROR: microbiz-desk:" "matches more than one customer"
                 "Mama Ada, Mama Ngozi" "Use the full name")
    (is (= 50000 (microbiz-desk-number (microbiz-desk-call "microbiz_who_owes_me") "total_owed")) "neither debt changed")))

(deftest microbiz-desk-cell-who-owes-lists-the-biggest-first ()
  (with-microbiz-desk-cell
    (microbiz-desk-call "microbiz_add_sale" "customer" "Ada" "item" "yarn" "amount" 5000)
    (microbiz-desk-call "microbiz_add_sale" "customer" "Bola" "item" "dry fish" "amount" 12000)
    (let* ((who (microbiz-desk-call "microbiz_who_owes_me"))
           (debtors (nlk:json-value who :array "debtors")))
      (is (= 2 (length debtors)))
      (is (equal "Bola" (nlk:json-value (aref debtors 0) :string "customer")) "the biggest debt is first")
      (is (equal "Ada" (nlk:json-value (aref debtors 1) :string "customer")))
      (is-carrying (summary (nlk:json-value who :string "summary"))
                   "2 customer(s) owe you NGN 17,000 in total"
                   "1. Bola: NGN 12,000"
                   "2. Ada: NGN 5,000"))))

(deftest microbiz-desk-cell-a-statement-has-every-row ()
  (with-microbiz-desk-cell
    (microbiz-desk-call "microbiz_add_sale" "customer" "Mama Ngozi" "item" "2 bags of rice"
                         "amount" 45000 "amount_paid" 20000)
    (microbiz-desk-call "microbiz_add_sale" "customer" "Mama Ngozi" "item" "salt" "amount" 2500)
    (let ((answer (microbiz-desk-call "microbiz_customer_statement" "customer" "Mama Ngozi")))
      (is (= 2 (length (nlk:json-value answer :array "sales"))) "both sales are on the statement")
      (is (= 1 (length (nlk:json-value answer :array "payments"))) "and the payment made at the sale")
      (is (= 27500 (microbiz-desk-number answer "balance_owed")) "the balance is every sale less every payment")
      (is-carrying (summary (nlk:json-value answer :string "summary"))
                   "Statement for Mama Ngozi:"
                   "  SALE  " "2 bags of rice  NGN 45,000"
                   "  PAID  " "paid at sale"
                   "Balance owed: NGN 27,500"))
    (is-carrying (text (microbiz-desk-text "microbiz_customer_statement" "customer" "Nobody"))
                 "ERROR: microbiz-desk:" "No customer called 'Nobody'")))

(deftest microbiz-desk-cell-amounts-never-drift ()
  (with-microbiz-desk-cell
    (dotimes (n 3)
      (microbiz-desk-call "microbiz_add_sale" "customer" "Ada" "item" "sachet water"
                          "amount" 0.1))
    (let ((who (microbiz-desk-call "microbiz_who_owes_me")))
      (is-carrying (summary (nlk:json-value who :string "summary"))
                   "1 customer(s) owe you NGN 0.30 in total"
                   "1. Ada: NGN 0.30")
      (is (< (abs (- (microbiz-desk-number who "total_owed") 0.3d0)) 1d-6) "three tenths of the currency is owed, to the precision a float carries"))
    (let ((answer (microbiz-desk-call "microbiz_add_sale" "customer" "Bola" "item" "yarn"
                                      "amount" "2,500.5")))
      (is-carrying (sale (nlk:json-value answer :string "summary"))
                   ("Recorded NGN 2,500.50 sale of yarn to Bola"
                    "a typed amount with a comma and a half reads exactly"))
      (let* ((desk (nlk:decode-json (nlk:read-text (microbiz-desk-file))))
             (sales (nlk:json-value desk :array "sales"))
             (last (aref sales (1- (length sales)))))
        (is (= 250050 (nlk:json-value last :integer "amount")) "the desk keeps it as whole minor units, where nothing drifts")))))

(deftest microbiz-desk-cell-the-weekly-report-counts-its-period ()
  (with-microbiz-desk-cell
    (microbiz-desk-call "microbiz_add_sale" "customer" "Ada" "item" "yarn" "amount" 5000
                         "amount_paid" 2000)
    (microbiz-desk-call "microbiz_add_sale" "customer" "Bola" "item" "dry fish" "amount" 12000)
    (let ((answer (microbiz-desk-call "microbiz_report" "days" 7)))
      (is (= 7 (nlk:json-value answer :integer "days")))
      (is (= 2 (nlk:json-value answer :integer "sales_count")) "both sales are in the week")
      (is (= 17000 (microbiz-desk-number answer "revenue")) "the revenue is the whole of both")
      (is (= 2000 (microbiz-desk-number answer "collected")) "the cash collected is what came in")
      (is (= 15000 (microbiz-desk-number answer "total_owed_all_time")) "and the rest is still owed")
      (is-carrying (summary (nlk:json-value answer :string "summary"))
                   "Business report: last 7 day(s)"
                   "Sales made: 2 totalling NGN 17,000"
                   "Cash collected: NGN 2,000"
                   "Still owed to you (all time): NGN 15,000 by 2 customer(s)"
                   "Best customers: Bola" "Top sellers: "))
    (is (equal 7 (nlk:json-value (microbiz-desk-call "microbiz_report") :integer "days")) "a report with no period is the week")
    (is-carrying (bad-days (microbiz-desk-text "microbiz_report" "days" 0))
                 "ERROR: microbiz-desk:" "days must be a whole number between 1 and 3650")))

;;; --- the command ---------------------------------------------------------------

(deftest microbiz-desk-cell-the-command-records-a-sale ()
  (with-microbiz-desk-cell
    (is-carrying (sale (microbiz-desk-command "sale \"Mama Ngozi\" \"2 bags rice\" 45000 --paid 20000"))
                 "Recorded NGN 45,000 sale of 2 bags rice to Mama Ngozi"
                 "Paid now: NGN 20,000" "Mama Ngozi now owes NGN 25,000")
    (is-carrying (paid (microbiz-desk-command "pay \"Mama Ngozi\" 10000 --note transfer"))
                 "Recorded NGN 10,000 from Mama Ngozi. Still owes NGN 15,000")
    (is-carrying (owing (microbiz-desk-command "owing"))
                 "1 customer(s) owe you NGN 15,000 in total")
    (is-carrying (statement (microbiz-desk-command "statement \"Mama Ngozi\""))
                 "Statement for Mama Ngozi:" "Balance owed: NGN 15,000")
    (is-carrying (report (microbiz-desk-command "report --days 7"))
                 "Business report: last 7 day(s)" "Sales made: 1 totalling NGN 45,000")
    (is-carrying (help (microbiz-desk-command "help")) "MicroBiz Desk commands:" "--paid AMOUNT")
    (is-carrying (usage (microbiz-desk-command "sale \"Mama Ngozi\"")) "Usage: /microbiz-desk sale")
    (is-carrying (marks (microbiz-desk-command "pay \"Mama 1000"))
                 "Problem: check your quotation marks.")
    (is (search "Problem:" (microbiz-desk-command "pay \"Mama Ngozi\" 99999")) "the command refuses an overpayment in words, not a condition")))

(deftest microbiz-desk-cell-the-desk-outlives-a-restart ()
  (microbiz-desk-empty)
  (with-cell-stop ((microbiz-desk-start "data_dir" (microbiz-desk-folder)))
    (microbiz-desk-call "microbiz_add_sale" "customer" "Ada" "item" "yarn" "amount" 5000))
  (with-cell-stop ((microbiz-desk-start "data_dir" (microbiz-desk-folder)))
    (is (= 5000 (microbiz-desk-number (microbiz-desk-call "microbiz_who_owes_me") "total_owed")) "the file in the folder holds the desk between starts")))
