;;;; cell-test.lisp --- the lifecycle, the import, the match, the ladder, the lock, the outbox.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test runs at the fixed report date, so "40 days overdue" is a fact
;;;; of the fixtures and not of the day the suite runs.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "cash-desk"
  (:help :cash-desk)
  (:command "cashdesk")
  (:running (is (equal nodecode-cash-desk::+primer+ (nle:help :cash-desk)) "the manual is the primer")
            (is (= 10 (count-if (lambda (entry) (and (stringp (first entry))
                                                          (search "cashdesk_" (first entry))))
                                 nle::*tools*)) "ten tools, the original plugin's"))
  (:refused ("date_order" "ymd") ("default_language" "de")))

;;; --- importing -------------------------------------------------------------------

(deftest cash-desk-cell-an-export-comes-in-once ()
  (with-cashdesk ()
    (seeded-ledger)
    (is (= 2 (length (getf (ledger) :customers))) "two customers imported")
    (is (= 2 (length (getf (ledger) :invoices))) "two invoices imported")
    (is (= 100000 (nodecode-cash-desk::getf (an-invoice "INV-1") :amount)) "the amount is stored in cents")
    (let ((path (ledger-file "invoices.csv")))
      (is (search "already imported"
                  (refusal-text nodecode-cash-desk:cash-desk-error
                                (nodecode-cash-desk:import-file "invoices" path))) "the same file twice is refused"))))

(deftest cash-desk-cell-a-refused-file-writes-nothing ()
  (with-cashdesk ()
    (let ((path (write-fixture "broken.csv" "invoice_no,issue_date,amount
INV-9,2026-09-01,100.00
")))
      (is (search "column is missing"
                  (refusal-text nodecode-cash-desk:cash-desk-error
                                (nodecode-cash-desk:import-file "invoices" path))) "a missing customer column refuses the file")
      (is (null (getf (ledger) :invoices)) "and nothing was imported"))))

;;; --- matching --------------------------------------------------------------------

(deftest cash-desk-cell-a-referenced-payment-closes-an-invoice ()
  (with-cashdesk ()
    (seeded-ledger)
    (write-fixture "bank.csv" "id,date,amount,currency,counterparty,reference,memo
B1,2026-10-05,1000.00,EUR,NORTHGATE STUDIO LTD,INV-1,invoice payment
B2,2026-10-06,700.00,EUR,BIRCHFIELD CONSULTING GMBH,project fee,
")
    (nodecode-cash-desk:import-file "bank" (ledger-file "bank.csv"))
    (let ((text (nodecode-cash-desk:match-payments :as-of +report-date+)))
      (is (search "1 applied" text) "the exact, referenced payment is applied"))
    (is (string= "paid" (nodecode-cash-desk::getf (an-invoice "INV-1") :status)) "the invoice is paid")
    (is (= 0 (nodecode-cash-desk::getf (an-invoice "INV-1") :balance)) "with no balance left")
    (is (= 1 (count-reviews)) "the stray payment is in review")
    (is (string= "UNMATCHED" (nodecode-cash-desk::getf (first-review) :kind)) "and says why")))

(deftest cash-desk-cell-a-partial-payment-holds-the-invoice ()
  (with-cashdesk ()
    (seeded-ledger)
    (write-fixture "bank.csv" "id,date,amount,currency,counterparty,reference
B1,2026-10-05,400.00,EUR,NORTHGATE STUDIO LTD,INV-1
")
    (nodecode-cash-desk:import-file "bank" (ledger-file "bank.csv"))
    (nodecode-cash-desk:match-payments :as-of +report-date+)
    (is (string= "PARTIAL" (nodecode-cash-desk::getf (first-review) :kind)) "a short payment is a review, never applied")
    (is (equal '("INV-1") (nodecode-cash-desk::getf (first-review) :holds)) "and it holds the invoice it may pay")
    (is (string= "open" (nodecode-cash-desk::getf (an-invoice "INV-1") :status)) "the invoice is still open")))

(deftest cash-desk-cell-a-currency-mismatch-is-not-applied ()
  (with-cashdesk ()
    (seeded-ledger)
    (write-fixture "bank.csv" "id,date,amount,currency,counterparty,reference
B1,2026-10-05,1000.00,USD,NORTHGATE STUDIO LTD,INV-1
")
    (nodecode-cash-desk:import-file "bank" (ledger-file "bank.csv"))
    (nodecode-cash-desk:match-payments :as-of +report-date+)
    (is (string= "CURRENCY" (nodecode-cash-desk::getf (first-review) :kind)) "another currency is a review")
    (is (= 100000 (nodecode-cash-desk::getf (an-invoice "INV-1") :balance)) "and the balance is untouched")))

;;; --- the ladder ------------------------------------------------------------------

(deftest cash-desk-cell-the-ladder-drafts-one-letter-per-customer ()
  (with-cashdesk ()
    (seeded-ledger)
    (let ((text (nodecode-cash-desk:draft-reminders :as-of +report-date+)))
      (is (search "drafted 2" text) "one draft for each customer"))
    (let ((draft (a-draft)))
      (is (string= "stage28" (nodecode-cash-desk::getf draft :kind)) "INV-1 at 40 days reaches the 28-day letter")
      (is (search "40 days overdue" (nodecode-cash-desk::getf draft :subject)) "the subject names the real age")
      (is (search "INV-1" (nodecode-cash-desk::getf draft :body)) "the invoice is in the letter")
      (is (search "DE89 3704 0044 0532 0130 00" (nodecode-cash-desk::getf draft :body)) "with the ledger's own IBAN")
      (is (plusp (length (nodecode-cash-desk::getf draft :hash))) "and a hash a person approves"))))

(deftest cash-desk-cell-a-small-balance-is-not-chased ()
  (with-cashdesk ()
    (seeded-ledger)
    (let* ((board (ledger))
           (invoice (nodecode-cash-desk::invoice-by-number board "INV-1")))
      (setf (nodecode-cash-desk::getf invoice :balance) 500)
      (nodecode-cash-desk::write-ledger board))
    (let ((text (nodecode-cash-desk:draft-reminders :as-of +report-date+)))
      (is (search "drafted 1" text) "only the invoice above the floor is chased"))
    (is (search "INV-2" (nodecode-cash-desk::getf (a-draft) :body)) "the chased letter is the other customer's")))

(deftest cash-desk-cell-a-promise-pauses-the-chase ()
  (with-cashdesk ()
    (seeded-ledger)
    (nodecode-cash-desk:record-reply "C001" "we will pay on Friday"
                                      :kind "promise" :promised-date "2026-11-12")
    (nodecode-cash-desk:draft-reminders :as-of +report-date+)
    (is (null (find "INV-1" (getf (ledger) :drafts) :key (lambda (draft)
                                                            (nodecode-cash-desk::getf draft :invoice-numbers))
                  :test (lambda (a b) (member a b :test #'string=)))) "the promising customer gets no letter")
    (nodecode-cash-desk:release "C001")
    (is (search "drafted 1"
                (nodecode-cash-desk:draft-reminders :as-of +report-date+)) "and release brings the chase back")))

;;; --- Figure Lock and the outbox ----------------------------------------------------

(deftest cash-desk-cell-figure-lock-refuses-a-changed-amount ()
  (with-cashdesk ()
    (seeded-ledger)
    (nodecode-cash-desk:draft-reminders :as-of +report-date+)
    (let* ((draft (a-draft))
           (body (nodecode-cash-desk::getf draft :body))
           (changed (nodecode-cash-desk::string-replace "EUR 1,000.00" "EUR 1,500.00" body)))
      (is (search "Figure Lock rejected"
                  (refusal-text nodecode-cash-desk:cash-desk-error
                                (nodecode-cash-desk:revise
                                 (nodecode-cash-desk::getf draft :id) changed))) "a raised amount is refused")
      (is (string= body (nodecode-cash-desk::getf (a-draft) :body)) "and the draft is untouched"))))

(deftest cash-desk-cell-a-prose-revision-passes-the-lock ()
  (with-cashdesk ()
    (seeded-ledger)
    (nodecode-cash-desk:draft-reminders :as-of +report-date+)
    (let* ((draft (a-draft))
           (id (nodecode-cash-desk::getf draft :id))
           (hash (nodecode-cash-desk::getf draft :hash))
           (body (nodecode-cash-desk::string-replace
                  "Kind regards," "Best regards," (nodecode-cash-desk::getf draft :body))))
      (is (search "revised" (nodecode-cash-desk:revise id body)) "the prose change is accepted")
      (is (not (string= hash (nodecode-cash-desk::getf (a-draft) :hash))) "and the hash changes with the text"))))

(deftest cash-desk-cell-a-person-approves-and-the-eml-is-written ()
  (with-cashdesk ()
    (seeded-ledger)
    (nodecode-cash-desk:draft-reminders :as-of +report-date+)
    (let* ((draft (a-draft))
           (id (nodecode-cash-desk::getf draft :id))
           (hash (nodecode-cash-desk::getf draft :hash)))
      (is (search "approves it first"
                  (refusal-text nodecode-cash-desk:cash-desk-error
                                (nodecode-cash-desk:send-draft id hash))) "nothing is sent before approval")
      (nodecode-cash-desk:approve id)
      (is (search "sent" (nodecode-cash-desk:send-draft id hash)) "the approved letter is written")
      (let* ((sent (first (nodecode-cash-desk::getf (ledger) :sends)))
             (path (and sent (probe-file (nlk:home (format nil "cash-desk/outbox/~a"
                                                           (nodecode-cash-desk::getf sent :file)))))))
        (is-present (path path)
          (is (search "INV-1" (nlk:read-text path)) "the .eml carries the letter")
          (is (search "Subject:" (nlk:read-text path)) "with its headers"))))))
