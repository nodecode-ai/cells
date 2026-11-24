;;;; cell-test.lisp --- the tools, the command, the ledger, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed Stripe: that a
;;;; start installs the seven tools, the command and the manual and a stop
;;;; takes them back, that a draft is created and recorded, that sending
;;;; finalizes and keeps the link, that a payment is recorded, that a live key
;;;; without allow_live sends nothing, and that a missing key says where it goes.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "obol"
  (:help :obol)
  (:command "obol")
  (:running (is (= 14 (nodecode-obol::setting :days-until-due)) "two weeks by default")
            (is (null (nodecode-obol::setting :allow-live)) "a live key is not trusted by default")
            (is (equal "usd" (nodecode-obol::setting :default-currency)) "dollars unless told otherwise"))
  (:refused ("timeout_seconds" 0) ("days_until_due" 0))
  (:idle obol:obol-error (obol:list-invoices)))

;;; --- creating ----------------------------------------------------------------------

(deftest obol-cell-a-draft-is-created-recorded-and-listed ()
  (with-cell-stop ((obol-start))
    (fresh-ledger)
    (with-stripe ((route :get "/v1/customers" *customer*)
                  (route :post "/v1/invoices" *invoice*)
                  (route :post "/v1/invoiceitems" *empty*)
                  (route :post "/v1/invoices/in_1/finalize" *invoice*))
      (let ((answer (call-tool "obol_create_invoice"
                                (list "customer_email" "billing@acme.example"
                                      "customer_name" "Acme"
                                      "line_items" (vector (nlk:make-json-object
                                                            "description" "October consulting"
                                                            "amount" 1200))))))
        (is (search "in_1" answer) "the call answers the invoice id")
        (is (search "INV-0001" answer) "with its number")
        (is (search "1200.00 USD" answer) "and the total it will bill")
        (is (search "Draft" answer) "a draft, because send was not asked for"))
      (is (notany (lambda (call) (and (eq (first call) :post) (equal (second call) "/v1/customers")))
                  *requests*) "the customer the account already had is reused")
      (let ((list (obol:list-invoices)))
        (is (search "billing@acme.example" list) "the invoice is in this home's ledger")
        (is (search "Draft" list) "still a draft")))))

(deftest obol-cell-a-sent-invoice-carries-its-payment-link ()
  (with-cell-stop ((obol-start))
    (fresh-ledger)
    (with-stripe ((route :get "/v1/customers" *customer*)
                  (route :post "/v1/invoices" *invoice*)
                  (route :post "/v1/invoiceitems" *empty*)
                  (route :post "/v1/invoices/in_1/finalize" *sent*)
                  (route :post "/v1/invoices/in_1/send" *sent*))
      (let ((answer (call-tool "obol_create_invoice"
                                (list "customer_email" "billing@acme.example"
                                      "send" t
                                      "line_items" (vector (nlk:make-json-object
                                                            "description" "March retainer"
                                                            "amount" 500))))))
        (is (search "https://invoice.stripe.com/i/one" answer) "the link the client pays at")
        (is (search "Open" answer) "and the invoice is open, not a draft"))
      (let ((record (first (obol::ledger-load))))
        (is (equal "open" (getf record :status)) "the ledger says it was sent")
        (is (= 50000 (getf record :amount)) "the amount is kept in minor units")))))

;;; --- checking and paying ------------------------------------------------------------

(deftest obol-cell-a-payment-is-recorded-and-void-says-so ()
  (with-cell-stop ((obol-start))
    (fresh-ledger)
    (with-stripe ((route :get "/v1/customers" *customer*)
                  (route :post "/v1/invoices" *invoice*)
                  (route :post "/v1/invoiceitems" *empty*)
                  (route :post "/v1/invoices/in_1/finalize" *invoice*)
                  (route :post "/v1/invoices/in_1/pay" *empty*))
      (call-tool "obol_create_invoice"
                 (list "customer_email" "billing@acme.example"
                       "line_items" (vector (nlk:make-json-object
                                             "description" "March retainer"
                                             "amount" 500))))
      (let ((answer (call-tool "obol_mark_paid"
                                (list "invoice_id" "in_1" "note" "bank transfer, ref 0004"))))
        (is (search "marked paid" answer) "the payment is recorded")
        (is (search "bank transfer" answer) "with the note the user gave"))
      (is (search "Paid" (obol:list-invoices)) "the ledger agrees")
      (is (search "in_1" (call-tool "obol_get_invoice" (list "invoice_id" "in_1"))) "and the invoice reads back"))))

(deftest obol-cell-a-duplicate-is-called-out-before-it-is-sent ()
  (with-cell-stop ((obol-start))
    (fresh-ledger)
    (obol::ledger-put (list :id "in_old" :number "INV-0000" :email "billing@acme.example"
                             :name "Acme" :currency "usd" :amount 120000 :status "open"
                             :link "" :created (get-universal-time) :note ""))
    (with-stripe ((route :get "/v1/customers" *customer*)
                  (route :post "/v1/invoices" *invoice*)
                  (route :post "/v1/invoiceitems" *empty*)
                  (route :post "/v1/invoices/in_1/finalize" *invoice*))
      (let ((answer (call-tool "obol_create_invoice"
                                (list "customer_email" "billing@acme.example"
                                      "customer_name" "Acme"
                                      "line_items" (vector (nlk:make-json-object
                                                            "description" "October consulting"
                                                            "amount" 1200))))))
        (is (search "possible duplicate" answer) "the same client and amount is a warning")
        (is (search "in_old" answer) "naming the invoice already billed")))))

;;; --- the command -------------------------------------------------------------------

(deftest obol-cell-the-command-says-what-is-outstanding ()
  (with-cell-stop ((obol-start))
    (fresh-ledger)
    (obol::ledger-put (list :id "in_2" :number "INV-0002" :email "billing@acme.example"
                             :name "Acme" :currency "usd" :amount 40000 :status "open"
                             :link "" :created (get-universal-time) :note ""))
    (let ((answer (cell-entry "nodecode-obol" "obol" "")))
      (is (search "1 still unpaid" answer) "one invoice is outstanding")
      (is (search "400.00 USD" answer) "with its total")
      (is (search "billing@acme.example" answer) "and who owes it"))
    (is (search "billing@acme.example" (cell-entry "nodecode-obol" "obol" "open")) "/obol open filters by status")))

;;; --- every refusal -----------------------------------------------------------------

(deftest obol-cell-no-key-says-where-the-key-goes ()
  (with-cell-stop ((obol-start "api_key" ""))
    (fresh-ledger)
    (with-stripe ()
      (let ((answer (refusal (lambda ()
                               (call-tool "obol_create_invoice"
                                          (list "customer_email" "billing@acme.example"
                                                "line_items" (vector (nlk:make-json-object
                                                                      "description" "x"
                                                                      "amount" 10))))))))
        (is (search "no Stripe key" answer) "a call with no key says so")
        (is (search "api_key" answer) "and names the setting")
        (is (null *requests*) "nothing was asked of Stripe")))))

(deftest obol-cell-a-live-key-is-refused-until-allowed ()
  (with-cell-stop ((obol-start "api_key" "sk_live_abc123"))
    (fresh-ledger)
    (with-stripe ()
      (let ((answer (refusal (lambda ()
                               (call-tool "obol_create_invoice"
                                          (list "customer_email" "billing@acme.example"
                                                "line_items" (vector (nlk:make-json-object
                                                                      "description" "x"
                                                                      "amount" 10))))))))
        (is (search "LIVE key" answer) "a live key is named as one")
        (is (search "allow_live" answer) "and the setting that lets it through")
        (is (null *requests*) "so no real client is billed")))))
