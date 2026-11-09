;;;; support.lisp --- Cash Desk test runner and shared fixtures.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a CASH-DESK-CELL-
;;;; name prefix; RUN-CASH-DESK-TESTS runs exactly that slice. Every test gets
;;;; a scratch home, so the ledger lands nowhere near the operator's.
;;;;
;;;; Nothing is stubbed and nothing is dialled: the cell's one I/O is the
;;;; ledger JSON file and the CSV files the test writes beside it.

(in-package #:nodecode.test)

(define-test-slice "cash-desk" "CASH-DESK-CELL-" :start nodecode-cash-desk:start-cell)

(defparameter +test-sender+ "Northgate Studio Ltd"
  "The business that signs the test drafts.")

(defmacro with-cashdesk ((&rest config) &body body)
  "BODY with the cell started on CONFIG (sender_name always set) and stopped
on unwind."
  `(with-cell-stop ((cash-desk-start "sender_name" ,+test-sender+ ,@config))
     ,@body))

(defun ledger-file (name)
  "A path inside the ledger folder NAME."
  (nlk:home (format nil "cash-desk/~a" name)))

(defun write-fixture (name text)
  "Write TEXT as a file NAME inside the ledger folder. => its path."
  (let ((path (ledger-file name)))
    (nlk:write-file-atomically path text :mode #o600 :directory-mode #o700)
    path))

(defparameter +customers-csv+
  "customer_id,name,email,language
C001,Northgate Studio Ltd,ann@northgate.example,en
C002,Birchfield Consulting GmbH,bo@birchfield.example,en
"
  "Two customers, both with an email address.")

(defparameter +invoices-csv+
  "invoice_no,customer_id,issue_date,due_date,amount,currency,reference,iban
INV-1,C001,2026-09-01,2026-10-01,1000.00,EUR,1001,DE89370400440532013000
INV-2,C002,2026-10-01,2026-11-01,500.00,EUR,1002,
"
  "Two open invoices, both overdue by the fixed report date.")

(defun seeded-ledger ()
  "Customers and invoices imported, and the paths the test wrote."
  (let ((customers (write-fixture "customers.csv" +customers-csv+))
        (invoices (write-fixture "invoices.csv" +invoices-csv+)))
    (nodecode-cash-desk:import-file "customers" customers)
    (nodecode-cash-desk:import-file "invoices" invoices)
    (values customers invoices)))

(defun ledger ()
  (nodecode-cash-desk::read-ledger))

(defun an-invoice (number)
  (nodecode-cash-desk::invoice-by-number (ledger) number))

(defun a-draft (&optional (position 0))
  (nth position (getf (ledger) :drafts)))

(defun count-reviews ()
  (length (getf (ledger) :reviews)))

(defun first-review ()
  (first (getf (ledger) :reviews)))

(defparameter +report-date+ "2026-11-10"
  "The fixed as-of the tests plan and match at: INV-1 is 40 days overdue,
INV-2 is 9 days overdue.")
