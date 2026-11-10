;;;; cell.lisp --- the ksa_vat_calculate and zatca_validate_invoice tools, /zatca, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of qeemah-cloud/hermes-plugin-zatca, a Hermes Agent
;;;; plugin that registered two deterministic tools for Saudi e-invoicing:
;;;; ksa_vat_calculate (15% VAT split into taxable amount, tax and total) and
;;;; zatca_validate_invoice (the ZATCA Phase 2 field rules over one invoice).
;;;; Both took a JSON object and answered a JSON object; here the same two
;;;; tools take the same members and answer the same facts as text the model
;;;; reads directly, and the same arithmetic answers the operator through
;;;; /zatca. The plugin never dialled anything, and neither does the cell: it
;;;; reads no key, no file and no clock, and it does not file with ZATCA.

(in-package #:nodecode-zatca)

;; The condition FAIL, the settings variable and RUNNING-SETTINGS the slash
;; command refuses through while the cell is stopped.
(nlk:define-peripheral zatca :not-running t)

;;; --- the contracts ---------------------------------------------------------------

(nlk:define-contract vat-arguments
  (:doc "Calculate Saudi Arabia's standard-rate VAT (15% since July 2020, 5%
before it) on one amount in Saudi Riyals, and say the taxable amount, the tax
and the total. Call it whenever an amount is quoted before VAT and the invoice
has to carry the tax, or when a VAT-inclusive price has to be split. It does
the arithmetic exactly, in halalas, rounding a half halala up; it does not
look up a rate, fetch an exchange rate, or file anything with ZATCA.")
  ("amount" :number :required t
            :doc "the amount in Saudi Riyals (SAR), before VAT unless is_inclusive says otherwise")
  ("is_inclusive" :boolean
                  :doc "true when amount already includes VAT; absent or false when it is an amount before VAT"))

(nlk:define-contract invoice-arguments
  (:doc "Check one invoice's fields against the ZATCA Phase 2 (FATOORA)
business rules a seller can see before clearance: the UUID is version 4
(BR-KSA-03), the seller's VAT number is 15 digits starting and ending with 3
(BR-KSA-06), the invoice counter value is at least 1 (BR-KSA-14), the issue
date and time are well formed when given (BR-KSA-05, BR-KSA-25), the VAT is
the standard rate of the taxable amount and the total is the taxable amount
plus the VAT (BR-KSA-52, BR-KSA-53). It answers which rules failed, each with
its code. This is a field-level pre-check: it reads no XML, no QR code and no
cryptographic stamp, and it submits nothing to ZATCA.")
  ("uuid" :text :required t
          :doc "the invoice's UUID as it will be written in it, version 4")
  ("seller_vat" :string :default ""
                :doc "the seller's 15-digit VAT registration number; absent, the zatca section's seller_vat is used")
  ("icv" :integer :required t
         :doc "the invoice counter value (ICV): the position of this invoice in the seller's sequence, at least 1")
  ("issue_date" :string :default ""
                :doc "the issue date, YYYY-MM-DD, in Saudi Arabia; absent, the date is not checked")
  ("issue_time" :string :default ""
                :doc "the issue time in Saudi Arabia (AST, UTC+3), HH:MM:SS; absent, the time is not checked")
  ("total_without_vat" :number :required t
                        :doc "the taxable amount in SAR: the line totals before VAT")
  ("vat_amount" :number :required t
                :doc "the VAT charged on this invoice, in SAR")
  ("total_with_vat" :number :required t
                    :doc "the total payable on this invoice, in SAR"))

;;; --- the tools ------------------------------------------------------------------

(defun run-vat (arguments)
  "The ksa_vat_calculate tool: the call's amount split at the section's rate."
  (let ((amount (vat-arguments-amount arguments)))
    (if (minusp amount)
        "zatca: amount must be a non-negative number of Saudi Riyals"
        (vat-answer amount (vat-arguments-is-inclusive arguments) (setting :vat-rate-percent)))))

(defun invoice-answer (arguments seller)
  "ARGUMENTS' invoice checked, its seller VAT number being SELLER's."
  (multiple-value-bind (failures checks)
      (check-invoice :uuid (invoice-arguments-uuid arguments)
                     :seller-vat seller
                     :icv (invoice-arguments-icv arguments)
                     :issue-date (invoice-arguments-issue-date arguments)
                     :issue-time (invoice-arguments-issue-time arguments)
                     :taxable (invoice-arguments-total-without-vat arguments)
                     :vat (invoice-arguments-vat-amount arguments)
                     :total (invoice-arguments-total-with-vat arguments)
                     :rate-percent (setting :vat-rate-percent)
                     :tolerance (setting :tolerance-halalas))
    (report-text (invoice-arguments-uuid arguments) failures checks)))

(defun run-validate (arguments)
  "The zatca_validate_invoice tool: the call's invoice checked against the rules."
  (let* ((given (nlk:trimmed (or (invoice-arguments-seller-vat arguments) "")))
         (configured (nlk:trimmed (or (setting :seller-vat) "")))
         (seller (if (plusp (length given)) given configured)))
    (if (plusp (length seller))
        (invoice-answer arguments seller)
        "zatca: this call has no seller_vat and the zatca section sets none - pass seller_vat, or set it once for every invoice")))

;;; --- /zatca ---------------------------------------------------------------------

(defparameter +usage+
  "zatca: /zatca <amount> [inclusive] - the VAT on or inside an amount in SAR
       /zatca check <uuid> <seller-vat> <icv> <without-vat> <vat> <with-vat> [YYYY-MM-DD] [HH:MM:SS]
       /zatca 2500              what 15% adds to SAR 2,500
       /zatca 2300 inclusive   what SAR 2,300 holds of VAT
       /zatca check 8f2c1a44-3c7e-4f3f-9a2b-6d1e0f7c4b21 300000000000003 12 1000 150 1150"
  "What /zatca says when it is typed alone, or its arguments do not parse.")

(defun run-amount (parts)
  "The amount form of /zatca: an amount and, optionally, inclusive."
  (let* ((text (first parts))
         (flag (second parts))
         (amount (parse-amount text))
         (inclusive (and flag (member (string-downcase flag)
                                      '("inclusive" "with-vat" "gross") :test #'string=))))
    (cond ((null amount)
           (format nil "zatca: ~a is not an amount in SAR~%~a" text +usage+))
          ((minusp amount)
           "zatca: amount must be a non-negative number of Saudi Riyals")
          ((and flag (not inclusive))
           (format nil "zatca: ~a is neither inclusive nor net~%~a" flag +usage+))
          (t (vat-answer amount (and inclusive t) (setting :vat-rate-percent))))))

(defun run-check (parts)
  "The check form of /zatca: one whole invoice, checked."
  (if (< (length parts) 6)
      (format nil "zatca: check takes a uuid, a seller VAT number, an ICV and three amounts~%~a" +usage+)
      (destructuring-bind (uuid seller icv-text taxable-text vat-text total-text
                           &optional date time)
          parts
        (let ((icv (parse-integer icv-text :junk-allowed t))
              (taxable (parse-amount taxable-text))
              (vat (parse-amount vat-text))
              (total (parse-amount total-text)))
          (cond ((null icv)
                 (format nil "zatca: ~a is not an invoice counter value" icv-text))
                ((or (null taxable) (null vat) (null total))
                 (format nil "zatca: the three amounts must be numbers of SAR, not ~a, ~a, ~a"
                         taxable-text vat-text total-text))
                (t (multiple-value-bind (failures checks)
                       (check-invoice :uuid uuid :seller-vat seller :icv icv
                                      :issue-date (or date "") :issue-time (or time "")
                                      :taxable taxable :vat vat :total total
                                      :rate-percent (setting :vat-rate-percent)
                                      :tolerance (setting :tolerance-halalas))
                     (report-text uuid failures checks))))))))

(defun run-slash (args session-id)
  "The /zatca command: VAT math for an amount, or the invoice checks. =>
the answer's text."
  (declare (ignore session-id))
  (running-settings)
  (let* ((text (nlk:trimmed (or args "")))
         (parts (and (plusp (length text)) (words text))))
    (cond ((null parts) +usage+)
          ((string-equal (first parts) "check") (run-check (rest parts)))
          (t (run-amount parts)))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +manual+
  "zatca: Saudi Arabia's VAT and the ZATCA Phase 2 (FATOORA) e-invoicing
field rules, calculated and checked offline.

The tool ksa_vat_calculate takes amount (SAR) and is_inclusive, and answers
the taxable amount, the VAT at the section's rate (15 by default; 5 for an
invoice dated before July 2020) and the total. The split is done in halalas -
SAR cents - with a half halala rounded up, so the three figures always add up.

The tool zatca_validate_invoice takes uuid, seller_vat, icv,
total_without_vat, vat_amount and total_with_vat, and issue_date and
issue_time when they are known, and answers which rules failed and how many
ran: BR-KSA-03 the uuid is a version 4 UUID; BR-KSA-06 the seller's VAT
number is 15 digits starting and ending with 3; BR-KSA-14 the invoice counter
value is an integer of at least 1; BR-KSA-05 the date is a real calendar date
written YYYY-MM-DD; BR-KSA-25 the time is written HH:MM:SS in Saudi Arabia
(AST, UTC+3); BR-KSA-52 the VAT is the rate of the taxable amount; BR-KSA-53
the total is the taxable amount plus the VAT. The last two allow the
section's tolerance_halalas (0.05 SAR by default) for line-level rounding. A
call that leaves seller_vat out uses the section's seller_vat.

/zatca does both for the operator: /zatca 2500 answers what 15% adds to SAR
2,500, /zatca 2300 inclusive answers what SAR 2,300 holds of VAT, and /zatca
check followed by a uuid, a seller VAT number, an ICV, the three amounts and
optionally a date and a time checks a whole invoice.

What it is not: it reads no invoice XML, no QR code and no cryptographic
stamp, and it submits nothing to ZATCA - it is the field-level pre-check a
seller can run before clearance. Nothing leaves this machine: no network, no
file, no key, no clock. The section zatca sets seller_vat (your 15-digit VAT
registration number), vat_rate_percent and tolerance_halalas."
  "What (help :zatca) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell zatca
  (:section ("zatca")
    (:guide "seller_vat is your 15-digit VAT registration number: set it once and the validate tool fills it in; vat_rate_percent and tolerance_halalas are the original's 15 and 5, and nothing else is required")
    ("seller_vat" :string :required t
                  :doc "your 15-digit Saudi VAT registration number; a call that leaves seller_vat out uses it")
    ("vat_rate_percent" :integer :default 15 :min 0
                        :doc "the standard rate: 15 since July 2020, 5 before it")
    ("tolerance_halalas" :integer :default 5 :min 0
                         :doc "how far a VAT amount or a total may sit from the rate, in halalas (5 is 0.05 SAR)"))
  (:settings (lambda (values table)
               (declare (ignore table))
               values))
  (:tool "ksa_vat_calculate" (nlk:contract 'vat-arguments) 'run-vat)
  (:tool "zatca_validate_invoice" (nlk:contract 'invoice-arguments) 'run-validate)
  (:command "zatca" 'run-slash
            :description "Saudi VAT at the standard rate and the ZATCA Phase 2 invoice checks"
            :argument-hint "AMOUNT [inclusive] | check UUID SELLER-VAT ICV NET VAT TOTAL")
  (:help :zatca "zatca: Saudi VAT math and ZATCA Phase 2 invoice field checks - /zatca <amount> [inclusive]"
         +manual+))
