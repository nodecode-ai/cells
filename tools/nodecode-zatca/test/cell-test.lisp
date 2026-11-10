;;;; cell-test.lisp --- the cell's half: the two tools, /zatca, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the declaration installs ksa_vat_calculate,
;;;; zatca_validate_invoice, /zatca and (help :zatca) and takes all four back
;;;; on stop; each tool answers through the wire a provider's call comes
;;;; through; /zatca answers the same for the operator; a call that leaves out
;;;; a required member is refused at the boundary; and a call with no seller
;;;; VAT number anywhere answers with the setting it is missing, not a guess.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "zatca"
  (:config "seller_vat" +clean-seller+)
  (:help :zatca)
  (:command "zatca")
  (:running
   (is (= 15 (nodecode-zatca::setting :vat-rate-percent)) "the original's 15% rate")
   (is (= 5 (nodecode-zatca::setting :tolerance-halalas)) "and its 0.05 SAR tolerance"))
  (:refused ("vat_rate_percent" -1)))

(deftest zatca-cell-the-vat-tool-answers-the-split ()
  (with-cell-stop ((zatca-start "seller_vat" +clean-seller+))
    (let ((answer (zatca-vat-call "zatca-call-1" "{\"amount\": 2500}")))
      (is (search "15% VAT on SAR 2,500.00" answer) "the amount and which way the rate went")
      (is (search "VAT             SAR 375.00" answer) "the tax")
      (is (search "total           SAR 2,875.00" answer) "and the total"))
    (let ((answer (zatca-vat-call "zatca-call-2" "{\"amount\": 2300, \"is_inclusive\": true}")))
      (is (search "taxable amount  SAR 2,000.00" answer) "an inclusive amount is split back"))
    (is (search "non-negative"
                (zatca-vat-call "zatca-call-3" "{\"amount\": -5}")) "a negative amount is a line the model can act on, not a failed call")))

(deftest zatca-cell-the-invoice-tool-answers-the-checks ()
  (with-cell-stop ((zatca-start "seller_vat" +clean-seller+))
    (let ((answer (zatca-invoice-call "zatca-call-4" (zatca-invoice-json))))
      (is (search "5 ZATCA Phase 2 checks on invoice 8f2c1a44" answer) "a clean invoice is checked")
      (is (search "all passed" answer) "and passes"))
    (let ((answer (zatca-invoice-call "zatca-call-5"
                                      (zatca-invoice-json :date "2026-11-08" :time "14:35:00"))))
      (is (search "7 ZATCA Phase 2 checks" answer) "a dated one is checked further"))
    (let ((answer (zatca-invoice-call "zatca-call-6" (zatca-invoice-json :seller "300000000000004"))))
      (is (search "1 of 5 ZATCA Phase 2 checks failed" answer) "one broken field is one failure")
      (is (search "BR-KSA-06" answer) "named with its rule"))))

(deftest zatca-cell-the-section-seller-steps-in ()
  (with-cell-stop ((zatca-start "seller_vat" +clean-seller+))
    (let ((answer (zatca-invoice-call "zatca-call-7"
                                      (format nil "{\"uuid\": ~s, \"icv\": 12, \"total_without_vat\": 1000, \"vat_amount\": 150, \"total_with_vat\": 1150}"
                                              +clean-uuid+))))
      (is (search "all passed" answer) "a call with no seller_vat uses the section's")))
  (with-cell-stop ((zatca-start))
    (is (search "the zatca section sets none"
                (zatca-invoice-call "zatca-call-8"
                                    (format nil "{\"uuid\": ~s, \"icv\": 12, \"total_without_vat\": 1000, \"vat_amount\": 150, \"total_with_vat\": 1150}"
                                            +clean-uuid+))) "and with none anywhere the tool says which setting to set")))

(deftest zatca-cell-the-command-answers-the-operator ()
  (with-cell-stop ((zatca-start "seller_vat" +clean-seller+))
    (is (search "SAR 375.00" (nodecode-zatca:run-slash "2500" nil)) "/zatca computes what is typed after it")
    (is (search "SAR 2,000.00" (nodecode-zatca:run-slash "2300 inclusive" nil)) "and takes an inclusive amount")
    (is (search "/zatca <amount>" (nodecode-zatca:run-slash "" nil)) "bare, it says how to use it")
    (is (search "all passed"
                (nodecode-zatca:run-slash
                 (format nil "check ~a ~a 12 1000 150 1150 2026-11-08 14:35:00" +clean-uuid+ +clean-seller+)
                 nil)) "check runs a whole invoice")
    (is (search "BR-KSA-06"
                (nodecode-zatca:run-slash
                 (format nil "check ~a 300000000000004 12 1000 150 1150" +clean-uuid+) nil)) "and names the rule a broken one breaks")
    (is (search "not an amount" (nodecode-zatca:run-slash "many" nil)) "a word is not an amount")))

(deftest zatca-cell-a-call-without-its-required-members-is-refused ()
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-zatca::vat-arguments)
                             (nlk:make-json-object "is_inclusive" t))) "a VAT call with no amount is refused by the declaration")
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-zatca::invoice-arguments)
                             (nlk:make-json-object "uuid" +clean-uuid+ "icv" 1))) "and an invoice call with no amounts"))

(deftest zatca-cell-the-section-asks-for-the-sellers-number ()
  (let ((problems (nlk:section-problems (nlk:find-section '("zatca")) (nlk:make-json-object))))
    (is (= 1 (length problems)) "an unanswered section is one problem")
    (is (search "seller_vat" (first problems)) "the seller's VAT registration number")))
