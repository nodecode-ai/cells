;;;; support.lisp --- the zatca test runner and the helpers every test reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a ZATCA name
;;;; prefix; RUN-ZATCA-TESTS runs exactly that slice. Nothing here dials
;;;; anything: the cell reads no file, no environment and no network, so there
;;;; is nothing to stub but the wire a provider's tool call comes through.

(in-package #:nodecode.test)

(define-test-slice "zatca" "ZATCA" :start nodecode-zatca:start-cell)

(defparameter +clean-uuid+ "8f2c1a44-3c7e-4f3f-9a2b-6d1e0f7c4b21"
  "A version 4 UUID, as an invoice carries one.")

(defparameter +clean-seller+ "300000000000003"
  "A well-formed Saudi VAT registration number: 15 digits, 3 at both ends.")

(defparameter +clean-invoice+ (list :taxable 1000 :vat 150 :total 1150)
  "One invoice that passes every rule: 15% of SAR 1,000 is SAR 150.")

(defun zatca-vat-call (id arguments)
  "ksa_vat_calculate called as the provider's call calls it: the answer's text."
  (nle::message-content (execute-wire-call id "ksa_vat_calculate" arguments)))

(defun zatca-invoice-call (id arguments)
  "zatca_validate_invoice called the same way."
  (nle::message-content (execute-wire-call id "zatca_validate_invoice" arguments)))

(defun zatca-invoice-json (&key (uuid +clean-uuid+) (seller +clean-seller+) (icv 12)
                                 (taxable 1000) (vat 150) (total 1150)
                                 (date "") (time ""))
  "A zatca_validate_invoice call's JSON: the clean invoice unless a field is named."
  (format nil "{\"uuid\": ~s, \"seller_vat\": ~s, \"icv\": ~a, \"total_without_vat\": ~a, \"vat_amount\": ~a, \"total_with_vat\": ~a, \"issue_date\": ~s, \"issue_time\": ~s}"
          uuid seller icv taxable vat total date time))
