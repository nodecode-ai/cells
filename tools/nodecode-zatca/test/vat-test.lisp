;;;; vat-test.lisp --- the money and the rules, on their own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: money is carried in halalas and a half halala
;;;; rounds up; the VAT split adds up from either end, at 15% and at the 5%
;;;; rate of a pre-2020 invoice; and every rule the original's handler
;;;; claimed - the UUID, the seller's VAT number, the ICV, the date, the time
;;;; and the two sums - is one that runs and one that fails when it should.

(in-package #:nodecode.test)

(deftest zatca-halalas-are-whole-cents-rounded-half-up ()
  (is (= 250000 (nodecode-zatca:halalas 2500)) "SAR 2,500 is 250,000 halalas")
  (is (= 13 (nodecode-zatca:halalas 0.125)) "a half halala rounds up, not to even")
  (is (equal "2,500.00" (nodecode-zatca:money 250000)) "money is written with its thousands")
  (is (equal "SAR -12.50" (nodecode-zatca:money-sar -1250)) "and keeps its sign"))

(deftest zatca-the-vat-split-adds-up-from-either-end ()
  (multiple-value-bind (taxable vat total) (nodecode-zatca:vat-breakdown 250000 15 nil)
    (is (= 250000 taxable) "before VAT, the amount is the taxable amount")
    (is (= 37500 vat) "15% of it")
    (is (= 287500 total) "and the total it comes to"))
  (multiple-value-bind (taxable vat total) (nodecode-zatca:vat-breakdown 230000 15 t)
    (is (= 200000 taxable) "inclusive, the tax is taken out of the amount")
    (is (= 30000 vat) "leaving the tax it held")
    (is (= 230000 total) "and the amount as it was given"))
  (multiple-value-bind (taxable vat total) (nodecode-zatca:vat-breakdown 100000 5 nil)
    (is (= 5000 vat) "a pre-July-2020 invoice is 5%")
    (is (= 105000 total) "and its total")
    (is (= 100000 taxable) "on the same taxable amount"))
  (let ((answer (nodecode-zatca:vat-answer 2300 t 15)))
    (is (search "15% VAT inside SAR 2,300.00" answer) "the answer says which way the rate went")
    (is (search "total           SAR 2,300.00" answer) "and restates the whole")))

(deftest zatca-each-field-rule-fails-on-its-own ()
  (flet ((fails (&rest limits)
           ;; LIMITS first: a plist read left to right keeps the first of a
           ;; repeated key, which is the one the test meant to change.
           (apply #'nodecode-zatca:check-invoice
                  (append limits (list :uuid +clean-uuid+ :seller-vat +clean-seller+ :icv 12)
                          +clean-invoice+))))
    (multiple-value-bind (failures checks) (fails)
      (is (null failures) "a clean invoice fails nothing")
      (is (= 5 checks) "in five checks"))
    (multiple-value-bind (failures checks) (fails :issue-date "2026-11-08" :issue-time "14:35:00")
      (is (null failures) "a dated invoice fails nothing either")
      (is (= 7 checks) "in seven"))
    (multiple-value-bind (failures checks)
        (nodecode-zatca:check-invoice :uuid "not-a-uuid" :seller-vat "300000000000004"
                                       :icv 0 :taxable 1000 :vat 150 :total 1150)
      (is (= 3 (length failures)) "three broken fields are three failures")
      (is (= 5 checks) "still five checks")
      (is (search "BR-KSA-03" (first failures)) "the uuid is named first")
      (is (search "BR-KSA-06" (second failures)) "then the seller's number")
      (is (search "BR-KSA-14" (third failures)) "then the counter value"))
    (multiple-value-bind (failures checks) (fails :vat 160)
      (is (= 2 (length failures)) "a VAT amount off the rate fails, and the total that no longer adds up with it")
      (is (search "BR-KSA-52" (first failures)) "the rate is named first")
      (is (= 5 checks) "and is counted among the five"))
    (multiple-value-bind (failures checks) (fails :total 1200)
      (is (search "BR-KSA-53" (first failures)) "a total that is not the sum is BR-KSA-53")
      (is (= 5 checks) "counted the same way"))
    (multiple-value-bind (failures checks) (fails :issue-date "2026-02-29" :issue-time "24:00:00")
      (is (= 2 (length failures)) "a date that does not exist and an hour that does not either")
      (is (= 7 checks) "each fails on its own rule"))))

(deftest zatca-a-checked-field-is-a-well-formed-one ()
  (is (nodecode-zatca:valid-uuid-p +clean-uuid+) "a version 4 uuid")
  (is (not (nodecode-zatca:valid-uuid-p "8f2c1a44-3c7e-1f3f-9a2b-6d1e0f7c4b21")) "a version 1 one is not")
  (is (not (nodecode-zatca:valid-uuid-p "8f2c1a44-3c7e-4f3f-ca2b-6d1e0f7c4b21")) "nor one outside the variant")
  (is (nodecode-zatca:valid-seller-vat-p +clean-seller+) "15 digits, 3 at both ends")
  (is (not (nodecode-zatca:valid-seller-vat-p "30000000000000")) "a short number is not one")
  (is (not (nodecode-zatca:valid-seller-vat-p "300000000000004")) "nor one that does not end in 3")
  (is (nodecode-zatca:valid-date-p "2024-02-29") "a leap day exists")
  (is (not (nodecode-zatca:valid-date-p "2026-02-29")) "in a common year it does not")
  (is (not (nodecode-zatca:valid-date-p "2026-13-01")) "there is no thirteenth month")
  (is (nodecode-zatca:valid-time-p "14:35:00") "an afternoon time")
  (is (not (nodecode-zatca:valid-time-p "24:00:00")) "the day ends before 24:00")
  (is (not (nodecode-zatca:valid-time-p "14:35")) "and a time is written with its seconds"))

(deftest zatca-the-report-says-what-happened ()
  (is (search "5 ZATCA Phase 2 checks on invoice u-1"
              (nodecode-zatca:report-text "u-1" nil 5)) "a clean invoice is one line")
  (is (search "all passed" (nodecode-zatca:report-text "u-1" nil 5)) "saying so")
  (let ((text (nodecode-zatca:report-text "u-1" '("BR-KSA-14: icv 0") 5)))
    (is (search "1 of 5 ZATCA Phase 2 checks failed on invoice u-1" text) "a failed one counts itself")
    (is (search "  BR-KSA-14: icv 0" text) "and lists each rule on its own line")))

(deftest zatca-what-the-operator-types-is-read-carefully ()
  (is (= 2500 (nodecode-zatca:parse-amount "2,500")) "thousands separators are allowed")
  (is (= 2500 (nodecode-zatca:parse-amount "2500")) "so is a plain number")
  (is (null (nodecode-zatca:parse-amount "abc")) "a word is not an amount")
  (is (null (nodecode-zatca:parse-amount "25 SAR")) "nor is an amount with a unit after it")
  (is (equal '("check" "1" "2") (nodecode-zatca:words "  check   1	2 ")) "words split on runs of space"))
