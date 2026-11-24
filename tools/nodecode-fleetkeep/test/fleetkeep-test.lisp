;;;; fleetkeep-test.lisp --- the CSV, the six columns, and every rule.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the arithmetic the original left to prose: that the
;;;; CSV is read and written as RFC 4180 says; that a header is mapped to the
;;;; six columns the import takes and a column it does not take is named and
;;;; kept out; that a missing or a duplicate registration is flagged and the
;;;; row kept; that an ambiguous date is flagged and left blank rather than
;;;; read one way, and one that reads only one way is flagged as not ISO; that
;;;; a bad mileage and a zero interval are flagged; that a cell a spreadsheet
;;;; might run holds its row out of the import-ready file while a plain signed
;;;; number does not; and that the review says all of it in words.

(in-package #:nodecode.test)

(deftest fleetkeep-csv-s-survey-is-kept-whole ()
  (let ((rows (nodecode-fleetkeep:parse-csv
               (format nil "\"a,b\",c~%\"line~%break\",\"quote \"\" here\"~%"))))
    (is (equal '("a,b" "c") (first rows)) "a quoted comma is data")
    (is (equal (format nil "line~%break") (first (second rows))) "a quoted newline is data")
    (is (equal "quote \" here" (second (second rows))) "a doubled quote is one quote")
    (is (= 2 (length rows)) "two records"))
  (is (= 2 (length (nodecode-fleetkeep:parse-csv
                   (format nil "a,b~a~ac,d~%" #\Return #\Newline)))) "a CRLF ends a record as an LF does")
  (is (= 2 (length (nodecode-fleetkeep:parse-csv (format nil "a,b~%c,d~%")))) "a final newline adds no record")
  (is (equal '(("a" "")) (nodecode-fleetkeep:parse-csv "a,")) "a trailing empty field stays")
  (is (null (nodecode-fleetkeep:parse-csv "")) "an empty text reads as no record")
  (let ((rows '(("registration" "label") ("AB12 CDE" "Van, big") ("CD34 FGH" "line"))))
    (is (equal rows (nodecode-fleetkeep:parse-csv (nodecode-fleetkeep:write-csv rows))) "what is written reads back the same")))

(deftest fleetkeep-a-header-maps-to-the-six-columns ()
  (multiple-value-bind (mapped unmapped duplicate)
      (nodecode-fleetkeep::map-headers
       '("Registration Number" "Vehicle" "Insurance Due" "Mileage" "Driver" "Name"))
    (is (equal "registration" (car (first mapped))) "a spelling the table names is mapped")
    (is (= 0 (cdr (first mapped))) "at the column it stands in")
    (is (equal "label" (car (assoc "label" mapped :test #'string=))) "Vehicle is a label")
    (is (equal "last_service_mileage" (car (assoc "last_service_mileage" mapped :test #'string=))) "Mileage is the mileage")
    (is (equal '("Driver") (mapcar #'car unmapped)) "a column the six do not take is named")
    (is (equal '("label" "Name" 5) (first duplicate)) "a second column for one name is named too")))

(deftest fleetkeep-the-review-flags-what-the-import-would-guess ()
  (let* ((result (nodecode-fleetkeep:review +messy-csv+))
         (kinds (fleetkeep-kinds result))
         (ready (getf result :ready)))
    (is (= 5 (getf result :rows)) "five rows read")
    (is (equal '("notes") (mapcar #'car (getf result :unmapped))) "the notes column is unmapped")
    (is (equal '("registration" "label" "insurance_due" "last_service_date"
                 "last_service_mileage" "service_interval_months")
               (mapcar #'car (getf result :mapped))) "and the six are mapped, in the import's order")
    (is (= 1 (length (fleetkeep-kinds result "insurance_due"))) "one insurance date to review")
    (is (eq :ambiguous-date (first (fleetkeep-kinds result "insurance_due"))) "an ambiguous date is flagged, not read")
    (is (eq :not-iso (first (fleetkeep-kinds result "last_service_date"))) "a date that reads one way is not ISO yet")
    (is (member :duplicate-registration kinds) "a repeated registration is flagged")
    (is (member :missing-registration kinds) "a blank registration is flagged, and the row kept")
    (is (member :negative-mileage kinds) "a negative mileage is flagged")
    (is (member :bad-mileage kinds) "a word where a mileage goes is flagged")
    (is (member :bad-interval kinds) "a zero interval is flagged")
    (is (member :formula kinds) "a cell a spreadsheet might run is flagged")
    (is (= 1 (getf result :held)) "and its row is held out of the import-ready file")
    (is (= 4 (length ready)) "four rows are import-ready")
    (is (equal "" (or (third (first ready)) "")) "an ambiguous date is left blank rather than guessed")
    (is (equal "12000" (fifth (first ready))) "a good mileage is kept")
    (is (equal "EF34 GHI" (first (fourth ready))) "the last row is the good one")
    (is (equal "12500" (fifth (fourth ready))) "a thousands comma is read, and written without one")
    (let ((table (nodecode-fleetkeep:parse-csv (getf result :file))))
      (is (= 5 (length table)) "the file is the header and the four rows")
      (is (equal '("registration" "label" "insurance_due" "last_service_date"
                   "last_service_mileage" "service_interval_months")
                 (first table)) "under the import's own six columns")
      (is (null (search "HYPERLINK" (getf result :file))) "the held-out row is not in the file")
      (is (null (search "notes" (getf result :file))) "and neither is a column the import does not take"))
    (is-carrying (text (getf result :report))
      ("fleetkeep: 5 rows read") ("row 3 · registration: the same registration as row 2")
      ("row 4 · registration: no registration") ("neither chosen") ("held out for review"))
    (is (search "row 2 · insurance_due: \"03/04/2026\" reads 3 April 2026 day-first or 4 March 2026 month-first"
                (getf result :report)) "the ambiguous date is spelled out both ways")))

(deftest fleetkeep-a-clean-file-is-import-ready-whole ()
  (let ((result (nodecode-fleetkeep:review +clean-csv+)))
    (is (null (getf result :issues)) "nothing to review")
    (is (= 1 (length (getf result :ready))) "the row is import-ready")
    (is (= 0 (getf result :held)) "nothing held out")
    (is (equal (nodecode-fleetkeep:parse-csv +clean-csv+)
               (nodecode-fleetkeep:parse-csv (getf result :file))) "the file is the input, in the import's columns")
    (is-carrying (text (getf result :report)) ("1 row read") ("0 issues") ("1 row import-ready")))
  (let ((empty (nodecode-fleetkeep:review "  ")))
    (is (getf empty :empty) "an empty CSV is said, not guessed at")
    (is (search "the CSV has no header row" (getf empty :report)) "and the report says why")))

(deftest fleetkeep-a-formula-holds-its-row-back-and-a-number-does-not ()
  (flet ((formula-p (value) (nodecode-fleetkeep::formula-p value)))
    (is (formula-p "=SUM(A1:A9)") "an equals sign is a formula")
    (is (formula-p "  +A1") "after leading spaces too")
    (is (formula-p "@cmd") "and an at sign")
    (is (formula-p "-1+1") "a minus sign that is not a number")
    (is (null (formula-p "-500")) "a negative number is a number")
    (is (null (formula-p "+12")) "and so is a positive one")
    (is (null (formula-p "AB12 CDE")) "a registration is none of it"))
  (let ((off (nodecode-fleetkeep:review +messy-csv+ :formula-guard nil)))
    (is (= 0 (getf off :held)) "the guard off holds nothing back")
    (is (= 5 (length (getf off :ready))) "so every row is in the file")))

(deftest fleetkeep-a-date-is-flagged-or-read-as-it-stands ()
  (flet ((issue-of (value order) (multiple-value-list (nodecode-fleetkeep::date-issue value order))))
    (is (eq :ok (first (issue-of "2026-04-03" "day-first"))) "ISO is fine")
    (is (eq :ambiguous-date (first (issue-of "03/04/2026" "day-first"))) "a date both ways round is ambiguous")
    (is (search "4 March 2026" (second (issue-of "03/04/2026" "month-first"))) "the order setting names its own reading first")
    (is (eq :not-iso (first (issue-of "25/12/2026" "day-first"))) "a date only one way round is still not ISO")
    (is (eq :not-iso (first (issue-of "03/03/2026" "day-first"))) "one that reads the same both ways is not ambiguous")
    (is (eq :bad-date (first (issue-of "next week" "day-first"))) "a word is no date")
    (is (eq :bad-date (first (issue-of "13/14/2026" "day-first"))) "and neither are two months")))

(deftest fleetkeep-the-row-count-is-capped-and-said ()
  (let ((result (nodecode-fleetkeep:review +messy-csv+ :max-rows 2)))
    (is (= 2 (getf result :rows)) "two rows read")
    (is (= 3 (getf result :truncated)) "three passed over")
    (is-carrying (text (getf result :report)) ("the first 3 of them"))))
