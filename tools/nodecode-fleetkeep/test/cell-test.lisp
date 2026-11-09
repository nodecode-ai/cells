;;;; cell-test.lisp --- lifecycle; the tool through a call; /fleetkeep on a file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the start installs the tool, /fleetkeep and the
;;;; manual, and the stop takes all three back; a section missing the one
;;;; required setting refuses, as does one whose shapes are wrong; the tool
;;;; answers the review a call asks for and writes exactly the two files, and
;;;; writes none when it is told not to; and /fleetkeep bare reads the settings
;;;; back while /fleetkeep FILE.csv reviews that file and writes its two.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "fleetkeep"
  (:config "output_dir" (fleetkeep-out))
  (:help :fleetkeep)
  (:command "fleetkeep")
  (:running
   (is (equal "day-first" (nodecode-fleetkeep::setting :date-order)) "an ambiguous date is named day-first first")
   (is (eq t (nodecode-fleetkeep::setting :formula-guard)) "the formula guard is on")
   (is (= 5000 (nodecode-fleetkeep::setting :max-rows)) "five thousand rows a review"))
  (:refused ("max_rows" 0)
            ("date_order" "sideways")))

(deftest fleetkeep-cell-a-review-with-no-output-dir-refuses-naming-it ()
  ;; The section's :required is the setup panel's gate, not a start refusal: the
  ;; cell runs and the verb that must write is what refuses, naming the setting.
  (with-cell-stop ((fleetkeep-start))
    (let ((condition (signals-error nodecode-fleetkeep:fleetkeep-error
                        (nodecode-fleetkeep::review-text +clean-csv+))))
      (is condition "a review that must write with no output_dir refuses")
      (when condition
        (is-carrying (text (princ-to-string condition)) ("output_dir"))))))

(deftest fleetkeep-cell-the-tool-reviews-and-writes-two-files ()
  (with-cell-stop ((fleetkeep-start "output_dir" (fleetkeep-out)))
    (let ((answer (nle:message-content
                   (execute-wire-call "fk-tool-1" "fleetkeep_review"
                                      (format nil "{\"csv\": ~a, \"name\": \"messy\"}"
                                              (fleetkeep-json-string +messy-csv+))))))
      (is-carrying (text answer)
        ("fleetkeep: 5 rows read") ("4 rows import-ready") ("1 held out for review")
        ("messy.import.csv") ("row 2 · insurance_due"))
      (let ((written (fleetkeep-read "messy.import.csv")))
        (is-present written "the import-ready CSV was written"
          (is (= 5 (length (nodecode-fleetkeep:parse-csv written))) "five records: the header and four rows")
          (is (null (search "HYPERLINK" written)) "the row a formula held back is not in it")
          (is (search "AB12 CDE" written) "and a real registration is")))
      (let ((review (fleetkeep-read "messy.review.md")))
        (is-present review "and so was the review"
          (is (search "row 4 · registration: no registration" review) "which names the rows to look at"))))))

(deftest fleetkeep-cell-the-tool-can-answer-without-writing ()
  (with-cell-stop ((fleetkeep-start "output_dir" (fleetkeep-out)))
    (let ((answer (nle:message-content
                   (execute-wire-call "fk-tool-2" "fleetkeep_review"
                                      (format nil "{\"csv\": ~a, \"name\": \"quiet\", \"save\": false}"
                                              (fleetkeep-json-string +clean-csv+))))))
      (is-carrying (text answer) ("fleetkeep: 1 row read") ("0 issues") (:absent "wrote"))
      (is (null (fleetkeep-read "quiet.import.csv")) "nothing was written"))))

(deftest fleetkeep-cell-the-command-reads-a-file-and-the-settings ()
  (with-cell-stop ((fleetkeep-start "output_dir" (fleetkeep-out)))
    (let ((path (fleetkeep-file "sample.csv" +messy-csv+)))
      (is-carrying (text (cell-entry "nodecode-fleetkeep" "fleetkeep" path nil))
        ("fleetkeep: 5 rows read") ("sample.import.csv") ("sample.review.md"))
      (is (fleetkeep-read "sample.import.csv") "the import-ready CSV is on disk")
      (is-carrying (text (cell-entry "nodecode-fleetkeep" "fleetkeep" "" nil))
        ("fleetkeep: the proposed CSV and the review go to") ("/fleetkeep FILE.csv"))
      (is (search "no file at"
                  (cell-entry "nodecode-fleetkeep" "fleetkeep" "no/such/file.csv" nil)) "a path that holds no file is said, not guessed at"))))

(deftest fleetkeep-cell-a-formula-holds-its-row-back-until-the-guard-is-off ()
  (with-cell-stop ((fleetkeep-start "output_dir" (fleetkeep-out) "formula_guard" nil))
    (let ((answer (nle:message-content
                   (execute-wire-call "fk-tool-3" "fleetkeep_review"
                                      (format nil "{\"csv\": ~a, \"name\": \"all\"}"
                                              (fleetkeep-json-string +messy-csv+))))))
      (is-carrying (text answer) ("5 rows import-ready") (:absent "held out"))
      (let ((written (fleetkeep-read "all.import.csv")))
        (is-present written "the file was written with every row"
          (is (search "HYPERLINK" written) "the formula cell among them"))))))
