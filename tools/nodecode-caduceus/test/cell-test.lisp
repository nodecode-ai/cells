;;;; cell-test.lisp --- the caduceus cell against a fixture vault.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every test copies the four fixtures into a scratch folder and points
;;;; data_dir at it, so nothing here reads the operator's own vault, and the
;;;; whole slice runs with no network at all (the cell dials nothing). Each
;;;; tool is called the way a model's call reaches it - through the real
;;;; dispatch, contract and all - so the schema the provider is shown and the
;;;; body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "caduceus" "CADUCEUS-CELL-")

(defvar *vault* nil
  "The vault folder the test at hand points the cell at.")

(defvar *fixtures* nil
  "Where the test at hand copied the fixtures.")

(defparameter +fixture-names+ '("23andme.txt" "promethease.tsv" "export.xml" "observations.json")
  "The fixtures, copied into the scratch folder for every test.")

(defmacro with-vault (&body body)
  "BODY with a fresh vault folder and the fixtures copied beside it; the
folder is deleted on the way out."
  (let ((dir (gensym "DIR")))
    `(let* ((,dir (merge-pathnames (format nil "caduceus-test-~d-~d/"
                                           (get-universal-time) (random 1000000))
                                    (uiop:temporary-directory)))
            (vault (merge-pathnames "vault/" ,dir))
            (fixtures (merge-pathnames "fixtures/" ,dir)))
       (ensure-directories-exist (merge-pathnames "keep.txt" vault))
       (ensure-directories-exist (merge-pathnames "keep.txt" fixtures))
       (dolist (name +fixture-names+)
         (uiop:copy-file (asdf:system-relative-pathname "nodecode-caduceus/test"
                                                         (format nil "test/fixtures/~a" name))
                          (merge-pathnames name fixtures)))
       (let ((*vault* (namestring vault))
             (*fixtures* (namestring fixtures)))
         (unwind-protect (progn ,@body)
           (ignore-errors (uiop:delete-directory-tree ,dir :validate t)))))))

(defun caduceus-config (&rest pairs)
  "A whole shared config with one `caduceus' section built from PAIRS, its
data_dir defaulted to the test's vault."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "data_dir" section *vault*)
    (nlk:json-object "caduceus" section)))

(defun caduceus-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-caduceus:start-cell (apply #'caduceus-config pairs)))

(defun ca-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real
dispatch, the declaration and all."
  (nle::message-content
   (execute-wire-call id tool
                      (if (stringp arguments) arguments (nlk:encode-json-object arguments)))))

(defun ca-object (id tool arguments)
  "TOOL's answer, decoded."
  (nlk:decode-json (ca-answer id tool arguments)))

(defun ca-import (id name)
  "Import the fixture NAME through the tool, as a model would."
  (ca-object id "caduceus_capture"
             (nlk:json-object "op" "import"
                              "path" (namestring (merge-pathnames name *fixtures*)))))

(defun ca-named (array name value)
  "The entry of ARRAY whose key NAME is VALUE, or NIL."
  (loop for index from 0 below (length array)
        for entry = (aref array index)
        when (equal value (nlk:json-value entry :string name))
          return entry))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "caduceus"
  (:config "data_dir" *vault*)
  (:help :caduceus)
  (:command "caduceus")
  (:fixture with-vault)
  (:refused ("privacy_level" "verbose") ("hidden_categories" '("nonsense"))))

(deftest caduceus-cell-without-a-vault-refuses (with-vault)
  ;; The one setting no verb can work without is the folder the vault lives
  ;; in: a section that names none is refused at start, before anything
  ;; installs, and the refusal says what to set.
  (let ((condition (signals-error nlk:config-refusal (caduceus-start "data_dir" ""))))
    (is (search "no data_dir" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "caduceus_genome")) "and installs nothing")))

;;; --- caduceus_capture op=import ------------------------------------------------------

(deftest caduceus-cell-imports-a-raw-data-file (with-vault)
  (with-cell-stop ((caduceus-start))
    (let ((answer (ca-import "ca-import-1" "23andme.txt")))
      (is (plusp (nlk:json-value answer :integer "markers")) "the raw data file's markers are stored")
      (is (plusp (nlk:json-value answer :integer "called")) "and they are called"))
    (let ((summary (ca-object "ca-import-2" "caduceus_genome" (nlk:json-object "op" "summary"))))
      (is (plusp (nlk:json-value summary :integer "markers_called")))
      (is (plusp (length (nlk:json-array summary "top_findings"))) "the pack answers at least one finding"))))

(deftest caduceus-cell-refuses-a-relative-path (with-vault)
  (with-cell-stop ((caduceus-start))
    (let ((text (ca-answer "ca-import-3" "caduceus_capture"
                           (nlk:json-object "op" "import" "path" "23andme.txt"))))
      (is (search "absolute path" text) "a relative path is refused with what to give"))))

(deftest caduceus-cell-imports-a-promethease-table (with-vault)
  (with-cell-stop ((caduceus-start))
    (let ((answer (ca-import "ca-import-4" "promethease.tsv"))
          (search (ca-object "ca-search-1" "caduceus_search"
                             (nlk:json-object "query" "sickle"))))
      (is (= 2 (nlk:json-value answer :integer "annotations")) "both report rows are stored")
      (is (plusp (length (nlk:json-array search "results"))) "and they are searchable")
      (is (search "promethease" (nlk:json-value (aref (nlk:json-array search "results") 0)
                                                 :string "ref"))))))

;;; --- caduceus_genome -----------------------------------------------------------------

(deftest caduceus-cell-looks-a-marker-up (with-vault)
  (with-cell-stop ((caduceus-start))
    (ca-import "ca-genome-0" "23andme.txt")
    (let* ((answer (ca-object "ca-genome-1" "caduceus_genome"
                               (nlk:json-object "op" "lookup" "rsids" '("rs4988235"))))
           (marker (aref (nlk:json-array answer "markers") 0)))
      (is (equal "called" (nlk:json-value marker :string "state")))
      (is (equal "TT" (nlk:json-value marker :string "genotype")))
      (is (search "lactase" (nlk:json-value marker :string "finding")) "the finding fits the call"))))

(deftest caduceus-cell-answers-gene-drug-evidence (with-vault)
  (with-cell-stop ((caduceus-start))
    (ca-import "ca-pgx-0" "23andme.txt")
    (let* ((answer (ca-object "ca-pgx-1" "caduceus_genome"
                               (nlk:json-object "op" "pgx" "drug" "Plavix")))
           (entry (ca-named (nlk:json-array answer "entries") "gene" "CYP2C19")))
      (is (equal "clopidogrel" (nlk:json-value answer :string "drug")) "a brand name resolves to its generic")
      (is (equal "flag" (nlk:json-value entry :string "status")) "the called *2 allele is flagged")
      (is (search "not assessed" (string-downcase (nlk:json-value entry :string "finding"))) "a marker the chip lacks keeps the answer honest")
      (is (plusp (length (nlk:json-value entry :string "disclaimer")))))))

(deftest caduceus-cell-derives-apoe (with-vault)
  (with-cell-stop ((caduceus-start))
    (ca-import "ca-apoe-0" "23andme.txt")
    (let* ((answer (ca-object "ca-apoe-1" "caduceus_genome" (nlk:json-object "op" "derived")))
           (apoe (ca-named (nlk:json-array answer "calls") "gene" "APOE")))
      (is (equal "e3/e3" (aref (nlk:json-array apoe "genotypes") 0)) "two T calls at both APOE markers are e3/e3")
      (is (search "e4" (nlk:json-value apoe :string "interpretation")) "the interpretation names the pair"))))

;;; --- caduceus_trend ------------------------------------------------------------------

(deftest caduceus-cell-counts-a-health-export (with-vault)
  (with-cell-stop ((caduceus-start))
    (let ((import (ca-import "ca-trend-0" "export.xml"))
          (metrics (ca-object "ca-trend-1" "caduceus_trend" (nlk:json-object "op" "metrics"))))
      (is (plusp (nlk:json-value import :integer "metric_days")) "the export's days are stored")
      (is (plusp (length (nlk:json-array metrics "metrics"))))
      (is (plusp (nlk:json-value (ca-named (nlk:json-array metrics "metrics") "metric" "steps") :integer "days")) "steps cover three days"))))

(deftest caduceus-cell-buckets-a-series (with-vault)
  (with-cell-stop ((caduceus-start))
    (ca-import "ca-trend-2" "export.xml")
    (let* ((answer (ca-object "ca-trend-3" "caduceus_trend"
                               (nlk:json-object "op" "series" "metric" "steps" "bucket" "day")))
           (buckets (nlk:json-array answer "buckets"))
           (first-day (aref buckets 0)))
      (is (= 3 (length buckets)) "one bucket a day, three days")
      (is (= 3000 (nlk:json-value first-day :number "mean")) "the day's step records are summed")
      (is (equal "count" (nlk:json-value answer :string "unit")) "the series carries the metric's unit")
      (is (= 1 (nlk:json-value (ca-named buckets "bucket" "2026-01-01") :integer "count"))))))

(deftest caduceus-cell-imports-fhir-observations (with-vault)
  (with-cell-stop ((caduceus-start))
    (let* ((import (ca-import "ca-trend-4" "observations.json"))
           (metrics (ca-object "ca-trend-5" "caduceus_trend" (nlk:json-object "op" "metrics")))
           (a1c (ca-named (nlk:json-array metrics "metrics") "metric" "hemoglobin-a1c")))
      (is (= 2 (nlk:json-value import :integer "observations")) "both observations are stored")
      (is (plusp (nlk:json-value a1c :integer "samples")))
      (is (equal "2026-01-02" (nlk:json-value a1c :string "first_day"))))))

(deftest caduceus-cell-compares-around-a-date (with-vault)
  (with-cell-stop ((caduceus-start))
    (ca-import "ca-trend-6" "export.xml")
    (let ((answer (ca-object "ca-trend-7" "caduceus_trend"
                              (nlk:json-object "op" "compare" "metric" "steps"
                                               "pivot" "2026-01-02"
                                               "before_days" 1 "after_days" 1))))
      (is (= 3000 (nlk:json-value (nlk:json-value answer :object "before") :number "mean")))
      (is (= 3500 (nlk:json-value (nlk:json-value answer :object "after") :number "mean")))
      (is (= 500 (nlk:json-value answer :number "change")))
      (is (search "not a cause" (nlk:json-value answer :string "note"))))))

;;; --- caduceus_search -----------------------------------------------------------------

(deftest caduceus-cell-searches-and-withholds (with-vault)
  (with-cell-stop ((caduceus-start "privacy_level" "standard"))
    (ca-import "ca-search-2" "23andme.txt")
    (let ((open (ca-object "ca-search-3" "caduceus_search"
                            (nlk:json-object "query" "lactose")))
          (hidden (ca-object "ca-search-4" "caduceus_search"
                              (nlk:json-object "query" "factor v leiden"))))
      (is (plusp (length (nlk:json-array open "results"))) "a common finding is found")
      (is (zerop (length (nlk:json-array hidden "results"))) "a carrier finding stays hidden at the standard level")
      (is (plusp (nlk:json-value hidden :integer "withheld")) "and is counted, never described")
      (is (search "privacy" (nlk:json-value hidden :string "note")) "the note says where it went"))))

(deftest caduceus-cell-fetches-one-record-by-ref (with-vault)
  (with-cell-stop ((caduceus-start))
    (ca-import "ca-search-5" "23andme.txt")
    (let ((record (ca-object "ca-search-6" "caduceus_search"
                             (nlk:json-object "ref" "annotation:rs4988235"))))
      (is (equal "annotation" (nlk:json-value record :string "kind")))
      (is (search "lactase" (nlk:json-value record :string "body")) "the whole record comes back"))))

;;; --- notes and medications ------------------------------------------------------------

(deftest caduceus-cell-stores-a-journal-note (with-vault)
  (with-cell-stop ((caduceus-start))
    (let ((answer (ca-object "ca-note-1" "caduceus_capture"
                              (nlk:json-object "op" "journal"
                                               "text" "Started a walking habit, 8000 steps.")))
          (found (ca-object "ca-note-2" "caduceus_search"
                             (nlk:json-object "query" "walking habit"))))
      (is (eq t (nlk:json-value answer :boolean "stored")))
      (is (plusp (length (nlk:json-array found "results"))) "the note is searchable"))))

(deftest caduceus-cell-medication-needs-confirmation-first (with-vault)
  (with-cell-stop ((caduceus-start))
    (is (search "agree first"
                (ca-answer "ca-med-0" "caduceus_capture"
                           (nlk:json-object "op" "medication" "drug" "semaglutide"
                                            "event" "start"))))))


(deftest caduceus-cell-asks-before-recording-a-medication (with-vault)
  (with-cell-stop ((caduceus-start))
    (let ((unconfirmed (ca-answer "ca-med-1" "caduceus_capture"
                                  (nlk:json-object "op" "medication" "drug" "semaglutide"
                                                   "event" "start" "dose" "0.25 mg")))
          (confirmed (ca-object "ca-med-2" "caduceus_capture"
                                (nlk:json-object "op" "medication" "drug" "semaglutide"
                                                 "event" "start" "dose" "0.25 mg"
                                                 "date" "2026-02-01" "confirm" t))))
      (is (search "agree first" unconfirmed) "without confirm=true nothing is written")
      (is (eq t (nlk:json-value confirmed :boolean "recorded")))
      (is (equal "2026-02-01" (nlk:json-value confirmed :string "date"))))))

;;; --- the command, and what the cell says about itself ---------------------------------

(deftest caduceus-cell-command-answers-status (with-vault)
  (with-cell-stop ((caduceus-start))
    (ca-import "ca-cmd-0" "23andme.txt")
    (let ((text (cell-entry "nodecode-caduceus" "caduceus" "")))
      (is (search "Caduceus vault" text) "the command answers the vault")
      (is (search "markers" text) "with what it holds"))))

(deftest caduceus-cell-settings-answer-the-policy (with-vault)
  (with-cell-stop ((caduceus-start "hidden_categories" (vector "mental_health")))
    (let ((settings (ca-object "ca-set-1" "caduceus_capture" (nlk:json-object "op" "settings"))))
      (is (equal "open" (nlk:json-value settings :string "privacy_level")))
      (is (find "mental_health" (nlk:json-array settings "hidden_categories") :test #'equal)))))
