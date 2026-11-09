;;;; cell-test.lisp --- the apple-health cell against a fixture export.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The export is a file every test copies into a scratch directory of its
;;;; own, so nothing here reads the operator's Health export: test/export.xml
;;;; holds three days of records and one workout. Each tool is called the way
;;;; a model's call reaches it - through the real dispatch, contract and all -
;;;; so the schema the provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "apple-health" "APPLE-HEALTH-CELL-")

(defvar *export* nil
  "The fixture export.xml the test at hand points the cell at.")

(defparameter +fixture-relative+ "test/export.xml"
  "The fixture, relative to the system's .asd.")

(defmacro with-export (&body body)
  "BODY with *EXPORT* naming a fresh copy of the fixture export, in a scratch
directory this unwinds."
  (let ((dir (gensym "DIR")) (file (gensym "FILE")))
    `(let* ((,dir (merge-pathnames (format nil "apple-health-test-~d-~d/"
                                           (get-universal-time) (random 1000000))
                                    (uiop:temporary-directory)))
            (,file (merge-pathnames "export.xml" ,dir)))
       (ensure-directories-exist ,file)
       (uiop:copy-file (asdf:system-relative-pathname "nodecode-apple-health/test" +fixture-relative+)
                       ,file)
       (let ((*export* (namestring ,file)))
         (unwind-protect (progn ,@body)
           (ignore-errors (uiop:delete-directory-tree ,dir :validate t)))))))

(defun apple-health-config (&rest pairs)
  "A whole shared config holding the `applehealth' section PAIRS make, with
export_path defaulted to the fixture."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "export_path" section *export*)
    (nlk:json-object "applehealth" section)))

(defun apple-health-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-apple-health:start-cell (apply #'apple-health-config pairs)))

(defun ah-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun ah-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (ah-answer id tool arguments)))

(defun ah-named (array name)
  "The entry of ARRAY whose \"name\" is NAME, or NIL."
  (loop for index from 0 below (length array)
        for entry = (aref array index)
        when (equal name (nlk:json-value entry :string "name"))
          return entry))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "apple-health"
  (:config "export_path" *export*)
  (:help :apple-health)
  (:command "health")
  (:fixture with-export)
  (:refused ("privacy_mode" "verbose") ("max_records" 0)))

(deftest apple-health-cell-without-an-export-refuses (with-export)
  ;; The one setting no verb can work without: a section that names no export
  ;; is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (apple-health-start "export_path" ""))))
    (is (search "no export_path" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "apple_health_daily_summary")) "and installs nothing")))

(deftest apple-health-cell-finds-the-export-in-a-folder (with-export)
  ;; Pointing export_path at the unzipped folder is the same as pointing it at
  ;; the file: the cell resolves export.xml inside it.
  (with-cell-stop ((apple-health-start "export_path"
                                         (namestring (uiop:pathname-directory-pathname *export*))))
    (let ((object (ah-object "ah-folder-1" "apple_health_connection_status" "{}")))
      (is (eq t (nlk:json-value object :boolean "export_exists")) "the folder resolved to its export.xml"))))

;;; --- apple_health_connection_status -------------------------------------------------

(deftest apple-health-cell-answers-status (with-export)
  (with-cell-stop ((apple-health-start))
    (let ((object (ah-object "ah-status-1" "apple_health_connection_status" "{}")))
      (is (eq t (nlk:json-value object :boolean "ok")) "the export is there")
      (is (eq t (nlk:json-value object :boolean "export_exists")))
      (is (plusp (nlk:json-value object :integer "size_bytes")) "and its size is read")
      (is (equal "structured" (nlk:json-value object :string "privacy_mode")))
      (is (plusp (length (nlk:json-array object "next_steps"))) "with a next step"))))

(deftest apple-health-cell-status-without-a-file (with-export)
  (with-cell-stop ((apple-health-start "export_path" "/nonexistent/apple-health/export.xml"))
    (let ((object (ah-object "ah-status-2" "apple_health_connection_status" "{}")))
      (is (null (nlk:json-value object :boolean "ok")) "no file, not ok")
      (is (search "Export All Health Data" (nlk:json-value object :string "summary"))
          "and the answer says how to make one"))))

;;; --- apple_health_data_inventory ----------------------------------------------------

(deftest apple-health-cell-inventories-the-export (with-export)
  (with-cell-stop ((apple-health-start))
    (let* ((object (ah-object "ah-inv-1" "apple_health_data_inventory" "{}"))
           (types (nlk:json-array object "record_types"))
           (steps (ah-named types "StepCount")))
      (is (= 12 (nlk:json-value object :integer "total_records")) "every record is counted")
      (is (= 1 (nlk:json-value object :integer "total_workouts")))
      (is (equal "2026-01-01" (nlk:json-value object :string "first_day")))
      (is (equal "2026-01-03" (nlk:json-value object :string "last_day")))
      (is (and steps (= 4 (nlk:json-value steps :integer "count"))) "the type prefix is stripped")
      (is (equal "Running" (nlk:json-value
                            (aref (nlk:json-array object "workout_activities") 0) :string "name"))))))

;;; --- apple_health_daily_summary -----------------------------------------------------

(deftest apple-health-cell-summarizes-a-day (with-export)
  (with-cell-stop ((apple-health-start))
    (let* ((object (ah-object "ah-day-1" "apple_health_daily_summary" "{\"date\":\"2026-01-01\"}"))
           (heart (nlk:json-value object :object "heart_rate"))
           (workouts (nlk:json-array object "workouts")))
      (is (equal "2026-01-01" (nlk:json-value object :string "date")))
      (is (= 3000 (nlk:json-value object :integer "steps")) "the day's step records are summed")
      (is (= 2.5 (nlk:json-value object :number "distance_km")))
      (is (= 250 (nlk:json-value object :integer "active_energy_kcal")))
      (is (= 70 (nlk:json-value heart :number "average")) "two heart-rate readings average")
      (is (= 60 (nlk:json-value heart :integer "min")))
      (is (= 80 (nlk:json-value heart :integer "max")))
      (is (= 8 (nlk:json-value object :number "sleep_hours")) "eight hours of asleep records")
      (is (= 1 (length workouts)) "and the day's workout")
      (is (equal "Running" (nlk:json-value (aref workouts 0) :string "activity")))
      (is (= 30 (nlk:json-value (aref workouts 0) :number "minutes"))))))

(deftest apple-health-cell-summarizes-the-latest-day (with-export)
  (with-cell-stop ((apple-health-start))
    (let ((object (ah-object "ah-day-2" "apple_health_daily_summary" "{}")))
      (is (equal "2026-01-03" (nlk:json-value object :string "date")) "no date means the last day the export covers")
      (is (= 4000 (nlk:json-value object :integer "steps")))
      (is (plusp (length (nlk:json-value object :string "disclosure"))) "and the answer says where its numbers come from"))))

;;; --- apple_health_weekly_summary ----------------------------------------------------

(deftest apple-health-cell-summarizes-a-week (with-export)
  (with-cell-stop ((apple-health-start))
    (let* ((object (ah-object "ah-week-1" "apple_health_weekly_summary" "{\"end_date\":\"2026-01-03\",\"days\":3}"))
           (days (nlk:json-array object "days"))
           (totals (nlk:json-value object :object "totals")))
      (is (= 3 (length days)) "three days, oldest first")
      (is (equal "2026-01-01" (nlk:json-value (aref days 0) :string "date")))
      (is (equal "2026-01-03" (nlk:json-value (aref days 2) :string "date")))
      (is (= 10000 (nlk:json-value totals :integer "steps")) "the totals sum the window")
      (is (= 1 (nlk:json-value totals :integer "workouts")))
      (is (= 8 (nlk:json-value totals :number "sleep_hours"))))))

(deftest apple-health-cell-week-defaults-to-seven-days (with-export)
  (with-cell-stop ((apple-health-start))
    (let* ((object (ah-object "ah-week-2" "apple_health_weekly_summary" "{}"))
           (days (nlk:json-array object "days")))
      (is (= 7 (length days)) "seven days by default")
      (is (equal "2026-01-03" (nlk:json-value object :string "end_date"))))))

;;; --- apple_health_records -----------------------------------------------------------

(deftest apple-health-cell-lists-and-caps-records (with-export)
  (with-cell-stop ((apple-health-start))
    (let* ((object (ah-object "ah-rec-1" "apple_health_records" "{\"type\":\"StepCount\",\"limit\":2}"))
           (records (nlk:json-array object "records")))
      (is (= 2 (nlk:json-value object :integer "count")) "the per-call limit wins")
      (is (= 4 (nlk:json-value object :integer "matched")) "and the answer says how many there were")
      (is (eq t (nlk:json-value object :boolean "truncated")))
      (is (search "kept the first 2 of 4" (nlk:json-value object :string "truncated_note")))
      (is (equal "StepCount" (nlk:json-value (aref records 0) :string "type")))
      (is (null (nlk:json-value (aref records 0) :string "source")) "structured mode leaves the source out"))))

(deftest apple-health-cell-raw-mode-names-the-source (with-export)
  (with-cell-stop ((apple-health-start "privacy_mode" "raw"))
    (let* ((object (ah-object "ah-rec-2" "apple_health_records" "{\"type\":\"StepCount\",\"limit\":1}"))
           (record (aref (nlk:json-array object "records") 0)))
      (is (equal "raw" (nlk:json-value object :string "privacy_mode")))
      (is (equal "iPhone" (nlk:json-value record :string "source")) "raw mode answers the source name")
      (is (search "+0000" (nlk:json-value record :string "start")) "and the export's own timestamps"))))

(deftest apple-health-cell-records-filter-by-day (with-export)
  (with-cell-stop ((apple-health-start))
    (let ((object (ah-object "ah-rec-3" "apple_health_records"
                             "{\"type\":\"StepCount\",\"start_date\":\"2026-01-02\",\"end_date\":\"2026-01-02\"}")))
      (is (= 1 (nlk:json-value object :integer "matched")) "only the second day's step record")
      (is (= 3000 (nlk:json-value (aref (nlk:json-array object "records") 0) :integer "value"))))))

;;; --- apple_health_workouts -----------------------------------------------------------

(deftest apple-health-cell-lists-workouts (with-export)
  (with-cell-stop ((apple-health-start))
    (let* ((object (ah-object "ah-wo-1" "apple_health_workouts" "{}"))
           (workouts (nlk:json-array object "workouts"))
           (workout (aref workouts 0)))
      (is (= 1 (nlk:json-value object :integer "count")))
      (is (equal "Running" (nlk:json-value workout :string "activity")))
      (is (= 30 (nlk:json-value workout :number "minutes")))
      (is (= 5 (nlk:json-value workout :number "distance_km")))
      (is (= 300 (nlk:json-value workout :integer "energy_kcal"))))))

(deftest apple-health-cell-workouts-outside-the-window (with-export)
  (with-cell-stop ((apple-health-start))
    (let ((object (ah-object "ah-wo-2" "apple_health_workouts"
                             "{\"start_date\":\"2026-01-02\",\"end_date\":\"2026-01-03\"}")))
      (is (zerop (nlk:json-value object :integer "count")) "no workout in those days"))))

;;; --- apple_health_privacy_audit, and /health -----------------------------------------

(deftest apple-health-cell-audits-its-own-posture (with-export)
  (with-cell-stop ((apple-health-start))
    (let ((object (ah-object "ah-audit-1" "apple_health_privacy_audit" "{}")))
      (is (null (nlk:json-value object :boolean "network")) "nothing leaves the machine")
      (is (search "export" (nlk:json-value object :string "reads")))
      (is (plusp (length (nlk:json-array object "notes")))))))

(deftest apple-health-cell-health-command-answers (with-export)
  (with-cell-stop ((apple-health-start))
    (let ((text (cell-entry "nodecode-apple-health" "health" "3")))
      (is (search "Apple Health" text) "the command answers the window as text")
      (is (search "2026-01-03" text))
      (is (search "total  steps 10000" text) "with the totals line"))))
