;;;; cell-test.lisp --- the profiles, the parent's gate, the analysis, the plan,
;;;; the reassessment, the delete, and the file that keeps them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own contract: the cell starts and stops,
;;;; each tool answers through the seam the model calls it by, a candidate is
;;;; not a question until a parent confirms it, a batch of scores is written
;;;; whole or not at all, one error stays suspected, an independent failure is
;;;; evidence and a hinted pass moves nothing, the plan carries its action and
;;;; its reassessment day, a delete needs the display name, and the records
;;;; outlive a restart because one file holds them. Every call runs against a
;;;; records file in the test's own home, so nothing touches a real record.

(in-package #:nodecode.test)

(define-test-slice "good-student" "GOOD-STUDENT-CELL-" :start nodecode-good-student:start-cell)

;;; --- fixtures -----------------------------------------------------------------

(defun gs-test-folder ()
  "The folder one test's records file is kept in, inside the test's own home."
  (namestring (nlk:home "good-student-test/")))

(defun gs-records-file ()
  (merge-pathnames "records.json" (nlk:home "good-student-test/")))

(defun gs-empty ()
  "One test's records: the file removed, so the test starts from none."
  (when (probe-file (gs-records-file)) (delete-file (gs-records-file))))

(defun gs-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun gs-json (text) (nlk:decode-json text))

(defun gs-created (arguments)
  "Create a profile through the tool and answer its id."
  (let ((answer (gs-json (gs-tool "good_student_create_student" arguments))))
    (nlk:json-value (nlk:json-value answer :object "student") :string "student_id")))

(defun gs-score (subject name at score max)
  (nlk:json-object "subject" subject "assessment_name" name "assessed_at" at
                    "assessment_type" "exam" "score" score "max_score" max))

(defun gs-candidate (text kc source)
  (nlk:json-object "question_text" text "knowledge_point" kc
                    "subject" "math" "error_reason" "procedure_gap" "source_ref" source))

(defun gs-file-candidate (student-id)
  "Put one candidate in the waiting area and answer its id."
  (let ((answer (gs-tool "good_student_ingest_candidates"
                          (nlk:json-object
                           "student_id" student-id
                           "batch" (nlk:json-object
                                    "questions" (vector (gs-candidate "3/4 - 1/2 = ?"
                                                                      "subtracting fractions"
                                                                      "sheet 1")))))))
    (nlk:json-value (gs-json answer) :array "candidates")))

(defun gs-confirm (student-id candidate-id)
  (gs-tool "good_student_confirm_questions"
           (nlk:json-object "student_id" student-id
                            "items" (vector (nlk:json-object "candidate_id" candidate-id
                                                             "action" "confirm")))))

;;; --- the lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "good-student"
  (:config "data_dir" (gs-test-folder))
  (:help :good-student)
  (:command "good-student")
  (:running
   (is (nle::find-tool "good_student_list_students") "the profiles tool is registered")
   (is (nle::find-tool "good_student_create_student"))
   (is (nle::find-tool "good_student_record_scores"))
   (is (nle::find-tool "good_student_list_scores"))
   (is (nle::find-tool "good_student_ingest_candidates"))
   (is (nle::find-tool "good_student_list_pending"))
   (is (nle::find-tool "good_student_confirm_questions"))
   (is (nle::find-tool "good_student_analyze"))
   (is (nle::find-tool "good_student_create_plan"))
   (is (nle::find-tool "good_student_record_reassessment"))
   (is (nle::find-tool "good_student_weekly_brief"))
   (is (nle::find-tool "good_student_delete_student"))
   (is (= 80 (nodecode-good-student::setting :pass-percent)) "the pass line defaults to 80 percent")
   (is (= 14 (nodecode-good-student::setting :review-days)) "a mastered point stands for a fortnight"))
  (:stopped (is (null (nle::find-tool "good_student_analyze")) "stop unregisters the tools"))
  (:refused ("pass_percent" 0))
  (:idle nodecode-good-student:good-student-error (nodecode-good-student::run-command "" nil)))

;;; --- profiles and scores ------------------------------------------------------------

(deftest good-student-cell-a-profile-and-a-batch-of-scores ()
  (gs-empty)
  (with-cell-stop ((good-student-start "data_dir" (gs-test-folder)))
    (let ((id (gs-created (nlk:json-object "display_name" "Ada" "grade" "Grade 5"
                                            "active_subjects" (vector "math" "reading")))))
      (is (stringp id) "a profile is created and answers its id")
      (let ((listed (gs-tool "good_student_list_students" (nlk:json-object))))
        (is-carrying listed ("Ada" "the profile is listed")))
      (let ((twice (gs-tool "good_student_create_student" (nlk:json-object "display_name" "ada"))))
        (is-carrying twice ("ERROR: good-student:" "a second profile is refused")
                           "already here"))
      (let ((written (gs-tool "good_student_record_scores"
                              (nlk:json-object
                               "student_id" id
                               "records" (vector (gs-score "math" "Unit 1" "2026-09-01T09:00:00Z" 18 20)
                                                 (gs-score "reading" "Unit 1" "2026-09-02T09:00:00Z" 8 10))))))
        (is-carrying written ("\"recorded\":2" "both scores are written")))
      (let ((listed (gs-tool "good_student_list_scores"
                             (nlk:json-object "student_id" id "subject" "math"))))
        (is-carrying listed ("\"total\":1" "the filter keeps one subject")
                     ("\"average_percent\":90" "and the subject is summarised as a percentage")))
      ;; a batch with one bad record writes neither: all or none, as the original has it
      (let ((refused (gs-tool "good_student_record_scores"
                              (nlk:json-object
                               "student_id" id
                               "records" (vector (gs-score "math" "Unit 2" "2026-09-08T09:00:00Z" 19 20)
                                                 (nlk:json-object "subject" "math"
                                                                  "assessment_name" "Unit 3"
                                                                  "assessed_at" "2026-09-09T09:00:00Z"))))))
        (is-carrying refused ("ERROR: good-student:" "a bad record refuses the batch")
                             "grade label"))
      (let ((after (gs-tool "good_student_list_scores" (nlk:json-object "student_id" id))))
        (is-carrying after ("\"total\":2" "the bad batch left the two written scores alone"))))))

;;; --- the parent's gate, the analysis and the plan -----------------------------------

(deftest good-student-cell-candidates-are-not-questions-until-a-parent-confirms ()
  (gs-empty)
  (with-cell-stop ((good-student-start "data_dir" (gs-test-folder)))
    (let* ((id (gs-created (nlk:json-object "display_name" "Bo")))
           (batch (nlk:json-object
                   "questions" (vector (gs-candidate "1/2 + 1/3 = ?" "adding fractions" "worksheet 4")
                                       (gs-candidate "2/3 + 1/6 = ?" "adding fractions" "test 2"))))
           (ingested (gs-tool "good_student_ingest_candidates"
                              (nlk:json-object "student_id" id "batch" batch))))
      (is-carrying ingested ("\"candidates\":[" "both candidates are waiting"))
      (let ((before (gs-tool "good_student_analyze" (nlk:json-object "student_id" id))))
        (is-carrying before ("\"insufficient\":true" "a candidate is not evidence yet")))
      (let* ((pending (gs-json (gs-tool "good_student_list_pending" (nlk:json-object "student_id" id))))
             (items (nlk:json-array pending "pending")))
        (is (= 2 (length items)) "both are pending")
        (let ((confirmed (gs-confirm id (nlk:json-value (aref items 0) :string "candidate_id"))))
          (is-carrying confirmed ("\"confirmed\":1" "a parent confirms it")))
        (let ((again (gs-tool "good_student_list_pending" (nlk:json-object "student_id" id))))
          (is-carrying again ("\"total\":1" "one waits, one is confirmed"))))
      (let ((analysis (gs-tool "good_student_analyze" (nlk:json-object "student_id" id))))
        (is-carrying analysis ("\"insufficient\":false" "the confirmed question is evidence")
                     ("\"status\":\"suspected\"" "one knowledge point, suspected, not a verdict")
                     ("\"kc_id\":\"adding-fractions\"" "the point answers an id a reassessment names")))
      (let ((plan (gs-tool "good_student_create_plan" (nlk:json-object "student_id" id))))
        (is-carrying plan ("\"actions\":[" "the plan carries an action")
                     ("write the steps out as a template" "the template the error reason names")
                     ("\"reassess_on\":\"20" "and the day to reassess on"))))))

;;; --- the reassessment ---------------------------------------------------------------

(deftest good-student-cell-only-an-independent-reassessment-moves-a-point ()
  (gs-empty)
  (with-cell-stop ((good-student-start "data_dir" (gs-test-folder)))
    (let* ((id (gs-created (nlk:json-object "display_name" "Cy")))
           (candidates (gs-file-candidate id)))
      (gs-confirm id (aref candidates 0))
      (let ((failed (gs-tool "good_student_record_reassessment"
                             (nlk:json-object "student_id" id "kc_id" "subtracting-fractions"
                                              "correct_count" 2 "total_count" 5))))
        (is-carrying failed ("\"passed\":false" "two of five is under the pass line")
                     ("\"status\":\"evidenced\"" "an independent failure is evidence")))
      (let ((hinted (gs-tool "good_student_record_reassessment"
                             (nlk:json-object "student_id" id "kc_id" "subtracting-fractions"
                                              "correct_count" 5 "total_count" 5
                                              "is_new_variant" nil))))
        (is-carrying hinted ("\"passed\":true" "five of five passes")
                     ("\"independent\":false" "but a seen variant is no evidence")
                     ("\"status\":\"evidenced\"" "so the point does not advance")))
      (let ((unknown (gs-tool "good_student_record_reassessment"
                              (nlk:json-object "student_id" id "kc_id" "adding fractions"
                                               "correct_count" 1 "total_count" 1))))
        (is-carrying unknown ("ERROR: good-student:" "a point that is not there is refused")
                             "no confirmed knowledge point is"))
      (let ((brief (gs-tool "good_student_weekly_brief" (nlk:json-object "student_id" id))))
        (is-carrying brief ("\"by_status\"" "the brief reads the analysis")
                     ("\"reassessments\"" "and this week's reassessments"))))))

;;; --- delete, and the file that keeps everything -------------------------------------

(deftest good-student-cell-a-delete-needs-the-display-name ()
  (gs-empty)
  (with-cell-stop ((good-student-start "data_dir" (gs-test-folder)))
    (let ((id (gs-created (nlk:json-object "display_name" "Dee"))))
      (let ((scope (gs-tool "good_student_delete_student" (nlk:json-object "student_id" id))))
        (is-carrying scope ("\"confirmation_required\":true" "without a phrase it only says the scope")))
      (let ((wrong (gs-tool "good_student_delete_student"
                            (nlk:json-object "student_id" id "confirm_phrase" "someone else"))))
        (is-carrying wrong ("ERROR: good-student:" "a wrong phrase deletes nothing")
                           "must be the display name"))
      (let ((still (gs-tool "good_student_list_students" (nlk:json-object))))
        (is-carrying still ("Dee" "the profile is still there")))
      (let ((gone (gs-tool "good_student_delete_student"
                           (nlk:json-object "student_id" id "confirm_phrase" "Dee"))))
        (is-carrying gone ("\"deleted\":true" "the display name deletes it")))
      (let ((none (gs-tool "good_student_list_students" (nlk:json-object))))
        (is-carrying none ("\"total\":0" "and nothing is left"))))))

(deftest good-student-cell-the-records-outlive-a-restart ()
  (gs-empty)
  (with-cell-stop ((good-student-start "data_dir" (gs-test-folder)))
    (let ((id (gs-created (nlk:json-object "display_name" "Eve"))))
      (gs-tool "good_student_record_scores"
               (nlk:json-object "student_id" id
                                "records" (vector (gs-score "math" "Unit 1" "2026-09-03T09:00:00Z" 15 20))))))
  (with-cell-stop ((good-student-start "data_dir" (gs-test-folder)))
    (let ((after (gs-tool "good_student_list_students" (nlk:json-object))))
      (is-carrying after ("Eve" "the profile is still there after a restart")))
    (is (probe-file (gs-records-file)) "one file in the folder holds it")))
