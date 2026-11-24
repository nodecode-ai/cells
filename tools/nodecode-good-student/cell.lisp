;;;; cell.lisp --- the wrong-question coach: candidates a parent confirms, weak
;;;; points measured against independent reassessments, a week of small actions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of harrylabsj/hermes-plugin-good-student (see NOTICE).
;;;; The original's fifteen MCP tools are twelve NLE:DEFINE-CELL tools here,
;;;; over one JSON file in this home instead of its SQLite store, and its
;;;; /good-student is a slash command. The gate the original is built on is
;;;; kept: recognition output is a candidate, nothing enters a child's learning
;;;; profile until a parent confirms it, one error is suspected and never
;;;; mastery, and only an independent reassessment -- a new variant, no hints --
;;;; moves a weak point, past the section's own pass line.

(defpackage #:nodecode-good-student
  (:use #:cl)
  (:export #:good-student-error #:run-command))

(in-package #:nodecode-good-student)

(nlk:define-peripheral good-student :not-running t :lock "good-student")

(defconstant +format-version+ 1 "The records file's own format, so a later one can tell.")
(defconstant +excerpt-chars+ 120 "How much of a question an analysis carries as evidence.")
(defconstant +max-display-name+ 64 "A student's display name, in characters.")
(defconstant +max-question-chars+ 2000 "One question's text, in characters.")
(defconstant +max-scores+ 200 "Scores one call may record, as the original bounds it.")

(defparameter *error-reasons*
  '("concept_gap" "procedure_gap" "misread" "calculation" "memory" "careless_checking" "unknown")
  "The original's error reason codes, unchanged; `unknown' generates no strong
conclusion and is a question for the parent.")

(defparameter *actionable-statuses* '("suspected" "evidenced" "review_due")
  "The statuses a study action is generated for, as the original's set has them;
`improving' waits for the next reassessment, `mastered' for its review.")

(defparameter *plan-templates*
  '(("concept_gap"
     :action "explain the idea aloud, read one worked example, then do two easy problems"
     :minutes 25 :questions 3 :interval 3
     :method "a new variant of the same idea, different wording, with no hints"
     :acceptance "at least 80% correct on the new variant, with no hints")
    ("procedure_gap"
     :action "write the steps out as a template, do the first one from it, then take the template away"
     :minutes 20 :questions 3 :interval 3
     :method "two problems of the same kind with no step hints"
     :acceptance "both problems done with no hints and both correct")
    ("misread"
     :action "practise circling the key words and restating what is known and what is asked"
     :minutes 15 :questions 4 :interval 2
     :method "a variant with denser information"
     :acceptance "at least 75% correct, and no misreading again")
    ("calculation"
     :action "find the exact rule that went wrong and do a short set of sums on just it"
     :minutes 10 :questions 10 :interval 2
     :method "a few sums, timed"
     :acceptance "at least 90% correct inside the time")
    ("memory"
     :action "spaced review with active recall: say it before looking"
     :minutes 10 :questions 5 :interval 1
     :method "recall on days 1, 3, 7 and 14"
     :acceptance "at least 80% correct on each recall")
    ("careless_checking"
     :action "run a fixed checklist before handing the work in, with no extra practice"
     :minutes 10 :questions 4 :interval 2
     :method "hand in only after the checklist is gone through"
     :acceptance "at least 90% correct after checking")
    ("unknown"
     :action "ask the parent or the teacher what went wrong before practising"
     :minutes 15 :questions 2 :interval 3
     :method "a new variant once the reason is known"
     :acceptance "the reason is named and the new variant is done with no hints"))
  "The original's reason -> action table, one template each: what to do, how
long, how many questions, when to reassess, and what passes.")

(defparameter *if-fail*
  "If the reassessment does not pass: the point stays suspected, practise again
sooner with easier questions, and reassess three days later; two failures in a
row are worth taking to the teacher."
  "The original's next step after a failed reassessment.")

;;; --- when and where ---------------------------------------------------------------

(defun now-text ()
  "Now, as UTC ISO-8601 to the second."
  (nlk:iso-time (nlk:unix-now) :millis nil))

(defun data-folder ()
  "The folder the records file lives in: the section's, or this home's own."
  (let ((dir (setting :data-dir)))
    (if (and dir (plusp (length (nlk:trimmed dir))))
        (uiop:ensure-directory-pathname (nlk:trimmed dir))
        (nlk:home "good-student/"))))

(defun records-file ()
  (merge-pathnames "records.json" (data-folder)))

(defun date-part (instant)
  (if (and instant (>= (length instant) 10)) (subseq instant 0 10) instant))

(defun date-after (instant days)
  "INSTANT's date, DAYS later, as a date; INSTANT is a date or an ISO time."
  (let* ((date (date-part instant))
         (year (parse-integer date :start 0 :end 4))
         (month (parse-integer date :start 5 :end 7))
         (day (parse-integer date :start 8 :end 10))
         (seconds (+ (encode-universal-time 0 0 12 day month year 0) (* days 86400))))
    (subseq (nlk:iso-time (- seconds (encode-universal-time 0 0 0 1 1 1970 0)) :millis nil) 0 10)))

;;; --- the records file ---------------------------------------------------------------

(defun empty-records ()
  (nlk:json-object "format" +format-version+ "students" (vector)))

(defun read-records ()
  "The records as one decoded JSON object; a file that does not decode is the
parent's to fix, never silently an empty one."
  (let ((file (records-file)))
    (if (probe-file file)
        (let ((object (nlk:decode-json (uiop:read-file-string file))))
          (if (hash-table-p object) object (empty-records)))
        (empty-records))))

(defun write-records (records)
  (ensure-directories-exist (records-file))
  (nlk:write-file-atomically (records-file)
                              (format nil "~a~%" (nlk:encode-json-object records))
                              :mode :private)
  records)

(defmacro with-records ((records) &body body)
  "BODY with RECORDS read from the file and written back, under the folder's
lock so two tools of one turn never interleave a read-modify-write."
  `(nlk:with-folder-lock ((data-folder))
     (let ((,records (read-records)))
       (multiple-value-prog1 (progn ,@body)
         (write-records ,records)))))

;;; --- readers over the records -------------------------------------------------------

(defun students (records) (nlk:json-array records "students"))

(defun text-of (object name) (nlk:json-value object :string name))
(defun integer-of (object name) (nlk:json-value object :integer name))
(defun boolean-of (object name) (and (nlk:json-value object :boolean name) t))
(defun array-of (object name) (nlk:json-array object name))

(defun append-member (object name value)
  (setf (gethash name object) (concatenate 'vector (array-of object name) (vector value)))
  value)

(defun remove-member (vector predicate)
  (coerce (remove-if predicate (coerce vector 'list)) 'vector))

(defun excerpt (text)
  (let ((text (or text "")))
    (subseq text 0 (min +excerpt-chars+ (length text)))))

(defun new-id (prefix)
  (format nil "~a-~a" prefix
          (subseq (nlk:sha256-text (format nil "~a/~a/~a" (nlk:unix-now) (random most-positive-fixnum) prefix))
                  7 19)))

(defun kc-slug (name)
  "A knowledge point's name as an id: lowercase, runs of anything else a dash."
  (let ((out (make-string-output-stream))
        (gap nil))
    (loop for character across (string-downcase (or name ""))
          do (cond ((or (alphanumericp character) (> (char-code character) 127))
                    (when gap (write-char #\- out) (setf gap nil))
                    (write-char character out))
                   ((plusp (file-position out)) (setf gap t))))
    (string-right-trim "-" (get-output-stream-string out))))

;;; --- arguments ----------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model
repairs from -- the shape every tool of the original answers a refusal in."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: good-student: ~a" condition)))))

(defun arg (args name)
  (and (hash-table-p args) (gethash name args)))

(defun arg-text (args name)
  (let ((value (arg args name)))
    (and (stringp value) value)))

(defun arg-strings (args name)
  "ARGS' NAME as a list of strings, a lone string read as one."
  (let ((value (arg args name)))
    (cond ((stringp value) (list value))
          ((vectorp value) (remove-if-not #'stringp (coerce value 'list)))
          (t '()))))

(defun required-text (args name)
  (let ((value (nlk:trimmed (or (arg-text args name) ""))))
    (if (plusp (length value)) value (fail "~a is required" name))))

(defun required-integer (args name)
  (let ((value (arg args name)))
    (if (integerp value) value (fail "~a is required and must be an integer" name))))

(defun optional-boolean (args name default)
  (multiple-value-bind (value present) (nlk:json-value args :boolean name)
    (if present (and value t) default)))

;;; --- students -----------------------------------------------------------------------

(defun students-line (records)
  "One line naming every student profile the file holds."
  (let ((all (students records)))
    (if (zerop (length all))
        "no student profiles yet: good_student_create_student makes one"
        (format nil "~{~a~^, ~}"
                (loop for student across all
                      collect (format nil "~a (~@[~a, ~]~d questions)"
                                      (text-of student "display_name")
                                      (text-of student "grade")
                                      (length (array-of student "questions"))))))))

(defun find-student (records reference)
  "The student REFERENCE names -- its id, or its display name when exactly one
carries it -- or a refusal."
  (let* ((all (coerce (students records) 'list))
         (reference (nlk:trimmed (or reference ""))))
    (when (zerop (length reference))
      (fail "student_id is required: good_student_list_students names every profile"))
    (or (find-if (lambda (student) (string= reference (or (text-of student "id") ""))) all)
        (let ((named (remove-if-not (lambda (student)
                                       (string-equal reference (or (text-of student "display_name") "")))
                                     all)))
          (cond ((= 1 (length named)) (first named))
                ((> (length named) 1)
                 (fail "~s names more than one student: pass the student_id good_student_list_students answers"
                       reference))
                (t (fail "no student is ~s here: good_student_list_students names every profile"
                         reference)))))))

(defun student-summary (student)
  (nlk:json-object
   "student_id" (text-of student "id")
   "display_name" (text-of student "display_name")
   "grade" (or (text-of student "grade") :null)
   "subjects" (array-of student "active_subjects")
   "goals" (array-of student "goals")
   "scores" (length (array-of student "scores"))
   "questions" (length (array-of student "questions"))
   "pending" (length (array-of student "candidates"))
   "created_at" (text-of student "created_at")))

(defun tool-list-students (args)
  "good_student_list_students: every profile, read-only."
  (declare (ignore args))
  (answering
   (let ((records (read-records)))
     (nlk:encode-json-object
      (nlk:json-object "students" (coerce (mapcar #'student-summary (coerce (students records) 'list)) 'vector)
                        "total" (length (students records)))))))

(defun bounded-text (what value maximum)
  (let ((text (nlk:trimmed (or value ""))))
    (when (zerop (length text))
      (fail "~a is required" what))
    (when (> (length text) maximum)
      (fail "~a is at most ~d characters and this one is ~d" what maximum (length text)))
    text))

(defun tool-create-student (args)
  "good_student_create_student: a new profile; a display name is required and
is not unique enough to be the key, so an id is minted."
  (answering
   (let ((name (bounded-text "display_name" (arg-text args "display_name") +max-display-name+))
         (grade (arg-text args "grade"))
         (subjects (arg-strings args "active_subjects"))
         (goals (arg-strings args "goals")))
     (with-records (records)
       (when (find-if (lambda (student) (string-equal name (or (text-of student "display_name") "")))
                       (coerce (students records) 'list))
         (fail "a student called ~s is already here; two children get two different names, or pass its student_id"
               name))
       (let ((student (nlk:json-object
                       "id" (new-id "student")
                       "display_name" name
                       "grade" (or grade :null)
                       "active_subjects" (coerce subjects 'vector)
                       "goals" (coerce goals 'vector)
                       "created_at" (now-text)
                       "scores" (vector) "questions" (vector) "candidates" (vector)
                       "reassessments" (vector) "plans" (vector))))
         (append-member records "students" student)
         (nlk:encode-json-object
          (nlk:json-object "created" t "student" (student-summary student)
                            "note" "nothing is in this profile but what you record: questions wait in the candidate area until a parent confirms them")))))))

(defun tool-delete-student (args)
  "good_student_delete_student: without confirm_phrase it says what would go;
with the student's display name as the phrase it deletes."
  (answering
   (with-records (records)
     (let* ((student (find-student records (arg-text args "student_id")))
            (phrase (nlk:trimmed (or (arg-text args "confirm_phrase") "")))
            (name (or (text-of student "display_name") "")))
       (cond
         ((zerop (length phrase))
          (nlk:encode-json-object
           (nlk:json-object
            "confirmation_required" t
            "student" (student-summary student)
            "scope" (nlk:json-object
                     "scores" (length (array-of student "scores"))
                     "questions" (length (array-of student "questions"))
                     "pending" (length (array-of student "candidates"))
                     "plans" (length (array-of student "plans"))
                     "reassessments" (length (array-of student "reassessments")))
            "next" (format nil "ask the parent, then call again with confirm_phrase set to the display name ~s"
                            name))))
         ((not (string-equal phrase name))
          (fail "confirm_phrase must be the display name exactly (~s), not ~s" name phrase))
         (t
          (setf (gethash "students" records)
                (remove-member (students records)
                               (lambda (other) (string= (text-of other "id") (text-of student "id")))))
          (nlk:encode-json-object (nlk:json-object "deleted" t "display_name" name
                                                     "scope" "every score, question, candidate, plan and reassessment of it"))))))))

;;; --- scores -------------------------------------------------------------------------

(defparameter *assessment-types* '("exam" "quiz" "homework" "practice" "other")
  "The original's assessment types, unchanged.")

(defun score-record (object)
  "One score read and checked, before anything is written: a batch is all or
none, as the original's is."
  (let* ((subject (bounded-text "subject" (text-of object "subject") 120))
         (name (bounded-text "assessment_name" (text-of object "assessment_name") 200))
         (at (bounded-text "assessed_at" (text-of object "assessed_at") 40))
         (type (or (text-of object "assessment_type") "exam"))
         (score (nlk:json-value object :number "score"))
         (max (nlk:json-value object :number "max_score"))
         (label (text-of object "grade_label")))
    (unless (member type *assessment-types* :test #'string=)
      (fail "assessment_type ~s is not one of ~{~a~^, ~}" type *assessment-types*))
    (unless (or score label)
      (fail "a score carries a number or a grade label: ~s has neither" name))
    (when (and score (minusp score))
      (fail "a score cannot be below zero: ~s" name))
    (when (and max (not (plusp max)))
      (fail "max_score must be above zero: ~s" name))
    (nlk:json-object
     "id" (new-id "score")
     "subject" subject
     "assessment_name" name
     "assessment_type" type
     "score" (or score :null)
     "max_score" (or max :null)
     "grade_label" (or label :null)
     "term" (or (text-of object "term") :null)
     "class_rank" (or (integer-of object "class_rank") :null)
     "grade_rank" (or (integer-of object "grade_rank") :null)
     "class_size" (or (integer-of object "class_size") :null)
     "assessed_at" at
     "notes" (or (text-of object "notes") :null)
     "source_ref" (or (text-of object "source_ref") :null))))

(defun percent-of (score)
  "SCORE as a percentage of its full marks, or NIL when either is missing."
  (let ((value (nlk:json-value score :number "score"))
        (max (nlk:json-value score :number "max_score")))
    (and value max (plusp max) (* 100 (/ value max)))))

(defun rounded (number)
  (/ (round (* number 10)) 10))

(defun tool-record-scores (args)
  "good_student_record_scores: a batch of scores, written whole or not at all."
  (answering
   (let* ((batch (array-of args "records"))
          (count (length batch)))
     (unless (plusp count)
       (fail "records is a list of at least one score"))
     (when (> count +max-scores+)
       (fail "one call records at most ~d scores and this one has ~d" +max-scores+ count))
     (let ((read (map 'list #'score-record batch)))
       (with-records (records)
         (let ((student (find-student records (arg-text args "student_id"))))
           (dolist (score read) (append-member student "scores" score))
           (nlk:encode-json-object
            (nlk:json-object
             "student_id" (text-of student "id")
             "recorded" (length read)
             "subjects" (coerce (remove-duplicates (mapcar (lambda (score) (text-of score "subject")) read)
                                                   :test #'string=)
                                'vector)
             "note" "a score is a performance record, not mastery: diagnose from confirmed wrong questions"))))))))

(defun keep-score-p (score args)
  (let ((subject (arg-text args "subject"))
        (type (arg-text args "assessment_type"))
        (term (arg-text args "term"))
        (from (arg-text args "from_date"))
        (to (arg-text args "to_date"))
        (at (or (text-of score "assessed_at") "")))
    (and (or (null subject) (string= subject (or (text-of score "subject") "")))
         (or (null type) (string= type (or (text-of score "assessment_type") "")))
         (or (null term) (string= term (or (text-of score "term") "")))
         (or (null from) (string>= at from))
         (or (null to) (string<= at to)))))

(defun tool-list-scores (args)
  "good_student_list_scores: the scores a filter keeps, with a summary a
subject at a time."
  (answering
   (let* ((records (read-records))
          (student (find-student records (arg-text args "student_id")))
          (kept (remove-if-not (lambda (score) (keep-score-p score args))
                                (coerce (array-of student "scores") 'list)))
          (subjects (remove-duplicates (mapcar (lambda (score) (text-of score "subject")) kept)
                                       :test #'string=)))
     (nlk:encode-json-object
      (nlk:json-object
       "student_id" (text-of student "id")
       "scores" (coerce kept 'vector)
       "total" (length kept)
       "by_subject"
       (coerce
        (loop for subject in subjects
              for mine = (remove-if-not (lambda (score) (string= subject (text-of score "subject"))) kept)
              for percents = (remove nil (mapcar #'percent-of mine))
              collect (nlk:json-object
                       "subject" subject
                       "count" (length mine)
                       "average_percent" (if percents (rounded (/ (reduce #'+ percents) (length percents))) :null)))
        'vector))))))

;;; --- candidates: what the model recognized, waiting for a parent ----------------------

(defun candidate-json (object)
  "One candidate read from the batch the model sends, before anything is
written: a batch is all or none."
  (let* ((text (bounded-text "question_text" (text-of object "question_text") +max-question-chars+))
         (kc (bounded-text "knowledge_point" (or (text-of object "knowledge_point")
                                                 (text-of object "kc"))
                           120))
         (reason (or (text-of object "error_reason") "unknown")))
    (unless (member reason *error-reasons* :test #'string=)
      (fail "error_reason ~s is not one of ~{~a~^, ~}" reason *error-reasons*))
    (nlk:json-object
     "id" (new-id "candidate")
     "question_text" text
     "knowledge_point" kc
     "kc_id" (kc-slug kc)
     "subject" (or (text-of object "subject") :null)
     "error_reason" reason
     "source_ref" (or (text-of object "source_ref") :null)
     "attention" (and (nlk:json-value object :boolean "attention") t)
     "captured_at" (now-text))))

(defun expires-at (hours)
  "HOURS from now, as an ISO time to the minute: when an unconfirmed
candidate stops waiting."
  (subseq (nlk:iso-time (+ (nlk:unix-now) (* 3600 hours)) :millis nil) 0 16))

(defun tool-ingest-candidates (args)
  "good_student_ingest_candidates: candidates into the waiting area, where
they expire unconfirmed and never touch the profile."
  (answering
   (let* ((batch (arg args "batch"))
          (questions (and (hash-table-p batch) (array-of batch "questions")))
          (count (if questions (length questions) 0)))
     (unless (plusp count)
       (fail "batch carries a questions list of at least one: { \"questions\": [ { \"question_text\": ..., \"knowledge_point\": ..., \"error_reason\": ... } ] }"))
     (let ((read (map 'list #'candidate-json questions)))
       (with-records (records)
         (let* ((student (find-student records (arg-text args "student_id")))
                (hours (setting :candidate-hours))
                (expiry (expires-at hours)))
           (dolist (candidate read)
             (setf (gethash "expires_at" candidate) expiry
                   (gethash "captured_at" candidate) (now-text))
             (append-member student "candidates" candidate))
           (nlk:encode-json-object
            (nlk:json-object
             "student_id" (text-of student "id")
             "candidates" (coerce (mapcar (lambda (candidate) (text-of candidate "id")) read) 'vector)
             "expires_at" expiry
             "note" (format nil "nothing here is a fact yet: good_student_list_pending shows them and a parent confirms each one (~d hours)" hours)))))))))

(defun pending-candidates (student)
  (remove-if-not (lambda (candidate)
                   (let ((expires (text-of candidate "expires_at"))) 
                     (or (null expires) (string> expires (now-text)))))
                 (coerce (array-of student "candidates") 'list)))

(defun tool-list-pending (args)
  "good_student_list_pending: what waits for a parent, expired ones cleared."
  (answering
   (with-records (records)
     (let* ((student (find-student records (arg-text args "student_id")))
            (pending (pending-candidates student)))
       (setf (gethash "candidates" student) (coerce pending 'vector))
       (nlk:encode-json-object
        (nlk:json-object
         "student_id" (text-of student "id")
         "pending" (coerce (mapcar (lambda (candidate)
                                     (nlk:json-object
                                      "candidate_id" (text-of candidate "id")
                                      "question_text" (text-of candidate "question_text")
                                      "knowledge_point" (text-of candidate "knowledge_point")
                                      "subject" (or (text-of candidate "subject") :null)
                                      "error_reason" (text-of candidate "error_reason")
                                      "attention" (boolean-of candidate "attention")
                                      "captured_at" (text-of candidate "captured_at")
                                      "expires_at" (text-of candidate "expires_at")))
                                   pending)
                          'vector)
         "total" (length pending)
         "note" "a candidate enters the learning profile only when a parent confirms it"))))))

(defun confirm-item (student item)
  "One confirmation or rejection applied to STUDENT; answers its action."
  (let* ((id (required-text item "candidate_id"))
         (action (or (text-of item "action") "confirm"))
         (candidate (or (find-if (lambda (other) (string= id (or (text-of other "id") "")))
                                  (coerce (array-of student "candidates") 'list))
                        (fail "no candidate ~s is waiting; good_student_list_pending lists them" id))))
    (unless (member action '("confirm" "reject") :test #'string=)
      (fail "action is \"confirm\" or \"reject\", not ~s" action))
    (setf (gethash "candidates" student)
          (remove-member (array-of student "candidates")
                         (lambda (other) (string= id (or (text-of other "id") "")))))
    (if (string= action "reject")
        (nlk:json-object "candidate_id" id "action" "rejected")
        (let ((edits (arg item "edits")))
          (let ((question (nlk:json-object
                           "id" (new-id "question")
                           "question_text" (or (and (hash-table-p edits) (text-of edits "question_text"))
                                               (text-of candidate "question_text"))
                           "knowledge_point" (or (and (hash-table-p edits) (text-of edits "knowledge_point"))
                                                 (text-of candidate "knowledge_point"))
                           "subject" (or (and (hash-table-p edits) (text-of edits "subject"))
                                         (text-of candidate "subject") :null)
                           "error_reason" (or (and (hash-table-p edits) (text-of edits "error_reason"))
                                              (text-of candidate "error_reason"))
                           "source_ref" (or (text-of candidate "source_ref") :null)
                           "confirmed_at" (now-text))))
            (setf (gethash "kc_id" question) (kc-slug (text-of question "knowledge_point")))
            (append-member student "questions" question)
            (nlk:json-object "candidate_id" id "action" "confirmed"
                              "question_id" (text-of question "id")
                              "knowledge_point" (text-of question "knowledge_point")))))))

(defun tool-confirm-questions (args)
  "good_student_confirm_questions: a parent's yes or no, all items or none."
  (answering
   (let ((items (array-of args "items")))
     (unless (plusp (length items))
       (fail "items is a list of at least one { \"candidate_id\": ..., \"action\": \"confirm\" | \"reject\" }"))
     (with-records (records)
       (let* ((student (find-student records (arg-text args "student_id")))
              (done (map 'list (lambda (item) (confirm-item student item)) items)))
         (nlk:encode-json-object
          (nlk:json-object
           "student_id" (text-of student "id")
           "confirmed" (count-if (lambda (answer) (string= "confirmed" (text-of answer "action"))) done)
           "rejected" (count-if (lambda (answer) (string= "rejected" (text-of answer "action"))) done)
           "items" (coerce done 'vector))))))))

;;; --- the analysis --------------------------------------------------------------------

(defun independent-p (reassessment)
  "A reassessment moves a point only when it is a new variant with no hints."
  (and (boolean-of reassessment "is_new_variant") (boolean-of reassessment "no_hints")))

(defun passed-p (reassessment)
  (let ((correct (integer-of reassessment "correct_count"))
        (total (integer-of reassessment "total_count")))
    (and correct total (plusp total) (>= (* 100 correct) (* (setting :pass-percent) total)))))

(defun most-common (strings)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (string strings) (incf (gethash string counts 0)))
    (let ((best nil) (best-count 0))
      (maphash (lambda (key count) (when (> count best-count) (setf best key best-count count))) counts)
      best)))

(defun weakness-of (kc-id wrongs reassessments now)
  "One knowledge point as the original's analysis reads it: one error is
suspected, an independent failure is evidenced, a passed independent
reassessment improves it, two in a row master it, and a mastered point whose
review date has passed is due again."
  (let* ((kc (text-of (first wrongs) "knowledge_point"))
         (mine (remove-if-not (lambda (reassessment)
                                 (string= kc-id (or (text-of reassessment "kc_id")
                                                    (kc-slug (text-of reassessment "kc")))))
                               reassessments))
         (independent (remove-if-not #'independent-p mine))
         (latest-wrong (reduce (lambda (a b) (if (string> a b) a b))
                               (mapcar (lambda (question) (or (text-of question "confirmed_at") "")) wrongs)
                               :initial-value ""))
         (since (remove-if (lambda (reassessment)
                             (string< (or (text-of reassessment "completed_at") "") latest-wrong))
                           independent))
         (evidence-count (+ (length wrongs) (length mine)))
         (sources (remove-duplicates (remove nil (mapcar (lambda (question) (text-of question "source_ref"))
                                                         wrongs))
                                     :test #'string=))
         (corrected (remove nil (mapcar (lambda (question)
                                          (and (string= "corrected" (or (text-of question "correction_status") ""))
                                               (text-of question "confirmed_at")))
                                        wrongs)))
         (status "suspected")
         (risk "low")
         (confidence "low")
         (reasons (list (if (= 1 (length wrongs)) "single_error" "multiple_errors")))
         (next-review nil))
    (when (>= (length sources) 2)
      (setf risk "medium" confidence "medium")
      (push "repeated_across_sources" reasons))
    (when (and corrected (some (lambda (question)
                                 (string> (or (text-of question "confirmed_at") "") (first (sort (copy-list corrected) #'string<))))
                               wrongs))
      (setf risk "high" confidence "medium")
      (push "recurrence_after_correction" reasons))
    (when since
      (let ((last (car (last since))))
        (if (not (passed-p last))
            (progn (setf status "evidenced" risk "high")
                   (setf confidence (if (>= evidence-count 2) "high" "medium"))
                   (push "failed_independent_reassessment" reasons))
            (let ((streak 0))
              (setf status "improving")
              (push "passed_independent_reassessment" reasons)
              (dolist (reassessment (reverse since))
                (if (passed-p reassessment) (incf streak) (return)))
              (when (>= streak 2)
                (setf status "mastered"
                      next-review (date-after (text-of last "completed_at") (setting :review-days)))
                (push "consecutive_independent_passes" reasons)
                (when (string> now (format nil "~aT00:00:00Z" next-review))
                  (setf status "review_due")
                  (push "review_due_by_decay" reasons)))))))
    (when (< evidence-count 2)
      (setf confidence "low"))
    (list :kc kc
          :kc-id kc-id
          :subject (text-of (first wrongs) "subject")
          :status status
          :risk risk
          :confidence confidence
          :reason (most-common (mapcar (lambda (question)
                                          (or (text-of question "error_reason") "unknown"))
                                        wrongs))
          :evidence-count evidence-count
          :reasons (reverse reasons)
          :last-error-at (and (plusp (length latest-wrong)) latest-wrong)
          :next-review (and (member status '("mastered" "review_due") :test #'string=) next-review)
          :evidence (mapcar (lambda (question)
                              (list :source (text-of question "source_ref")
                                    :at (text-of question "confirmed_at")
                                    :excerpt (excerpt (text-of question "question_text"))))
                            wrongs))))

(defun analyze-student (student)
  "The student's weaknesses, a knowledge point each, sorted by id."
  (let ((by-kc (make-hash-table :test #'equal))
        (now (now-text))
        (reassessments (coerce (array-of student "reassessments") 'list)))
    (dolist (question (coerce (array-of student "questions") 'list))
      (push question (gethash (kc-slug (text-of question "knowledge_point")) by-kc)))
    (sort (loop for kc being the hash-keys of by-kc
                collect (weakness-of kc (reverse (gethash kc by-kc)) reassessments now))
          #'string< :key (lambda (weakness) (getf weakness :kc-id)))))

(defun weakness-json (weakness)
  (nlk:json-object
   "kc_id" (getf weakness :kc-id)
   "knowledge_point" (getf weakness :kc)
   "subject" (or (getf weakness :subject) :null)
   "status" (getf weakness :status)
   "actionable" (and (member (getf weakness :status) *actionable-statuses* :test #'string=) t)
   "risk_level" (getf weakness :risk)
   "confidence_level" (getf weakness :confidence)
   "error_reason" (getf weakness :reason)
   "evidence_count" (getf weakness :evidence-count)
   "reasons" (coerce (getf weakness :reasons) 'vector)
   "last_error_at" (or (getf weakness :last-error-at) :null)
   "next_review_at" (or (getf weakness :next-review) :null)
   "evidence" (coerce (loop for item in (getf weakness :evidence)
                             collect (nlk:json-object "source_ref" (or (getf item :source) :null)
                                                       "confirmed_at" (or (getf item :at) :null)
                                                       "question_excerpt" (getf item :excerpt)))
                       'vector)))

(defun by-status (weaknesses)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (weakness weaknesses) (incf (gethash (getf weakness :status) counts 0)))
    (apply #'nlk:make-json-object
           (loop for status in '("suspected" "evidenced" "improving" "mastered" "review_due")
                 append (list status (gethash status counts 0))))))

(defun analysis-json (student weaknesses)
  (nlk:json-object
   "student_id" (text-of student "id")
   "insufficient" (null weaknesses)
   "weaknesses" (coerce (mapcar #'weakness-json weaknesses) 'vector)
   "summary" (nlk:json-object
              "by_status" (by-status weaknesses)
              "total" (length weaknesses)
              "note" (if weaknesses
                          "a suspected point is a question, not a verdict on the child"
                          "no confirmed wrong questions yet: confirm candidates before analysing"))
   "pass_percent" (setting :pass-percent)))

(defun tool-analyze (args)
  "good_student_analyze: the weak points the confirmed questions support."
  (answering
   (let* ((records (read-records))
          (student (find-student records (arg-text args "student_id"))))
     (nlk:encode-json-object (analysis-json student (analyze-student student))))))

;;; --- the plan -------------------------------------------------------------------------

(defun plan-template (reason)
  (or (cdr (assoc reason *plan-templates* :test #'string=))
      (cdr (assoc "unknown" *plan-templates* :test #'string=))))

(defun plan-action (weakness now)
  "One action for one weakness: why it is there, what to do, how long, when to
reassess, what passes, and the next step if it does not."
  (let ((template (plan-template (or (getf weakness :reason) "unknown"))))
    (nlk:json-object
     "kc_id" (getf weakness :kc-id)
     "knowledge_point" (getf weakness :kc)
     "status" (getf weakness :status)
     "error_reason" (or (getf weakness :reason) "unknown")
     "why" (format nil "~d confirmed wrong question~:p, status ~a, ~a"
                    (getf weakness :evidence-count) (getf weakness :status)
                    (getf weakness :risk))
     "action" (getf template :action)
     "duration_minutes" (getf template :minutes)
     "question_count" (getf template :questions)
     "reassessment_method" (getf template :method)
     "reassess_on" (date-after now (getf template :interval))
     "acceptance_criteria" (getf template :acceptance)
     "if_fail" *if-fail*
     "evidence" (coerce (mapcar (lambda (item) (getf item :excerpt))
                                 (getf weakness :evidence))
                         'vector))))

(defun build-plan (weaknesses now)
  "The original's plan: one action per actionable weak point whose error
reason is named, and the ones held back with the reason."
  (let ((actions '()) (skipped '()))
    (dolist (weakness weaknesses)
      (cond
        ((not (member (getf weakness :status) *actionable-statuses* :test #'string=))
         (push (nlk:json-object "kc_id" (getf weakness :kc-id)
                                "knowledge_point" (getf weakness :kc)
                                "status" (getf weakness :status)
                                "why" "not actionable yet: it waits for the next reassessment")
               skipped))
        ((member (getf weakness :reason) '(nil "unknown") :test #'equal)
         (push (nlk:json-object "kc_id" (getf weakness :kc-id)
                                "knowledge_point" (getf weakness :kc)
                                "status" (getf weakness :status)
                                "why" "the error reason is not confirmed: ask the parent or the teacher before practising")
               skipped))
        (t (push (plan-action weakness now) actions))))
    (values (reverse actions) (reverse skipped))))

(defun tool-create-plan (args)
  "good_student_create_plan: this week's actions from the analysis."
  (answering
   (with-records (records)
     (let* ((student (find-student records (arg-text args "student_id")))
            (now (now-text))
            (weaknesses (analyze-student student)))
       (multiple-value-bind (actions skipped) (build-plan weaknesses now)
         (let ((plan (nlk:json-object
                      "id" (new-id "plan")
                      "created_at" now
                      "actions" (coerce actions 'vector)
                      "skipped" (coerce skipped 'vector))))
           (append-member student "plans" plan)
           (nlk:encode-json-object
            (nlk:json-object
             "student_id" (text-of student "id")
             "plan_id" (text-of plan "id")
             "created_at" now
             "actions" (coerce actions 'vector)
             "skipped" (coerce skipped 'vector)
             "summary" (nlk:json-object
                        "actions" (length actions)
                        "skipped" (length skipped)
                        "note" (if actions
                                    "one action each: small, checkable, and reassessed on its own day"
                                    "nothing to practise yet: confirm candidate questions first"))))))))))

;;; --- the reassessment ---------------------------------------------------------------

(defun knowledge-point-of (student reference)
  "The confirmed knowledge point REFERENCE names -- its id or its name."
  (let* ((questions (coerce (array-of student "questions") 'list))
         (reference (nlk:trimmed (or reference ""))))
    (or (find-if (lambda (question) (string= reference (kc-slug (text-of question "knowledge_point"))))
                 questions)
        (find-if (lambda (question) (string-equal reference (or (text-of question "knowledge_point") "")))
                 questions)
        (fail "no confirmed knowledge point is ~s: good_student_analyze lists them by kc_id"
              reference))))

(defun tool-record-reassessment (args)
  "good_student_record_reassessment: one reassessment, and the status it moves."
  (answering
   (with-records (records)
     (let* ((student (find-student records (arg-text args "student_id")))
            (question (knowledge-point-of student (arg-text args "kc_id")))
            (kc (text-of question "knowledge_point"))
            (correct (required-integer args "correct_count"))
            (total (required-integer args "total_count"))
            (variant (optional-boolean args "is_new_variant" t))
            (no-hints (optional-boolean args "no_hints" t))
            (at (or (and (arg-text args "completed_at") (nlk:trimmed (arg-text args "completed_at")))
                    (now-text)))
            (reassessment (nlk:json-object
                           "id" (new-id "reassessment")
                           "kc" kc
                           "kc_id" (kc-slug kc)
                           "correct_count" correct
                           "total_count" total
                           "is_new_variant" (if variant t :false)
                           "no_hints" (if no-hints t :false)
                           "time_seconds" (or (nlk:json-value args :integer "time_seconds") :null)
                           "self_reported_confidence" (or (nlk:json-value args :number "self_reported_confidence") :null)
                           "completed_at" at)))
       (unless (<= 0 correct total)
         (fail "correct_count is between 0 and total_count (~d)" total))
       (setf (gethash "passed" reassessment) (if (passed-p reassessment) t :false))
       (append-member student "reassessments" reassessment)
       (let* ((weakness (find (kc-slug kc) (analyze-student student)
                              :key (lambda (one) (getf one :kc-id)) :test #'string=)))
         (nlk:encode-json-object
          (nlk:json-object
           "student_id" (text-of student "id")
           "reassessment_id" (text-of reassessment "id")
           "kc_id" (kc-slug kc)
           "knowledge_point" kc
           "accuracy_percent" (rounded (* 100 (/ correct total)))
           "passed" (boolean-of reassessment "passed")
           "independent" (if (and variant no-hints) t :false)
           "status" (or (and weakness (getf weakness :status)) "suspected")
           "note" (cond ((not (and variant no-hints))
                         "a reassessment that reuses a seen question or gives hints does not advance the point")
                        ((boolean-of reassessment "passed")
                         "an independent pass moves the point on; two in a row master it")
                        (t "an independent failure is evidence, not a verdict: practise again")))))))))

;;; --- the weekly brief -----------------------------------------------------------------

(defun reassessment-week (student from)
  (remove-if (lambda (reassessment) (string< (or (text-of reassessment "completed_at") "") from))
             (coerce (array-of student "reassessments") 'list)))

(defun tool-weekly-brief (args)
  "good_student_weekly_brief: the parent's week, read-only."
  (answering
   (let* ((records (read-records))
          (student (find-student records (arg-text args "student_id")))
          (now (now-text))
          (week-start (date-after now -7))
          (today (date-part now))
          (weaknesses (analyze-student student))
          (plans (coerce (array-of student "plans") 'list))
          (actions (and plans (array-of (car (last plans)) "actions")))
          (open (remove-if-not (lambda (action) (string>= (or (text-of action "reassess_on") "")
                                                          today))
                                (coerce (or actions #()) 'list)))
          (overdue (remove-if (lambda (action) (string>= (or (text-of action "reassess_on") "")
                                                         today))
                              (coerce (or actions #()) 'list)))
          (due (remove-if-not (lambda (weakness)
                                 (let ((review (getf weakness :next-review)))
                                   (and review (string>= review today)
                                        (string<= review (date-after now 7)))))
                               weaknesses))
          (week (reassessment-week student week-start))
          (new-questions (remove-if (lambda (question)
                                      (string< (or (text-of question "confirmed_at") "") week-start))
                                    (coerce (array-of student "questions") 'list))))
     (nlk:encode-json-object
      (nlk:json-object
       "student_id" (text-of student "id")
       "display_name" (text-of student "display_name")
       "week_start" week-start
       "new_questions" (length new-questions)
       "by_status" (let ((counts (make-hash-table :test #'equal)))
                      (dolist (weakness weaknesses) (incf (gethash (getf weakness :status) counts 0)))
                      (apply #'nlk:make-json-object
                             (loop for status in '("suspected" "evidenced" "improving" "mastered" "review_due")
                                   append (list status (gethash status counts 0)))))
       "actions" (nlk:json-object
                  "open" (length open)
                  "overdue" (coerce (mapcar (lambda (action)
                                               (nlk:json-object
                                                "knowledge_point" (text-of action "knowledge_point")
                                                "action" (text-of action "action")
                                                "reassess_on" (or (text-of action "reassess_on") :null)))
                                             overdue)
                                    'vector))
       "due_reviews" (coerce (mapcar (lambda (weakness)
                                       (nlk:json-object
                                        "kc_id" (getf weakness :kc-id)
                                        "knowledge_point" (getf weakness :kc)
                                        "next_review_at" (or (getf weakness :next-review) :null)))
                                     due)
                              'vector)
       "reassessments" (nlk:json-object
                        "count" (length week)
                        "passed" (count-if (lambda (one) (boolean-of one "passed")) week)
                        "failed" (count-if-not (lambda (one) (boolean-of one "passed")) week))
       "pass_percent" (setting :pass-percent))))))

;;; --- /good-student --------------------------------------------------------------------

(defun students-text (records)
  (let ((all (coerce (students records) 'list)))
    (if (null all)
        "good-student: no profiles yet -- ask for one and good_student_create_student makes it"
        (format nil "good-student: ~d profile~:p~%~{~a~%~}"
                (length all)
                (loop for student in all
                      collect (format nil "  ~a  (~@[~a, ~]~d questions, ~d waiting)"
                                      (text-of student "display_name")
                                      (text-of student "grade")
                                      (length (array-of student "questions"))
                                      (length (pending-candidates student))))))))

(defun student-text (student)
  "One student's week as the operator reads it at the keyboard."
  (let* ((weaknesses (analyze-student student))
         (actionable (remove-if-not (lambda (weakness)
                                      (member (getf weakness :status) *actionable-statuses* :test #'string=))
                                    weaknesses))
         (pending (length (pending-candidates student))))
    (with-output-to-string (out)
      (format out "~a~@[ (~a)~]: ~d confirmed question~:p, ~d waiting for a parent, ~d weak point~:p~%"
              (text-of student "display_name") (text-of student "grade")
              (length (array-of student "questions")) pending (length weaknesses))
      (dolist (weakness actionable)
        (format out "  ~a: ~a (~a)~%" (getf weakness :kc) (getf weakness :status) (getf weakness :risk)))
      (when (null weaknesses)
        (format out "  nothing to analyse yet: confirm candidates first~%"))
      (loop for weakness in weaknesses
            for review = (getf weakness :next-review)
            when review
              do (format out "  review ~a: ~a~%" review (getf weakness :kc))))))

(defun run-command (args session-id)
  "/good-student typed alone: every profile and what waits; with a name, one
student's week."
  (declare (ignore session-id))
  (let ((records (read-records))
        (text (nlk:trimmed (or args ""))))
    (cond ((zerop (length text)) (students-text records))
          ((string-equal text "pending")
           (format nil "~{~a~^~%~}"
                   (loop for student across (students records)
                         append (loop for candidate in (pending-candidates student)
                                      collect (format nil "~a: ~a (~a)~@[  [~a]~]"
                                                      (text-of student "display_name")
                                                      (excerpt (text-of candidate "question_text"))
                                                      (text-of candidate "knowledge_point")
                                                      (and (boolean-of candidate "attention")
                                                           "needs a closer look"))))))
          (t (student-text (find-student records text))))))

;;; --- the manual -----------------------------------------------------------------------

(defparameter +primer+
  "Good-student keeps a child's learning record on this machine, through the
nodecode-good-student cell: twelve good_student_* tools, and Lisp for a
script. Nothing leaves the machine and no key is needed.
  The gate: recognition output is a CANDIDATE. good_student_ingest_candidates
  puts recognized wrong questions in the waiting area; good_student_list_pending
  shows them and speeds the ones the model was unsure of; a parent confirms or
  rejects each with good_student_confirm_questions, and only a confirmed
  question is in the profile. Unconfirmed candidates expire.
  Profiles: good_student_list_students, good_student_create_student (a display
  name, not a key; two children get two names), good_student_delete_student
  (answers the scope without a confirm_phrase, deletes when it is the display name).
  Scores: good_student_record_scores writes a batch whole or not at all;
  good_student_list_scores reads them back with a subject summary. A score is a
  performance record, not mastery.
  The loop: good_student_analyze (suspected -> evidenced -> improving ->
  mastered, review when mastered goes stale), good_student_create_plan (one
  small action per actionable point, with its own reassessment day),
  good_student_record_reassessment (only a new variant with no hints moves a
  point; the pass line is pass_percent), good_student_weekly_brief (the
  parent's week, read-only).
  Never call a child weak from scores; suspected points are questions, not
  verdicts. /good-student shows the profiles and the week."
  "What (help :good-student) answers while the cell runs.")

;;; --- the declaration ------------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *list-students-schema*
  (tool-schema "List every student profile on this machine, with counts. Read-only: call it first in a new session."
                '()))

(defparameter *create-student-schema*
  (tool-schema "Create a student profile. The display name is what a parent says and what delete confirms; two children get two profiles."
                '(("display_name" "string" "The name the parent uses, 1-64 characters." t)
                  ("grade" "string" "The year or grade, as the parent says it.")
                  ("active_subjects" "array" "Subjects being studied, e.g. math, reading.")
                  ("goals" "array" "What the parent wants this term."))))

(defparameter *record-scores-schema*
  (tool-schema "Record exam, quiz or homework scores. A batch is written whole or not at all. A score is a performance record, not mastery."
                '(("student_id" "string" "The profile's id or display name." t)
                  ("records" "array" "One object a score: subject, assessment_name, assessed_at, and score with max_score or grade_label." t))))

(defparameter *list-scores-schema*
  (tool-schema "Read recorded scores back, with a per-subject summary, filtered by subject, type, term or dates."
                '(("student_id" "string" "The profile's id or display name." t)
                  ("subject" "string" "Only this subject.")
                  ("assessment_type" "string" "exam, quiz, homework, practice or other.")
                  ("term" "string" "Only this term.")
                  ("from_date" "string" "ISO date or time, inclusive.")
                  ("to_date" "string" "ISO date or time, inclusive."))))

(defparameter *ingest-schema*
  (tool-schema "Put recognized wrong questions into the waiting area as candidates. Nothing enters the learning profile until a parent confirms it, and an unconfirmed candidate expires."
                '(("student_id" "string" "The profile's id or display name." t)
                  ("batch" "object" "An object with questions: one object each, question_text, knowledge_point, and optionally subject, error_reason, source_ref, attention." t))))

(defparameter *pending-schema*
  (tool-schema "List the candidates waiting for a parent, with the ones worth a closer look marked."
                '(("student_id" "string" "The profile's id or display name." t))))

(defparameter *confirm-schema*
  (tool-schema "Record a parent's confirmation or rejection of candidates, all items or none. Only confirmed questions enter the learning profile."
                '(("student_id" "string" "The profile's id or display name." t)
                  ("items" "array" "One object each: candidate_id, action confirm or reject, optional edits of question_text, knowledge_point, subject, error_reason." t))))

(defparameter *analyze-schema*
  (tool-schema "Analyse confirmed wrong questions into weak knowledge points with status, risk, confidence and evidence. Never invents a mastery figure: with no confirmed questions it answers insufficient."
                '(("student_id" "string" "The profile's id or display name." t))))

(defparameter *plan-schema*
  (tool-schema "Turn the analysis into this week's study actions: what to do, how long, how many questions, the reassessment day, the pass line, and the next step if it does not pass."
                '(("student_id" "string" "The profile's id or display name." t))))

(defparameter *reassessment-schema*
  (tool-schema "Record one reassessment of a knowledge point. Only a new variant done with no hints moves the point; the pass line is the section's pass_percent."
                '(("student_id" "string" "The profile's id or display name." t)
                  ("kc_id" "string" "The knowledge point's kc_id, from good_student_analyze." t)
                  ("correct_count" "integer" "How many were right." t)
                  ("total_count" "integer" "How many were attempted." t)
                  ("is_new_variant" "boolean" "A variant the child has not seen; default true.")
                  ("no_hints" "boolean" "Done with no hints; default true.")
                  ("completed_at" "string" "ISO time; now when absent.")
                  ("time_seconds" "integer" "How long it took.")
                  ("self_reported_confidence" "number" "0 to 1, if the child said."))))

(defparameter *brief-schema*
  (tool-schema "The parent's week for one student, read-only: new wrong questions, weak points by status, open and overdue actions, reviews due in seven days, reassessments this week."
                '(("student_id" "string" "The profile's id or display name." t))))

(defparameter *delete-schema*
  (tool-schema "Delete a student and everything in its profile. Without confirm_phrase it answers only the scope; call again with the display name as the phrase after the parent agrees."
                '(("student_id" "string" "The profile's id or display name." t)
                  ("confirm_phrase" "string" "The student's display name, exactly, once the parent has agreed."))))

(nle:define-cell good-student
  (:section ("good-student")
    (:guide "nothing here is required: the records live in this home unless data_dir names a folder; pass_percent is the reassessment line, review_days when a mastered point goes stale, candidate_hours how long an unconfirmed candidate waits")
    ("data_dir" :path
     :doc "the folder records.json is kept in; empty means this home's good-student folder")
    ("pass_percent" :integer :default 80 :min 1
     :doc "the accuracy an independent reassessment must reach to pass, in percent")
    ("review_days" :integer :default 14 :min 1
     :doc "how long a mastered knowledge point stands before its review is due")
    ("candidate_hours" :integer :default 24 :min 1
     :doc "how long an unconfirmed candidate waits before it expires"))
  (:help :good-student "good-student: a child's wrong questions, confirmed by a parent, analysed and turned into a week - the good_student_* tools, /good-student" +primer+)
  (:command "good-student" 'run-command
            :description "The student profiles, what waits for a parent, or one child's week"
            :argument-hint "STUDENT · pending")
  (:tool "good_student_list_students" *list-students-schema* 'tool-list-students)
  (:tool "good_student_create_student" *create-student-schema* 'tool-create-student)
  (:tool "good_student_record_scores" *record-scores-schema* 'tool-record-scores)
  (:tool "good_student_list_scores" *list-scores-schema* 'tool-list-scores)
  (:tool "good_student_ingest_candidates" *ingest-schema* 'tool-ingest-candidates)
  (:tool "good_student_list_pending" *pending-schema* 'tool-list-pending)
  (:tool "good_student_confirm_questions" *confirm-schema* 'tool-confirm-questions)
  (:tool "good_student_analyze" *analyze-schema* 'tool-analyze)
  (:tool "good_student_create_plan" *plan-schema* 'tool-create-plan)
  (:tool "good_student_record_reassessment" *reassessment-schema* 'tool-record-reassessment)
  (:tool "good_student_weekly_brief" *brief-schema* 'tool-weekly-brief)
  (:tool "good_student_delete_student" *delete-schema* 'tool-delete-student)
  (:start (lambda ()
            (ensure-directories-exist (records-file)))))
