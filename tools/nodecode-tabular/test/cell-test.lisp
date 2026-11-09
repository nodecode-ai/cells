;;;; cell-test.lisp --- lifecycle, the parser, the profile, the split, the save.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every file is one this test wrote in a directory of its own; no test
;;;; reads the operator's data or opens a socket, and no test starts a turn.
;;;; The three tools are also called through the real dispatch seam once, so
;;;; the registration, the declared shape and the answer a model sees are
;;;; proved together.

(in-package #:nodecode.test)

(define-test-slice "tabular" "TABULAR-CELL-" :start nodecode-tabular:start-cell)

(define-cell-lifecycle-tests "tabular"
  (:help :tabular)
  (:command "tabular")
  (:running (is (nle::find-tool "tabular_profile") "the profile tool is registered")
            (is (nle::find-tool "tabular_split") "the split tool is registered")
            (is (nle::find-tool "tabular_save") "the save tool is registered")
            (is (equal #\, (nodecode-tabular::delimiter-of "")) "the section's delimiter is the default")
            (is (eq :very-small (nodecode-tabular::band-of 120)) "the policy's bands are the pack's"))
  (:stopped (is (null (nle::find-tool "tabular_profile")) "stop unregisters the tools")
            (is (null (nle::find-registered-command "tabular")) "and /tabular"))
  (:refused ("delimiter" "space") ("max_rows_scanned" 5))
  (:idle tabular:tabular-error (tabular:profile-file "no such file.csv")))

;;; --- fixtures ------------------------------------------------------------------

(defun tabular-temp-dir ()
  "A directory of this test's own, created fresh."
  (let ((dir (merge-pathnames (format nil "tabular-~(~a~)-~d/"
                                       (gensym "test") (get-universal-time))
                               (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defun tabular-fixture (dir &key (rows 120))
  "Write churn.csv under DIR: ROWS rows, a key, a numeric column, a
three-level categorical, a column missing in a third of the rows, and a
target with about 30 percent ones. => the pathname."
  (let ((file (merge-pathnames "churn.csv" dir)))
    (uiop:with-output-file (out file :if-exists :supersede :external-format :utf-8)
      (format out "id,age,plan,churn,score~%")
      (loop for i from 1 to rows
            do (format out "~d,~d,~a,~d,~a~%"
                       i
                       (+ 20 (mod i 40))
                       (nth (mod i 3) (list "basic" "pro" "team"))
                       (if (< (mod i 10) 3) 1 0)
                       (if (zerop (mod i 3)) "" (format nil "~,2f" (/ i 7.0))))))
    file))

(defmacro with-tabular ((&rest section) &body body)
  "Start the cell on SECTION and stop it after BODY, whatever BODY does."
  `(with-cell-stop ((tabular-start ,@section)) ,@body))

(defun tabular-cells (file)
  "FILE's rows as a list of vectors, header dropped, read back the way a
project's own tool would."
  (multiple-value-bind (header rows) (nodecode-tabular::parse-delimited
                                       (uiop:read-file-string file) #\,)
    (declare (ignore header))
    rows))

;;; --- the parser -----------------------------------------------------------------

(deftest tabular-cell-a-quoted-field-keeps-what-it-holds ()
  (multiple-value-bind (columns rows)
      (nodecode-tabular::parse-delimited
       (format nil "a,b,c~%1,\"x, y\",\"say \"\"hi\"\"\"~%2,\"two~%lines\",3~%") #\,)
    (is (equalp (vector "a" "b" "c") columns) "the header")
    (is (= 2 (length rows)) "two rows, though one of them holds a line break")
    (is (equalp (vector "1" "x, y" "say \"hi\"") (first rows)) "a comma and a doubled quote inside quotes")
    (is (equalp (vector "2" #.(format nil "two~%lines") "3") (second rows)) "a line break inside quotes")
    (is (equalp (vector "9" "") (nodecode-tabular::parse-delimited "9," #\,)) "an empty last field"))
  (is (equalp (vector "9" "8") (nodecode-tabular::parse-delimited "9,8" #\,))
      "a file with no trailing newline ends its row")
  (let* ((dir (tabular-temp-dir))
         (table (nodecode-tabular::read-table (tabular-fixture dir)))
         (back (merge-pathnames "back.csv" dir)))
    (nodecode-tabular::write-table back table (nodecode-tabular::table.rows table))
    (let* ((again (nodecode-tabular::read-table back)))
      (is (equalp (nodecode-tabular::table.columns table)
                  (nodecode-tabular::table.columns again))
          "the header round-trips")
      (is (equalp (nodecode-tabular::table.rows table) (nodecode-tabular::table.rows again))
          "and every row with it"))
    (uiop:delete-directory-tree dir :validate t)))

;;; --- the profile ----------------------------------------------------------------

(deftest tabular-cell-the-profile-says-what-the-pack-would ()
  (let* ((dir (tabular-temp-dir))
         (file (tabular-fixture dir)))
    (unwind-protect
         (with-tabular ("workspace" (namestring dir))
           (let ((text (tabular:profile-file (namestring file) :target "churn")))
             (is (search "120 rows x 5 columns" text) "the shape")
             (is (search "very small" text) "the band the row count puts it in")
             (is (search "binary classification" text) "the task, from the target's two values")
             (is (search "largest 70.0%" text) "the majority share")
             (is (search "stratified 80/20" text) "a classification target is split stratified")
             (is (search "use 5-fold cross-validation" text) "under 5000 rows, cross-validate")
             (is (search "a test fold of 24 rows" text) "and the floor is named")
             (is (search "a key: drop it before fitting" text) "the id column reads as a key")
             (is (search "categorical, 3 levels" text) "the plan column reads as categorical")
             (is (search "missing in more than a fifth" text) "the sparse column is flagged")
             (is (search "roc_auc (primary)" text) "the metric the policy names for the task")
             (is (search "never report training performance" text) "and the one rule every report keeps"))
           ;; A relative path resolves against the section's workspace.
           (is (search "churn.csv" (tabular:profile-file "churn.csv" :target "churn"))
               "a relative path is read from workspace")
           ;; No target named: the shape still comes, the task does not.
           (let ((text (tabular:profile-file (namestring file))))
             (is (search "undecided" text) "no target, no task")
             (is (search "no target named" text) "and the note says why"))
           ;; A target that is not a column is said, not guessed.
           (is (search "is not one of the columns" (tabular:profile-file (namestring file) :target "sold"))
               "an absent target is named")
           (is (search "no file at" (refusal-text tabular:tabular-error
                                                  (tabular:profile-file (namestring (merge-pathnames "nope.csv" dir)))))
               "an absent file refuses with the path it looked for")
           (is (search "small: 1,000 to 50,000" (tabular:profile-file (namestring (tabular-fixture dir :rows 1500))))
               "a bigger file reads as the next band up"))
      (uiop:delete-directory-tree dir :validate t))))

;;; --- the split ------------------------------------------------------------------

(deftest tabular-cell-the-split-is-disjoint-stratified-and-reproducible ()
  (let* ((dir (tabular-temp-dir))
         (file (tabular-fixture dir))
         (one (merge-pathnames "one/" dir))
         (two (merge-pathnames "two/" dir))
         (other (merge-pathnames "other/" dir)))
    (unwind-protect
         (with-tabular ("workspace" (namestring dir))
           (let ((said (tabular:split-file (namestring file) :target "churn" :seed 7 :out-dir (namestring one))))
             (is (search "96 train rows" said) "four fifths in train")
             (is (search "24 test rows" said) "one fifth held out")
             (is (search "stratified on churn" said) "and the stratification is said"))
           (tabular:split-file (namestring file) :target "churn" :seed 7 :out-dir (namestring two))
           (let* ((train (tabular-cells (merge-pathnames "churn.train.csv" one)))
                  (test (tabular-cells (merge-pathnames "churn.test.csv" one)))
                  (again (tabular-cells (merge-pathnames "churn.test.csv" two)))
                  (train-ids (map 'list (lambda (row) (aref row 0)) train))
                  (test-ids (map 'list (lambda (row) (aref row 0)) test)))
             (is (= 96 (length train)) "the train file holds 96 rows")
             (is (= 24 (length test)) "the test file holds 24 rows")
             (is (= 120 (length (remove-duplicates (append train-ids test-ids) :test #'equal)))
                 "no row is in both halves")
             (is (equal (mapcar (lambda (row) (aref row 0)) again) test-ids)
                 "the same seed writes the same test rows")
             (is (= 7 (count "1" test :key (lambda (row) (aref row 3)) :test #'equal))
                 "the minority class keeps its share exactly")
             (is (uiop:string-prefix-p "id,age,plan,churn,score" (uiop:read-file-string (merge-pathnames "churn.train.csv" one)))
                 "and the header comes back with the rows"))
           (tabular:split-file (namestring file) :target "churn" :seed 8 :out-dir (namestring other))
           (is (not (equal (mapcar (lambda (row) (aref row 0))
                                   (tabular-cells (merge-pathnames "churn.test.csv" other)))
                           (mapcar (lambda (row) (aref row 0))
                                   (tabular-cells (merge-pathnames "churn.test.csv" one)))))
               "another seed picks other rows")
           (is (search "is not one of" (refusal-text tabular:tabular-error
                                                     (tabular:split-file (namestring file) :target "sold")))
               "a target that is not a column refuses")
           (is (search "outside 0.05 to 0.5"
                       (refusal-text tabular:tabular-error
                                     (tabular:split-file (namestring file) :test-fraction 2.0)))
               "a fraction past half refuses"))
      (uiop:delete-directory-tree dir :validate t))))

;;; --- the saved profile ----------------------------------------------------------

(deftest tabular-cell-a-profile-is-saved-only-where-the-section-says ()
  (let* ((dir (tabular-temp-dir))
         (file (tabular-fixture dir))
         (artifacts (merge-pathnames "artifacts/" dir)))
    (unwind-protect
         (progn
           (with-tabular ("workspace" (namestring dir))
             (is (search "artifacts_dir is not set"
                         (refusal-text tabular:tabular-error
                                        (tabular:save-profile (namestring file))))
                 "no directory, no artifact: the one required setting refuses"))
           (with-tabular ("workspace" (namestring dir) "artifacts_dir" (namestring artifacts))
             (let* ((said (tabular:save-profile (namestring file) :target "churn" :name "run-1"))
                    (written (merge-pathnames "run-1.profile.json" artifacts)))
               (is (search "run-1.profile.json" said) "the answer names the file")
               (is (probe-file written) "and the file is there")
               (let ((json (shasht:read-json (uiop:read-file-string written))))
                 (is (= 120 (gethash "rows" json)) "the row count is in it")
                 (is (= 5 (length (gethash "columns" json))) "one object a column")
                 (is (equal "binary classification" (gethash "task" (gethash "task" json)))
                     "the task the profile decided")
                 (is (equal "very-small" (gethash "band" json)) "and the band")))))
      (uiop:delete-directory-tree dir :validate t))))

;;; --- the same three, through the dispatch seam -----------------------------------

(deftest tabular-cell-the-tools-answer-where-a-model-calls-them (with-session-store ("tabular-wire"))
  (let* ((dir (tabular-temp-dir))
         (file (tabular-fixture dir)))
    (unwind-protect
         (with-tabular ("workspace" (namestring dir))
           (flet ((answer (name arguments)
                    (let ((content (nle::message-content
                                    (execute-wire-call "call-1" name
                                                       (nlk:encode-json-object arguments)))))
                      (if (stringp content) content (princ-to-string content)))))
             (let ((text (answer "tabular_profile"
                                 (nlk:json-object "path" (namestring file) "target" "churn"))))
               (is (search "120 rows x 5 columns" text)
                   (format nil "the profile tool answers its report (~a)" text))
               (is (search "binary classification" text) (format nil "with the task it detected (~a)" text)))
             (let ((refused (answer "tabular_split" (nlk:json-object "target" "churn"))))
               (is (search "contract_refused" refused)
                   (format nil "a call without the required path is refused before the body runs (~a)" refused)))
             (let ((absent (answer "tabular_profile"
                                   (nlk:json-object "path" (namestring file) "target" "sold"))))
               (is (search "not one of the columns" absent)
                   (format nil "and a target the file does not carry is said in the answer (~a)" absent)))))
      (uiop:delete-directory-tree dir :validate t))))
