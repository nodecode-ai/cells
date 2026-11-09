;;;; profile.lisp --- the profile and the split of skills/tabular-inspect and
;;;; skills/tabular-split, done on the machine instead of in a Python kernel.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the prose pack asked an agent to compute with pandas, a narrow and
;;;; fixed part of it, is here as one pass over one file: shape, per-column
;;;; role and missingness, cardinality, imbalance, duplicate rows, leakage
;;;; hints by name, and the plan the policy in defaults.yaml implies. No
;;;; model is fitted here -- that half stays with whatever toolchain the
;;;; operator's project already has.

(in-package #:nodecode-tabular)

(defparameter *policy*
  '(:very-small-max 1000
    :small-max 50000
    :refuse-below 50
    :tabicl-row-ceiling 100000
    :tabicl-feature-ceiling 2000
    :max-unique-for-classification 20
    :imbalance-minority 0.10
    :test-size 0.2
    :cv-below-rows 5000
    :cv-folds 5
    :min-test-rows 30
    :ensemble-min-improvement 0.005)
  "The numbers skills/tabular/defaults.yaml carries, so the verb and the prose
cannot drift. YAML is the readable copy; this is the copy that runs.")

(defparameter *leak-hints* '("label_" "target_" "outcome" "result" "status_final"
                             "churn_date" "resolved_at" "refund" "closed_at" "final_")
  "Column-name fragments the profile skill flags as recorded after the outcome.")

;;; --- one cell, read -------------------------------------------------------------

(defun parse-number-text (text)
  "TEXT as an integer or double, exactly when the whole of it is a number.
=> (values VALUE INTEGER-P), or NIL."
  (let ((n (length text)) (i 0) (sign 1) (digits 0) (mantissa 0) (scale 1)
        (exponent 0) (exponent-sign 1) (integer t))
    (flet ((digit () (and (< i n) (digit-char-p (char text i))))
           (upto (number) (+ (* number 10) (digit-char-p (char text i)))))
      (when (and (< i n) (char= (char text i) #\-)) (setf sign -1) (incf i))
      (when (and (< i n) (char= (char text i) #\+)) (incf i))
      (loop for d = (digit) while d do (setf mantissa (upto mantissa)) (incf digits) (incf i))
      (when (and (< i n) (char= (char text i) #\.))
        (setf integer nil)
        (incf i)
        (loop for d = (digit) while d
              do (setf mantissa (upto mantissa) scale (* scale 10))
                 (incf digits) (incf i)))
      (when (zerop digits) (return-from parse-number-text nil))
      (when (and (< i n) (member (char text i) '(#\e #\E)))
        (setf integer nil)
        (incf i)
        (cond ((and (< i n) (char= (char text i) #\-)) (setf exponent-sign -1) (incf i))
              ((and (< i n) (char= (char text i) #\+)) (incf i)))
        (let ((start i))
          (loop for d = (digit) while d do (setf exponent (upto exponent)) (incf i))
          (when (= start i) (return-from parse-number-text nil))))
      (unless (= i n) (return-from parse-number-text nil))
      (let ((value (* sign mantissa (expt 10 (* exponent-sign exponent)) (/ 1 scale))))
        (values (if integer value (coerce value 'double-float)) integer)))))

(defun date-text-p (text)
  "Whether TEXT opens with a full date: 2024-05-01, 2024/05/01, 01.05.2024.
The profile skill counts a column datetime when more than nine in ten of its
values parse; this reads the digits and the separators, which is enough to
choose a time-aware split."
  (flet ((digits (from to) (every #'digit-char-p (subseq text from to))))
    (cond ((< (length text) 10) nil)
          ((and (member (char text 4) '(#\- #\/)) (member (char text 7) '(#\- #\/)))
           (and (digits 0 4) (digits 5 7) (digits 8 10)))
          ((and (member (char text 2) '(#\- #\/ #\.)) (member (char text 5) '(#\- #\/ #\.)))
           (and (digits 0 2) (digits 3 5) (digits 6 8)))
          (t nil))))

(defun cell-of (row index)
  "ROW's cell at INDEX, or \"\" when the row is short."
  (let ((cell (and (< index (length row)) (aref row index))))
    (if cell (nlk:trimmed cell) "")))

;;; --- one column, profiled ------------------------------------------------------

(defstruct (column (:constructor %make-column (name index))
                   (:conc-name "COLUMN."))
  "What one pass over a column's cells found: NAME, INDEX, the TYPE it reads
as, MISSING and TOTAL counts over the rows scanned, DISTINCT how many values
it carries (capped), TOP the most common one, MIN and MAX the numeric range,
and DATES how many cells read as a date."
  (name "" :read-only t)
  (index 0 :read-only t)
  (type "text")
  (missing 0)
  (total 0)
  (distinct 0)
  (top nil)
  (top-count 0)
  (min nil)
  (max nil)
  (dates 0)
  (capped nil)
  (all-integer t)
  (numeric t))

(defun analyze-column (name index rows &key (distinct-cap 20000))
  "NAME at INDEX profiled over ROWS, a list of vectors of cells."
  (let ((column (%make-column name index))
        (seen (make-hash-table :test #'equal)))
    (dolist (row rows)
      (incf (column.total column))
      (let ((text (cell-of row index)))
        (cond ((missing-text-p text) (incf (column.missing column)))
              (t
               (unless (> (hash-table-count seen) distinct-cap)
                 (let ((count (incf (gethash text seen 0))))
                   (when (> count (column.top-count column))
                     (setf (column.top column) text (column.top-count column) count))))
               (when (date-text-p text) (incf (column.dates column)))
               (multiple-value-bind (value integer) (parse-number-text text)
                 (if (null value)
                     (setf (column.numeric column) nil (column.all-integer column) nil)
                     (progn
                       (unless integer (setf (column.all-integer column) nil))
                       (setf (column.min column) (if (column.min column) (min (column.min column) value) value)
                             (column.max column) (if (column.max column) (max (column.max column) value) value)))))))))
    (setf (column.distinct column) (hash-table-count seen)
          (column.capped column) (> (hash-table-count seen) distinct-cap))
    (setf (column.type column)
          (cond ((= (column.missing column) (column.total column)) "empty")
                ((not (column.numeric column)) (if (> (column.dates column) (/ (column.total column) 2)) "datetime" "text"))
                ((column.all-integer column) "integer")
                (t "number")))
    column))

(defun column-role (column rows)
  "The role the profile skill would give COLUMN: :key, :constant, :categorical,
:high-cardinality, :datetime, :numeric, :text or :empty."
  (declare (ignore rows))
  (let ((present (- (column.total column) (column.missing column)))
        (name (string-downcase (column.name column))))
    (cond ((zerop present) :empty)
          ((= (column.distinct column) 1) :constant)
          ((and (>= present 10) (>= (column.distinct column) (* 0.98 present))
                (not (member (column.type column) '("integer" "number") :test #'string=)))
           :key)
          ((or (search "_id" name :from-end t) (string= name "id") (string= name "uuid"))
           :key)
          ((member (column.type column) '("datetime") :test #'string=) :datetime)
          ((member (column.type column) '("integer" "number") :test #'string=) :numeric)
          ((<= (column.distinct column) (getf *policy* :max-unique-for-classification)) :categorical)
          (t :high-cardinality))))

;; The first column playing ROLE by the policy’s reading, or NIL.
(defun role-column (data role)
  (flet ((role-of (candidate) (column-role candidate (dataset.scanned data))))
    (find role (dataset.columns-data data) :key (function role-of))))

(defun role-note (role column)
  "One clause for the report's column line."
  (case role
    (:empty "every cell missing")
    (:constant "one value: drop it")
    (:key "a key: drop it before fitting")
    (:categorical (format nil "categorical, ~d levels" (column.distinct column)))
    (:high-cardinality (format nil "high cardinality (~d values): let CatBoost handle it, do not one-hot"
                               (column.distinct column)))
    (:datetime "dates: use for a time-aware split or derive features, never feed raw")
    (:numeric "")
    (t "text: out of scope unless asked")))

;;; --- the dataset --------------------------------------------------------------

(defstruct (dataset (:constructor %make-dataset (path))
                    (:conc-name "DATASET."))
  "One file profiled: PATH what was read, ROWS x COLUMNS its shape, SCANNED
how many rows the statistics cover, SEPARATOR the delimiter, COLUMNS-DATA the
COLUMN structs, DUPLICATES how many exact duplicate rows, TASK the detected
task plist, NOTES what the profile flags."
  (path "")
  (name "")
  (rows 0)
  (scanned 0)
  (separator #\,)
  (columns-data '())
  (duplicates 0)
  (task nil)
  (notes '()))

(defun detect-task (data &key target)
  "=> (values TASK PLIST NOTES), TASK one of :classification, :regression,
:unknown: the profile skill's rules, in its order, over the target column."
  (let* ((columns (dataset.columns-data data))
         (column (find (string-downcase (or target "")) columns
                       :key (lambda (c) (string-downcase (column.name c))) :test #'string=)))
    (cond
      ((or (null target) (string= target ""))
       (values nil (list "no target named: name one and the profile decides the task")))
      ((null column)
       (values nil (list (format nil "target ~s is not one of the columns" target))))
      ((string= (column.type column) "datetime")
       (values nil (list "the target reads as a date: this is a forecasting problem, stop and ask")))
      ((= (column.missing column) (column.total column))
       (values nil (list "the target is missing in every row")))
      ((and (column.numeric column) (column.all-integer column)
            (<= (column.distinct column) (getf *policy* :max-unique-for-classification)))
       (values (list :task (if (= (column.distinct column) 2) "binary classification"
                                "multiclass classification")
                     :classes (column.distinct column)
                     :share (and (plusp (column.total column))
                                 (/ (float (column.top-count column) 1d0)
                                    (- (column.total column) (column.missing column))))
                     :column (column.name column))
               nil))
      ((column.numeric column)
       (values (list :task "regression" :column (column.name column)) nil))
      (t (values (list :task (if (= (column.distinct column) 2) "binary classification"
                                  "multiclass classification")
                       :classes (column.distinct column)
                       :share (and (plusp (column.total column))
                                   (/ (float (column.top-count column) 1d0)
                                      (- (column.total column) (column.missing column))))
                       :column (column.name column))
                 nil)))))

(defun count-duplicates (rows)
  "How many of ROWS are exact duplicates of a row before them."
  (let ((seen (make-hash-table :test #'equal)) (duplicates 0))
    (dolist (row rows)
      (let ((key (format nil "~{~a~^|~}" (coerce row 'list))))
        (if (gethash key seen) (incf duplicates) (setf (gethash key seen) t))))
    duplicates))

(defun profile-dataset (table &key path target (scan-limit 20000))
  "TABLE profiled, with the statistics over at most SCAN-LIMIT of its rows."
  (let* ((all (table.rows table))
         (scanned (if (> (length all) scan-limit) (subseq all 0 scan-limit) all))
         (data (%make-dataset (or path ""))))
    (setf (dataset.name data) (if path (file-namestring path) "")
          (dataset.rows data) (length all)
          (dataset.scanned data) (length scanned)
          (dataset.separator data) (table.separator table)
          (dataset.duplicates data) (count-duplicates all)
          (dataset.columns-data data)
          (loop for name across (table.columns table)
                for index from 0
                collect (analyze-column name index scanned)))
    (multiple-value-bind (task notes) (detect-task data :target target)
      (setf (dataset.task data) task)
      (setf (dataset.notes data) (append notes (profile-notes data))))
    data))

(defun profile-notes (data)
  "What the profile skill flags that the column lines do not already say."
  (let ((notes '())
        (bands (getf *policy* :max-unique-for-classification)))
    (declare (ignore bands))
    (when (plusp (dataset.duplicates data))
      (push (format nil "~:d exact duplicate rows: they can land in both halves of a split"
                    (dataset.duplicates data))
            notes))
    (dolist (column (dataset.columns-data data))
      (let ((name (string-downcase (column.name column))))
        (when (and (some (lambda (hint) (search hint name :from-end t)) *leak-hints*)
                   (not (and (dataset.task data)
                             (string= name (string-downcase (getf (dataset.task data) :column))))))
          (push (format nil "~a is named like a field recorded after the outcome: check it for leakage"
                        (column.name column))
                notes)))
      (when (and (plusp (column.total column))
                 (> (column.missing column) (* 0.2 (column.total column))))
        (push (format nil "~a is missing in more than a fifth of the rows: say whether that is structural"
                      (column.name column))
              notes)))
    (loop for column in (dataset.columns-data data)
          when (eq (column-role column (dataset.scanned data)) :key)
            do (push (format nil "~a looks like a key: drop it before fitting, and keep it for a grouped split"
                             (column.name column))
                     notes))
    (nreverse notes)))

;;; --- the policy the profile implies --------------------------------------------

(defun band-of (rows)
  "ROWS as the policy's band: :tiny, :very-small, :small or :large."
  (cond ((< rows (getf *policy* :refuse-below)) :tiny)
        ((< rows (getf *policy* :very-small-max)) :very-small)
        ((<= rows (getf *policy* :small-max)) :small)
        (t :large)))

(defun band-text (rows)
  "The band, in the words the policy uses."
  (ecase (band-of rows)
    (:tiny (format nil "too small: under ~:d labeled rows, stop and ask before modelling"
                   (getf *policy* :refuse-below)))
    (:very-small (format nil "very small: under ~:d rows, compare baseline, CatBoost and TabICL"
                         (getf *policy* :very-small-max)))
    (:small (format nil "small: ~:d to ~:d rows" (getf *policy* :very-small-max)
                    (getf *policy* :small-max)))
    (:large (format nil "large: over ~:d rows, CatBoost first and TabICL only under ~:d features"
                    (getf *policy* :small-max) (getf *policy* :tabicl-feature-ceiling)))))

(defun split-text (data)
  "The split the policy asks for, one line."
  (let* ((rows (dataset.rows data))
         (fraction (getf *policy* :test-size))
         (test-rows (round (* rows fraction)))
         (task (dataset.task data))
         (datetime (role-column data :datetime))
         (key (role-column data :key)))
    (declare (ignore datetime))
    (with-output-to-string (out)
      (format out "~a ~d/~d train/test holdout on ~d rows"
              (if (and task (search "classification" (getf task :task))) "stratified" "random")
              (round (* 100 (- 1 fraction))) (round (* 100 fraction)) rows)
      (when (< rows (getf *policy* :cv-below-rows))
        (format out "; under ~:d rows use ~d-fold cross-validation instead of one holdout"
                (getf *policy* :cv-below-rows) (getf *policy* :cv-folds)))
      (when (< test-rows (getf *policy* :min-test-rows))
        (format out "; a test fold of ~:d rows is under the ~:d-row floor, so cross-validate"
                test-rows (getf *policy* :min-test-rows)))
      (when key
        (format out "; keep a whole group's rows together (~a)" (column.name key))))))

(defun models-text (data)
  "The model order the policy asks for, one line."
  (ecase (band-of (dataset.rows data))
    (:tiny "nothing yet: ask the operator first")
    (:very-small "baseline, then CatBoost, then TabICL -- compare all three on the same holdout")
    (:small "baseline, then CatBoost, then TabICL; an ensemble only if it beats the best single one")
    (:large "CatBoost first; TabICL only while the feature count stays modest; ensemble only if it wins")))

(defun metrics-text (data)
  "The primary and secondary metric the policy names for the detected task."
  (let ((task (and (dataset.task data) (getf (dataset.task data) :task))))
    (cond ((null task) "name the task first: roc_auc and macro_f1 need to know which one it is")
          ((search "binary" task) "roc_auc (primary), then average_precision, brier_score, f1")
          ((search "multiclass" task) "macro_f1 (primary), then balanced_accuracy, log_loss")
          (t "rmse (primary), then mae, r2, mape"))))

;;; --- the report ---------------------------------------------------------------

(defun pad (text width)
  "TEXT followed by spaces to WIDTH, or one space when it is longer."
  (let ((text (princ-to-string text)))
    (if (< (length text) width)
        (concatenate 'string text (make-string (- width (length text)) :initial-element #\Space))
        (concatenate 'string text " "))))

(defun separator-name (separator)
  (case separator (#\Tab "tab-separated") (#\, "comma-separated") (#\; "semicolon-separated")
    (#\| "pipe-separated") (t (format nil "separated on ~c" separator))))

(defun dataset-text (data)
  "DATA as the report a reader (or a model) plans from."
  (with-output-to-string (out)
    (flet ((line (label text) (format out "  ~a~a~%" (pad label 12) text)))
      (format out "tabular: ~a~%" (dataset.path data))
      (line "shape" (format nil "~:d rows x ~:d columns, ~a~@[, first ~:d scanned~]"
                             (dataset.rows data) (length (dataset.columns-data data))
                             (separator-name (dataset.separator data))
                             (unless (= (dataset.scanned data) (dataset.rows data))
                               (dataset.scanned data))))
      (line "band" (band-text (dataset.rows data)))
      (let ((task (dataset.task data)))
        (line "task" (if (null task)
                          "undecided: see the notes"
                          (format nil "~a~@[ (~d classes, largest ~,1f%)~]~@[, leakage aside~]"
                                  (getf task :task) (getf task :classes)
                                  (and (getf task :share) (* 100 (getf task :share)))
                                  nil))))
      (line "split" (split-text data))
      (line "models" (models-text data))
      (line "metric" (metrics-text data))
      (format out "  columns~%")
      (dolist (column (dataset.columns-data data))
        (let* ((role (column-role column (dataset.scanned data)))
               (note (role-note role column)))
          (format out "    ~a~a~:d distinct  ~:d missing~@[ (~,1f%)~]~@[  ~a~]~%"
                  (pad (column.name column) 20)
                  (pad (column.type column) 10)
                  (column.distinct column)
                  (column.missing column)
                  (and (plusp (column.missing column)) (plusp (column.total column))
                       (* 100 (/ (float (column.missing column) 1d0)
                                 (column.total column))))
                  (if (zerop (length note)) nil note))))
      (when (dataset.notes data)
        (format out "  notes~%")
        (dolist (note (dataset.notes data))
          (format out "    - ~a~%" note)))
      (format out "  next  fit the simplest model that is adequate; never report training performance as model performance~%"))))

;;; --- the same thing, as a file a later turn reads -------------------------------

(defun column-json (column rows)
  "COLUMN as a JSON object, with the role it plays."
  (nlk:json-object
   "name" (column.name column)
   "type" (column.type column)
   "role" (string-downcase (string (column-role column rows)))
   "distinct" (column.distinct column)
   "missing" (column.missing column)
   "total" (column.total column)))

(defun dataset-json (data)
  "DATA as a JSON object: what a second turn reads instead of re-profiling."
  (nlk:json-object
   "path" (dataset.path data)
   "rows" (dataset.rows data)
   "columns" (coerce (loop for column in (dataset.columns-data data)
                            collect (column-json column (dataset.scanned data)))
                     'vector)
   "duplicates" (dataset.duplicates data)
   "task" (let ((task (dataset.task data)))
            (if task (nlk:json-object
                      "task" (getf task :task)
                      "column" (or (getf task :column) "")
                      "classes" (or (getf task :classes) 0))
                (nlk:json-object "task" "undecided" "column" "" "classes" 0)))
   "band" (string-downcase (string (band-of (dataset.rows data))))
   "split" (split-text data)
   "models" (models-text data)
   "metrics" (metrics-text data)
   "notes" (coerce (dataset.notes data) 'vector)))

;;; --- the split, done honestly and deterministically -----------------------------

(defun mix (x)
  "X mixed, splitmix64's finalizer: a deterministic 64-bit answer to a row
index, so the same seed picks the same rows on any machine."
  (let ((mask #xFFFFFFFFFFFFFFFF))
    (setf x (logand (+ x #x9E3779B97F4A7C15) mask))
    (setf x (logand (* (logxor x (ash x -30)) #xBF58476D1CE4E5B9) mask))
    (setf x (logand (* (logxor x (ash x -27)) #x94D049BB133111EB) mask))
    (logxor x (ash x -31))))

(defun split-key (seed index)
  "The order (SEED, INDEX) ranks in: a split is a sort, never a coin flipped
in passing, so the same file and seed answer the same halves."
  (mix (logand (+ (* (logand seed #xFFFFFFFF) 2654435761) index) #xFFFFFFFFFFFFFFFF)))

(defun find-column-index (table name)
  "The index of NAME's column, case-insensitively, or NIL."
  (and name
       (plusp (length name))
       (position (string-downcase name)
                 (table.columns table)
                 :key #'string-downcase :test #'string=)))

(defun split-rows (table &key target seed (fraction 0.2))
  "TABLE's rows split into train and test: (values TRAIN TEST ASIDE), the
tests in file order, stratified on TARGET when it names a column, and chosen
by ranking (SEED, row index) rather than at random."
  (let* ((rows (coerce (table.rows table) 'vector))
         (n (length rows))
         (index (find-column-index table target))
         (test '()))
    (flet ((take (members)
             (let ((k (max 1 (min (1- (length members))
                                  (round (* (length members) fraction))))))
               (when (plusp (length members))
                 (dolist (i (subseq (sort (copy-list members) #'<
                                           :key (lambda (j) (split-key seed j)))
                                     0 k))
                   (push i test))))))
      (if index
          (let ((groups (make-hash-table :test #'equal)))
            (loop for i from 0 below n
                  do (push i (gethash (cell-of (aref rows i) index) groups)))
            (maphash (lambda (value members) (declare (ignore value)) (take members)) groups))
          (take (loop for i from 0 below n collect i))))
    (let ((test (sort test #'<)))
      (values (loop for i from 0 below n unless (member i test) collect (aref rows i))
              (loop for i in test collect (aref rows i))
              (length test)))))
