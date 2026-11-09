;;;; cell.lisp --- the section, the three verbs, the manual, /tabular.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tabular skill pack was eleven Markdown skills an agent read and then
;;;; carried out in Python. Here the part that needs no model at all is a verb
;;;; -- profile a file, split it honestly, save the profile -- and the rest
;;;; stays prose, on the manual, with the pack's own files beside it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "tabular": {"workspace": "", "delimiter": ",", "max_rows_scanned": 20000,
;;;;               "artifacts_dir": ""}
;;;; workspace is where a relative path is read from; artifacts_dir is where
;;;; tabular_save writes, and only that verb needs it.

(in-package #:nodecode-tabular)

(defparameter +manual+
  "Tabular work starts with the data, not with a model. This cell profiles a
delimited file and splits it honestly, on this machine, and carries the rest of
the tabular skill pack as prose beside it: skill/skills/tabular/SKILL.md is the entry
point, skill/skills/tabular/defaults.yaml the numbers, skill/references/pitfalls.md
the traps, skill/skills/tabular-*/SKILL.md one skill per step, and skill/examples/
three worked walks through it.

Workflow, in order: inspect, load, decide the task, decide the split, choose a
model, train, evaluate, and only then save. Never fit before inspecting, and
never report training performance as model performance.

Model policy: the simplest model that is adequate. Baseline first, then CatBoost,
then TabICL, and an ensemble only when evaluation shows it beats the best single
model on the same held-out rows. Stop and ask the operator when the target is
ambiguous or absent, when a feature leaks the outcome, when there are fewer than
50 labeled rows, when the requested metric does not fit the task, or when a
grouping or temporal constraint would empty the test set.

The verbs, callable from eval or by the model as tools:
  (tabular:profile-file \"data.csv\" :target \"churn\")
      shape, band, task, per-column role, missingness, cardinality, duplicate
      rows, leakage hints by name, and the split and model order the policy
      implies. It fits nothing and changes nothing.
  (tabular:split-file \"data.csv\" :target \"churn\" :seed 42)
      writes data.train.csv and data.test.csv beside the file (or into
      :out-dir), stratified on the target when one is named, chosen by ranking
      (seed, row index) so the same seed reproduces the same halves. A grouping
      or temporal constraint the policy asks for is still the caller's to
      honour.
  (tabular:save-profile \"data.csv\" :target \"churn\" :name \"run-1\")
      the same profile as JSON under artifacts_dir, for a later turn to read
      instead of profiling the file again.
The pack's Python steps -- pandas, CatBoost, TabICL, joblib checkpoints -- are
not replaced: run them in the project's own environment, with the split this
cell wrote."
  "What (help :tabular) answers while the cell runs.")

;;; --- the two request shapes ----------------------------------------------------

(nlk:define-contract tabular-request
  (:doc "Profile one delimited file: shape, column roles, missingness, cardinality and the plan the policy implies. Reads the file and answers text; fits nothing.")
  ("path" :text :required t :doc "the file to read, relative to workspace when that is set")
  ("target" :string :default "" :doc "the column to model; empty profiles the file without deciding the task")
  ("delimiter" :string :default "" :doc "one of , ; | or tab; empty uses the section's delimiter"))

(nlk:define-contract tabular-split-request
  (:doc "Split one delimited file into train and test files, stratified on the target when one is named, chosen by a seed so a rerun reproduces the same halves.")
  ("path" :text :required t :doc "the file to split, relative to workspace when that is set")
  ("target" :string :default "" :doc "the column to stratify on; empty splits without stratifying")
  ("delimiter" :string :default "" :doc "one of , ; | or tab; empty uses the section's delimiter")
  ("seed" :integer :default 42 :doc "the seed the halves are ranked by")
  ("test_fraction" :number :default 0 :doc "the share held out, between 0.05 and 0.5; 0 uses the section default")
  ("out_dir" :string :default "" :doc "where the two files go; empty writes beside the file"))

(nlk:define-contract tabular-save-request
  (:doc "Write the profile of one delimited file as JSON under artifacts_dir, for a later turn to read instead of profiling again.")
  ("path" :text :required t :doc "the file to profile, relative to workspace when that is set")
  ("target" :string :default "" :doc "the column to model; empty profiles without deciding the task")
  ("delimiter" :string :default "" :doc "one of , ; | or tab; empty uses the section's delimiter")
  ("name" :string :default "" :doc "the artifact's name; empty uses the file's own name"))

;;; --- paths and delimiters ------------------------------------------------------

(defun expand-tilde (text)
  "TEXT with a leading ~ replaced by the account's home."
  (if (and (plusp (length text)) (char= (char text 0) #\~))
      (namestring (merge-pathnames (subseq text (if (and (> (length text) 1)
                                                          (member (char text 1) '(#\/ #\\)))
                                                  2 1))
                                    (user-homedir-pathname)))
      text))

(defun pathname-of (text)
  (uiop:parse-native-namestring (expand-tilde (nlk:trimmed text))))

(defun resolve-path (text &key directory)
  "TEXT as a pathname: absolute as written, otherwise under DIRECTORY (a
pathname) or the section's workspace, otherwise as the process reads it."
  (let* ((path (pathname-of text))
         (base (or directory
                   (let ((workspace (setting :workspace)))
                     (and workspace (plusp (length (nlk:trimmed workspace)))
                          (uiop:ensure-directory-pathname (pathname-of workspace)))))))
    (cond ((uiop:absolute-pathname-p path) path)
          (base (merge-pathnames path base))
          (t path))))

(defun delimiter-of (text)
  "The character to split on: TEXT when it names one, else the section's."
  (let ((text (nlk:trimmed (if (and text (plusp (length (nlk:trimmed text))))
                                text
                                (setting :delimiter)))))
    (cond ((string= text "tab") #\Tab)
          ((string= text (string #\Tab)) #\Tab)
          ((= (length text) 1) (char text 0))
          ((string= text "comma") #\,)
          ((string= text "semicolon") #\;)
          ((string= text "pipe") #\|)
          (t (fail "~s is not a delimiter: name , ; | or tab" text)))))

(defun ensure-file (path)
  "PATH, or the refusal a caller can read."
  (if (probe-file path) path (fail "no file at ~a" (namestring path))))

(defun read-profiled (path target delimiter)
  "The report DATA for PATH, the three verbs' shared first half."
  (let ((file (ensure-file path)))
    (profile-dataset (read-table file :separator (delimiter-of delimiter))
                     :path (namestring (or (probe-file file) file))
                     :target (let ((target (and target (nlk:trimmed target))))
                               (and target (plusp (length target)) target))
                     :scan-limit (setting :max-rows-scanned))))

;;; --- the three verbs -----------------------------------------------------------

(define-verb profile-file (path &key target delimiter)
  "Profile PATH -- shape, band, task, per-column role and missingness, and the
split and model order the policy implies -- as text. Fits nothing."
  (dataset-text (read-profiled (resolve-path path) target delimiter)))

(define-verb split-file (path &key target delimiter seed test-fraction out-dir)
  "Write PATH's rows as two files, train and test, stratified on TARGET when
it names a column. => one line saying what was written and where."
  (let* ((file (ensure-file (resolve-path path)))
         (table (read-table file :separator (delimiter-of delimiter)))
         (target (let ((target (and target (nlk:trimmed target))))
                   (and target (plusp (length target)) target)))
         (fraction (let ((fraction (or test-fraction 0)))
                     (cond ((plusp fraction)
                            (unless (<= 0.05 fraction 0.5)
                              (fail "test_fraction ~a is outside 0.05 to 0.5" fraction))
                            (float fraction 1d0))
                           (t (float (getf *policy* :test-size) 1d0)))))
         (directory (if (and out-dir (plusp (length (nlk:trimmed out-dir))))
                        (uiop:ensure-directory-pathname (resolve-path out-dir))
                        (uiop:pathname-directory-pathname file)))
         (stem (pathname-name file)))
    (when (and target (null (find-column-index table target)))
      (fail "target ~s is not one of ~{~a~^, ~}" target (coerce (table.columns table) 'list)))
    (multiple-value-bind (train test aside)
        (split-rows table :target target :seed (or seed 42) :fraction fraction)
      (let ((train-path (merge-pathnames (format nil "~a.train.csv" stem) directory))
            (test-path (merge-pathnames (format nil "~a.test.csv" stem) directory)))
        (write-table train-path table train)
        (write-table test-path table test)
        (format nil "tabular: ~:d train rows to ~a, ~:d test rows to ~a (~:d of ~:d held out, seed ~:d~@[, stratified on ~a~])"
                (length train) (namestring train-path)
                (length test) (namestring test-path)
                aside (+ (length train) (length test))
                (or seed 42) target)))))

(define-verb save-profile (path &key target delimiter name)
  "Write PATH's profile as JSON under artifacts_dir. => where it landed."
  (let ((directory (let ((dir (setting :artifacts-dir)))
                     (and dir (plusp (length (nlk:trimmed dir)))
                          (uiop:ensure-directory-pathname (resolve-path dir))))))
    (unless directory
      (fail "artifacts_dir is not set: set it in the tabular section of the config, then (restart-cells \"~a\")"
            (package-name *package*)))
    (let* ((data (read-profiled (resolve-path path) target delimiter))
           (stem (or (let ((name (and name (nlk:trimmed name))))
                       (and name (plusp (length name)) name))
                     (pathname-name (pathname-of path))))
           (file (merge-pathnames (format nil "~a.profile.json" stem) directory)))
      (ensure-directories-exist file)
      (uiop:with-output-file (out file :if-exists :supersede :external-format :utf-8)
        (write-string (nlk:pretty-json (dataset-json data)) out)
        (terpri out))
      (format nil "tabular: profile written to ~a (~:d rows, ~:d columns, ~a)"
              (namestring file) (dataset.rows data)
              (length (dataset.columns-data data)) (band-text (dataset.rows data))))))

;;; --- the tool bodies: the declared record, unpacked -------------------------------

;; A tool registered against a contract is handed the record the declaration
;; materialized, never the hash; the verbs above take ordinary arguments so
;; eval can call them, and these three are the seam between the two.

(defun profile-tool (request)
  (profile-file (tabular-request-path request)
                :target (tabular-request-target request)
                :delimiter (tabular-request-delimiter request)))

(defun split-tool (request)
  (split-file (tabular-split-request-path request)
              :target (tabular-split-request-target request)
              :delimiter (tabular-split-request-delimiter request)
              :seed (tabular-split-request-seed request)
              :test-fraction (tabular-split-request-test-fraction request)
              :out-dir (tabular-split-request-out-dir request)))

(defun save-tool (request)
  (save-profile (tabular-save-request-path request)
                :target (tabular-save-request-target request)
                :delimiter (tabular-save-request-delimiter request)
                :name (tabular-save-request-name request)))

;;; --- /tabular ------------------------------------------------------------------

(defun workflow-text ()
  "What /tabular typed alone answers: the workflow, the policy in force, and
where the pack's own files sit."
  (format nil "~{~a~%~}"
          (list "tabular: inspect, split, decide, fit, evaluate -- in that order."
                (format nil "  bands      under ~:d rows ask first; under ~:d compare all three families; ~:d and up CatBoost first"
                        (getf *policy* :refuse-below) (getf *policy* :very-small-max)
                        (getf *policy* :small-max))
                (format nil "  split      ~d/~d holdout, cross-validate under ~:d rows, stratify on a classification target"
                        (round (* 100 (getf *policy* :test-size)))
                        (round (* 100 (- 1 (getf *policy* :test-size))))
                        (getf *policy* :cv-below-rows))
                (format nil "  settings   workspace ~s, delimiter ~s, max_rows_scanned ~d, artifacts_dir ~s"
                        (or (setting :workspace) "") (setting :delimiter)
                        (setting :max-rows-scanned) (or (setting :artifacts-dir) "unset"))
                "  verbs      (tabular:profile-file path :target name), (tabular:split-file ...), (tabular:save-profile ...)"
                "  prose      (help :tabular), and skill/ beside this cell's file for the pack's own skills")))

(defun run-slash (args session-id)
  "/tabular typed alone: the workflow and the settings in force. /tabular
PATH: the profile of that file."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (handler-case
        (if (plusp (length text)) (dataset-text (read-profiled (resolve-path text) nil nil))
            (workflow-text))
      (tabular-error (condition) (format nil "tabular: ~a" condition))
      (file-error (condition) (format nil "tabular: ~a" condition)))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell tabular
  (:section ("tabular")
    (:guide "workspace is where relative paths are read from and artifacts_dir where a saved profile lands; a delimiter is one of , ; | or tab; max_rows_scanned bounds the pass a profile makes over a big file")
    ("workspace" :string :default ""
     :doc "the directory relative paths are read from; empty means the process's own")
    ("delimiter" :choice :options '("," ";" "|" "tab") :default ","
     :doc "the delimiter a file is split on unless a call names another")
    ("max_rows_scanned" :integer :default 20000 :min 100
     :doc "how many rows a profile reads before it stops; the row count is still the file's")
    ("artifacts_dir" :string :required t
     :doc "where tabular_save writes its JSON; the other verbs need no directory"))
  (:tool "tabular_profile" (nlk:contract 'tabular-request) 'profile-tool)
  (:tool "tabular_split" (nlk:contract 'tabular-split-request) 'split-tool)
  (:tool "tabular_save" (nlk:contract 'tabular-save-request) 'save-tool)
  (:help :tabular
    "tabular: profile and split a delimited file with no Python, and the plan a tabular analysis starts from"
    +manual+)
  (:command "tabular" 'run-slash
            :description "Profile a table, or show the workflow"))
