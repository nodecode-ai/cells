;;;; cell.lisp --- the primer, the three read verbs, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin was three tools -- inspect_project, diagnose_error and
;;;; review_changes -- over a project directory. This cell keeps the three
;;;; names, the arguments and the JSON each one answers, and reads the same
;;;; things: names and sizes, a pasted trace, and tracked git diffs. Nothing
;;;; here writes to the project, runs it, or leaves this machine.

(in-package #:nodecode-devsentry)

(defparameter +primer+
  "devsentry reads a project without changing it. Three tools, each answering JSON:
  inspect_project(path, max_depth 1-12 default 6, max_entries 1-2000 default 500)
      the folder inventoried: entries, approximate language totals by file
      extension, and the dependency, build, cache and version-control folders it
      did not enter. File contents are not opened and symlinks are not followed.
  diagnose_error(trace, project_path?, context_lines 0-5 default 2)
      a pasted traceback read as exception names, frames (Python, JavaScript,
      Java, .NET, Go, Rust) and explainable leads. With project_path, a few
      source lines are quoted for cited files that stay inside it. The answer
      says certainty: leads_only -- leads to verify, not a confirmed cause.
  review_changes(path, context_lines 0-10 default 3, max_patch_bytes 200-80000)
      tracked git changes, staged and unstaged as separate scopes, with a patch
      a file. Nothing is staged or written; untracked files are counted and not
      read, and a path that looks like a credential carries no patch.
How to work: name the directory explicitly; every path is the folder a person
named. Read the leads as things to check rather than a diagnosis. The operator
reads the same inventory with /devsentry, or /devsentry PATH."
  "What (help :devsentry) answers while the cell runs.")

;;; --- the answer's shape ----------------------------------------------------------

(defparameter +answer-chars+ 60000
  "How much JSON one answer may be before it is reduced.")

(defun merge-json (into from)
  "Every member of FROM set on INTO."
  (maphash (lambda (key value) (setf (gethash key into) value)) from)
  into)

(defun error-answer (tool code message)
  "The answer of a call that could not run."
  (nlk:json-object "ok" nil
                    "tool" tool
                    "error" (nlk:json-object "code" code
                                              "message" (nlk:clip (princ-to-string message) 500))))

(defun encode-answer (object)
  "OBJECT encoded, reduced when it is over the bound, and never over it."
  (let ((text (nlk:encode-json-object object)))
    (if (<= (length text) +answer-chars+)
        text
        (progn
          (setf (gethash "output_truncated" object) t)
          (dolist (key '("entries" "frames" "leads"))
            (let ((value (gethash key object)))
              (when (and (vectorp value) (> (length value) 10))
                (setf (gethash key object) (subseq value 0 10)))))
          (dolist (key '("staged" "unstaged"))
            (let ((scope (gethash key object)))
              (when (hash-table-p scope)
                (let ((files (gethash "files" scope)))
                  (when (and (vectorp files) (> (length files) 8))
                    (let ((kept (subseq files 0 8)))
                      (loop for entry across kept
                            for patch = (gethash "patch" entry)
                            when (and (stringp patch) (> (length patch) 400))
                              do (setf (gethash "patch" entry) (subseq patch 0 400)
                                       (gethash "patch_truncated" entry) t))
                      (setf (gethash "files" scope) kept)))))))
          (let ((second (nlk:encode-json-object object)))
            (if (<= (length second) +answer-chars+)
                second
                (nlk:encode-json-object
                 (nlk:json-object
                  "ok" nil
                  "error" (nlk:json-object
                            "code" "output_too_large"
                            "message" "The result exceeded the output bound after reduction.")))))))))

(defmacro answering (tool &body body)
  "BODY's JSON object encoded, a refusal answered as the JSON that names it."
  `(handler-case (encode-answer (progn ,@body))
     (devsentry-refusal (condition)
       (encode-answer (error-answer ,tool
                                     (devsentry-refusal-code condition)
                                     (devsentry-refusal-detail condition))))
     (error (condition)
       (encode-answer (error-answer ,tool "internal" (princ-to-string condition))))))

(defun bounded-integer (value default lower upper)
  "VALUE clamped into LOWER..UPPER => (values NUMBER CLAMPED-P)."
  (cond ((null value) (values default nil))
        ((not (integerp value)) (refuse "invalid_argument" "expected an integer"))
        ((< value lower) (values lower t))
        ((> value upper) (values upper t))
        (t (values value nil))))

;;; --- the boundaries the model crosses ---------------------------------------------

(nlk:define-contract inspect-project
  (:doc "Read-only inventory of one project directory. Returns nested entries,
approximate language counts by file extension, and the directories that were
skipped because they are dependencies, build output, caches, or version
control. Does not open file contents, follow symlinks, or execute project code.")
  ("path" :text :required t
   :doc "the directory to inventory, absolute or relative to this process's directory")
  ("max_depth" :integer
   :doc "how many directory levels to enter below the selected directory, clamped to 1..12 (default 6)")
  ("max_entries" :integer
   :doc "how many entries to return, clamped to 1..2000 (default 500)"))

(nlk:define-contract diagnose-error
  (:doc "Analyze a pasted error message or stack trace: exception names, file
paths, line numbers and explainable debugging leads. Leads are suggestions to
verify, not a confirmed root cause. With project_path, a few source lines are
quoted for cited files that stay inside that directory and are not symlinks.
Does not execute project code.")
  ("trace" :text :required t :slot trace-text
   :doc "the pasted error message or stack trace; the tool only parses this text")
  ("project_path" :string
   :doc "a project directory: a cited file is read only when it resolves inside it")
  ("context_lines" :integer
   :doc "source lines on each side of a cited line, clamped to 0..5 (default 2)"))

(nlk:define-contract review-changes
  (:doc "Read the tracked Git changes of one repository directory: staged
changes (index against HEAD) and unstaged changes (working tree against the
index) as separate scopes. Untracked files are counted and not read, no patch
is carried for a path that looks like a secret, and nothing is staged or
edited.")
  ("path" :text :required t
   :doc "a Git work tree, or a directory inside one; the review is limited to this directory")
  ("context_lines" :integer
   :doc "unified diff context lines, clamped to 0..10 (default 3)")
  ("max_patch_bytes" :integer
   :doc "bytes kept from each file's patch, clamped to 200..80000 (default 20000)"))

;;; --- the three tools -------------------------------------------------------------

(defun inspect-project-tool (report)
  "inspect_project: the folder inventoried, read-only."
  (answering "inspect_project"
    (multiple-value-bind (root info) (resolve-directory (inspect-project-path report))
      (multiple-value-bind (max-depth depth-clamped)
          (bounded-integer (inspect-project-max-depth report) (setting :max-depth) 1 12)
        (multiple-value-bind (max-entries entries-clamped)
            (bounded-integer (inspect-project-max-entries report) (setting :max-entries) 1 2000)
          (let ((answer (nlk:json-object
                         "ok" t
                         "tool" "inspect_project"
                         "path" info
                         "limits" (nlk:json-object "max_depth" max-depth
                                                    "max_entries" max-entries
                                                    "clamped" (and (or depth-clamped entries-clamped) t)))))
            (merge-json answer (inspect-directory root max-depth max-entries))))))))

(defun diagnose-error-tool (report)
  "diagnose_error: a pasted trace, and the lines it cites when they are here."
  (let ((trace (diagnose-error-trace-text report)))
    (when (zerop (length (nlk:trimmed trace)))
      (refuse "invalid_argument" "trace must be a non-empty string"))
    (when (find #\Null trace)
      (refuse "invalid_argument" "trace contains a null byte"))
    (answering "diagnose_error"
      (multiple-value-bind (context clamped)
          (bounded-integer (diagnose-error-context-lines report) (setting :context-lines) 0 5)
        (let ((parsed (parse-trace trace))
              (project-info nil))
          (let ((raw (diagnose-error-project-path report)))
            (when raw
              (handler-case
                  (multiple-value-bind (root info) (resolve-directory raw)
                    (attach-source-context parsed root context)
                    (setf project-info info))
                (devsentry-refusal (condition)
                  (setf project-info
                        (nlk:json-object
                         "ok" nil
                         "error" (nlk:json-object
                                   "code" (devsentry-refusal-code condition)
                                   "message" (devsentry-refusal-detail condition))))))))
          (trace-answer parsed
                        :context-lines context
                        :clamped clamped
                        :project-info project-info
                        :excerpt (unless (getf parsed :parsed)
                                   (let ((text (nlk:trimmed trace)))
                                     (subseq text 0 (min 500 (length text)))))))))))

(defun review-changes-tool (report)
  "review_changes: tracked diffs, staged and unstaged, read-only."
  (answering "review_changes"
    (multiple-value-bind (root info) (resolve-directory (review-changes-path report))
      (multiple-value-bind (context context-clamped)
          (bounded-integer (review-changes-context-lines report) (setting :context-lines) 0 10)
        (multiple-value-bind (max-patch patch-clamped)
            (bounded-integer (review-changes-max-patch-bytes report) (setting :max-patch-bytes) 200 80000)
          (let ((body (review-repository root context max-patch)))
            (if (and (null (gethash "ok" body)) (null (gethash "tool" body)))
                (let ((error (gethash "error" body)))
                  (error-answer "review_changes"
                                 (or (gethash "code" error) "git_error")
                                 (or (gethash "message" error) "git failed")))
                (progn
                  (setf (gethash "tool" body) "review_changes"
                        (gethash "path" body) info
                        (gethash "limits" body) (nlk:json-object
                                                   "context_lines" context
                                                   "max_patch_bytes" max-patch
                                                   "clamped" (and (or context-clamped patch-clamped) t)))
                  body))))))))

;;; --- the operator's own look ------------------------------------------------------

(defun summary-text (arguments)
  "What /devsentry shows: the folder's own counts and its largest languages."
  (let ((path (nlk:trimmed (or arguments ""))))
    (multiple-value-bind (root info)
        (resolve-directory (if (plusp (length path)) path
                                (uiop:native-namestring (uiop:getcwd))))
      (declare (ignore info))
      (let* ((body (inspect-directory root (setting :max-depth) (setting :max-entries)))
             (summary (gethash "summary" body))
             (sensitive (gethash "sensitive_files" summary)))
        (with-output-to-string (out)
          (format out "devsentry: ~a~%~d file~:p, ~d folder~:p, ~d symlink~:p~@[, ~d credential-like name~:p not read~]"
                  root (gethash "files" summary) (gethash "directories" summary)
                  (gethash "symlinks" summary) (and (plusp sensitive) sensitive))
          (loop for row across (gethash "languages" body)
                for index from 0
                while (< index 8)
                do (format out "~%  ~24a ~d file~:p" (gethash "language" row)
                           (gethash "files" row)))
          (loop for folder across (gethash "skipped" body)
                for index from 0
                while (< index 4)
                do (format out "~%  skipped ~a (~a)" (gethash "path" folder)
                           (gethash "reason" folder))))))))

(defun run-command (arguments session-id)
  "/devsentry [PATH]: this folder's inventory, or the one named."
  (declare (ignore session-id))
  (handler-case (summary-text arguments)
    (devsentry-refusal (condition)
      (format nil "devsentry: ~a" (devsentry-refusal-detail condition)))
    (error (condition)
      (format nil "devsentry: ~a" (nlk:one-line (princ-to-string condition))))))

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell devsentry
  (:section ("devsentry")
    (:guide "nothing is required: a tool call names its own directory, and every setting below is a default it reads when the call does not say")
    ("max_depth" :integer :default 6 :min 1
     :doc "how many directory levels an inventory enters")
    ("max_entries" :integer :default 500 :min 1
     :doc "how many entries one inventory returns")
    ("context_lines" :integer :default 2 :min 0
     :doc "source lines quoted on each side of a cited line, and the diff's context")
    ("max_patch_bytes" :integer :default 20000 :min 200
     :doc "bytes kept from each file's patch")
    ("git_timeout_seconds" :integer :default 20 :min 1
     :doc "one git run's ceiling"))
  (:help :devsentry
         "devsentry: read-only project inventory, traceback leads and tracked git diffs - (help :devsentry)"
         +primer+)
  (:tool "inspect_project" (nlk:contract 'inspect-project) #'inspect-project-tool)
  (:tool "diagnose_error" (nlk:contract 'diagnose-error) #'diagnose-error-tool)
  (:tool "review_changes" (nlk:contract 'review-changes) #'review-changes-tool)
  (:command "devsentry" 'run-command
            :description "Inventory this project, or the folder you name"
            :argument-hint "[PATH]"))
