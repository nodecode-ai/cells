;;;; cell.lisp --- the primer, the three reviews, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin was one tool, `diff_review', whose three modes are
;;;; kept here as they were: the working tree against a base, the commits a
;;;; branch adds on a merge target, or a diff handed in as text. What it
;;;; answers is the same JSON, built here instead of by Python.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "diff-review": {"repo": "/work/project", "timeout_seconds": 30,
;;;;                   "max_warnings": 200, "max_files": 500}
;;;; Nothing here is required: with no section at all the working tree of this
;;;; process's own directory is what gets reviewed.

(in-package #:nodecode-diff-review)

(defparameter +primer+
  "diff_review reads a git diff and reports, on added lines only: debug prints
(`print(`, `pprint(`, JavaScript `console.*` calls), possible hardcoded secrets
(password, secret, api_key, access_key, private_key, token, quoted or in .env
and YAML form), Python `except:` and `except Exception`, and TODO/FIXME/HACK/XXX.
Three modes, one tool:
  diff_review                          the working tree against base (default
      HEAD): staged, unstaged and untracked files. This is the one to call
      before you commit.
  diff_review(target: \"origin/main\")   the commits on base that are not on
      target, like a pull request's diff (git diff target...base). Committed
      changes only; uncommitted work is said in `notes'.
  diff_review(diff_text: \"...\")        a unified diff you already have, e.g.
      `git diff` read from a terminal this process cannot reach.
`path' names the repository directory, absolute; without it the diff-review
section's repo is used, and without that this process's own directory. base
and target take branches, tags or commit SHAs, never file paths.
The answer is JSON, the original plugin's shape: `status' is ok, no_diff or
error (with `message' and `error_type'); then files_changed, total_added,
total_removed; `files' (path, added, removed, status -- added, modified,
deleted, renamed, copied, binary, mode_changed, untracked) and
`untracked_files' (its first 200 by name), `files' cut at the section's
max_files with files_truncated saying so; `warnings', each (file, line,
issue) on an added line, cut at max_warnings with warning_count and
warnings_truncated saying how many there were; `comparison' (mode, base,
target, merge_base, repo_root); and `notes' -- what was skipped, and the
things a reviewer should not be surprised by.
Untracked files are read as their content (regular files only, at most 1 MiB
each, 200 files and 5 MiB a review); a binary one is listed and not read.
git 2.28 or newer must be installed; the runs take the section's
timeout_seconds and their whole process tree is killed past it. A credential
the shape of a token in a message is masked. Nothing is written and nothing
leaves this machine.
How to work: run diff_review before committing or opening a pull request, fix
what it names (a debug print left in, a secret that belongs in the environment,
a TODO you meant to close), and run it again until only what you meant is left.
The operator reads the same review with /diff-review, or /diff-review
origin/main for the branch's own commits."
  "What (help :diff-review) answers while the cell runs.")

;;; --- where the review looks --------------------------------------------------------

(defun absolute-directory (value what)
  "VALUE as an absolute directory name, its trailing separator taken off, or
a refusal naming WHAT the caller wanted."
  (let* ((text (string-trim '(#\Space #\Tab)
                             (if (stringp value) value (uiop:native-namestring value))))
         (expanded (if (uiop:string-prefix-p "~" text)
                       (uiop:native-namestring (nlk:expand-home text))
                       text)))
    (unless (or (and (plusp (length expanded)) (char= #\/ (char expanded 0)))
                ;; a Windows drive, C:\... or C:/...
                (and (> (length expanded) 2)
                     (alpha-char-p (char expanded 0))
                     (char= #\: (char expanded 1))))
      (refuse "invalid_argument" "~a must be an absolute path, got ~s" what value))
    (unless (uiop:directory-exists-p (uiop:parse-native-namestring expanded))
      (refuse "not_a_directory" "~a names no directory here: ~s" what value))
    (string-right-trim "/\\" (uiop:native-namestring (uiop:parse-native-namestring expanded)))))

(defun workspace (path)
  "The directory to review: PATH when the call named one, else the section's
repo, else this process's own working directory."
  (let ((named (and path (string-trim '(#\Space #\Tab) path))))
    (cond ((and named (plusp (length named))) (absolute-directory named "path"))
          ((setting :repo) (absolute-directory (setting :repo) "repo"))
          (t (string-right-trim "/\\" (uiop:native-namestring (uiop:getcwd)))))))

;;; --- the three reviews ---------------------------------------------------------------

(defun review-diff-text (text &key notes)
  "TEXT read as a diff => the answer. NOTES are added to it as it stands."
  (let ((result (make-result "diff_text"))
        (out (make-collector)))
    (dolist (note notes) (push note (collector-notes out)))
    (when (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
      (return-from review-diff-text (fill-result (no-diff result) out 0)))
    (let ((count (parse-diff text out)))
      (when (zerop count)
        (return-from review-diff-text
          (error-result "invalid_argument"
                        "diff_text does not contain a unified diff (no file headers or @@ hunks found)"
                        "diff_text")))
      (fill-result result out count))))

(defun review-worktree (root base)
  "ROOT's working tree against BASE => the answer."
  (let* ((label (or base "HEAD"))
         (result (make-result "worktree" :base label :repo-root root))
         (out (make-collector))
         (sha (resolve-commit root label)))
    (unless sha
      (if (and (equal label "HEAD") (head-unborn-p root))
          (progn
            (setf sha (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (run-ok '("hash-object" "-t" "tree" "--stdin") root)))
            (push "HEAD has no commits yet; compared against an empty tree"
                  (collector-notes out)))
          (setf sha (require-commit root label))))
    (let* ((count (parse-diff (run-ok (append +diff-flags+ (list sha "--")) root) out))
           (untracked (untracked-paths root)))
      (incf count (scan-untracked root untracked out))
      (setf (gethash "untracked_files" result)
            (coerce (subseq untracked 0 (min *max-untracked-listed* (length untracked))) 'vector))
      (when (> (length untracked) *max-untracked-listed*)
        (push (format nil "~d untracked files; untracked_files lists the first ~d"
                      (length untracked) *max-untracked-listed*)
              (collector-notes out)))
      (fill-result result out count)
      (if (and (zerop count) (null untracked))
          (no-diff result)
          result))))

(defun review-merge-target (root base target)
  "The commits BASE adds over TARGET in ROOT => the answer."
  (let* ((label (or base "HEAD"))
         (result (make-result "merge_target" :base label :target target :repo-root root))
         (out (make-collector))
         (target-sha (require-commit root target)))
    (when (and (equal label "HEAD") (null (resolve-commit root "HEAD")))
      (refuse "unknown_revision"
              "HEAD has no commits yet, so there is nothing to compare with target"))
    (let ((base-sha (require-commit root label)))
      (multiple-value-bind (text stderr status)
          (run-status (list "merge-base" target-sha base-sha) root)
        (let ((merge-base (and text (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
          (cond ((and (eql status 1) (or (null merge-base) (zerop (length merge-base))))
                 (refuse "no_merge_base" "~s and ~s have no common ancestor"
                         (shown label) (shown target)))
                ((or (not (eql status 0)) (null merge-base) (zerop (length merge-base)))
                 (refuse "git_failed" "git failed: ~a" (failure-line status stderr))))
          (setf (gethash "merge_base" (gethash "comparison" result)) merge-base)
          (let ((count (parse-diff (run-ok (append +diff-flags+ (list merge-base base-sha "--"))
                                            root)
                                    out)))
            (when (equal label "HEAD")
              (multiple-value-bind (dirty stderr status)
                  (run-status '("diff" "--quiet" "--no-ext-diff" "--no-textconv" "HEAD" "--") root)
                (declare (ignore dirty stderr))
                (when (or (eql status 1) (plusp (length (untracked-paths root))))
                  (push "the working tree has uncommitted changes or untracked files; ~
                         target mode reviews committed changes only (call without target to ~
                         review the working tree)"
                        (collector-notes out)))))
            (fill-result result out count)
            (if (zerop count) (no-diff result) result)))))))

(defun review-json (path base target diff-text)
  "One review's answer, as the JSON string the tool returns."
  (let ((*max-warnings* (setting :max-warnings))
        (*max-files* (setting :max-files)))
    (handler-case
        (encode
         (if diff-text
             (review-diff-text diff-text
                                :notes (when (or (and base (plusp (length base)))
                                                 (and target (plusp (length target))))
                                         (list "base/target are ignored when diff_text is given")))
             (let* ((directory (workspace path))
                    (label (trim-ref "base" base))
                    (target-ref (trim-ref "target" target)))
               (check-git directory)
               (let ((root (repository-root directory)))
                 (if target-ref
                     (review-merge-target root label target-ref)
                     (review-worktree root label))))))
      (review-exception (condition)
        (encode (error-result (review-exception-kind condition)
                              (review-exception-detail condition)
                              (mode-of base target diff-text))))
      (error (condition)
        (encode (error-result "git_failed" (princ-to-string condition)
                              (mode-of base target diff-text)))))))

;;; --- the operator's own review ---------------------------------------------------

(defun summary-text (json)
  "The JSON answer JSON, as the few lines /diff-review shows."
  (let* ((result (nlk:decode-json json))
         (status (gethash "status" result)))
    (cond
      ((equal status "error")
       (format nil "diff-review: ~a: ~a" (gethash "error_type" result)
               (gethash "message" result)))
      ((equal status "no_diff")
       (format nil "diff-review: no differences found (~a)"
               (gethash "mode" (gethash "comparison" result))))
      (t (with-output-to-string (out)
           (format out "diff-review: ~d file~:p changed, +~d -~d, ~d warning~:p"
                   (gethash "files_changed" result)
                   (gethash "total_added" result)
                   (gethash "total_removed" result)
                   (gethash "warning_count" result))
           (loop for warning across (gethash "warnings" result)
                 do (format out "~%  ~a:~a  ~a" (gethash "file" warning)
                            (gethash "line" warning) (gethash "issue" warning)))
           (loop for note across (gethash "notes" result)
                 do (format out "~%  note: ~a" note)))))))

(defun run-command (arguments session-id)
  "One /diff-review invocation: the working tree, or the branch's own commits
against the TARGET the operator names."
  (declare (ignore session-id))
  (let ((target (string-trim '(#\Space #\Tab) (or arguments ""))))
    (summary-text (review-json nil nil (and (plusp (length target)) target) nil))))

;;; --- what the model is shown and calls ---------------------------------------------

(nlk:define-contract diff-review
  (:doc "Review git changes for common code quality issues on added lines: debug
prints, console calls, hardcoded secrets, bare or broad except clauses, and
TODO/FIXME/HACK/XXX markers. By default reviews the working tree (staged,
unstaged and untracked files) against base. Pass target='origin/main' before
opening a pull request: it reviews the commits on base since it branched from
target, like a pull request view. Pass diff_text when git is not reachable from
here. Returns JSON with status ok, no_diff or error (with message and
error_type), per-file counts, warnings, untracked_files and notes.")
  ("path" :string
   :doc "the repository directory to review, absolute; without it the diff-review section's repo, and without that this process's own directory")
  ("base" :string
   :doc "the commit to review from (default HEAD); without target the working tree is compared against it")
  ("target" :string
   :doc "the branch you plan to merge into, e.g. origin/main: reviews what base adds relative to it (git diff target...base)")
  ("diff_text" :text
   :doc "a raw unified diff to analyze instead of running git; base and target are then ignored"))

(defun review (report)
  "diff_review: one git diff, read for what a reviewer would flag."
  (review-json (diff-review-path report)
                (diff-review-base report)
                (diff-review-target report)
                (diff-review-diff-text report)))

(nle:define-cell diff-review
  (:section ("diff-review")
    (:guide "git 2.28 or newer on PATH is all it needs; repo picks the directory to review, and every other setting has a default")
    ("repo" :path
     :doc "the repository to review, absolute; this process's own directory when unset")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one git run's ceiling; its whole process tree is killed past it")
    ("max_warnings" :integer :default 200 :min 1
     :doc "how many warnings one answer lists; warning_count still says how many were found")
    ("max_files" :integer :default 500 :min 1
     :doc "how many files one answer lists; files_changed still says how many were seen"))
  (:help :diff-review
         "diff-review: review a git diff for debug prints, secrets, bare excepts and TODOs - (help :diff-review)"
         +primer+)
  (:tool "diff_review" (nlk:contract 'diff-review) #'review)
  (:command "diff-review" 'run-command
            :description "Review the working tree's diff for what should not be committed"
            :argument-hint "[TARGET]"
            :session nil))
