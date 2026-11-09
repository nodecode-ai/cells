;;;; git.lisp --- finding the repository, running git, reading untracked files.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; NODECODE-DIFF-REVIEW::*RUN* is the one seam: a tool builds a git command
;;;; line and hands it here, so a test asserts the argv and answers the output
;;;; with no repository, no git install and no network.
;;;;
;;;; The flags are fixed and the environment is scrubbed for the reason the
;;;; original plugin fixed them: the operator's git config must not change what
;;;; the parser sees, and a GIT_DIR inherited from somewhere else must not
;;;; point the review at another repository.

(in-package #:nodecode-diff-review)

(defparameter +git-environment+
  '("GIT_TERMINAL_PROMPT=0" "GIT_OPTIONAL_LOCKS=0" "LC_ALL=C"
    "GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_OBJECT_DIRECTORY"
    "GIT_ALTERNATE_OBJECT_DIRECTORIES" "GIT_COMMON_DIR" "GIT_NAMESPACE" "GIT_PREFIX"
    "GIT_EXTERNAL_DIFF" "GIT_DIFF_OPTS" "GIT_CONFIG_PARAMETERS")
  "What every git run reads: prompts and optional locks off, C locale, and
the variables that would point git at another repository or change its diff
output removed from what this process holds.")

(defparameter +diff-flags+
  '("-c" "core.quotePath=false" "diff" "--no-color" "--no-ext-diff" "--no-textconv"
    "--no-relative" "--src-prefix=a/" "--dst-prefix=b/" "--submodule=short" "-M")
  "The diff command the parser was written for, its flags fixed.")

(defun run-git (argv directory)
  "Run ARGV, git's own command line, in DIRECTORY to completion or the
section's timeout_seconds => (values STDOUT STDERR STATUS)."
  (handler-case
      (nlk:run-bounded argv :seconds (setting :timeout-seconds) :directory directory
                            :environment +git-environment+)
    (error (condition)
      ;; git absent is an ordinary first state of this cell, so it is words the
      ;; model can act on: 127 is what a shell says for a command it lacks.
      (values nil (format nil "~a could not be run: ~a" (first argv) condition) 127))))

(defparameter *run* #'run-git
  "How one git command line runs: (funcall *RUN* ARGV DIRECTORY) => (values
STDOUT STDERR STATUS), STATUS an exit code or :TIMEOUT.")

;;; --- refusals the answer carries --------------------------------------------------

(define-condition review-exception (error)
  ((kind :initarg :kind :reader review-exception-kind)
   (detail :initarg :detail :reader review-exception-detail))
  (:documentation "One refusal this cell reports inside its JSON answer, as the
original did: the kind its callers read, and the sentence the model reads.")
  (:report (lambda (condition stream)
             (write-string (review-exception-detail condition) stream))))

(defun refuse (kind format-control &rest arguments)
  "Signal a REVIEW-EXCEPTION of KIND with the sentence ARGUMENTS format."
  (error 'review-exception :kind kind :detail (apply #'format nil format-control arguments)))

(defun last-line (text)
  "TEXT's last non-empty line, trimmed: what git was really complaining about."
  (let* ((lines (split-lines text))
         (last (or (car (last (remove-if (lambda (line)
                                           (zerop (length (string-trim '(#\Space #\Tab #\Return)
                                                                      line))))
                                         lines)))
                   text)))
    (string-trim '(#\Space #\Tab #\Newline #\Return) last)))

(defun failure-line (status stderr)
  "How one failed git run is told to the model."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (redact (or stderr "")))))
    (if (plusp (length text)) (last-line text) (format nil "exit code ~a" status))))

;;; --- running git --------------------------------------------------------------------

(defun run-status (argv directory)
  "One git run => (values STDOUT STDERR STATUS)."
  (funcall *run* (cons "git" argv) directory))

(defun run-ok (argv directory)
  "One git run that must succeed => its stdout; a refusal when it did not."
  (multiple-value-bind (out stderr status) (run-status argv directory)
    (unless (eql status 0)
      (refuse "git_failed" "git failed: ~a" (failure-line status stderr)))
    (or out "")))

(defun check-git (directory)
  "Refuse a git older than 2.28, the version the flags this cell passes need."
  (let ((version (run-ok '("--version") directory)))
    (cl-ppcre:register-groups-bind (major minor) ("(\\d+)\\.(\\d+)" version)
      (when (or (< (parse-integer major) 2)
                (and (= (parse-integer major) 2) (< (parse-integer minor) 28)))
        (refuse "git_failed" "git 2.28 or newer is required (found ~a)"
                (string-trim '(#\Space #\Newline #\Return) version))))))

(defun repository-root (directory)
  "DIRECTORY's work tree root, or the refusal git's own words make."
  (multiple-value-bind (out stderr status) (run-status '("rev-parse" "--show-toplevel") directory)
    (let ((root (and out (string-trim '(#\Space #\Tab #\Newline #\Return) out))))
      (when (and (eql status 0) root (plusp (length root)))
        (return-from repository-root root)))
    (let ((text (string-downcase (or stderr ""))))
      (when (or (search "not a git repository" text)
                (search "must be run in a work tree" text))
        (refuse "not_a_repository" "~a is not inside a git work tree" directory))
      (refuse "git_failed" "git failed: ~a" (failure-line status stderr)))))

(defun resolve-commit (root ref)
  "ROOT's commit REF names, or NIL for one it does not."
  (multiple-value-bind (out stderr status)
      (run-status (list "rev-parse" "--verify" "--quiet" "--end-of-options"
                        (concatenate 'string ref "^{commit}"))
                  root)
    (declare (ignore stderr))
    (let ((sha (and out (string-trim '(#\Space #\Tab #\Newline #\Return) out))))
      (and (eql status 0) sha (plusp (length sha)) sha))))

(defun require-commit (root ref)
  "ROOT's commit REF names, or the refusal the call gets."
  (or (resolve-commit root ref)
      (refuse "unknown_revision"
              "~s is not a commit in this repository (base and target take branches, ~
               tags or commit SHAs, not file paths)"
              (shown ref))))

(defun head-unborn-p (root)
  "Whether ROOT's HEAD names a branch with no commit yet."
  (multiple-value-bind (out stderr status) (run-status '("symbolic-ref" "--quiet" "HEAD") root)
    (declare (ignore out stderr))
    (and (eql status 0) (null (resolve-commit root "HEAD")))))

(defun untracked-paths (root)
  "The paths git reports as untracked and not ignored, as a list of strings."
  (let ((text (run-ok '("ls-files" "--others" "--exclude-standard" "-z") root)))
    (remove "" (split-on text #\Null) :test #'equal)))

(defun split-on (text delimiter)
  "TEXT split at every DELIMITER, empty pieces kept."
  (let ((pieces '())
        (start 0))
    (loop
      (let ((at (position delimiter text :start start)))
        (push (subseq text start at) pieces)
        (if at (setf start (1+ at)) (return (nreverse pieces)))))))

;;; --- untracked files, read like the original read them ---------------------------

(defparameter *max-untracked-file-bytes* (* 1024 1024)
  "How much of one untracked file is read: a new source file, not a log or a dump.")
(defparameter *max-untracked-total-bytes* (* 5 1024 1024)
  "How much of the untracked files' text one review reads in all.")
(defparameter *max-untracked-scanned* 200
  "How many untracked files one review scans.")
(defparameter *max-untracked-listed* 200
  "How many untracked files one answer lists by name.")

(defun regular-file-p (path)
  "Whether PATH is a regular file, a symbolic link to one included."
  #+sbcl (handler-case
             (eql (logand (sb-posix:stat-mode (sb-posix:lstat path)) sb-posix:s-ifmt)
                  sb-posix:s-ifreg)
           (error () nil))
  #-sbcl (let ((probe (probe-file path)))
           (and probe (not (uiop:directory-pathname-p probe)))))

(defun read-file-bytes (path limit)
  "Read PATH's first LIMIT bytes => (values OCTETS KIND), KIND one of :ok,
:oversized, :unreadable or :not-regular."
  (unless (regular-file-p path)
    (return-from read-file-bytes (values nil :not-regular)))
  (handler-case
      (with-open-file (in path :direction :input :element-type '(unsigned-byte 8)
                               :if-does-not-exist nil)
        (unless in (return-from read-file-bytes (values nil :unreadable)))
        (let* ((buffer (make-array (1+ limit) :element-type '(unsigned-byte 8)))
               (count (read-sequence buffer in)))
          (if (> count limit)
              (values nil :oversized)
              (values (subseq buffer 0 count) :ok))))
    (error () (values nil :unreadable))))

(defun octets-text (octets)
  "OCTETS read as UTF-8 text, a bad byte standing as a question mark."
  #+sbcl (sb-ext:octets-to-string octets :external-format '(:utf-8 :replacement #\?))
  #-sbcl (map 'string #'code-char octets))

(defun binary-bytes-p (octets)
  "Whether OCTETS look binary: a NUL in the first 8 KiB."
  (not (null (find 0 octets :end (min 8192 (length octets))))))

(defun under (root name)
  "NAME as a path under ROOT, git's own separators kept."
  (concatenate 'string (string-right-trim "/\\" root) "/" name))

(defun note-skipped (skipped reason name)
  "Record NAME under the REASON it was not scanned."
  (push name (gethash reason skipped)))

(defun names-line (names)
  "Up to five NAMEs, and how many more there were."
  (let ((shown (mapcar (lambda (name) (clip name 80)) (subseq names 0 (min 5 (length names))))))
    (format nil "~{~a~^, ~}~@[ and ~d more~]"
            shown
            (let ((rest (- (length names) (length shown)))) (and (plusp rest) rest)))))

(defun scan-untracked (root paths out)
  "Scan the untracked PATHS under ROOT into OUT => how many were read."
  (let ((scanned 0)
        (total 0)
        (skipped (make-hash-table :test #'equal)))
    (dolist (name paths)
      (if (or (>= scanned *max-untracked-scanned*)
              (>= total *max-untracked-total-bytes*))
          (note-skipped skipped "over the scan limit (200 files / 5 MiB)" name)
          (multiple-value-bind (bytes kind)
              (read-file-bytes (under root name) *max-untracked-file-bytes*)
            (case kind
              (:not-regular (note-skipped skipped "not a regular file" name))
              (:unreadable (note-skipped skipped "unreadable" name))
              (:oversized (note-skipped skipped "larger than 1 MiB" name))
              (t
               (if (> (+ total (length bytes)) *max-untracked-total-bytes*)
                   (note-skipped skipped "over the scan limit (200 files / 5 MiB)" name)
                   (progn
                     (incf scanned)
                     (incf total (length bytes))
                     (scan-untracked-entry name bytes out))))))))
    (let ((order '("not a regular file" "larger than 1 MiB" "unreadable"
                   "over the scan limit (200 files / 5 MiB)")))
      (dolist (reason order)
        (let ((names (reverse (gethash reason skipped))))
          (when names
            (push (format nil "~d untracked file(s) not scanned (~a): ~a"
                          (length names) reason (names-line names))
                  (collector-notes out))))))
    scanned))

(defun scan-untracked-entry (name bytes out)
  "One untracked file's entry, its lines read as added and its text put
through the rules."
  (let ((entry (nlk:json-object "path" name "added" 0 "removed" 0 "status" "untracked")))
    (if (binary-bytes-p bytes)
        (setf (gethash "binary" entry) t)
        (let ((lines (split-lines (octets-text bytes))))
          (loop for line in lines
                for number from 1
                do (added-line out name number (strip-cr line)))
          (setf (gethash "added" entry) (length lines))))
    (push entry (collector-files out))))

;;; --- the answer ---------------------------------------------------------------------

(defparameter +result-budget-chars+ 90000
  "How much JSON one answer may be; the original's per-result budget, under it.")

(defun make-result (mode &key base target merge-base repo-root)
  "The answer of one review, before any file is read into it."
  (nlk:json-object
   "status" "ok"
   "files_changed" 0
   "total_added" 0
   "total_removed" 0
   "files" (vector)
   "warnings" (vector)
   "warning_count" 0
   "untracked_files" (vector)
   "comparison" (nlk:json-object "mode" mode
                                  :opt "base" base
                                  :opt "target" target
                                  :opt "merge_base" merge-base
                                  :opt "repo_root" repo-root)
   "notes" (vector)))

(defun fill-result (result out count)
  "RESULT, filled from what OUT read over COUNT files."
  (let ((files (reverse (collector-files out)))
        (warnings (reverse (collector-warnings out))))
    (setf (gethash "files_changed" result) count
          (gethash "total_added" result) (collector-total-added out)
          (gethash "total_removed" result) (collector-total-removed out)
          (gethash "files" result) (coerce (subseq files 0 (min *max-files* (length files))) 'vector)
          (gethash "warnings" result) (coerce warnings 'vector)
          (gethash "warning_count" result) (collector-warning-count out)
          (gethash "notes" result) (coerce (reverse (collector-notes out)) 'vector))
    (when (> (length files) *max-files*)
      (setf (gethash "files_truncated" result) t))
    (when (> (collector-warning-count out) (length warnings))
      (setf (gethash "warnings_truncated" result) t)))
  result)

(defun no-diff (result)
  "RESULT as the answer of a review that found nothing."
  (setf (gethash "status" result) "no_diff"
        (gethash "message" result) "No differences found.")
  result)

(defun error-result (kind detail mode &key base target)
  "The answer of a review that could not run, in the original's shape."
  (let ((result (make-result mode :base base :target target)))
    (setf (gethash "status" result) "error"
          (gethash "message" result) (clip (redact detail) 500)
          (gethash "error" result) (clip (redact detail) 500)
          (gethash "error_type" result) kind)
    result))

(defun mode-of (base target diff-text)
  "Which of the three reviews the arguments asked for."
  (declare (ignore base))
  (cond (diff-text "diff_text") (target "merge_target") (t "worktree")))

(defun encode (result)
  "RESULT as the one string the tool answers."
  (nlk:encode-json-object result))

(defun trim-ref (name value)
  "VALUE as a revision NAME may name, or a refusal. A ref that looks like an
option is refused, so a call cannot smuggle a flag into the command line."
  (let ((text (and value (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (cond ((or (null text) (zerop (length text))) nil)
          ((> (length text) 256)
           (refuse "invalid_argument" "~a is longer than 256 characters" name))
          ((char= #\- (char text 0))
           (refuse "invalid_argument" "~a must be a git revision, not an option: ~s" name (shown text)))
          ((some (lambda (character) (or (< (char-code character) 32)
                                         (= (char-code character) 127)))
                 text)
           (refuse "invalid_argument" "~a contains control characters" name))
          (t text))))
