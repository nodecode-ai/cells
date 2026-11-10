;;;; git.lisp --- tracked git diffs, read without staging or editing anything.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Git is run with an argument list, never through a shell, and its own
;;;; configuration is overridden for the run: aliases, hooks, an external diff
;;;; driver, a textconv filter and fsmonitor from the repository cannot change
;;;; what this cell reads. Nothing is staged, committed or written; the work
;;;; tree's index is hashed before and after, and the answer says whether it
;;;; moved. A patch whose path looks like a credential is not carried at all.

(in-package #:nodecode-devsentry)

(defparameter +git-environment+
  '("GIT_TERMINAL_PROMPT=0" "GIT_OPTIONAL_LOCKS=0" "GIT_PAGER=cat" "LC_ALL=C"
    "GIT_CONFIG_GLOBAL" "GIT_CONFIG_SYSTEM"
    "GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_OBJECT_DIRECTORY"
    "GIT_ALTERNATE_OBJECT_DIRECTORIES" "GIT_COMMON_DIR" "GIT_NAMESPACE" "GIT_PREFIX"
    "GIT_EXTERNAL_DIFF" "GIT_DIFF_OPTS" "GIT_CONFIG_PARAMETERS")
  "What every git run reads: no prompt, no optional locks, C locale, and the
variables that would point git at another repository or change its output
removed from what this process holds.")

(defvar *empty-hooks-directory* nil
  "The empty directory git is told to look for hooks in, made once.")

(defun empty-hooks-directory ()
  "An empty directory outside the repository, so no hook of it can run."
  (or *empty-hooks-directory*
      (setf *empty-hooks-directory*
            (let* ((stamp (nlk:short-digest (format nil "~a/~a" (get-universal-time)
                                                    (random most-positive-fixnum))))
                   (dir (merge-pathnames (format nil "devsentry-hooks-~a/" stamp)
                                         (uiop:temporary-directory))))
              (ensure-directories-exist dir)
              (string-right-trim "/\\\\" (uiop:native-namestring dir))))))

(defun run-git (argv directory)
  "Run ARGV, git's own command line, in DIRECTORY => (values OUT ERR STATUS)."
  (handler-case
      (nlk:run-bounded argv :seconds (setting :git-timeout-seconds) :directory directory
                            :environment +git-environment+)
    (error (condition)
      ;; git absent is an ordinary first state, so it is words the model can act on.
      (values nil (format nil "~a could not be run: ~a" (first argv) condition) 127))))

(defparameter *run* #'run-git
  "How one git command line runs: (funcall *RUN* ARGV DIRECTORY) => (values OUT
ERR STATUS). A test answers this with no repository and no git install.")

(defun trim-output (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))

(defun git-argv (repo args)
  "The command line for ARGS run in REPO, its configuration overridden."
  (when (or (null args) (not (every (lambda (argument) (plusp (length argument))) args)))
    (refuse "internal" "refusing to build an empty git command"))
  (append (list "git" "--no-optional-locks"
                "-c" "core.fsmonitor="
                "-c" "core.untrackedCache=false"
                "-c" "diff.external="
                "-c" (format nil "core.hooksPath=~a" (empty-hooks-directory))
                "-c" (format nil "alias.~a=" (first args))
                "-C" repo)
          args))

(defun run-status (repo args)
  "One read-only git command => (values OUT ERR STATUS)."
  (funcall *run* (git-argv repo args) repo))

(defun split-nul (text)
  "TEXT split at every NUL, an empty trailing piece dropped."
  (let ((pieces '()) (start 0))
    (loop
      (let ((at (position #\Null text :start start)))
        (push (subseq text start at) pieces)
        (if at (setf start (1+ at)) (return (nreverse pieces)))))))

;;; --- the repository a directory is in ---------------------------------------------

(defun index-digest (repo)
  "The work tree's index file as a digest, or NIL when it cannot be read."
  (multiple-value-bind (out stderr status) (run-status repo '("rev-parse" "--absolute-git-dir"))
    (declare (ignore stderr))
    (when (eql status 0)
      (let ((index (merge-pathnames "index"
                                     (uiop:ensure-directory-pathname (trim-output out)))))
        (handler-case
            (let ((bytes (nlk:file-bytes index)))
              (and bytes (plusp (length bytes))
                   (nlk:sha256-text (map 'string #'code-char bytes))))
          (error () nil))))))

(defun relative-scope (root toplevel)
  "ROOT's path below TOPLEVEL as git names it, a single dot for the work tree itself."
  (cond ((equal root toplevel) ".")
        ((uiop:string-prefix-p (format nil "~a/" toplevel) root)
         (subseq root (1+ (length toplevel))))
        (t nil)))

;;; --- a scope's files and their patches -------------------------------------------

(defparameter *max-files* 40 "How many changed files one scope lists.")

(defun status-records (blob)
  "Git's -z name-status output read as plists of :status, :path and :old-path."
  (let ((records '())
        (rest (split-nul blob)))
    (loop while rest
          for status = (pop rest)
          do (cond
               ((zerop (length status)))
               ((member (char status 0) '(#\R #\C))
                (let ((old (pop rest)) (new (pop rest)))
                  (when (and old new)
                    (push (list :status status :path new :old-path old) records))))
               (t (let ((path (pop rest)))
                    (when path (push (list :status status :path path) records))))))
    (nreverse records)))

(defun clip-text (text limit)
  "TEXT at most LIMIT UTF-8 bytes => (values TEXT TRUNCATED-P)."
  (if (<= (nlk:utf-8-size text) limit)
      (values text nil)
      (values (subseq text 0 (min (length text) limit)) t)))

(defun patch-for (repo cached path context-lines max-patch-bytes)
  "One file's patch in one scope, as the answer carries it."
  (if (or (sensitive-name-p (file-namestring (uiop:parse-native-namestring path))))
      (nlk:json-object "patch_omitted" t "reason" "sensitive_path")
      (multiple-value-bind (out stderr status)
          (run-status repo
                      (append (list "diff" "--no-ext-diff" "--no-textconv" "--no-color"
                                    (format nil "--unified=~d" context-lines))
                              (when cached (list "--cached"))
                              (list "--" path)))
          (if (not (eql status 0))
              (nlk:json-object "patch_omitted" t "reason" "git_error"
                                "message" (nlk:clip (or (trim-output stderr) (trim-output out)
                                                         "git diff failed")
                                                     300))
              (let ((text (or out "")))
                (if (or (search "GIT binary patch" text)
                        (uiop:string-prefix-p "Binary files " text)
                        (search (format nil "~%Binary files ") text))
                    (nlk:json-object "patch_omitted" t "reason" "binary")
                    (multiple-value-bind (clipped truncated) (clip-text text max-patch-bytes)
                      (let ((answer (nlk:json-object "patch" clipped)))
                        (when truncated (setf (gethash "patch_truncated" answer) t))
                        answer))))))))

(defun review-scope (repo cached pathspec context-lines max-patch-bytes)
  "One scope of tracked changes, staged or unstaged, as a JSON object."
  (multiple-value-bind (out stderr status)
      (run-status repo
                  (append (list "diff" "--name-status" "--no-ext-diff" "--no-textconv"
                                "--no-color" "-z")
                          (when cached (list "--cached"))
                          (when pathspec (list "--" pathspec))))
    (if (not (eql status 0))
        (nlk:json-object "ok" nil "files" (vector) "file_count" 0
                          "error" (nlk:clip (or (trim-output stderr) "git diff --name-status failed") 500))
        (let ((records (status-records (or out "")))
              (files '())
              (hidden 0))
          (dolist (record records)
            (if (>= (length files) *max-files*)
                (incf hidden)
                (let* ((path (getf record :path))
                       (old (getf record :old-path))
                       (sensitive (or (sensitive-name-p (file-namestring (uiop:parse-native-namestring path)))
                                      (and old (sensitive-name-p (file-namestring (uiop:parse-native-namestring old))))))
                       (entry (nlk:json-object "path" path "status" (getf record :status))))
                  (when old (setf (gethash "old_path" entry) old))
                  (if sensitive
                      (setf (gethash "patch_omitted" entry) t
                            (gethash "reason" entry) "sensitive_path")
                      (let ((patch (patch-for repo cached path context-lines max-patch-bytes)))
                        (maphash (lambda (key value) (setf (gethash key entry) value)) patch)))
                  (push entry files))))
          (let ((answer (nlk:json-object "ok" t "files" (coerce (nreverse files) 'vector)
                                          "file_count" (length records))))
            (when (plusp hidden)
              (setf (gethash "files_omitted_for_bound" answer) hidden))
            answer)))))

(defun untracked-count (repo pathspec)
  "How many untracked files git reports, or NIL when it could not say."
  (multiple-value-bind (out stderr status)
      (run-status repo (append (list "ls-files" "--others" "--exclude-standard" "-z")
                               (when pathspec (list "--" pathspec))))
    (declare (ignore stderr))
    (when (eql status 0)
      (length (remove "" (split-nul (or out "")) :test #'equal)))))

;;; --- the review -------------------------------------------------------------------

(defun review-repository (root context-lines max-patch-bytes)
  "ROOT's staged and unstaged tracked changes, as a JSON object."
  (multiple-value-bind (inside stderr status) (run-status root '("rev-parse" "--is-inside-work-tree"))
    (unless (and (eql status 0) (equal (trim-output inside) "true"))
      (return-from review-repository
        (nlk:json-object "ok" nil
                          "error" (nlk:json-object
                                    "code" "not_a_git_repository"
                                    "message" (nlk:clip (or (trim-output stderr)
                                                             "the directory is not inside a Git work tree")
                                                         500))))))
  (multiple-value-bind (out stderr status) (run-status root '("rev-parse" "--show-toplevel"))
    (unless (eql status 0)
      (return-from review-repository
        (nlk:json-object "ok" nil
                          "error" (nlk:json-object
                                    "code" "not_a_git_repository"
                                    "message" (nlk:clip (or (trim-output stderr) "missing work tree") 500)))))
    (let* ((toplevel (string-right-trim "/\\\\" (uiop:native-namestring
                                                (or (probe-file (trim-output out)) (trim-output out)))))
           (scope (relative-scope root toplevel)))
      (when (null scope)
        (return-from review-repository
          (nlk:json-object "ok" nil
                            "error" (nlk:json-object "code" "outside_work_tree"
                                                      "message" "the directory is outside the Git work tree"))))
      (let* ((pathspec (unless (equal scope ".") scope))
             (before (index-digest toplevel))
             (head (multiple-value-bind (sha err sha-status)
                       (run-status toplevel '("rev-parse" "HEAD"))
                     (declare (ignore err))
                     (and (eql sha-status 0) (trim-output sha))))
             (staged (review-scope toplevel t pathspec context-lines max-patch-bytes))
             (unstaged (review-scope toplevel nil pathspec context-lines max-patch-bytes))
             (untracked (untracked-count toplevel pathspec))
             (after (index-digest toplevel)))
        (nlk:json-object
         "ok" (and (gethash "ok" staged) (gethash "ok" unstaged) t)
         "repository" (nlk:json-object "resolved" root "toplevel" toplevel
                                        :opt "head" head
                                        "scope_path" (or pathspec "."))
         "scope" (nlk:json-object
                   "staged" "Tracked changes in the index compared with HEAD. Nothing was staged by this tool."
                   "unstaged" "Tracked working-tree changes compared with the index."
                   "untracked" "Excluded. Untracked names and contents are not part of this review.")
         "staged" staged
         "unstaged" unstaged
         :opt "untracked_count" untracked
         "mutation" (nlk:json-object
                      "staging" "not performed"
                      "worktree_writes" "not performed"
                      "index_sha256_unchanged" (and (equal before after) t))
         "notes" (coerce (list "External diff drivers and textconv filters were disabled for these commands."
                                "Git aliases, hooks, and fsmonitor from the repository were overridden for this process."
                                "Patch text for paths that look like secrets was omitted."
                                "Untracked files were counted and were not read.")
                          'vector))))))
