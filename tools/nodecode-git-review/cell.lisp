;;;; cell.lisp --- git review: one change, every file, one scroll of text.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes Desktop pane: a browser window that runs git
;;;; through a Python backend and paints the diff next to the chat. This cell
;;;; keeps what the pane computes and gives it to the model instead of a
;;;; window: the same read scopes (staged, unstaged, uncommitted, branch,
;;;; commits), the same unified patch with new files rendered as their own
;;;; hunks, a per-file churn summary, and the same write path -- stage,
;;;; commit, push, pull or merge request -- behind one read_only switch.
;;;;
;;;; Every command is a local program run in the repository's own directory:
;;;; git, and gh or glab only when a pull request is asked for. The cell
;;;; itself contacts nothing, and reads nothing outside the named repository.
;;;;
;;;; Tests rebind *RUN*, the one process seam, so nothing here needs git to
;;;; be installed to be proved.

(in-package #:nodecode-git-review)

(nlk:define-peripheral git-review :not-running t)

(defparameter +scopes+ '("unstaged" "staged" "uncommitted" "branch" "commits")
  "The ways a change can be read, as the original pane names them.")

(defparameter +new-file-lines+ 400
  "How many lines of one new, untracked file are shown.")

(defparameter +new-files+ 40
  "How many new, untracked files are shown; the rest are listed only.")

(defvar *run* nil
  "The one process seam: (DIR PROGRAM ARGS TIMEOUT) => (values OUTPUT EXIT-CODE).
NIL means the real subprocess, REAL-RUN. Tests rebind it.")

;;; --- the process seam ---------------------------------------------------------------

(defun real-run (dir program args timeout)
  "Run PROGRAM with ARGS in DIR, its standard error with its output, its
output bounded by a file and its time by TIMEOUT. => (values OUTPUT EXIT-CODE),
EXIT-CODE NIL when it outlived TIMEOUT and was stopped."
  (let ((path (merge-pathnames (format nil "git-review-~d-~d.txt"
                                        (get-universal-time) (random 1000000))
                                (uiop:temporary-directory))))
    (ensure-directories-exist path)
    (unwind-protect
         (handler-case
             (let* ((process (uiop:launch-program (cons program args)
                                                   :directory (uiop:ensure-directory-pathname dir)
                                                   :output path
                                                   :error-output :output
                                                   :if-output-exists :supersede
                                                   :external-format :utf-8))
                    (deadline (+ (get-internal-real-time)
                                 (* (max 1 timeout) internal-time-units-per-second)))
                    (code nil))
               (loop while (uiop:process-alive-p process)
                     do (if (> (get-internal-real-time) deadline)
                            (progn (ignore-errors (uiop:terminate-process process :urgent t))
                                   (return))
                            (sleep 0.02)))
               (setf code (ignore-errors (uiop:wait-process process)))
               (values (if (probe-file path) (uiop:read-file-string path) "") code))
           (error (e)
             (fail "cannot run ~a in ~a: ~a" program dir e)))
      (ignore-errors (delete-file path)))))

(defun run-command (dir program &rest args)
  "PROGRAM with ARGS in DIR, through *RUN*. => (values OUTPUT EXIT-CODE)."
  (funcall (or *run* #'real-run) dir program args (setting :timeout-seconds)))

(defun git (dir &rest args)
  "One git command in DIR. => (values OUTPUT OK-P)."
  (multiple-value-bind (output code) (apply #'run-command dir "git" args)
    (values (or output "") (eql 0 code))))

(defun one-line (text)
  "TEXT as one line, short enough to sit in a refusal."
  (let* ((text (or text ""))
         (newline (position #\Newline text)))
    (when newline (setf text (subseq text 0 newline)))
    (when (> (length text) 200) (setf text (concatenate 'string (subseq text 0 200) "...")))
    text))

(defmacro with-git ((output-var dir &rest args) &body body)
  "Bind OUTPUT-VAR to DIR's answer to the git ARGS, requiring success."
  `(multiple-value-bind (,output-var ok) (git ,dir ,@args)
     (unless ok
       (fail "git ~{~a~^ ~} failed: ~a" (list ,@args) (one-line ,output-var)))
     ,@body))

(defun text-lines (text)
  (uiop:split-string (or text "") :separator '(#\Newline)))

(defun trimmed (text)
  (nlk:trimmed (or text "")))

(defun string-join (strings separator)
  (format nil (format nil "~~{~~a~~^~a~~}" separator) strings))

;;; --- the repository -----------------------------------------------------------------

(defun default-repo ()
  "The repository a call that names none means: the session's own working
directory, else the first path the section offers, else this process's."
  (or (let ((session (getf (ignore-errors (nle:turn)) :session-id)))
        (and session (ignore-errors (nlk:find-session-cwd session))))
      (first (ignore-errors (getf (running-settings) :repos)))
      (uiop:getcwd)))

(defun canonical-repo (repo)
  "REPO (or the default) as a git work tree's top level, or a refusal."
  (let* ((given (trimmed repo))
         (path (if (plusp (length given)) (nlk:expand-home given) (default-repo)))
         (dir (uiop:ensure-directory-pathname path)))
    (unless (uiop:directory-exists-p dir)
      (fail "~a is not a directory" path))
    (multiple-value-bind (output ok) (git dir "rev-parse" "--show-toplevel")
      (unless ok
        (fail "~a is not a git work tree: ~a" path (one-line output)))
      (let ((toplevel (trimmed output)))
        (if (plusp (length toplevel)) toplevel path)))))

(defun check-path (path)
  "PATH as a repository-relative path, or NIL, or a refusal."
  (let ((path (trimmed path)))
    (if (zerop (length path))
        nil
        (progn
          (when (or (find (code-char 0) path)
                    (search ".." path)
                    (member (char path 0) '(#\/ #\\ #\-))
                    (and (> (length path) 1) (char= (char path 1) #\:)))
            (fail "~s is not a repository-relative path" path))
          path))))

(defun path-args (path)
  "The pathspec arguments for PATH: literal, after the --."
  (if path (list "--" (format nil ":(literal)~a" path)) (list "--")))

(defun head-p (dir)
  "True when DIR has a commit to compare a working tree against."
  (nth-value 1 (git dir "rev-parse" "--verify" "--quiet" "HEAD")))

;;; --- the scopes --------------------------------------------------------------------

(defun diff-flags (ignore-ws)
  "The flags every read shares: no colours, no external driver, no textconv,
and the section's own amount of unchanged context."
  (append (list "diff" "--no-color" "--no-ext-diff" "--no-textconv"
                (format nil "-U~d" (setting :context-lines)))
          (when ignore-ws (list "-w"))))

(defun run-diff (dir args)
  (multiple-value-bind (output ok) (apply #'git dir args)
    (if ok
        output
        (fail "git ~{~a~^ ~} failed: ~a" args (one-line output)))))

(defun untracked-files (dir path)
  "The untracked files of DIR the path filter keeps, in git's order."
  (multiple-value-bind (output ok)
      (apply #'git dir (append (list "ls-files" "--others" "--exclude-standard")
                               (path-args path)))
    (if ok (remove "" (text-lines output) :test #'string=) '())))

(defun new-file-patch (dir file)
  "FILE, untracked, as a patch adding it: its header, then its lines. A file
too large or binary is named and not shown."
  (handler-case
      (let* ((path (merge-pathnames file dir))
             (text (uiop:read-file-string path :external-format :utf-8)))
        (if (find (code-char 0) (subseq text 0 (min (length text) 8000)))
            (format nil "diff --git a/~a b/~a~%new file mode 100644~%Binary file ~a not shown~%"
                    file file file)
            (let* ((lines (text-lines text))
                   (lines (if (and (plusp (length lines)) (string= (car (last lines)) ""))
                              (butlast lines)
                              lines))
                   (body (subseq lines 0 (min (length lines) +new-file-lines+))))
              (with-output-to-string (out)
                (format out "diff --git a/~a b/~a~%new file mode 100644~%--- /dev/null~%+++ b/~a~%@@ -0,0 +1,~d @@~%"
                        file file file (length lines))
                (dolist (line body) (format out "+~a~%" line))
                (when (> (length lines) (length body))
                  (format out "[~d more lines of ~a not shown]~%"
                          (- (length lines) (length body)) file))))))
    (error () (format nil "diff --git a/~a b/~a~%new file mode 100644~%[~a could not be read]~%"
                      file file file))))

(defun new-files-patch (dir path)
  "Every new, untracked file the path filter keeps, as patches."
  (if (not (setting :new-files))
      ""
      (let ((files (untracked-files dir path)))
        (with-output-to-string (out)
          (loop for file in (subseq files 0 (min (length files) +new-files+))
                do (write-string (new-file-patch dir file) out))
          (when (> (length files) +new-files+)
            (format out "[~d more new files listed only]~%"
                    (- (length files) +new-files+)))))))

(defun base-ref (dir base)
  "BASE, the section's base, or the branch HEAD most likely left."
  (or (first (remove "" (list (trimmed base) (trimmed (setting :base))) :test #'string=))
      (multiple-value-bind (output ok)
          (git dir "symbolic-ref" "--quiet" "--short" "refs/remotes/origin/HEAD")
        (let ((name (and ok (trimmed output))))
          (and name (plusp (length name)) name)))
      (loop for candidate in '("origin/main" "origin/master" "main" "master")
            when (nth-value 1 (git dir "rev-parse" "--verify" "--quiet" candidate))
              return candidate)
      (fail "cannot tell which branch ~a left: name base in the git-review section" dir)))

(defun merge-base-of (dir base)
  (with-git (output dir "merge-base" base "HEAD")
    (trimmed output)))

(defun sha-p (text)
  (and (<= 7 (length text) 64)
       (every (lambda (c) (digit-char-p c 16)) text)))

(defun ref-patches (dir ref ignore-ws)
  "The patch each commit of REF (comma-separated commit ids) makes, in order."
  (let ((shas (remove "" (mapcar #'trimmed (uiop:split-string ref :separator '(#\,)))
                      :test #'string=)))
    (when (null shas) (fail "ref is empty"))
    (dolist (sha shas)
      (unless (sha-p sha) (fail "~s is not a commit id" sha)))
    (with-output-to-string (out)
      (dolist (sha shas)
        (multiple-value-bind (output ok)
            (apply #'git dir (append (list "show" "--no-color" "--no-ext-diff"
                                            (format nil "-U~d" (setting :context-lines)))
                                      (when ignore-ws (list "-w"))
                                      (list "--format=commit %h %s" sha)))
          (if ok
              (write-string output out)
              (format out "commit ~a: ~a~%" sha (one-line output))))))))

(defun commits-since (dir base)
  "The commits BASE..HEAD added, newest first, one per line."
  (with-git (output dir "log" "--no-color" "--no-decorate" "--oneline"
                     "-n" "250" (format nil "~a..HEAD" base))
    (trimmed output)))

(defun review-patch (dir scope base ref path ignore-ws)
  "=> plist (:patch TEXT :about TEXT :commits TEXT) for one scope."
  (let ((paths (path-args path))
        (flags (diff-flags ignore-ws)))
    (cond
      ((string= scope "staged")
       (list :patch (run-diff dir (append flags (list "--cached") paths))
             :about "the index, against HEAD"))
      ((string= scope "unstaged")
       (list :patch (concatenate 'string (run-diff dir (append flags paths))
                                 (new-files-patch dir path))
             :about "the working tree, against the index"))
      ((string= scope "uncommitted")
       (if (head-p dir)
           (list :patch (concatenate 'string
                                     (run-diff dir (append flags (list "HEAD") paths))
                                     (new-files-patch dir path))
                 :about "the working tree, against HEAD")
           (list :patch (concatenate 'string
                                     (run-diff dir (append flags (list "--cached") paths))
                                     (new-files-patch dir path))
                 :about "the index, against an empty tree (no commit yet)")))
      ((or (string= scope "branch") (string= scope "commits"))
       (let* ((base (base-ref dir base))
              (merge (merge-base-of dir base)))
         (if (and (string= scope "commits") (plusp (length (trimmed ref))))
             (list :patch (ref-patches dir ref ignore-ws)
                   :about (format nil "the commits ~a" (trimmed ref)))
             (list :patch (run-diff dir (append flags (list merge) paths))
                   :about (format nil "~a..HEAD, from the merge base ~a"
                                  base (subseq merge 0 (min 8 (length merge))))
                   :commits (and (string= scope "commits")
                                 (commits-since dir base))))))
      (t (fail "~s is no scope: ~{~a~^, ~}" scope +scopes+)))))

;;; --- what the patch says ------------------------------------------------------------

(defun plausible-name (name)
  "A patch's file name: its b/ or a/ prefix off, quotes and driver tails off."
  (let ((name (trimmed name)))
    (when (and (plusp (length name)) (char= (char name 0) #\"))
      (setf name (string-trim "\"" name)))
    (when (and (> (length name) 2) (or (string= "b/" (subseq name 0 2))
                                        (string= "a/" (subseq name 0 2))))
      (setf name (subseq name 2)))
    (let ((tab (position #\Tab name)))
      (if tab (subseq name 0 tab) name))))

(defun patch-files (patch)
  "=> list of (PATH PLUS MINUS), one per file, in the patch's order."
  (let ((files '()) (entry nil))
    (dolist (line (text-lines patch))
      (when (and (>= (length line) 4) (string= "+++ " (subseq line 0 4)))
        (let ((name (subseq line 4)))
          (unless (string= (trimmed name) "/dev/null")
            (setf entry (list (plausible-name name) 0 0))
            (push entry files))))
      (when (and entry (plusp (length line)))
        (let ((head (char line 0)))
          (cond ((and (char= head #\+)
                      (not (and (>= (length line) 3) (string= "+++" (subseq line 0 3)))))
                 (incf (second entry)))
                ((and (char= head #\-)
                      (not (and (>= (length line) 3) (string= "---" (subseq line 0 3)))))
                 (incf (third entry)))))))
    (nreverse files)))

(defun worktree-status (dir)
  "=> (values BRANCH ALIST): the checked-out branch, and each changed path's
two-letter status from git status."
  (multiple-value-bind (output ok)
      (git dir "status" "--porcelain=v1" "-uall" "-b")
    (if (not ok)
        (values "" '())
        (let ((branch "") (rows '()))
          (dolist (line (text-lines output))
            (cond ((and (>= (length line) 3) (string= "## " (subseq line 0 3)))
                   (let* ((rest (subseq line 3))
                          (dots (search "..." rest))
                          (on (search " on " rest)))
                     (setf branch (trimmed (cond (dots (subseq rest 0 dots))
                                                 (on (subseq rest (+ on 4)))
                                                 (t rest))))))
                  ((>= (length line) 4)
                   (let* ((code (subseq line 0 2))
                          (path (subseq line 3))
                          (arrow (search " -> " path)))
                     (when arrow (setf path (subseq path (+ arrow 4))))
                     (push (cons path code) rows)))))
          (values branch (nreverse rows))))))

(defun status-of (path statuses)
  (or (cdr (assoc path statuses :test #'string=)) "  "))

(defun plus-total (files) (reduce #'+ files :key #'second :initial-value 0))
(defun minus-total (files) (reduce #'+ files :key #'third :initial-value 0))

(defun cut (text limit)
  "TEXT cut at LIMIT characters on a line boundary, with the note that says so."
  (if (<= (length text) limit)
      text
      (let* ((head (subseq text 0 limit))
             (break (position #\Newline head :from-end t)))
        (format nil "~a~%[patch cut here: ~d of ~d characters shown; read one scope or one path at a time]~%"
                (if break (subseq head 0 break) head)
                (if break break limit)
                (length text)))))

(defun render-review (&key repo scope path base ref ignore-ws files-only)
  "The review text one call answers: the header, one line a changed file, the
commits a commits scope added, and the patch itself."
  (let* ((scope (string-downcase (trimmed scope)))
         (scope (if (zerop (length scope)) "uncommitted" scope)))
    (unless (member scope +scopes+ :test #'string=)
      (fail "~s is no scope: ~{~a~^, ~}" scope +scopes+))
    (let* ((dir (canonical-repo repo))
           (path (check-path path))
           (result (review-patch dir scope base ref path ignore-ws))
           (patch (getf result :patch))
           (files (patch-files patch)))
      (multiple-value-bind (branch statuses) (worktree-status dir)
        (with-output-to-string (out)
          (format out "git review: ~a~%scope ~a: ~a~%branch ~a, ~d file~:p changed, +~d -~d~%"
                  dir scope (getf result :about)
                  (if (plusp (length branch)) branch "(unknown)")
                  (length files) (plus-total files) (minus-total files))
          (dolist (file files)
            (format out "  ~a  +~d -~d  ~a~%"
                    (status-of (first file) statuses) (second file) (third file) (first file)))
          (when (getf result :commits)
            (format out "~%commits:~%~a~%" (getf result :commits)))
          (cond (files-only
                 (format out "~%(the patch itself is not shown: files_only)~%"))
                ((zerop (length (trimmed patch)))
                 (format out "~%no changes in this scope~%"))
                (t (format out "~%patch:~%~a" (cut patch (setting :max-chars))))))))))

;;; --- the write path -----------------------------------------------------------------

(defun steps-text (steps)
  (with-output-to-string (out)
    (dolist (step steps) (format out "~a~%" step))))

(defun upstream-p (dir)
  (nth-value 1 (git dir "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}")))

(defun remote-url (dir)
  (multiple-value-bind (output ok) (git dir "remote" "get-url" "origin")
    (and ok (trimmed output))))

(defun pr-program (dir)
  "The program that opens a pull or merge request for DIR's remote."
  (if (search "gitlab" (string-downcase (or (remote-url dir) ""))) "glab" "gh"))

(defun strings (value)
  "VALUE as a list of strings: a JSON array of them, one string, or NIL."
  (cond ((null value) '())
        ((stringp value) (if (zerop (length (trimmed value))) '() (list value)))
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t '())))

(defun ship (&key repo message paths push pr all)
  "Stage, commit, and optionally push and open a pull request. => the report."
  (when (setting :read-only)
    (fail "read_only is on: set it off in the git-review section to stage, commit, push or open a pull request"))
  (let* ((dir (canonical-repo repo))
         (message (trimmed message))
         (paths (remove nil (mapcar #'check-path (strings paths))))
         (steps '()))
    (when (zerop (length message))
      (fail "a commit needs a message"))
    (flet ((stage-args ()
             (if (or all (null paths))
                 (list "add" "-A" "--")
                 (append (list "add" "-A" "--")
                         (mapcar (lambda (p) (format nil ":(literal)~a" p)) paths))))
           (note-step (label output ok)
             (push (format nil "~a: ~a" label (one-line output)) steps)
             (unless ok (fail "~a failed: ~a" label (one-line output))))
           (push-step ()
             (multiple-value-bind (output ok)
                 (if (upstream-p dir)
                     (git dir "push")
                     (multiple-value-bind (branch code) (git dir "rev-parse" "--abbrev-ref" "HEAD")
                       (declare (ignore code))
                       (git dir "push" "-u" "origin" (trimmed branch))))
               (note-step "pushed" output ok))))
      (multiple-value-bind (output ok) (apply #'git dir (stage-args))
        (note-step (if (or all (null paths)) "staged everything" "staged the named paths") output ok))
      (multiple-value-bind (output ok) (git dir "commit" "--no-verify" "-m" message)
        (note-step "committed" output ok))
      (when (or push pr) (push-step))
      (when pr
        (let* ((program (pr-program dir))
               (args (if (string= program "glab")
                         (list "mr" "create" "--fill")
                         (list "pr" "create" "--fill"))))
          (multiple-value-bind (output code) (apply #'run-command dir program args)
            (note-step (format nil "~a ~{~a~^ ~}" program args) output (eql 0 code)))))
      (steps-text (nreverse steps)))))

;;; --- the boundaries -----------------------------------------------------------------

(nlk:define-contract git-review-request
  (:doc "Read one change of a git repository: every changed file in one unified diff, with a per-file summary, for the scopes a review needs.")
  ("repo" :string :default "" :doc "absolute path to the git worktree to show; empty means the session's own working directory")
  ("scope" :text :default "uncommitted" :options (list "unstaged" "staged" "uncommitted" "branch" "commits")
   :doc "what to show: uncommitted = working tree against HEAD (the default review view); unstaged = against the index; staged = the index; branch = the branch's net change since it left its base; commits = the commits it added")
  ("path" :string :default "" :doc "repository-relative path to filter the diff to, one file or one folder")
  ("base" :string :default "" :doc "the ref the branch and commits scopes compare against; empty picks the repository's default branch")
  ("ref" :string :default "" :doc "for the commits scope: comma-separated commit ids, each shown as its own patch")
  ("ignore_ws" :boolean :default nil :doc "ignore whitespace-only differences")
  ("files_only" :boolean :default nil :doc "list the changed files and their churn without the patch"))

(nlk:define-contract git-commit-request
  (:doc "The write path of a git review: stage, commit, and optionally push and open a pull or merge request. Nothing here is reversible, so commit only what the person asked for and never rewrite history.")
  ("repo" :string :default "" :doc "absolute path to the git worktree; empty means the session's own working directory")
  ("message" :text :required t :doc "the commit message, in the style of the repository's own subjects")
  ("paths" :array :of :string :doc "repository-relative paths to stage; empty stages every change")
  ("all" :boolean :default nil :doc "stage every change, whatever paths names")
  ("push" :boolean :default nil :slot push-commit :doc "push the branch afterwards, publishing it to origin when it has no upstream")
  ("pr" :boolean :default nil :doc "push, then open a pull or merge request for the branch with gh or glab"))

(defun review-tool (request)
  "The GIT_REVIEW tool: the change REQUEST names, as text."
  (handler-case
      (progn
        (running-settings)
        (render-review :repo (git-review-request-repo request)
                       :scope (git-review-request-scope request)
                       :path (git-review-request-path request)
                       :base (git-review-request-base request)
                       :ref (git-review-request-ref request)
                       :ignore-ws (git-review-request-ignore-ws request)
                       :files-only (git-review-request-files-only request)))
    (git-review-error (e) (nle:failure (format nil "ERROR: ~a" e)))))

(defun commit-tool (request)
  "The GIT_COMMIT tool: the write path REQUEST names, as its report."
  (handler-case
      (progn
        (running-settings)
        (ship :repo (git-commit-request-repo request)
              :message (git-commit-request-message request)
              :paths (git-commit-request-paths request)
              :all (git-commit-request-all request)
              :push (git-commit-request-push-commit request)
              :pr (git-commit-request-pr request)))
    (git-review-error (e) (nle:failure (format nil "ERROR: ~a" e)))))

;;; --- /review and the eval seam -------------------------------------------------------

(define-verb review (&key repo scope path base ref ignore-ws files-only)
  "Read one change of a repository, for eval: (git-review:review :repo \"a\"
:scope \"staged\"). It answers the same text the git_review tool answers."
  (running-settings)
  (render-review :repo repo :scope scope :path path :base base :ref ref
                 :ignore-ws ignore-ws :files-only files-only))

(defun review-command (args session-id)
  "One /review [SCOPE] [PATH]: the change this session's repository holds."
  (handler-case
      (let* ((words (remove "" (mapcar #'trimmed
                                       (uiop:split-string (or args "") :separator '(#\Space)))
                            :test #'string=))
             (scope (if (and words (member (first words) +scopes+ :test #'string-equal))
                        (string-downcase (pop words))
                        "uncommitted"))
             (path (if words (string-join words " ") ""))
             (repo (or (and (stringp session-id)
                            (ignore-errors (nlk:find-session-cwd session-id)))
                       "")))
        (values (review :repo repo :scope scope :path path)))
    (git-review-error (e) (format nil "review: ~a" e))))

;;; --- the primer --------------------------------------------------------------------

(defparameter +primer+
  "git review reads a repository's change as one scroll of text, the way a pull
request is read, through two tools and /review:
  git_review   repo (a path; empty = this session's working directory), scope
      unstaged | staged | uncommitted (the default) | branch | commits,
      path (one file or folder), base, ref (comma-separated commit ids, commits
      scope), ignore_ws, files_only. It answers the branch, one line a changed
      file with its +/- churn and status, and the unified patch, with new
      untracked files rendered as their own hunks.
  git_commit   message, paths (empty = everything), all, push, pr. It stages,
      commits with hooks off, and on push publishes a branch that has no
      upstream; pr opens a pull or merge request with gh or glab.
/review [scope] [path] reads the same, for the person typing it.
New files above a few hundred lines are named and cut. A patch longer than
max_chars is cut with a note, so read one scope or one path at a time.
Committing, pushing and opening a pull request are the person's decisions: do
them when asked, and never rewrite history. With read_only set, every write
is refused."
  "What (help :git-review) answers while the cell runs.")

(nle:define-cell git-review
  (:section ("git-review")
    (:guide "git on PATH is all it needs; read_only refuses stage, commit, push and pull requests; base is the ref the branch and commits scopes compare against, and empty picks origin/HEAD, main or master")
    ("repos" :list :doc "repository paths to fall back on when a session has no working directory")
    ("read_only" :boolean :default nil :doc "refuse stage, commit, push and pull requests")
    ("base" :string :default "" :doc "ref that the branch and commits scopes compare against; empty picks the repository's default branch")
    ("max_chars" :integer :default 60000 :min 2000 :doc "the most patch text one call answers")
    ("context_lines" :integer :default 3 :min 0 :doc "unchanged lines kept around each change")
    ("new_files" :boolean :default t :doc "show the content of new, untracked files")
    ("timeout_seconds" :integer :default 20 :min 1 :doc "one git command's ceiling"))
  (:tool "git_review" (nlk:contract 'git-review-request) 'review-tool)
  (:tool "git_commit" (nlk:contract 'git-commit-request) 'commit-tool)
  (:help :git-review
         "git-review: read a repository's change as one diff (git_review), then stage, commit, push and open a pull request (git_commit)"
         +primer+)
  (:command "review" 'review-command
            :description "Read this repository's change: /review [scope] [path]"
            :argument-hint "SCOPE PATH"))
