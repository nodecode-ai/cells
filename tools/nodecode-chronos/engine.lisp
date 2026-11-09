;;;; engine.lisp --- the shadow repository under the workspace, and the four
;;;; verbs over it: checkpoint, list, diff, restore.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin keeps a private git repository inside the workspace
;;;; and records a commit on every checkpoint, so the project's own history
;;;; never moves and a checkpoint costs only what changed. This is the same
;;;; design, in Lisp: <workspace>/.nodecode-chronos/repo is a repository of
;;;; its own, driven with --git-dir and --work-tree, whose commit message is
;;;; the metadata ("CHRONOS_CP: {json}"), and <workspace>/.nodecode-chronos/
;;;; carries a .gitignore of "*" so the operator's own git status stays clean.
;;;;
;;;; A restore writes the checkpoint's tree back with `git read-tree --reset
;;;; -u' and leaves HEAD where it was, so every restore is itself a state a
;;;; later checkpoint records; and a restore takes a safety checkpoint of the
;;;; state it is about to overwrite first.

(in-package #:nodecode-chronos)

(defparameter +default-ignored+
  '(".git" ".nodecode-chronos" "node_modules" ".venv" "venv" "__pycache__")
  "Paths a checkpoint never covers: the project's own history, this cell's
shadow repository, and dependency or cache directories.")

(defparameter +message-prefix+ "CHRONOS_CP: "
  "The commit message starts with this, then one line of JSON: the checkpoint's
description, the tool that took it, and when.")

(defparameter +empty-tree+ "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  "The well-known empty tree, the base a root checkpoint's diff is taken against.")

(defvar *workspace-root* nil
  "The directory checkpoints cover when a test binds it; NIL reads the
section's workspace, and that defaults to this process's own directory.")

;;; --- paths -----------------------------------------------------------------------

(defun under (root relative)
  (concatenate 'string root "/" relative))

(defun shadow-dir (root) (under root ".nodecode-chronos"))

(defun repo-dir (root) (under root ".nodecode-chronos/repo"))

(defun excludes-file (root) (under root ".nodecode-chronos/excludes"))

(defun absolute-directory (value what)
  "VALUE as an absolute directory name without its trailing separator, or a
refusal naming WHAT the caller wanted."
  (let* ((text (string-trim '(#\Space #\Tab)
                             (if (stringp value) value (uiop:native-namestring value))))
         (path (if (uiop:string-prefix-p "~" text)
                   (nlk:expand-home text)
                   (uiop:parse-native-namestring text))))
    (unless (uiop:absolute-pathname-p path)
      (refuse "~a must be an absolute path, got ~s" what value))
    (unless (uiop:directory-exists-p path)
      (refuse "~a names no directory here: ~s" what value))
    (string-right-trim "/\\" (uiop:native-namestring path))))

(defun workspace ()
  "The directory checkpoints cover: the one a test bound, else the section's
workspace, else this process's own directory."
  (or *workspace-root*
      (let ((configured (setting :workspace)))
        (if (and configured (plusp (length (string-trim '(#\Space #\Tab) configured))))
            (absolute-directory configured "workspace")
            (string-right-trim "/\\" (uiop:native-namestring (uiop:getcwd)))))))

;;; --- the excluded and hidden paths ------------------------------------------------

(defun ignored-patterns ()
  "The paths no checkpoint covers: the defaults, then the section's own. A
negated pattern is dropped: it could bring .git or the shadow repository back in."
  (remove-duplicates
   (append +default-ignored+
           (remove-if (lambda (pattern)
                        (or (zerop (length pattern)) (char= #\! (char pattern 0))))
                      (mapcar #'nlk:trimmed (or (setting :ignored) '()))))
   :test #'string=))

(defun excludes-text ()
  "The gitignore file the shadow repository reads: one pattern a line, plus the
directory form of each, so a directory goes in whole."
  (with-output-to-string (out)
    (dolist (pattern (ignored-patterns))
      (format out "~a~%~a/**~%" pattern (string-right-trim "/" pattern)))))

(defun ensure-text (path text)
  "Write TEXT to PATH when PATH does not already hold it."
  (unless (and (probe-file path) (equal (uiop:read-file-string path) text))
    (nlk:write-file-atomically path text)))

;;; --- one git command line ---------------------------------------------------------

(defun git-argv (root &rest arguments)
  "ARGS for one git run against ROOT's shadow repository: the repository and
work tree named outright, no hooks, no signing, and the identity a checkpoint
is committed under, so nothing here reads the operator's own git config."
  (append (list "git"
                "-c" "safe.directory=*"
                "-c" (format nil "core.excludesFile=~a" (excludes-file root))
                "-c" (format nil "core.hooksPath=~a" (under (shadow-dir root) "hooks"))
                "-c" "commit.gpgsign=false"
                "-c" "gc.auto=0"
                "-c" "user.name=nodecode-chronos"
                "-c" "user.email=chronos@nodecode.invalid"
                "--git-dir" (repo-dir root)
                "--work-tree" root)
          arguments))

(defun git-run (root &rest arguments)
  "One git run against ROOT's shadow repository => (values STDOUT STDERR STATUS)."
  (run-status (apply #'git-argv root arguments) root))

;;; --- the repository ---------------------------------------------------------------

(defun head (root)
  "ROOT's shadow repository HEAD as a hash, or NIL when it has none."
  (multiple-value-bind (out stderr status) (git-run root "rev-parse" "--verify" "--quiet" "HEAD")
    (declare (ignore stderr))
    (let ((text (and (eql status 0) out (nlk:trimmed out))))
      (and text (plusp (length text)) text))))

(defun ensure-repo (root)
  "ROOT's shadow repository, made now when this is the first checkpoint here,
and given its baseline commit when it has none."
  (let ((head-file (under (repo-dir root) "HEAD")))
    (unless (probe-file head-file)
      (multiple-value-bind (out stderr status)
          (run-status (list "git" "-c" "init.defaultBranch=chronos" "init" "--quiet" (repo-dir root)) root)
        (declare (ignore out))
        (unless (eql status 0)
          (refuse "could not start a shadow repository in ~a: ~a"
                  (shadow-dir root) (failure-line status stderr)))))
    (ensure-text (excludes-file root) (excludes-text))
    (ensure-text (under (shadow-dir root) ".gitignore") "*~%")
    (unless (head root)
      (git-run root "add" "-A" "--" ".")
      (git-run root "commit" "--allow-empty" "--no-verify" "--no-gpg-sign"
               "-m" (commit-message "Baseline checkpoint" "system"))
      (unless (head root)
        (refuse "could not write the baseline checkpoint in ~a" (shadow-dir root))))
    root))

(defun json-object-of (plist)
  "PLIST as the JSON object an answer or a commit message carries."
  (let ((table (nlk:make-json-object)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash (string-downcase (string key)) table) value))
    table))

(defun commit-message (description tool)
  "The one-line JSON commit message a checkpoint is recorded under."
  (concatenate 'string +message-prefix+
               (nlk:encode-json-object
                (json-object-of (list :desc (clip description 300) :tool tool
                                       :time (get-universal-time))))))

(defun message-metadata (subject)
  "The metadata SUBJECT carries, or NIL when it is not one of ours."
  (when (and subject (uiop:string-prefix-p +message-prefix+ subject))
    (let ((json (ignore-errors (nlk:decode-json (subseq subject (length +message-prefix+))))))
      (when (hash-table-p json) json))))

(defun short-hash (hash)
  (and hash (subseq hash 0 (min 8 (length hash)))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, an ellipsis when it was longer."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (concatenate 'string (subseq text 0 (max 0 (1- limit))) "…"))))

;;; --- checkpoint ------------------------------------------------------------------

(defun checkpoint (root description &key (tool "manual"))
  "Record a checkpoint of ROOT now, unless nothing changed since the last one.
=> a plist: status, created, id, hash, description, tool, message."
  (ensure-repo root)
  (git-run root "add" "-A" "--" ".")
  (multiple-value-bind (out stderr status) (git-run root "diff-index" "--quiet" "HEAD" "--")
    (declare (ignore out stderr))
    (if (and (eql status 0) (head root))
        (list :status "ok" :created nil :id (short-hash (head root)) :hash (head root)
              :description description :tool tool
              :message "No workspace change since the last checkpoint.")
        (progn
          (multiple-value-bind (out stderr status)
              (git-run root "commit" "--allow-empty" "--no-verify" "--no-gpg-sign"
                       "-m" (commit-message description tool))
            (declare (ignore out))
            (unless (eql status 0)
              (refuse "git commit failed: ~a" (failure-line status stderr))))
          (let ((hash (head root)))
            (unless hash (refuse "the checkpoint left no HEAD in ~a" (repo-dir root)))
            (list :status "ok" :created t :id (short-hash hash) :hash hash
                  :description description :tool tool
                  :message (format nil "Checkpoint ~a created." (short-hash hash))))))))

(defun checkpoints (root &key (limit 15))
  "ROOT's checkpoints, newest first, as plists."
  (ensure-repo root)
  (multiple-value-bind (out stderr status)
      (git-run root "log" "--no-show-signature" "--no-color"
               "--pretty=format:%H%x09%ct%x09%s" "-n" (princ-to-string limit) "HEAD")
    (declare (ignore stderr))
    (if (eql status 0)
        (loop for line in (split-lines (or out ""))
              for fields = (uiop:split-string line :separator '(#\Tab))
              when (= 3 (length fields))
                collect (let ((metadata (message-metadata (third fields))))
                          (list :id (short-hash (first fields))
                                :hash (first fields)
                                :time (parse-integer (second fields) :junk-allowed t)
                                :description (or (and metadata (gethash "desc" metadata))
                                                 (third fields))
                                :tool (or (and metadata (gethash "tool" metadata)) "manual"))))
        '())))

;;; --- refs ------------------------------------------------------------------------

(defun resolve-ref (root ref)
  "REF as a commit in ROOT's shadow repository, or the refusal that it is not
one. `latest' (and nothing) is HEAD; `worktree' is answered as itself."
  (let ((text (nlk:trimmed (or ref ""))))
    (cond
      ((member text '("" "latest" "last" "head" "tip") :test #'string-equal)
       (or (head root) (refuse "no checkpoint recorded yet in ~a" (shadow-dir root))))
      ((string-equal text "worktree") "WORKTREE")
      ((or (zerop (length text)) (char= #\- (char text 0)) (find #\Space text))
       (refuse "~s is no checkpoint id" text))
      (t (multiple-value-bind (out stderr status)
             (git-run root "rev-parse" "--verify" "--quiet" (concatenate 'string text "^{commit}"))
           (declare (ignore stderr))
           (let ((sha (and (eql status 0) (nlk:trimmed out))))
             (if (and sha (plusp (length sha)))
                 sha
                 (refuse "no checkpoint ~s in ~a" text (shadow-dir root)))))))))

(defun parent-of (root hash)
  "HASH's parent, or the empty tree for a root checkpoint."
  (multiple-value-bind (out stderr status)
      (git-run root "rev-parse" "--verify" "--quiet" (concatenate 'string hash "^"))
    (declare (ignore stderr))
    (let ((sha (and (eql status 0) (nlk:trimmed out))))
      (if (and sha (plusp (length sha))) sha +empty-tree+))))

;;; --- diff ------------------------------------------------------------------------

(defun untracked-paths (root)
  "The files in ROOT that the index does not hold yet: what a checkpoint taken
now would add."
  (multiple-value-bind (out stderr status)
      (git-run root "ls-files" "--others" "--exclude-standard")
    (declare (ignore stderr))
    (if (eql status 0) (split-lines out) '())))

(defun tracked-paths (root ref)
  "Every path REF's tree holds."
  (let ((out (git-ok root "ls-tree" "-r" "--name-only" ref)))
    (split-lines out)))

(defun clip-diff (text max-chars)
  "TEXT cut to MAX-CHARS at a line boundary, saying so."
  (let ((text (or text "")))
    (if (<= (length text) max-chars)
        (values text nil)
        (values (concatenate 'string (subseq text 0 max-chars) "
... [diff truncated]")
                t))))

(defun diff-answer (root &key from to id (max-chars 400000))
  "What changed, between checkpoints or against the files on disk => a plist."
  (ensure-repo root)
  (multiple-value-bind (base target)
      (cond
        (id (let ((sha (resolve-ref root id)))
              (when (string= sha "WORKTREE")
                (refuse "checkpoint_id names a checkpoint, not the worktree"))
              (values (parent-of root sha) sha)))
        (from (let ((sha (resolve-ref root from)))
                (values sha
                        (if (or (null to) (string-equal (nlk:trimmed to) "worktree"))
                            "WORKTREE"
                            (resolve-ref root to)))))
        (t (values "HEAD" "WORKTREE")))
    (let* ((arguments (if (string= target "WORKTREE")
                          (list "diff" "--no-color" "--no-ext-diff" "--unified=3" base "--" ".")
                          (list "diff" "--no-color" "--no-ext-diff" "--unified=3" base target "--" ".")))
           (text (or (car (multiple-value-list (apply #'git-run root arguments))) ""))
           (stat (or (car (multiple-value-list
                           (apply #'git-run root (append (list "diff" "--stat") (rest arguments)))))
                     "")))
      (multiple-value-bind (clipped cut) (clip-diff text max-chars)
        (list :status "ok" :from (short-hash base) :to (short-hash target)
              :diff (if (plusp (length clipped)) clipped "(no modification)")
              :stat (clip-diff stat 20000) :truncated cut
              :untracked (if (string= target "WORKTREE") (untracked-paths root) '()))))))

;;; --- restore ---------------------------------------------------------------------

(defun restore-untracked (root safety untracked target)
  "Write the files the safety checkpoint kept, and TARGET does not track, back
on disk, and leave them unstaged again. => how many came back."
  (let ((mine (remove-if (lambda (path) (member path (tracked-paths root target) :test #'string=))
                         untracked)))
    (when mine
      (apply #'git-run root (append (list "checkout" safety "--") mine))
      (apply #'git-run root (append (list "rm" "--cached" "--ignore-unmatch" "--") mine)))
    (count-if #'probe-file mine)))

(defun rollback (root id &key (clean t))
  "Put ROOT's files back the way checkpoint ID left them. A safety checkpoint
of the state now is taken first, so the restore can itself be undone."
  (ensure-repo root)
  (let* ((target (resolve-ref root id))
         (untracked (if (string= target "WORKTREE") '() (untracked-paths root)))
         (safety (checkpoint root (format nil "Before restoring checkpoint ~a" (short-hash target))
                             :tool "chronos_rollback"))
         (safety-hash (getf safety :hash)))
    (when (string= target "WORKTREE")
      (refuse "worktree is where the files already are, not a checkpoint to restore"))
    (git-ok root "read-tree" "--reset" "-u" target)
    (let ((kept 0))
      (when (and (not clean) safety-hash untracked)
        (setf kept (restore-untracked root safety-hash untracked target)))
      (list :status "ok" :restored (short-hash target) :hash target
            :safety (getf safety :id) :clean (and clean t)
            :untracked-removed (if clean (length untracked) 0) :untracked-kept kept
            :message (format nil "Workspace restored to checkpoint ~a." (short-hash target))))))
