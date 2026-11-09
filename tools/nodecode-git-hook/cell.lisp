;;;; cell.lisp --- git hook: pull before reads, commit and push what a turn changed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of aean0x/hermes-git-hook. The worktrees a tool call touches are
;;;; pulled once per session while they are clean, and the files a turn changed
;;;; are committed and pushed at the turn's end: only those, never the rest of
;;;; the tree, never a secret-looking file. Two points do it. :TOOL wraps every
;;;; call -- the baseline snapshot and the ff-only pull before NEXT runs, the
;;;; value-carrying delta after it -- and :ROUND, which is a turn's end, flushes
;;;; the queue. A branch whose direct push the remote refuses on policy grounds
;;;; (a pull request is required) is left dirty and said once, instead of
;;;; stranding commits that can never land.

(in-package #:nodecode-git-hook)

(nlk:define-peripheral git-hook :not-running t :lock "git-hook trees")

;;; --- what is never synced --------------------------------------------------

(defparameter +skip-prefixes+ '("/nix/" "/proc/" "/sys/" "/dev/" "/run/")
  "Trees no pull, commit or push ever touches, whatever the section says. A
session's own scratch worktree is usually under a temp directory, so unlike
the original those are allowed.")

(defparameter +secret-prefixes+ '(".env" "auth.json" "credentials"
                                  "id_rsa" "id_dsa" "id_ecdsa" "id_ed25519")
  "File names never staged in any repository, whatever the section says.")

(defparameter +secret-suffixes+ '(".pem" ".key")
  "File name endings never staged, beside +SECRET-PREFIXES+.")

(defparameter +write-tools+ '("write-file" "write_file" "edit" "patch" "skill_manage")
  "Calls that land content: their baseline is taken, and no pull runs beside
one, so a pull can never race the write.")

(defparameter +pathless-tools+ '("eval" "sh" "run-program" "run-shell-command")
  "Calls that can change a file without naming one: they stand for the
session's own working directory.")

(defparameter +path-keys+ '("path" "file_path" "workdir" "dir" "directory")
  "Argument members a call names a filesystem path in.")

(defparameter +policy-patterns+ '("gh0" "protected branch"
                                  "refusing to update checked out branch"
                                  "hook declined to update"
                                  "must be made through a pull request"
                                  "required status check"
                                  "pre-receive hook declined")
  "A push the remote refused this way can never land on a retry: the branch is
closed to direct pushes, and a commit made there only strands.")

;;; --- state -----------------------------------------------------------------

(defvar *run* nil
  "The one process seam: (DIR ARGS SECONDS) => (values OUTPUT ERROR STATUS), as
NLK:RUN-BOUNDED answers, STATUS :TIMEOUT when the bound killed the child. NIL
means the real subprocess, REAL-RUN. Tests rebind it.")

(defvar *trees* (make-hash-table :test #'equal)
  "One session's bookkeeping per worktree: (SESSION-ID . ROOT) -> TREE-STATE.")

(defvar *noted* (make-hash-table :test #'equal)
  "Worktrees and branches a person was already told are closed to direct
pushes: (ROOT . BRANCH). The note is per branch, not per flush.")

(defvar *commit-on* t
  "Whether this run commits: the section's `commit' at start, /git-hook after.")

(defvar *push-on* t
  "Whether this run pushes: the section's `push' at start, /git-hook after.")

(defstruct (tree-state (:constructor make-tree-state ()))
  "One session's bookkeeping for one worktree."
  (taken nil)          ; the baseline snapshot was taken
  (snap '())           ; (REL . SIGNATURE), as of the last look
  (pending '())        ; the rels this session changed, waiting for the flush
  (pulled nil)         ; the once-per-session pull is behind us
  (protected nil)      ; the branch whose direct push was refused, or NIL
  (unpushed nil))      ; a commit of ours that did not reach the remote

;;; --- text ------------------------------------------------------------------

(defun base-name (path)
  "PATH's last component."
  (subseq path (1+ (or (position #\/ path :from-end t) -1))))

(defun repo-name (root)
  "ROOT as a note names it: its own last component."
  (base-name (string-right-trim "/" root)))

(defun one-line (text)
  "TEXT as one line of at most 200 characters, for a note."
  (let ((text (substitute #\Space #\Newline (or text ""))))
    (if (> (length text) 200) (concatenate 'string (subseq text 0 200) "...") text)))

(defun file-hint (rels)
  "A short list of RELS for a note."
  (let ((names (mapcar #'base-name (sort (copy-list rels) #'string<))))
    (if (<= (length names) 3)
        (format nil "~{~a~^, ~}" names)
        (format nil "~{~a~^, ~} (+~d more)" (subseq names 0 3) (- (length names) 3)))))

;;; --- the process seam ------------------------------------------------------

(defun real-run (dir args seconds)
  "GIT with ARGS in DIR, bounded by SECONDS, never prompting: a credential or
passphrase question inside a hook would hold the turn until the bound. A
repository's own core.fsmonitor command is never run either, since the
repository is the model's to pick. => (values OUTPUT ERROR STATUS)."
  (nlk:run-bounded (list* "git" "-c" "core.fsmonitor=" args)
                   :seconds seconds
                   :directory (uiop:ensure-directory-pathname dir)
                   :environment (list "GIT_TERMINAL_PROMPT=0"
                                      "GIT_SSH_COMMAND=ssh -oBatchMode=yes")))

(defun git (dir seconds &rest args)
  "One git command with ARGS in DIR. => (values OUTPUT ERROR STATUS)."
  (funcall (or *run* #'real-run) dir args seconds))

(defun git-ok (dir seconds &rest args)
  "One git command whose exit status is all the caller wants, its error output
falling back to its output. => (values OUTPUT OK-P)."
  (multiple-value-bind (output error status) (apply #'git dir seconds args)
    (values (if (plusp (length (or output ""))) output (or error "")) (eql 0 status))))

;;; --- git's own answer about the tree --------------------------------------

(defun parse-porcelain (text)
  "The dirty records in a `git status --porcelain -z -uall' answer, each
(REL STATUS): one per changed path, a rename or a copy carrying its source
path too."
  (let ((fields (uiop:split-string (or text "") :separator '(#\Null)))
        (records '()))
    (loop while fields
          for entry = (pop fields)
          do (when (and entry (> (length entry) 3))
               (let ((status (subseq entry 0 2))
                     (rel (subseq entry 3)))
                 (unless (zerop (length rel))
                   (push (list rel status) records))
                 (when (or (find #\R status) (find #\C status))
                   (let ((source (pop fields)))
                     (when (and source (plusp (length source)))
                       (push (list source status) records)))))))
    (nreverse records)))

(defun transient-p (rel)
  "Whether REL is a scratch name -- an atomic-write temp, an editor's lock
file -- that is gone by flush time. One dead pathspec makes `git add' exit
non-zero and stage NOTHING, which would lose the whole turn's work."
  (let ((name (base-name rel)))
    (or (and (uiop:string-prefix-p "#" name) (uiop:string-suffix-p name "#"))
        (uiop:string-prefix-p ".#" name)
        (search "tmp" name :test #'char-equal))))

(defun signature (root rel status)
  "Cheap content signature of one dirty path: its XY status, its size and its
write date. A path-only dirty set cannot see an edit to a file that was
ALREADY dirty -- the path is there before and after -- so folding the content
in makes that a delta again, and dirt nobody touched keeps its signature and
is still left alone."
  (let ((path (format nil "~a/~a" (string-right-trim "/" root) rel)))
    (handler-case
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (list status (file-length in) (file-write-date in)))
      (error () (list status -1 -1)))))

(defun snapshot (root)
  "ROOT's dirty paths mapped to their signature, read from git status. NIL
when git cannot tell, which leaves the tree alone."
  (multiple-value-bind (output error status) (git root 5 "status" "--porcelain" "-z" "-uall")
    (declare (ignore error))
    (when (eql 0 status)
      (loop for (rel entry-status) in (parse-porcelain output)
            unless (transient-p rel)
              collect (cons rel (signature root rel entry-status))))))

;;; --- secrets, and the paths a call names -----------------------------------

(defun secret-path-p (rel)
  "Whether REL's own name is one a secret hides behind: checked before every
staging, so a credential is never committed by this cell in any repository.
A `secret_names' entry in the section adds to the list."
  (let* ((name (string-downcase (base-name rel)))
         (extra (mapcar #'string-downcase (remove-if-not #'stringp (setting :secret-names)))))
    (or (some (lambda (prefix) (uiop:string-prefix-p prefix name))
              (append +secret-prefixes+ extra))
        (some (lambda (suffix) (uiop:string-suffix-p name suffix)) +secret-suffixes+))))

(defun skipped-p (path)
  "Whether PATH is under a tree no sync touches."
  (let ((text (concatenate 'string (string-right-trim "/" path) "/")))
    (some (lambda (prefix) (uiop:string-prefix-p prefix text)) +skip-prefixes+)))

(defun under-root-p (dir allowed)
  "Whether DIR is one of ALLOWED, or inside one."
  (let ((dir (string-right-trim "/" dir)))
    (some (lambda (root)
            (or (string= dir root)
                (uiop:string-prefix-p (concatenate 'string root "/") dir)))
          allowed)))

(defun allowed-roots ()
  "The directories whose worktrees may be synced, as the section lists them."
  (remove "" (mapcar (lambda (root) (string-right-trim "/" (nlk:trimmed root)))
                     (remove-if-not #'stringp (setting :roots)))
          :test #'string=))

(defun session-cwd (session)
  "The session's own working directory, or NIL."
  (and session (ignore-errors (nlk:find-session-cwd session))))

(defun absolute (path session)
  "PATH made absolute against the session's directory, as git will read it."
  (namestring (uiop:ensure-absolute-pathname
               path (or (session-cwd session) (uiop:getcwd)))))

(defun patch-file (line)
  "The file a patch header LINE names, or NIL."
  (dolist (head '("*** Update File: " "*** Add File: " "*** Delete File: "
                  "*** Rename File From: " "*** Rename File To: "))
    (when (uiop:string-prefix-p head line)
      (return (string-trim '(#\Space #\Tab #\Return) (subseq line (length head)))))))

(defun call-paths (op session)
  "The filesystem paths a tool call names, best effort, absolute: the path-like
arguments, and the files a patch lands on."
  (let ((arguments (getf op :arguments))
        (paths '()))
    (when (hash-table-p arguments)
      (dolist (key +path-keys+)
        (let ((value (gethash key arguments)))
          (when (and (stringp value) (plusp (length (nlk:trimmed value))))
            (pushnew (nlk:trimmed value) paths :test #'string=))))
      (let ((patch (gethash "patch" arguments)))
        (when (stringp patch)
          (dolist (line (uiop:split-string patch :separator '(#\Newline)))
            (let ((file (patch-file line)))
              (when file (pushnew file paths :test #'string=)))))))
    (loop for path in (nreverse paths)
          for full = (ignore-errors (absolute path session))
          when full collect full)))

(defun worktree-of (path allowed)
  "The worktree containing PATH, when it is under one of ALLOWED and not in a
tree no sync touches, else NIL."
  (let* ((full (string-right-trim "/" path))
         (start (if (uiop:directory-exists-p (uiop:ensure-directory-pathname full))
                    full
                    (let ((slash (position #\/ full :from-end t)))
                      (if (and slash (plusp slash)) (subseq full 0 slash) full)))))
    (unless (skipped-p start)
      (multiple-value-bind (output ok) (git-ok start 3 "rev-parse" "--show-toplevel")
        (let ((root (and ok (nlk:trimmed output))))
          (when (and root (plusp (length root))
                     (not (skipped-p root)) (under-root-p root allowed))
            root))))))

(defun write-call-p (op)
  "Whether this call lands content: no pull runs beside one."
  (member (string-downcase (string (or (getf op :name) ""))) +write-tools+ :test #'string=))

(defun call-roots (op session)
  "The worktrees this call touches, each one under an allowed root."
  (let* ((allowed (allowed-roots))
         (named (call-paths op session))
         (paths (or named
                    (and (member (string-downcase (string (or (getf op :name) "")))
                                 +pathless-tools+ :test #'string=)
                         (let ((cwd (session-cwd session))) (and cwd (list cwd)))))))
    (remove-duplicates (remove nil (mapcar (lambda (path) (worktree-of path allowed)) paths))
                       :test #'string=)))

;;; --- per-session bookkeeping ------------------------------------------------

(defun state-of (session root &key create)
  "SESSION's bookkeeping for ROOT, made when CREATE says so."
  (let ((key (cons (or session "") root)))
    (or (gethash key *trees*)
        (and create (setf (gethash key *trees*) (make-tree-state))))))

(defun branch (root)
  "ROOT's checked-out branch, the empty string when it cannot be read."
  (multiple-value-bind (output ok) (git-ok root 3 "rev-parse" "--abbrev-ref" "HEAD")
    (let ((name (and ok (nlk:trimmed output))))
      (or (and name (plusp (length name)) name) ""))))

(defun head-sha (root)
  "ROOT's HEAD commit id, empty on an unborn branch."
  (multiple-value-bind (output ok) (git-ok root 3 "rev-parse" "HEAD")
    (if ok (nlk:trimmed output) "")))

(defun short-id (sha)
  "SHA as a note names it: its first seven characters."
  (subseq sha 0 (min 7 (length sha))))

(defun upstream-p (root)
  "Whether ROOT's branch has an upstream to pull from and push to."
  (multiple-value-bind (output ok)
      (git-ok root 3 "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}")
    (and ok (plusp (length (nlk:trimmed output))))))

(defun busy-p (root)
  "Whether ROOT is mid-merge, mid-rebase or mid-cherry-pick: git accepts no
commit there, so the paths wait for a later pass."
  (let ((git-dir (format nil "~a/.git" (string-right-trim "/" root))))
    (and (uiop:directory-exists-p (uiop:ensure-directory-pathname git-dir))
         (some (lambda (name) (probe-file (format nil "~a/~a" git-dir name)))
               '("rebase-merge" "rebase-apply" "MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD")))))

(defun protected-branch-p (session root)
  "The branch of ROOT this session already learned refuses direct pushes."
  (let ((state (state-of session root))) (and state (tree-state-protected state))))

;;; --- the pull --------------------------------------------------------------

(defun mark-pulled (session root)
  "Record that ROOT's once-per-session pull is behind us."
  (setf (tree-state-pulled (state-of session root :create t)) t))

(defun pull-once (root session deadline)
  "One `git pull --ff-only' for ROOT this session, and only on a clean worktree
with an upstream. DEADLINE is the before-a-call phase's, in internal time, so
one slow pull cannot push the phase past its budget. => a status keyword."
  (let ((state (state-of session root)))
    (when (and state (tree-state-pulled state)) (return-from pull-once :already)))
  (cond
    ((busy-p root) :busy)
    ((snapshot root) :dirty)
    ((not (upstream-p root)) (mark-pulled session root) :no-upstream)
    (t (let ((remaining (/ (- deadline (get-internal-real-time))
                           (float internal-time-units-per-second))))
         (if (not (plusp remaining))
             :budget
             (let ((seconds (max 1 (min (setting :pull-timeout-seconds) (floor remaining)))))
               (multiple-value-bind (output error status)
                   (git root seconds "pull" "--ff-only" "--quiet")
                 (declare (ignore output error))
                 (when (eql 0 status) (mark-pulled session root))
                 (cond ((eq status :timeout) :timeout)
                       ((eql 0 status) :pulled)
                       (t :failed)))))))))

;;; --- the turn's delta ------------------------------------------------------

(defun settle-before (roots session pull)
  "Take each root's baseline snapshot before the call runs, and pull it once
this session when it is clean. One deadline covers every root, so the phase
cannot outlive its budget."
  (let ((deadline (+ (get-internal-real-time)
                      (* (setting :budget-seconds) internal-time-units-per-second))))
    (dolist (root roots)
      (let ((needed (with-git-hook-lock
                      (not (tree-state-taken (state-of session root :create t))))))
        (when needed
          (let ((snap (snapshot root)))
            (with-git-hook-lock
              (let ((state (state-of session root :create t)))
                (unless (tree-state-taken state)
                  (setf (tree-state-snap state) snap
                        (tree-state-taken state) t)))))))
      (when (and pull (eq :pulled (pull-once root session deadline)))
        (let ((snap (snapshot root)))
          (with-git-hook-lock
            (setf (tree-state-snap (state-of session root :create t)) snap)))))))

(defun record-delta (roots session)
  "Queue the paths whose signature moved since the last look: an edit to an
already-dirty file is a delta too, and dirt nobody touched is left alone."
  (let ((after (loop for root in roots collect (cons root (snapshot root)))))
    (with-git-hook-lock
      (loop for (root . snap) in after
            for state = (state-of session root :create t)
            do (loop for (rel . sig) in snap
                     unless (equal (cdr (assoc rel (tree-state-snap state) :test #'string=)) sig)
                       do (pushnew rel (tree-state-pending state) :test #'string=))
               (setf (tree-state-snap state) snap
                     (tree-state-taken state) t)))))

;;; --- the commit and the push -----------------------------------------------

(defun message-for (rels)
  "The commit subject: the section's own, else what the turn touched."
  (let ((override (nlk:trimmed (or (setting :message) ""))))
    (cond ((plusp (length override)) override)
          ((= 1 (length rels)) (format nil "update ~a" (base-name (first rels))))
          (t (format nil "update ~d files" (length rels))))))

(defun stageable (root rels)
  "The subset of RELS `git add' accepts: a path still there, or a tracked path
that is gone, which is a deletion like any other change. A scratch name is
never handed to git."
  (let ((present '()) (gone '()))
    (dolist (rel rels)
      (unless (transient-p rel)
        (if (probe-file (format nil "~a/~a" root rel))
            (push rel present)
            (push rel gone))))
    (when gone
      (multiple-value-bind (output ok)
          (apply #'git-ok (append (list root 5 "ls-files" "-z" "--") gone))
        (when ok
          (loop for rel in (uiop:split-string (or output "") :separator '(#\Null))
                unless (or (zerop (length rel)) (transient-p rel))
                  do (push rel present)))))
    (sort (remove-duplicates present :test #'string=) #'string<)))

(defun undo-own-commit (root previous sha)
  "Drop the commit this flush just made, its content staged, when HEAD is still
provably that commit: a commit someone else made in between is never touched."
  (when (and previous sha (string= (head-sha root) sha))
    (git-ok root 10 "reset" "--soft" previous)))

(defun policy-rejection-p (text)
  "Whether the remote refused a push on policy grounds -- a branch protection, a
ruleset that requires a pull request, a server-side check -- rather than for the
network, authentication or a timeout."
  (let ((text (string-downcase (or text ""))))
    (some (lambda (pattern) (search pattern text)) +policy-patterns+)))

(defun policy-note (root branch-name rels)
  "What to tell a person about a branch whose direct pushes the remote refuses,
once per worktree and branch."
  (let ((key (cons root branch-name)))
    (when (with-git-hook-lock (not (gethash key *noted*)))
      (with-git-hook-lock (setf (gethash key *noted*) t))
      (format nil "~a: origin rejects direct pushes to ~a (a pull request is required) - git-hook left ~a uncommitted and pushed nothing"
              (repo-name root) (if (plusp (length (or branch-name ""))) branch-name "HEAD")
              (if rels (file-hint rels) "the turn's files")))))

(defun push-head (root session sha previous rels)
  "Push the commit SHA this flush just made in ROOT. A push refused for policy
drops that commit again -- its content stays staged, so the edit is still in
the tree -- and closes the branch for the session; a transient failure keeps
the commit and is retried. => (values OUTCOME NOTE)."
  (let ((said (short-id sha)))
    (if (not *push-on*)
        (values :committed (format nil "~a: committed ~a (push is off)" (repo-name root) said))
      (let ((branch-name (branch root)))
        (multiple-value-bind (output error status)
            (if (upstream-p root)
                (git root (setting :push-timeout-seconds) "push")
                (git root (setting :push-timeout-seconds) "push" "origin" "HEAD"))
          (declare (ignore output))
          (cond
            ((eql 0 status)
             (values :pushed (format nil "~a: pushed ~a to ~a" (repo-name root) said
                                      (if (plusp (length branch-name)) branch-name "HEAD"))))
            ((policy-rejection-p error)
             (with-git-hook-lock
               (setf (tree-state-protected (state-of session root :create t)) branch-name))
             (if (undo-own-commit root previous (head-sha root))
                 (values :protected (policy-note root branch-name rels))
                 (values :protected-stranded (policy-note root branch-name rels))))
            (t (values :push-failed
                       (format nil "~a: committed ~a locally, push failed (~a); git-hook retries it"
                               (repo-name root) said (one-line error))))))))))

(defun ship (root rels session)
  "Stage exactly RELS, commit them, and push, with literal pathspecs so a name
with a bracket is a name and not a pattern. => (values OUTCOME NOTE)."
  (let ((previous (head-sha root))
        (paths (mapcar (lambda (rel) (format nil ":(literal)~a" rel)) rels)))
    (multiple-value-bind (output ok)
        (apply #'git-ok (append (list root 10 "add" "--") paths))
      (if (not ok)
          (values :add-failed
                  (format nil "~a: git add failed (~a); git-hook retries it"
                          (repo-name root) (one-line output)))
          (multiple-value-bind (output ok)
              (apply #'git-ok (append (list root 15 "-c" "user.useConfigOnly=true"
                                             "commit" "-m" (message-for rels) "--")
                                      paths))
            (if (not ok)
                (values :commit-skipped
                        (format nil "~a: commit skipped (no identity, or a hook refused: ~a); the files stay uncommitted"
                                (repo-name root) (one-line output)))
                (push-head root session (head-sha root) previous rels)))))))

(defun retry-push (root session)
  "Push a commit an earlier flush left local, when the branch is not closed.
=> (values OUTCOME NOTE)."
  (let ((closed (protected-branch-p session root)))
    (if (and closed (string= closed (branch root)))
        (values :protected-stranded (policy-note root closed '()))
        (push-head root session (head-sha root) (head-sha root) '()))))

(defun commit-and-push (root paths session)
  "Stage only PATHS in ROOT, commit and push. A secret-looking name is never
staged, and a path that no longer resolves is dropped, because one dead
pathspec makes `git add' stage nothing at all. => (values OUTCOME NOTE)."
  (let ((rels (remove-if #'secret-path-p
                         (sort (remove-duplicates (remove-if-not #'stringp paths)
                                                  :test #'string=)
                               #'string<))))
    (cond
      ((null rels) (values :clean nil))
      ((not *commit-on*) (values :clean nil))
      ((busy-p root)
       (values :busy (format nil "~a: a merge or a rebase is in progress; git-hook retries"
                              (repo-name root))))
      (t (let ((closed (protected-branch-p session root)))
           (if (and closed (string= closed (branch root)))
               (values :protected (policy-note root closed rels))
               (let ((rels (stageable root rels)))
                 (if (null rels)
                     (values :clean nil)
                     (ship root rels session)))))))))

;;; --- the flush -------------------------------------------------------------

(defun flush-tree (root paths session)
  "Commit and push PATHS in ROOT, or retry a commit that never reached the
remote; put back what must be retried, and answer the notes for it."
  (multiple-value-bind (outcome note)
      (if paths (commit-and-push root paths session) (retry-push root session))
    (with-git-hook-lock
      (let ((state (state-of session root :create t)))
        (when (member outcome '(:busy :locked :timeout :add-failed :commit-skipped :protected))
          (setf (tree-state-pending state) (union paths (tree-state-pending state)
                                                   :test #'string=)))
        (setf (tree-state-unpushed state) (member outcome '(:push-failed :protected-stranded)))))
    (if note (list note) '())))

(defun flush-session (session)
  "Commit and push what SESSION queued, one worktree at a time, and answer the
notes a person should read. Another session's queue in the same worktree is
left alone: it belongs to a turn that has not finished."
  (let ((work '()))
    (with-git-hook-lock
      (maphash (lambda (key state)
                 (when (and (equal (car key) (or session ""))
                            (or (tree-state-pending state) (tree-state-unpushed state)))
                   (push (list (cdr key) (tree-state-pending state)) work)
                   (setf (tree-state-pending state) '())))
               *trees*))
    (loop for (root paths) in (nreverse work)
          append (flush-tree root paths session))))

(defun flush-all ()
  "The stop thunk's last act: commit and push every session's queue."
  (let ((sessions '()))
    (with-git-hook-lock
      (maphash (lambda (key state)
                 (declare (ignore state))
                 (pushnew (car key) sessions :test #'equal))
               *trees*))
    (dolist (session sessions)
      (ignore-errors (say-notes (flush-session (if (string= session "") nil session)))))))

(defun say-notes (notes)
  "Put NOTES on the board under this cell's key, so a person reads them and the
model reads them in the notices block of its next request."
  (when notes
    (nle:notice (format nil "git-hook:~%~{- ~a~%~}" notes)
                :level :warning :key "nodecode-git-hook")))

;;; --- the two points --------------------------------------------------------

(defun current-session ()
  "The session the running turn belongs to, or NIL outside one."
  (getf (ignore-errors (nle:turn)) :session-id))

(defun watch-call (op next)
  "The :TOOL point: before the call runs, each worktree it touches is
snapshotted, and pulled once when the call is a read; after it runs, the paths
whose signature moved are queued for the turn's commit. Fail-open end to end: a
sync failure never fails the call that triggered it."
  (let* ((session (current-session))
         (roots (ignore-errors (call-roots op session)))
         (pull (and roots (not (write-call-p op)))))
    (when roots (ignore-errors (settle-before roots session pull)))
    (prog1 (funcall next op)
      (when roots (ignore-errors (record-delta roots session))))))

(defun flush-at-round (op next)
  "The :ROUND point, a turn's end: commit and push what the turn changed, say
what could not land, then let the round be built."
  (ignore-errors (say-notes (flush-session (getf op :session-id))))
  (funcall next op))

;;; --- /git-hook -------------------------------------------------------------

(defun status-lines ()
  "One line per worktree this session knows about, or NIL."
  (let ((lines '()))
    (with-git-hook-lock
      (maphash (lambda (key state)
                 (push (format nil "  ~a: ~d waiting~a~a"
                               (repo-name (cdr key))
                               (length (tree-state-pending state))
                               (if (tree-state-protected state)
                                   (format nil " (origin rejects direct pushes to ~a)"
                                           (tree-state-protected state))
                                   "")
                               (if (tree-state-unpushed state) " (a commit stays local)" ""))
                       lines))
               *trees*))
    (nreverse lines)))

(defun status-text ()
  "What the cell is doing: its switches, its allowlist, and this session's queues."
  (let ((lines (status-lines)))
    (format nil "git-hook: commits ~:[off~;on~], pushes ~:[off~;on~].~%  roots: ~{~a~^, ~}~%~@[~{~a~^~%~}~]"
            *commit-on* *push-on* (allowed-roots)
            (if lines lines (list "  no worktree seen yet: it acts on the first call that names a path under roots")))))

(defun run-command (arguments session-id)
  "The /git-hook command: status, flush, on, off, push on, push off."
  (running-settings)
  (let ((words (remove "" (mapcar #'nlk:trimmed (uiop:split-string (or arguments "") :separator '(#\Space)))
                       :test #'string=)))
    (let ((action (string-downcase (or (first words) "status"))))
      (cond
        ((member action '("status" "") :test #'string=) (values (status-text) nil))
        ((member action '("flush" "now") :test #'string=)
         (let ((notes (flush-session session-id)))
           (values (if notes (format nil "git-hook:~%~{- ~a~%~}" notes)
                       "git-hook: nothing was waiting to be committed.")
                   nil)))
        ((member action '("on" "off") :test #'string=)
         (setf *commit-on* (string= action "on"))
         (ignore-errors (nle:config-set '("git-hook" "commit") *commit-on*))
         (values (format nil "git-hook: commits ~:[off~;on~] from the next turn on (saved to the config)."
                         *commit-on*)
                 nil))
        ((and (string= action "push") (second words))
         (let ((on (string= (string-downcase (second words)) "on")))
           (setf *push-on* on)
           (ignore-errors (nle:config-set '("git-hook" "push") on))
           (values (format nil "git-hook: pushes ~:[off~;on~] (saved to the config)." on) nil)))
        (t (values "git-hook: /git-hook [status|flush|on|off|push on|push off]" nil))))))

;;; --- what the section has to say -------------------------------------------

(defun check-config ()
  "Refuse a section that names no worktree to sync. The allowlist is the whole
safety story -- a worktree outside every entry is never pulled, committed or
pushed -- so an empty one is a cell that could touch nothing anyway."
  (unless (allowed-roots)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "git-hook: set \"roots\" to the directories whose worktrees may be "
                                 "pulled, committed and pushed; a repository outside every entry is "
                                 "never touched"))))

;;; --- the primer ------------------------------------------------------------

(defparameter +primer+
  "git-hook keeps the git worktrees under its `roots' honest as a turn runs:
  before a call   at most one `git pull --ff-only' per worktree per session, and
      only on a clean worktree with an upstream, so you read current code.
  after a call    the paths whose status, size or write date moved are queued;
      an edit to a file that was already dirty counts, dirt nobody touched does
      not.
  at the turn's end   stage exactly those paths, commit, and push.
A secret-looking name (.env*, auth.json, credentials*, id_rsa* and friends, *.pem,
*.key, and the section's secret_names) is never staged. A branch whose direct
push the remote refuses because a pull request is required is left dirty, said
once, and not committed to again this session. /git-hook flush commits now;
/git-hook on|off turns commits off and on, `push off' keeps them local."
  "What (help :git-hook) answers while the cell runs.")

(nle:define-cell git-hook
  (:section ("git-hook")
    (:guide "roots is the allowlist: a worktree outside every entry is never touched; pull, commit and push are each on by default and each can be turned off; nothing is needed beside the git you already have")
    ("roots" :list :required t
     :doc "absolute directories whose worktrees are pulled, committed and pushed; a repository outside every one of them is never touched")
    ("pull" :boolean :default t
     :doc "pull --ff-only before a read, once per worktree per session, only on a clean worktree with an upstream")
    ("commit" :boolean :default t
     :doc "commit the files a turn changed, and only those")
    ("push" :boolean :default t
     :doc "push what was committed to the branch's own upstream")
    ("message" :string :default ""
     :doc "the commit subject; empty writes `update <file>` or `update N files`")
    ("secret_names" :list
     :doc "extra file-name prefixes never staged, beside .env*, auth.json, credentials*, id_rsa*, *.pem and *.key")
    ("pull_timeout_seconds" :integer :default 12 :min 1
     :doc "one pull's ceiling")
    ("push_timeout_seconds" :integer :default 20 :min 1
     :doc "one push's ceiling")
    ("budget_seconds" :integer :default 20 :min 1
     :doc "the whole before-a-call phase's ceiling across every worktree it touches"))
  (:help :git-hook
         "git-hook: pull clean worktrees before reads, commit and push only what each turn changed"
         +primer+)
  (:command "git-hook" 'run-command
            :description "git-hook: status, flush now, on, off, push on|off"
            :argument-hint "[status|flush|on|off|push on|off]")
  (:hook :tool #'watch-call)
  (:hook :round #'flush-at-round)
  (:start (lambda ()
            (check-config)
            (setf *commit-on* (and (setting :commit) t)
                  *push-on* (and (setting :push) t))
            (with-git-hook-lock
              (clrhash *trees*)
              (clrhash *noted*))
            (nle:on-stop #'flush-all))))
