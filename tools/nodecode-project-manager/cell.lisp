;;;; cell.lisp --- the repositories under your folders, and what git says about them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A watch path is a directory the operator names in the `project-manager'
;;;; section; every directory below it, `depth' levels deep, that holds a
;;;; .git (a directory, or the file a worktree has) is a project. Nothing is
;;;; stored: each answer is one walk of the filesystem and a few read-only
;;;; git reads under each project, short-lived in memory (cache_seconds).
;;;;
;;;; Five tools and five verbs answer the same scans, and /projects shows
;;;; them. Nothing is written, moved or committed: git is run as
;;;; `git -C <project> ...' for rev-parse, status, log, for-each-ref,
;;;; rev-list and config, and its answer is what the report says.

(in-package #:nodecode-project-manager)

;;; --- the manual ---------------------------------------------------------------
;;; (help :projects) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter +primer+
  "The operator's git projects are readable through the nodecode-project-manager cell:
five tools (list_projects, get_project, list_branches, get_recent_commits,
list_watch_paths) and the same scans as Lisp functions in the projects: package,
called through eval. Each answers a string; a refusal is
ERROR: PROJECT-MANAGER-ERROR with the reason.
The projects are the git repositories under the directories the section's watch_paths
names, `depth' levels below each, so a project is known by its folder name.
  (projects:projects [\"search\"])   every project with its branch, last commit and
      whether it has uncommitted changes; SEARCH keeps those whose name or path
      contains it.
  (projects:project \"api\")          one project: its path, branch, distance from its
      upstream, last commit, changed files and remotes.
  (projects:branches \"api\")         its local branches, newest commit first, the
      checked-out one marked.
  (projects:commits \"api\" :limit 20 :branch \"main\")   the newest commits.
  (projects:watch-paths)            the directories it looks in, and how many projects
      each holds.
If a project is missing, say which watch paths were read rather than guessing a path.
Nothing here writes: to commit, branch or push, run git as the operator would."
  "What (help :projects) answers while the cell runs.")

;;; --- the boundary ---------------------------------------------------------------

(nlk:define-contract project-list
  (:doc "Every git repository under the configured watch paths, with its current branch, its last commit and whether it has uncommitted changes.")
  ("query" :string :default ""
   :doc "keep only repositories whose name or path contains this, case-insensitive"))

(nlk:define-contract project-detail
  (:doc "One repository in full: its path, current branch, distance from its upstream, last commit, changed files and remotes.")
  ("name" :text :required t
   :doc "the repository's folder name, as list_projects shows it"))

(nlk:define-contract branch-list
  (:doc "The local branches of one repository, newest commit first, each with the tip commit it points at.")
  ("name" :text :required t
   :doc "the repository's folder name"))

(nlk:define-contract commit-list
  (:doc "The most recent commits of one repository, newest first, on the checked-out branch or one named.")
  ("name" :text :required t
   :doc "the repository's folder name")
  ("limit" :integer :default 0
   :doc "how many commits to list; 0 takes the section's commits")
  ("branch" :string :default ""
   :doc "a branch to read instead of the checked-out one"))

(nlk:define-contract watch-path-list
  (:doc "The directories this cell looks for git repositories in, and how many repositories it found under each.")
  ("root" :string :default ""
   :doc "keep only the watch paths whose directory contains this"))

;;; --- paths ---------------------------------------------------------------------

(defun root-path (text)
  "TEXT as a directory pathname, with ~ standing for the home directory."
  (let* ((text (nlk:trimmed (princ-to-string text)))
         (path (cond ((member text '("~" "~/") :test #'string=) (user-homedir-pathname))
                     ((and (> (length text) 1)
                           (char= #\~ (char text 0))
                           (member (char text 1) '(#\/ #\\)))
                      (merge-pathnames (subseq text 2) (user-homedir-pathname)))
                     (t (uiop:parse-native-namestring text)))))
    (uiop:ensure-directory-pathname path)))

(defun path-text (path)
  "PATH as a person reads it: native, without the trailing separator."
  (string-right-trim "/\\" (uiop:native-namestring path)))

(defun watch-roots ()
  "The watch paths the section names, as directory pathnames, in order."
  (let ((value (setting :watch-paths)))
    (loop for text in (cond ((null value) '())
                            ((listp value) value)
                            (t (list value)))
          for trimmed = (and (stringp text) (nlk:trimmed text))
          when (and trimmed (plusp (length trimmed)))
            collect (root-path trimmed))))

(defun project-name (directory)
  "DIRECTORY's own name: the project a person refers to it by."
  (car (last (pathname-directory directory))))

(defun skipped-folder-p (path)
  "Whether the walk never enters PATH: a hidden folder, or one of the trees
that hold other projects' copies rather than a project."
  (let* ((name (car (last (pathname-directory path))))
         (name (and name (string name))))
    (or (null name)
        (zerop (length name))
        (char= #\. (char name 0))
        (member name '("node_modules" "vendor" "target" "__pycache__") :test #'string-equal))))

(defun repository-p (directory)
  "Whether DIRECTORY is a git repository: a .git folder, or the file a
worktree and a submodule keep instead."
  (and (probe-file (merge-pathnames ".git" directory)) t))

(defun find-repositories (roots depth)
  "Every repository under ROOTS, DEPTH levels below each, sorted by path."
  (let ((found '())
        (queue (mapcar (lambda (root) (cons root 0)) roots)))
    (loop while queue
          do (destructuring-bind (directory . level) (pop queue)
               (when (uiop:directory-exists-p directory)
                 (cond ((repository-p directory)
                        (pushnew (or (ignore-errors (truename directory)) directory)
                                 found :test #'equal))
                       ((< level depth)
                        (dolist (sub (ignore-errors (uiop:subdirectories directory)))
                          (unless (skipped-folder-p sub)
                            (push (cons sub (1+ level)) queue))))))))
    (sort found #'string-lessp :key #'path-text)))

;;; --- git, the one program it runs ------------------------------------------------

(defun run-command (arguments directory)
  "ARGUMENTS run in DIRECTORY => (VALUES OUTPUT EXIT-CODE). A git that is not
there answers 127 rather than signalling: its text is the advice."
  (handler-case
      (uiop:run-program arguments :output :string :error-output :output
                                  :ignore-error-status t :directory directory)
    (error (condition) (values (format nil "~a" condition) 127))))

(defvar *git* #'run-command
  "The one seam that runs git: (ARGUMENTS DIRECTORY) => (VALUES OUTPUT
EXIT-CODE). Tests replace it; nothing else does.")

(defun git (directory &rest arguments)
  "GIT ARGUMENTS in DIRECTORY => (VALUES OUTPUT EXIT-CODE)."
  (funcall *git* (list* "git" "-C" (path-text directory) arguments) nil))

(defun git-output (directory &rest arguments)
  "What git ARGUMENTS answer in DIRECTORY, trimmed, or NIL when git failed or
said nothing."
  (multiple-value-bind (output code) (apply #'git directory arguments)
    (when (and (integerp code) (zerop code) output)
      (let ((text (nlk:trimmed output)))
        (and (plusp (length text)) text)))))

(defun split-tabs (line)
  "LINE's tab-separated fields."
  (let ((fields '())
        (start 0))
    (loop for position = (position #\Tab line :start start)
          do (push (subseq line start position) fields)
             (if position (setf start (1+ position)) (return)))
    (nreverse fields)))

(defun git-fields (directory &rest arguments)
  "The one line git ARGUMENTS answer in DIRECTORY, as its tab-separated
fields, or NIL when git failed or said nothing."
  (let ((output (apply #'git-output directory arguments)))
    (and output (plusp (length output)) (split-tabs (first (nlk:lines output))))))

(defun git-lines (directory &rest arguments)
  "The non-empty lines git ARGUMENTS answer in DIRECTORY, or NIL."
  (let ((output (apply #'git-output directory arguments)))
    (when output
      (remove-if (lambda (line) (zerop (length line)))
                 (coerce (nlk:lines output) 'list)))))

;;; --- what one project is ---------------------------------------------------------

(defun current-branch (directory)
  "DIRECTORY's checked-out branch, DETACHED when HEAD is a commit, or NIL."
  (let ((name (git-output directory "rev-parse" "--abbrev-ref" "HEAD")))
    (cond ((null name) nil)
          ((string= name "HEAD") "DETACHED")
          (t name))))

(defun last-commit (directory)
  "DIRECTORY's newest commit as (HASH WHEN AUTHOR SUBJECT), or NIL."
  (let ((fields (git-fields directory "log" "-1" "--format=%h%x09%aI%x09%an%x09%s")))
    (and (= 4 (length fields)) fields)))

(defun upstream-distance (directory)
  "How far DIRECTORY is from its upstream as (BEHIND AHEAD), or NIL when the
branch tracks nothing."
  (let ((fields (git-fields directory "rev-list" "--left-right" "--count" "@{upstream}...HEAD")))
    (when (and (= 2 (length fields))
               (every (lambda (field) (every #'digit-char-p field)) fields))
      (list (parse-integer (first fields)) (parse-integer (second fields))))))

(defun changed-files (directory)
  "The lines of git status for DIRECTORY, or NIL."
  (git-lines directory "status" "--porcelain"))

(defun project-entry (directory)
  "DIRECTORY read as one project: a plist of what an answer says about it."
  (let* ((changed (changed-files directory))
         (distance (upstream-distance directory)))
    (list :name (project-name directory)
          :path (path-text directory)
          :branch (current-branch directory)
          :commit (last-commit directory)
          :changed (length changed)
          :changed-files changed
          :behind (first distance)
          :ahead (second distance))))

;;; --- the scan, and its short memory ---------------------------------------------

(defvar *scan* nil
  "The last scan: (KEY STAMP ENTRIES), so a turn that asks twice walks the
disks once. Cleared at start and never written anywhere.")

(defun scan-key ()
  (list (mapcar #'path-text (watch-roots)) (setting :depth) (setting :max-projects)
        (setting :changed-files) (setting :cache-seconds)))

(defun scan-projects ()
  "Every project under the watch paths, read with git, at most max_projects of
them: the last scan for cache_seconds, else a fresh walk."
  (let* ((key (scan-key))
         (now (get-universal-time))
         (ttl (setting :cache-seconds))
         (cached (and *scan* (equal key (first *scan*)) (plusp ttl)
                      (< (- now (second *scan*)) ttl))))
    (if cached
        (third *scan*)
        (let* ((repositories (find-repositories (watch-roots) (setting :depth)))
               (kept (subseq repositories 0 (min (length repositories) (setting :max-projects))))
               (entries (mapcar #'project-entry kept)))
          (setf *scan* (list key now entries))
          entries))))

(defun project-named (name)
  "The project NAME names: its folder name, or a path that ends in it."
  (let ((name (nlk:trimmed (princ-to-string name))))
    (or (find name (scan-projects) :key (lambda (entry) (getf entry :name)) :test #'string-equal)
        (find name (scan-projects)
              :test (lambda (wanted path)
                      (let ((path (string-downcase path))
                            (wanted (string-downcase wanted)))
                        (and (<= (length wanted) (length path))
                             (string= wanted path :start2 (- (length path) (length wanted))))))
              :key (lambda (entry) (getf entry :path))))))

;;; --- the words ------------------------------------------------------------------

(defun human-search-p (search)
  (and (stringp search) (plusp (length (nlk:trimmed search)))))

(defun matching-projects (search)
  "The projects whose name or path contains SEARCH, or all of them."
  (let ((entries (scan-projects)))
    (if (human-search-p search)
        (let ((needle (nlk:trimmed search)))
          (remove-if-not (lambda (entry)
                           (or (search needle (getf entry :name) :test #'char-equal)
                               (search needle (getf entry :path) :test #'char-equal)))
                         entries))
        entries)))

(defun when-text (iso)
  "An ISO-8601 timestamp as a report writes it: its date and minute."
  (let ((iso (and iso (nlk:trimmed iso))))
    (if (and iso (>= (length iso) 16))
        (substitute #\Space #\T (subseq iso 0 16))
        (or iso ""))))

(defun commit-text (commit)
  "One commit as a report writes it: HASH WHEN SUBJECT, or nothing."
  (if (null commit)
      "no commits"
      (format nil "~a ~a  ~a" (first commit) (when-text (second commit)) (fourth commit))))

(defun project-line (entry)
  "One project as a list writes it: a line naming the branch, the last commit
and whatever is uncommitted."
  (format nil "  ~a  ~a  ~a~@[  (~d changed)~]"
          (getf entry :name)
          (or (getf entry :branch) "no branch")
          (commit-text (getf entry :commit))
          (let ((changed (getf entry :changed))) (and (plusp changed) changed))))

(defun projects-text (search)
  "The answer list_projects and /projects share: one line a project."
  (let ((roots (watch-roots))
        (entries (matching-projects search)))
    (if (null roots)
        "No watch paths are set: project-manager's watch_paths names the directories your repositories are in."
        (if (null entries)
            (format nil "No git project~@[ matching ~s~] under ~{~a~^, ~}.~%"
                    (and (human-search-p search) (nlk:trimmed search))
                    (mapcar #'path-text roots))
            (with-output-to-string (out)
              (format out "~d project~:p under ~{~a~^, ~}~@[ matching ~s~]:~%"
                      (length entries) (mapcar #'path-text roots)
                      (and (human-search-p search) (nlk:trimmed search)))
              (format out "  ~a~%" (string-downcase "NAME  BRANCH  LAST COMMIT"))
              (dolist (entry entries) (format out "~a~%" (project-line entry))))))))

(defun project-text (name)
  "The answer project and get_project share: everything one project is."
  (let ((entry (project-named name)))
    (unless entry
      (fail "no project named ~s under ~{~a~^, ~}" name (mapcar #'path-text (watch-roots))))
    (with-output-to-string (out)
      (format out "~a  ~a~%" (getf entry :name) (getf entry :path))
      (format out "  branch: ~a" (or (getf entry :branch) "none"))
      (when (or (getf entry :ahead) (getf entry :behind))
        (format out " (~d ahead, ~d behind its upstream)" (getf entry :ahead) (getf entry :behind)))
      (format out "~%")
      (let ((commit (getf entry :commit)))
        (if commit
            (format out "  last commit: ~a ~a by ~a~%    ~a~%"
                    (first commit) (when-text (second commit)) (third commit) (fourth commit))
            (format out "  last commit: none yet~%")))
      (let ((changed (or (getf entry :changed-files) '())))
        (format out "  ~d changed file~:p~@[:~]~%" (length changed) (and changed t))
        (dolist (line (subseq changed 0 (min (length changed) (setting :changed-files))))
          (format out "    ~a~%" line))
        (when (> (length changed) (setting :changed-files))
          (format out "    ... ~d more~%" (- (length changed) (setting :changed-files)))))
      (let ((remote (git-output (getf entry :path) "config" "--get" "remote.origin.url")))
        (when (and (stringp remote) (plusp (length remote)))
          (format out "  origin: ~a~%" remote))))))

;;; --- the verbs, which every tool answers with --------------------------------------

(defun projects (&optional search)
  "Every git project under the watch paths, optionally only those SEARCH names. => a string."
  (projects-text search))

(defun project (name)
  "One project by NAME as list_projects names it. => a string, or a refusal."
  (project-text name))

(defun branches (name)
  "NAME's local branches, newest commit first, the checked-out one marked. => a string."
  (let ((entry (or (project-named name)
                    (fail "no project named ~s under ~{~a~^, ~}" name
                          (mapcar #'path-text (watch-roots))))))
    (let ((current (getf entry :branch))
          (refs (git-lines (getf entry :path) "for-each-ref" "--sort=-committerdate"
                            "--format=%(refname:short)%09%(objectname:short)%09%(committerdate:iso-strict)%09%(subject)"
                            "refs/heads")))
      (if (null refs)
          (format nil "~a has no branches yet." (getf entry :name))
          (with-output-to-string (out)
            (format out "~a: ~d branches (checked out: ~a):~%"
                    (getf entry :name) (length refs) (or current "none"))
            (dolist (line (subseq refs 0 (min (length refs) (setting :branches))))
              (let ((fields (split-tabs line)))
                (when (= 4 (length fields))
                  (format out "  ~a ~a ~a  ~a ~a~%"
                          (if (string= (first fields) (or current "")) "*" " ")
                          (first fields) (second fields) (when-text (third fields)) (fourth fields))))))))))

(defun commits (name &key limit branch)
  "NAME's newest LIMIT commits, on BRANCH when one is named. => a string."
  (let* ((entry (or (project-named name)
                    (fail "no project named ~s under ~{~a~^, ~}" name
                          (mapcar #'path-text (watch-roots)))))
         (count (max 1 (min (or (and (integerp limit) (plusp limit) limit) (setting :commits)) 500)))
         (branch (and (stringp branch) (plusp (length (nlk:trimmed branch))) (nlk:trimmed branch)))
         (lines (apply #'git-lines (getf entry :path) "log" (format nil "-~d" count)
                       "--format=%h%x09%aI%x09%an%x09%s"
                       (and branch (list branch)))))
    (if (null lines)
        (format nil "~a has no commits~@[ on ~a~]." (getf entry :name) branch)
        (with-output-to-string (out)
          (format out "~a: ~d commit~:p~@[ on ~a~] (newest first):~%"
                  (getf entry :name) (length lines) branch)
          (dolist (line lines)
            (let ((fields (split-tabs line)))
              (when (= 4 (length fields))
                (format out "  ~a  ~a  ~a  ~a~%" (first fields) (when-text (second fields))
                        (third fields) (fourth fields)))))))))

(defun watch-paths (&optional root)
  "The watch paths and how many projects each holds; ROOT keeps those whose
path contains it. => a string."
  (let* ((named (and (stringp root) (nlk:trimmed root)))
         (named (and named (plusp (length named)) named))
         (roots (watch-roots))
         (roots (if named
                    (remove-if-not (lambda (one) (search named (path-text one) :test #'char-equal))
                                   roots)
                    roots))
         (entries (scan-projects)))
    (if (null roots)
        "No watch paths are set: project-manager's watch_paths names the directories your repositories are in."
        (with-output-to-string (out)
          (format out "~d watch path~:p:~%" (length roots))
          (dolist (root roots)
            (let* ((prefix (path-text root))
                   (mine (remove-if-not (lambda (entry)
                                          (let ((path (getf entry :path)))
                                            (and (<= (length prefix) (length path))
                                                 (string= prefix path :end2 (length prefix)))))
                                        entries)))
              (format out "  ~a  ~d project~:p~@[, not found~]~%"
                      prefix (length mine) (not (uiop:directory-exists-p root)))))))))

;;; --- the tools ------------------------------------------------------------------

(defun answering (function)
  "FUNCTION's answer as a tool answers: its refusal is text the model repairs
from, not a signal out of the call."
  (handler-case (funcall function)
    (project-manager-error (condition) (format nil "ERROR: ~a" condition))))

(defun tool-list-projects (query)
  (answering (lambda () (projects (project-list-query query)))))

(defun tool-get-project (query)
  (answering (lambda () (project (project-detail-name query)))))

(defun tool-list-branches (query)
  (answering (lambda () (branches (branch-list-name query)))))

(defun tool-get-recent-commits (query)
  (answering (lambda () (commits (commit-list-name query)
                                 :limit (commit-list-limit query)
                                 :branch (commit-list-branch query)))))

(defun tool-list-watch-paths (query)
  (answering (lambda () (watch-paths (watch-path-list-root query)))))

;;; --- /projects -------------------------------------------------------------------

(defun run-slash (args session-id)
  "/projects typed alone lists every project; /projects SEARCH keeps those
whose name or path contains it."
  (declare (ignore session-id))
  (let ((search (nlk:trimmed (or args ""))))
    (handler-case
        (let* ((entries (matching-projects search))
               (text (projects-text search)))
          (values text
                  (when entries
                    (nle:list-dialog
                     :title "Projects" :searchable t
                     :search-placeholder "filter by name or path"
                     :summary (format nil "~d project~:p under ~{~a~^, ~}"
                                       (length entries) (mapcar #'path-text (watch-roots)))
                     :rows (loop for entry in entries
                                 collect (list :label (getf entry :name)
                                               :detail (format nil "~a · ~a"
                                                               (or (getf entry :branch) "no branch")
                                                               (commit-text (getf entry :commit)))
                                               :suffix (getf entry :path)
                                               :value (getf entry :path)))))))
      (project-manager-error (condition) (format nil "ERROR: ~a" condition)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell project-manager
  (:section ("project-manager")
    (:guide "no key, nothing to install: it reads the git repositories under the directories you name, locally, and changes nothing. watch_paths are those directories; depth bounds the walk below each; next step: /projects")
    ("watch_paths" :list :required t
     :doc "directories your git repositories live in, ~ for your home directory; each is walked depth levels down")
    ("depth" :integer :default 2 :min 0
     :doc "how many levels below a watch path a repository may sit; 0 reads only the watch path itself")
    ("max_projects" :integer :default 50 :min 1
     :doc "the most repositories one scan reads, so a wide watch path stays quick")
    ("commits" :integer :default 10 :min 1
     :doc "how many commits get_recent_commits lists when the caller does not say")
    ("branches" :integer :default 25 :min 1
     :doc "the most branches one answer lists, newest commit first")
    ("changed_files" :integer :default 20 :min 1
     :doc "the most changed files a project's detail lists")
    ("cache_seconds" :integer :default 15 :min 0
     :doc "how long one scan answers from memory; 0 walks the disks on every call"))
  (:help :projects "projects: the git repositories under your watch paths - (projects:projects), (projects:branches \"api\"), /projects" +primer+)
  (:command "projects" 'run-slash
            :description "The git projects under your watch paths: branch, last commit, what changed"
            :argument-hint "SEARCH")
  (:tool "list_projects" (nlk:contract 'project-list) 'tool-list-projects)
  (:tool "get_project" (nlk:contract 'project-detail) 'tool-get-project)
  (:tool "list_branches" (nlk:contract 'branch-list) 'tool-list-branches)
  (:tool "get_recent_commits" (nlk:contract 'commit-list) 'tool-get-recent-commits)
  (:tool "list_watch_paths" (nlk:contract 'watch-path-list) 'tool-list-watch-paths)
  (:start (lambda () (setf *scan* nil))))
