;;;; cell.lisp --- a version of a file before a tool changes it.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One around-hook on the :TOOL point. Before a call runs, it works out
;;;; which files the call is about to change -- the paths a (write-file ...),
;;;; (edit ...) or (delete-file ...) form names, or whether a shell command
;;;; the form runs mutates anything at all -- and keeps what they hold now:
;;;;
;;;;   inside a git work tree   a snapshot commit of the whole work tree, on
;;;;                            refs/nodecode-autosave/<branch> with a reflog,
;;;;                            so git stores only what changed since the last
;;;;                            one; nothing else in the repository moves
;;;;   anywhere else            a copy of that one file under the cell's data
;;;;                            directory, `keep' versions deep
;;;;
;;;; It never blocks, rewrites or approves a call: whatever goes wrong, the
;;;; tool still runs, and an error lands in the ledger rather than in a turn.
;;;; It never touches HEAD, a branch, the real index or the working tree, and
;;;; never pushes -- a shadow ref is local recovery history, not a commit of
;;;; the project. /autosave and the exported functions list what is held and
;;;; put a version back.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "autosave": {"keep": 20, "max_file_mb": 10, "storage_dir": "",
;;;;                "skip": [], "git": true}

(defpackage #:nodecode-autosave
  (:use #:cl)
  (:export #:versions #:restore #:paths #:prune #:status))

(in-package #:nodecode-autosave)

(nlk:define-peripheral autosave :not-running t)

;;; --- what the model is told, and the words it reads --------------------------

(defparameter +manual+
  "The nodecode-autosave cell keeps a version of a file before a tool changes it: a
snapshot commit on refs/nodecode-autosave/<branch> when the file is in a git work
tree (git stores only what changed, and HEAD, branches, the index and the work tree
never move), or a copy of the file itself outside git. It runs before the call, never
blocks it, and is not a backup service: a skipped or oversized file has no version.
  (autosave:versions \"PATH\")   what is held for PATH, newest first: ((\"2026-01-02 03:04\" 1) ...)
  (autosave:restore \"PATH\" &optional N)  put version N (default 1, the newest) back on disk
  (autosave:paths)               every path and repository with versions, and how many
  (autosave:prune)               drop versions beyond the `keep' setting
  (autosave:status)              what is held, where, and what the settings are
/autosave answers the same from the operator's side: /autosave find PATH,
/autosave restore PATH 2, /autosave list, /autosave prune. Settings live under
\"autosave\" in the config; keep is how many versions a file holds."
  "What (help :autosave) answers while the cell runs.")

(defparameter +writers+
  '("write-file" "edit" "append-file" "delete-file" "rename-file" "copy-file" "truncate-file")
  "The heads whose first argument is a path a call changes.")

(defparameter +two-path-writers+ '("rename-file" "copy-file")
  "The heads whose second path is changed too.")

(defparameter +spawners+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose argument is a command the call runs.")

(defparameter +mutators+
  '("rm" "rmdir" "mv" "cp" "truncate" "dd" "shred" "patch" "unzip" "tar" "install"
    "chmod" "touch" "tee")
  "A shell command naming one of these changes a file.")

(defparameter +git-mutators+
  '("checkout" "restore" "reset" "clean" "stash" "rebase" "switch" "rm" "mv" "apply"
    "am" "merge" "pull" "cherry-pick" "revert")
  "A git subcommand naming one of these changes the work tree.")

(defparameter +credential-names+
  '(".env" "auth.json" ".netrc" ".git-credentials" ".npmrc" ".pypirc")
  "Name-only: a credentials file is skipped, never opened to decide whether it is one.")

(defparameter +credential-prefixes+ '(".env." "id_rsa" "id_ed25519" "id_ecdsa")
  "A file whose name starts with one of these is a credential store.")

(defparameter +dependency-names+
  '("node_modules" ".venv" "venv" "site-packages" "__pycache__" ".git" ".tox" ".mypy_cache"
    ".pytest_cache" ".cache" ".next" "target" ".gradle")
  "Path components never copied.")

;;; --- paths -------------------------------------------------------------------

(defun absolute-path (thing)
  "THING, a path string or pathname, as an absolute pathname."
  (let ((pathname (if (pathnamep thing)
                       thing
                       (uiop:parse-native-namestring (string thing)))))
    (if (uiop:absolute-pathname-p pathname)
        pathname
        (uiop:merge-pathnames* pathname (uiop:ensure-directory-pathname (uiop:getcwd))))))

(defun existing-dir (pathname)
  "The nearest directory at or above PATHNAME that exists."
  (let ((dir (if (uiop:directory-pathname-p pathname)
                 pathname
                 (uiop:pathname-directory-pathname pathname))))
    (loop while (and dir
                     (not (uiop:directory-exists-p dir))
                     (not (equal dir (uiop:pathname-parent-directory-pathname dir))))
          do (setf dir (uiop:pathname-parent-directory-pathname dir)))
    (and dir (uiop:directory-exists-p dir) dir)))

(defun path-components (path)
  "PATH's directory names, lowercased, without the root."
  (mapcar #'string-downcase (cdr (pathname-directory (absolute-path path)))))

(defun credential-p (path)
  "Whether PATH is a credentials store, by name alone."
  (let* ((name (string-downcase (or (pathname-name (pathname (absolute-path path))) "")))
         (parts (path-components path)))
    (or (member name +credential-names+ :test #'string=)
        (some (lambda (prefix) (uiop:string-prefix-p prefix name)) +credential-prefixes+)
        (member ".ssh" parts :test #'string=)
        (member ".aws" parts :test #'string=)
        (member ".docker" parts :test #'string=)
        (member ".kube" parts :test #'string=))))

(defun skipped-p (path)
  "Whether PATH is never saved at all."
  (let ((parts (path-components path)))
    (or (credential-p path)
        (some (lambda (name) (member name parts :test #'string=)) +dependency-names+)
        (some (lambda (name) (member name (coerce (or (setting :skip) '()) 'list) :test #'string=)) parts))))

(defun regular-file-p (path)
  "Whether PATH is a file this cell can copy: one that exists and is not a directory."
  (and (uiop:file-exists-p path) (not (uiop:directory-exists-p path))))

(defun file-size (path)
  (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (and in (file-length in))))

;;; --- the data directory ---------------------------------------------------------

(defun storage-root ()
  "Where this cell keeps its copies, its ledger and its repository list."
  (uiop:ensure-directory-pathname
   (let ((dir (setting :storage-dir)))
     (if (and dir (plusp (length dir)))
         (absolute-path dir)
         (nlk:home "autosave/")))))

(defun data-file (name)
  (merge-pathnames name (storage-root)))

(defun note-error (condition)
  "One line in the ledger; the ledger is never allowed to break a call."
  (ignore-errors
    (ensure-directories-exist (data-file "ledger.txt"))
    (with-open-file (out (data-file "ledger.txt") :direction :output :if-exists :append
                                                 :if-does-not-exist :create)
      (format out "~a ~a~%" (universal-time-string (get-universal-time))
              (substitute #\Space #\Newline (princ-to-string condition))))
    (when (> (file-size (data-file "ledger.txt")) 65536)
      (uiop:delete-file-if-exists (data-file "ledger.txt")))))

(defun universal-time-string (universal)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

;;; --- git ----------------------------------------------------------------------

(defun run-git (repo args &key environment output)
  "ARGS run in REPO. => (values TEXT EXIT-CODE)."
  (multiple-value-bind (text error code)
      (uiop:run-program (list* "git" "-C" (uiop:native-namestring (uiop:ensure-directory-pathname repo)) args)
                         :output (or output :string)
                         :error-output :string
                         :environment environment
                         :ignore-error-status t)
    (declare (ignore error))
    (values (if (stringp text) text "") (or code 0))))

(defvar *git-run* #'run-git
  "The one way this cell runs git. A test binds it to a fake, so no test needs a repository.")

(defun git-text (repo args &key environment)
  "=> (values TRIMMED-TEXT EXIT-CODE)."
  (multiple-value-bind (text code) (funcall *git-run* repo args :environment environment)
    (values (string-trim '(#\Space #\Tab #\Newline #\Return) text) code)))

(defun git-line (repo &rest args)
  "ARGS' answer when git succeeds and says something, else NIL."
  (multiple-value-bind (text code) (git-text repo args)
    (and (zerop code) (plusp (length text)) text)))

(defun repo-of (path)
  "The git work tree PATH sits in, as a directory pathname, or NIL."
  (let ((dir (existing-dir (absolute-path path))))
    (and dir
         (let ((line (git-line dir "rev-parse" "--show-toplevel")))
           (and line (uiop:ensure-directory-pathname line))))))

(defun relative-name (repo path)
  "PATH's name inside REPO, as git writes it."
  (let ((repo-name (uiop:native-namestring (uiop:ensure-directory-pathname repo)))
        (name (uiop:native-namestring (absolute-path path))))
    (when (uiop:string-prefix-p repo-name name)
      (subseq name (length repo-name)))))

(defun branch-name (repo)
  (or (git-line repo "symbolic-ref" "--short" "HEAD") "detached"))

(defun shadow-ref (repo)
  (format nil "refs/nodecode-autosave/~a" (branch-name repo)))

(defun posix-environment ()
  #+sbcl (sb-ext:posix-environ)
  #-sbcl nil)

(defun git-environment (index)
  (cons (format nil "GIT_INDEX_FILE=~a" index)
        (remove-if (lambda (entry) (uiop:string-prefix-p "GIT_INDEX_FILE=" entry))
                   (posix-environment))))

(defun temp-index ()
  (uiop:native-namestring
   (merge-pathnames (format nil "nodecode-autosave-index-~d-~d"
                            (get-universal-time) (random 1000000))
                    (uiop:temporary-directory))))

(defun remember-repo (repo)
  (ignore-errors
    (let* ((file (data-file "repos.txt"))
           (known (and (uiop:file-exists-p file)
                       (uiop:read-file-lines file))))
      (unless (member (uiop:native-namestring repo) known :test #'string=)
        (ensure-directories-exist file)
        (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create)
          (write-line (uiop:native-namestring repo) out))))))

(defun snapshot-repo (repo)
  "One snapshot commit of REPO's work tree on its shadow ref.
=> the commit id, or NIL when nothing changed or git refused."
  (let* ((repo (uiop:ensure-directory-pathname repo))
         (ref (shadow-ref repo))
         (index (temp-index))
         (environment (git-environment index)))
    (unwind-protect
         (progn
           (multiple-value-bind (text code) (git-text repo (list "add" "-A")
                                                       :environment environment)
             (declare (ignore text))
             (unless (zerop code) (return-from snapshot-repo nil)))
           (multiple-value-bind (tree code) (git-text repo '("write-tree") :environment environment)
             (unless (and (zerop code) (plusp (length tree))) (return-from snapshot-repo nil))
             (multiple-value-bind (current code) (git-text repo (list "rev-parse" "--verify" "--quiet"
                                                                      (format nil "~a^{tree}" ref)))
               (when (and (zerop code) (string= current tree))
                 (return-from snapshot-repo nil)))
             (let* ((head (git-line repo "rev-parse" "--verify" "HEAD"))
                    (args (if head
                              (list "commit-tree" tree "-p" head "-m" "nodecode autosave")
                              (list "commit-tree" tree "-m" "nodecode autosave")))
                    (subject (format nil "autosave ~a" (universal-time-string (get-universal-time)))))
               (declare (ignore subject))
               (multiple-value-bind (commit code) (git-text repo args)
                 (unless (and (zerop code) (plusp (length commit))) (return-from snapshot-repo nil))
                 (multiple-value-bind (text code) (git-text repo (list "update-ref" "--create-reflog"
                                                                        "-m" "nodecode autosave"
                                                                        ref commit))
                   (declare (ignore text))
                   (if (zerop code)
                       (progn (remember-repo repo) commit)
                       nil))))))
      (ignore-errors (uiop:delete-file-if-exists index)))))

(defun reflog-entries (repo ref)
  "REPO's shadow history: ((COMMIT EPOCH) ...) newest first."
  (multiple-value-bind (text code) (git-text repo (list "reflog" "show" "--format=%H %ct" ref))
    (when (zerop code)
      (loop for line in (uiop:split-string text :separator '(#\Newline))
            for parts = (uiop:split-string line :separator '(#\Space))
            when (= 2 (length parts))
              collect (list (first parts) (or (parse-integer (second parts) :junk-allowed t) 0))))))

(defun git-file-versions (repo path)
  (let ((ref (shadow-ref repo))
        (rel (relative-name repo path)))
    (when rel
      (loop for (commit epoch) in (reflog-entries repo ref)
            when (zerop (nth-value 1 (git-text repo (list "cat-file" "-e" (format nil "~a:~a" commit rel)))))
              collect (list :time epoch :place :git :id commit)))))

;;; --- copies --------------------------------------------------------------------

(defun copy-key (path)
  (subseq (nlk:sha256-text (uiop:native-namestring (absolute-path path))) 0 20))

(defun copy-dir (path)
  (merge-pathnames (format nil "files/~a/" (copy-key path)) (storage-root)))

(defun copy-version (path)
  "One copy of PATH as it is now, under its own name. => its name, or NIL."
  (let* ((path (absolute-path path))
         (dir (copy-dir path))
         (size (file-size path)))
    (when (and size
               (<= size (* 1024 1024 (setting :max-file-mb))))
      (ensure-directories-exist (merge-pathnames "path.txt" dir))
      (with-open-file (out (merge-pathnames "path.txt" dir) :direction :output
                             :if-exists :supersede :if-does-not-exist :create)
        (write-line (uiop:native-namestring path) out))
      (let* ((second (get-universal-time))
             (sequence (1+ (count-if (lambda (file) (eql second (version-time file)))
                                      (uiop:directory-files dir))))
             (name (format nil "~d-~3,'0d.v" second sequence))
             (target (merge-pathnames name dir)))
        (when (uiop:file-exists-p target) (delete-file target))
        (uiop:copy-file path target)
        (trim-copies path)
        name))))

(defun version-parts (file)
  "A version file's (SECOND SEQUENCE), or NIL."
  (let ((base (pathname-name file)))
    (when (and base (equal "v" (pathname-type file)))
      (let ((dash (position #\- base)))
        (when dash
          (list (or (parse-integer base :end dash :junk-allowed t) 0)
                (or (parse-integer base :start (1+ dash) :junk-allowed t) 0)))))))

(defun version-time (file)
  (first (version-parts file)))

(defun copy-versions (path)
  "((NAME SECOND SEQUENCE) ...) newest first."
  (let ((dir (copy-dir path)))
    (when (uiop:directory-exists-p dir)
      (sort (loop for file in (uiop:directory-files dir)
                  for parts = (version-parts file)
                  when parts collect (list* (file-namestring file) parts))
            #'version-newer-p :key #'rest))))

(defun version-newer-p (a b)
  "Whether version (SECOND SEQUENCE) A is newer than B."
  (or (> (first a) (first b))
      (and (= (first a) (first b)) (> (second a) (second b)))))

(defun trim-copies (path)
  (let ((versions (copy-versions path))
        (keep (setting :keep)))
    (dolist (old (nthcdr keep versions))
      (ignore-errors (delete-file (merge-pathnames (first old) (copy-dir path)))))))

;;; --- what a call is about to change -----------------------------------------------

(defun operator-name (x)
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun literal-path (x strings)
  "The path X names, or NIL: a string, or a ($ \"name\") the strings object resolves."
  (cond ((stringp x) x)
        ((and (consp x)
              (equal "$" (operator-name (first x)))
              (stringp (second x))
              (hash-table-p strings))
         (gethash (second x) strings))
        (t nil)))

(defun command-mutates-p (text)
  (let ((words (uiop:split-string text :separator '(#\Space #\Tab #\Newline #\; #\& #\|))))
    (and (stringp text)
         (let ((mutator (find-if (lambda (word) (member word +mutators+ :test #'string=)) words)))
           (cond (mutator t)
                 ((member "git" words :test #'string=)
                  (and (find-if (lambda (word) (member word +git-mutators+ :test #'string=)) words) t))
                 ((search ">" text) t)
                 ((and (member "sed" words :test #'string=) (member "-i" words :test #'string=)) t)
                 (t nil))))))

(defun form-targets (form strings)
  "=> (values PATHS SHELL-P): the paths FORM names as written, and whether it runs
a shell command that changes something it does not name."
  (let ((paths '()) (shell nil))
    (labels ((walk (x)
               (when (consp x)
                 (let ((head (operator-name (first x))))
                   (cond ((member head +writers+ :test #'string=)
                          (let ((path (literal-path (second x) strings)))
                            (when path (push path paths)))
                          (when (member head +two-path-writers+ :test #'string=)
                            (let ((path (literal-path (third x) strings)))
                              (when path (push path paths)))))
                         ((member head +spawners+ :test #'string=)
                          (when (some (lambda (argument)
                                        (and (stringp argument) (command-mutates-p argument)))
                                      (rest x))
                            (setf shell t)))))
                 (dolist (item x) (walk item)))))
      (walk form)
      (values (nreverse (remove-duplicates paths :test #'string=)) shell))))

(defun call-targets (op)
  "=> (values PATHS SHELL-P) for the tool call OP."
  (let ((args (getf op :arguments)))
    (when (hash-table-p args)
      (let ((form (gethash "form" args)))
        (if (stringp form)
            (multiple-value-bind (paths shell) (form-targets (read-form form) (gethash "strings" args))
              (values paths shell))
            ;; A tool that names its own path, outside eval.
            (let ((path (gethash "path" args)))
              (values (if (stringp path) (list path) '()) nil)))))))

(defun read-form (text)
  "TEXT read as the forms the eval tool would run; NIL when it does not read."
  (let ((forms '()))
    (handler-case
        (with-input-from-string (in text)
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (push form forms)))
      (error () (return-from read-form (or (nreverse forms) text))))
    (nreverse forms)))

(defun protect-path (path repos)
  "Keep PATH's current bytes: a repository snapshot once per call, else a copy.
REPOS is the list of repositories already snapshotted by this call."
  (let ((path (absolute-path path)))
    (unless (skipped-p path)
      (let ((repo (and (setting :git) (repo-of path))))
        (cond (repo (unless (member repo repos :test #'equal)
                      (snapshot-repo repo)
                      (push repo repos)))
              ((and (regular-file-p path) (file-size path))
               (copy-version path))))
      repos)))

(defun protect (op)
  "Keep what OP is about to change. Never signals: the call always runs."
  (handler-case
      (multiple-value-bind (paths shell) (call-targets op)
        (let ((repos '()))
          (dolist (path paths)
            (setf repos (protect-path path repos)))
          (when shell
            (let ((repo (and (setting :git) (repo-of (uiop:getcwd)))))
              (when (and repo (not (member repo repos :test #'equal)))
                (snapshot-repo repo))))))
    (error (condition) (note-error condition))))

(defun observe-tool (op next)
  "The :TOOL point hook: snapshot, then let the call run, whatever happened."
  (protect op)
  (funcall next op))


;;; --- reading it back ---------------------------------------------------------------

(defun versions (path)
  "What is held for PATH, newest first: a list of (:TIME :PLACE :ID) plists."
  (let* ((path (absolute-path path))
         (repo (and (setting :git) (repo-of path))))
    (if repo
        (git-file-versions repo path)
        (loop for (name second sequence) in (copy-versions path)
              collect (list :time second :place :copy :id name)))))

(defun version-line (version index)
  (format nil "~2d. ~a~@[ (in ~a)~]"
          index (universal-time-string (getf version :time))
          (and (eq (getf version :place) :git) (subseq (getf version :id) 0 8))))

(defun restore (path &optional (index 1))
  "Put version INDEX (1, the newest, by default) of PATH back on disk. => a line."
  (let* ((path (absolute-path path))
         (versions (versions path))
         (version (nth (1- index) versions)))
    (unless version
      (fail "~a has no version ~d" (uiop:native-namestring path) index))
    (ensure-directories-exist path)
    (ecase (getf version :place)
      (:git
       (let* ((repo (repo-of path))
              (rel (relative-name repo path))
              (spec (format nil "~a:~a" (getf version :id) rel)))
         (multiple-value-bind (text code) (funcall *git-run* repo (list "show" spec) :output path)
           (declare (ignore text))
           (unless (zerop code)
             (fail "git could not write ~a back" (uiop:native-namestring path))))))
      (:copy
       (when (uiop:file-exists-p path) (delete-file path))
       (uiop:copy-file (merge-pathnames (getf version :id) (copy-dir path)) path)))
    (format nil "restored ~a from ~a (~d of ~d)"
            (uiop:native-namestring path)
            (universal-time-string (getf version :time))
            index (length versions))))

(defun remembered-repos ()
  (let ((file (data-file "repos.txt")))
    (and (uiop:file-exists-p file)
         (remove-if-not #'plusp (mapcar (lambda (line) (string-trim " " line))
                                        (uiop:read-file-lines file))))))

(defun paths ()
  "Every path and repository this cell holds versions for, with how many."
  (let ((copies (let ((dir (merge-pathnames "files/" (storage-root))))
                  (when (uiop:directory-exists-p dir)
                    (loop for sub in (uiop:subdirectories dir)
                          for marker = (merge-pathnames "path.txt" sub)
                          when (uiop:file-exists-p marker)
                            collect (list :path (string-trim '(#\Space #\Newline)
                                                              (uiop:read-file-string marker))
                                          :count (length (copy-versions
                                                          (uiop:read-file-string marker))))))))
        (repos (loop for repo in (remembered-repos)
                     collect (list :path repo :count (length (reflog-entries
                                                              (uiop:ensure-directory-pathname repo)
                                                              (shadow-ref (uiop:ensure-directory-pathname repo))))))))
    (append copies repos)))

(defun prune (&optional (keep (setting :keep)))
  "Drop copies beyond KEEP per path. Git shadow history is left to git."
  (let ((dropped 0))
    (dolist (entry (paths))
      (let ((versions (copy-versions (getf entry :path))))
        (dolist (old (nthcdr keep versions))
          (when (ignore-errors (delete-file (merge-pathnames (first old)
                                                             (copy-dir (getf entry :path)))))
            (incf dropped)))))
    (format nil "autosave: dropped ~d version~:p beyond ~d" dropped keep)))

(defun status ()
  "One paragraph: what is held, where, and the settings it runs on."
  (let ((entries (paths)))
    (format nil "autosave: ~d path~:p with versions, ~d version~:p in all; keep ~d, copies up to ~d MiB; data in ~a~@[; git ~a~]"
            (length entries)
            (reduce #'+ entries :key (lambda (entry) (or (getf entry :count) 0)) :initial-value 0)
            (setting :keep) (setting :max-file-mb)
            (uiop:native-namestring (storage-root))
            (and (= 1 (length (remembered-repos)))
                 (uiop:native-namestring (first (remembered-repos)))))))

;;; --- /autosave -----------------------------------------------------------------

(defun split-verb (text)
  "=> (values VERB REST)."
  (let* ((text (string-trim '(#\Space #\Tab) (or text "")))
         (space (position #\Space text)))
    (if space
        (values (string-downcase (subseq text 0 space))
                (string-trim '(#\Space #\Tab) (subseq text space)))
        (values (string-downcase text) ""))))

(defun split-version (rest)
  "REST as (values PATH VERSION): a trailing integer is the version."
  (let ((space (position #\Space rest :from-end t)))
    (if space
        (let ((tail (subseq rest (1+ space))))
          (multiple-value-bind (number end) (parse-integer tail :junk-allowed t)
            (if (and number (= end (length tail)))
                (values (string-trim '(#\Space #\Tab) (subseq rest 0 space)) number)
                (values rest 1))))
        (values rest 1))))

(defun find-text (path)
  (let ((versions (versions path)))
    (if versions
        (format nil "autosave: ~d version~:p of ~a~%~{~a~^~%~}"
                (length versions) (uiop:native-namestring (absolute-path path))
                (loop for version in versions for index from 1
                      collect (version-line version index)))
        (format nil "autosave: nothing held for ~a yet" (uiop:native-namestring (absolute-path path))))))

(defun list-text ()
  (let ((entries (paths)))
    (if entries
        (format nil "autosave: ~{~a~^~%~}"
                (loop for entry in entries
                      collect (format nil "~4d  ~a" (or (getf entry :count) 0) (getf entry :path))))
        "autosave: nothing held yet; a version is kept before each edit of an existing file")))

(defun run-slash (args session-id)
  "The /autosave command: status, list, find PATH, restore PATH [N], prune."
  (declare (ignore session-id))
  (multiple-value-bind (verb rest) (split-verb args)
    (cond ((string= verb "") (status))
          ((string= verb "list") (list-text))
          ((string= verb "find")
           (if (plusp (length rest))
               (find-text rest)
               "usage: /autosave find PATH"))
          ((string= verb "restore")
           (if (plusp (length rest))
               (multiple-value-bind (path index) (split-version rest)
                 (restore path index))
               "usage: /autosave restore PATH [N]"))
          ((string= verb "prune") (prune))
          (t "autosave: /autosave [list | find PATH | restore PATH [N] | prune]"))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell autosave
  (:section ("autosave")
    (:guide "nothing here is required; keep bounds how many versions a file holds, storage_dir is where copies go")
    ("keep" :integer :default 20 :min 1 :doc "versions kept per file")
    ("max_file_mb" :integer :default 10 :min 1 :doc "biggest file copied outside git, in MiB")
    ("storage_dir" :path :doc "where copies and the ledger go; default autosave/ under the home")
    ("skip" :list :doc "extra path components never saved")
    ("git" :boolean :default t :doc "keep a work tree's snapshots as local shadow refs"))
  (:hook :tool #'observe-tool)
  (:command "autosave" 'run-slash
            :description "A file's versions before a tool changed it: list, find, restore, prune"
            :argument-hint "[list | find PATH | restore PATH [N] | prune]")
  (:help :autosave "autosave: a pre-edit version of every file a tool changes - /autosave find PATH, /autosave restore PATH [N]" +manual+))
