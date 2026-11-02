;;;; inventory.lisp --- whose machine this is: the projects and sites the checks run against.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A report is about the person's own stack, not the news: a check that
;;;; reads a site folder (npm, composer, WordPress) runs against every
;;;; project this machine holds, and a Python package is looked up in each
;;;; project's own virtualenv as well as the system python. The projects are
;;;; found, not asked for: folders under the home (and /var/www, /srv) that
;;;; hold a lockfile, a manifest, a WordPress core or a git repository, the
;;;; folders nodecode sessions ran in, and the config's project_dirs. The
;;;; walk reads names only -- it opens no file and runs nothing -- skips
;;;; hidden folders, dependency trees and caches, follows no symbolic link,
;;;; and stops at a depth and a folder budget, so a large home costs a
;;;; bounded second.

(in-package #:nodecode-cyber-security)

(defparameter +project-files+
  '("package-lock.json" "npm-shrinkwrap.json" "package.json" "pnpm-lock.yaml" "yarn.lock"
    "bun.lock" "bun.lockb" "composer.lock" "composer.json" "wp-config.php"
    "requirements.txt" "pyproject.toml" "poetry.lock" "uv.lock" "Pipfile.lock" "Pipfile"
    "setup.py" "Cargo.lock" "Cargo.toml" "go.mod" "Gemfile.lock" "Gemfile" "pom.xml"
    "build.gradle" "build.gradle.kts" "railway.json" "railway.toml" "Dockerfile"
    "docker-compose.yml" "docker-compose.yaml" "compose.yaml" "compose.yml" ".git")
  "A folder holding one of these files is a project or a site.")

(defparameter +project-folders+ '(".git" "wp-includes" ".venv")
  "A folder holding one of these folders is a project or a site.")

(defparameter +skipped-folders+
  '("node_modules" "bower_components" "vendor" "site-packages" "dist-packages" "__pycache__"
    "venv" "env" "target" "dist" "build" "out" "coverage" "tmp" "temp" "cache"
    "Library" "Applications" "AppData" "snap" "Trash")
  "Folders the walk never enters: dependency trees, build output, caches, the
OS's own. Hidden folders are skipped too, and ~/go/pkg (Go's module cache).")

(defparameter *walk-depth* 4 "How many levels under each root the walk reads.")
(defparameter *walk-budget* 20000 "The most folders one walk reads.")
(defparameter *project-limit* 400 "The most projects one inventory holds.")
(defparameter *inventory-ttl* 300 "Seconds a found inventory answers before it is walked again.")

(defun config-value (key default)
  "The running cell's setting KEY, or DEFAULT while the cell is not started."
  (handler-case (or (setting key) default) (error () default)))

(defun entry-name (path)
  "The last component of PATH, a file or a directory pathname, as a string."
  (if (uiop:directory-pathname-p path)
      (let ((last (car (last (pathname-directory path))))) (if (stringp last) last ""))
      (file-namestring path)))

(defun real-child-p (child parent-truename)
  "CHILD, a folder listed under the folder whose truename is PARENT-TRUENAME,
is really there and not a symbolic link out of it."
  (let ((true (ignore-errors (probe-file child))))
    (and true
         (equal (pathname-directory true)
                (append (pathname-directory parent-truename) (list (entry-name child)))))))

(defun skipped-folder-p (folder parent)
  (let ((name (entry-name folder)))
    (or (zerop (length name))
        (char= (char name 0) #\.)
        (member name +skipped-folders+ :test #'string-equal)
        (and (string= name "pkg") (string= (entry-name parent) "go")))))

(defun project-folder-p (files folders)
  "A folder whose file names FILES and folder names FOLDERS mark a project."
  (or (some (lambda (f) (member f +project-files+ :test #'string=)) files)
      (some (lambda (f) (member f +project-folders+ :test #'string=)) folders)))

(defun walk-projects (root &key (depth *walk-depth*) (budget (list *walk-budget*)))
  "Every project folder at ROOT or under it, at most DEPTH levels down,
reading no more folders than the one-element list BUDGET still allows."
  (let ((found '()))
    (labels ((walk (dir level)
               (when (and (plusp (car budget)) (< (length found) *project-limit*))
                 (decf (car budget))
                 (let* ((true (ignore-errors (probe-file dir)))
                        (subdirs (and true (ignore-errors (uiop:subdirectories dir))))
                        (files (and true (mapcar #'file-namestring
                                                 (ignore-errors (uiop:directory-files dir))))))
                   (when true
                     (when (project-folder-p files (mapcar #'entry-name subdirs))
                       (push (uiop:ensure-directory-pathname true) found))
                     (when (< level depth)
                       (dolist (sub subdirs)
                         (unless (or (skipped-folder-p sub dir) (not (real-child-p sub true)))
                           (walk sub (1+ level))))))))))
      (let ((root (ignore-errors (uiop:ensure-directory-pathname root))))
        (when (and root (uiop:directory-exists-p root))
          (walk root 0))))
    (nreverse found)))

(defun session-folders ()
  "The folders nodecode sessions on this machine ran in, each as its
repository's root when it sits in one; NIL without a store."
  (let ((list-sessions (find-symbol "LIST-SESSIONS" "NLK"))
        (project-root (find-symbol "PROJECT-ROOT" "NLK")))
    (when (and list-sessions (fboundp list-sessions))
      (remove-duplicates
       (loop for s in (ignore-errors (funcall list-sessions))
             for cwd = (and (listp s) (getf s :working-directory))
             for root = (and (stringp cwd) (plusp (length cwd))
                             (or (and project-root (fboundp project-root)
                                      (ignore-errors (funcall project-root cwd)))
                                 cwd))
             when root collect (namestring (uiop:ensure-directory-pathname root)))
       :test #'string=))))

(defun inventory-roots ()
  "Where the walk starts: the home, the usual web roots, the config's project_dirs."
  (remove-duplicates
   (append (list (namestring (user-homedir-pathname)) "/var/www/" "/srv/")
           (mapcar (lambda (d) (namestring (uiop:ensure-directory-pathname d)))
                   (remove-if-not #'stringp (config-value :project-dirs '()))))
   :test #'string=))

(defun find-projects ()
  "Every project and site folder this machine holds, as directory namestrings."
  (let* ((budget (list *walk-budget*))
         (depth (config-value :scan-depth *walk-depth*))
         (home (namestring (user-homedir-pathname)))
         (walked (loop for root in (inventory-roots)
                       append (walk-projects root :depth depth :budget budget)))
         (sessions (loop for folder in (session-folders)
                         ;; A session folder is walked shallow: it is a project, or holds a few.
                         unless (string= folder home)
                           append (walk-projects folder :depth 1 :budget budget))))
    (remove-if (lambda (f) (string= f home))
               (remove-duplicates (mapcar #'namestring (append walked sessions))
                                  :test #'string= :from-end t))))

(defparameter *find-projects* 'find-projects
  "How the inventory finds the projects: (funcall *FIND-PROJECTS*) => folders. Tests stub it.")

(defvar *inventory-cache* nil "(UNIVERSAL-TIME . FOLDERS) the last walk found.")

(defun projects (&key refresh)
  "The project and site folders the report checks, walked at most every
*INVENTORY-TTL* seconds (REFRESH walks now)."
  (when (or refresh (null *inventory-cache*)
            (> (- (get-universal-time) (car *inventory-cache*)) *inventory-ttl*)
            (not (eq (cddr *inventory-cache*) *find-projects*)))
    (setf *inventory-cache* (list* (get-universal-time) (funcall *find-projects*) *find-projects*)))
  (cadr *inventory-cache*))

(defun short-folder (folder)
  "FOLDER as a person reads it: under the home as ~/..., no trailing slash."
  (let* ((text (string-right-trim "/" (namestring folder)))
         (home (string-right-trim "/" (namestring (user-homedir-pathname)))))
    (if (and (plusp (length home)) (uiop:string-prefix-p (concatenate 'string home "/") text))
        (concatenate 'string "~" (subseq text (length home)))
        text)))

;;; --- Python packages in a project's own virtualenv --------------------------------

(defun normalized-package (name)
  "NAME as PEP 503 compares it: lower case, runs of - _ . as one -."
  (string-downcase (cl-ppcre:regex-replace-all "[-_.]+" name "-")))

(defun site-packages (dir)
  "The site-packages folders of DIR's virtualenvs (.venv, venv, env)."
  (let ((dir (uiop:ensure-directory-pathname dir)))
    (loop for env in '(".venv/" "venv/" "env/")
          for root = (merge-pathnames env dir)
          when (uiop:directory-exists-p root)
            append (append
                    (loop for lib in (ignore-errors (uiop:subdirectories (merge-pathnames "lib/" root)))
                          for sp = (merge-pathnames "site-packages/" lib)
                          when (and (uiop:string-prefix-p "python" (entry-name lib))
                                    (uiop:directory-exists-p sp))
                            collect sp)
                    (let ((win (merge-pathnames "Lib/site-packages/" root)))
                      (and (uiop:directory-exists-p win) (list win)))))))

(defun venv-package-version (dir package)
  "PACKAGE's version installed in one of DIR's virtualenvs, from its
.dist-info METADATA, or NIL. Reads files only: the project's python never runs."
  (let ((want (normalized-package package)))
    (dolist (sp (site-packages dir))
      (dolist (meta (ignore-errors (uiop:subdirectories sp)))
        (let* ((name (entry-name meta))
               (dash (position #\- name)))
          (when (and dash
                     (or (uiop:string-suffix-p name ".dist-info") (uiop:string-suffix-p name ".egg-info"))
                     (string= want (normalized-package (subseq name 0 dash))))
            (let* ((file (merge-pathnames (if (uiop:string-suffix-p name ".dist-info") "METADATA" "PKG-INFO") meta))
                   (text (ignore-errors (uiop:read-file-string file)))
                   (version (and text (cl-ppcre:register-groups-bind (v) ("(?m)^Version:\\s*(\\S+)" text) v))))
              (when version (return-from venv-package-version version)))))))))
