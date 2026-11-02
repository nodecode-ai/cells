;;;; footprint.lisp --- the kinds that tell whether a person uses the thing an
;;;; advisory names, read from files alone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An advisory with no machine check reaches everyone as READ: a Railway
;;;; breach, a crate's bug, a GitHub Action's revoked CA. These kinds let the
;;;; feed say how to see whether this person uses it, so the report shows it
;;;; to the people it touches and to no one else:
;;;;
;;;;   lockfile   a package's version in DIR's lockfile for its ECOSYSTEM
;;;;              (OSV's names: crates.io, Go, Maven, RubyGems, PyPI, npm,
;;;;              Packagist), judged like every versioned kind
;;;;   gh-action  the version of ACTION that DIR's GitHub workflows pin
;;;;   uses       signs that a SERVICE is used here: PATHS (under ~/, under
;;;;              {dir}, or absolute; MATCH narrows a path to one whose text
;;;;              matches), ENV variable names this process has set,
;;;;              PROGRAMS on PATH. A sign is a hit; none is :ABSENT. One
;;;;              check reads a folder ({dir} paths) or the machine (the
;;;;              rest), never both: a folder's check runs once per project,
;;;;              and a machine's sign would be named once for each
;;;;
;;;; They live in an advisory's FILE_CHECKS, beside CHECKS: copies installed
;;;; before these kinds would call a kind they do not know an error, and they
;;;; never read that key. None of them runs a program, so the live feed may
;;;; carry them freely (detect.lisp, "What a check runs").

(in-package #:nodecode-cyber-security)

(defparameter *home* nil
  "The folder ~/ names in a check's paths; NIL is this account's home. Tests
point it at a fixture.")

(defparameter *root* nil
  "The folder an absolute path in a check's paths is read under; NIL is /.
Tests point it at a fixture.")

(defun home-dir ()
  (uiop:ensure-directory-pathname (or *home* (user-homedir-pathname))))

(defparameter +read-limit+ (* 8 1024 1024)
  "The most of one file a check reads: lockfiles are text, rarely past a few MB.")

(defun read-text (path)
  "PATH's text (at most +READ-LIMIT+ characters), or NIL when it cannot be read."
  (handler-case
      (with-open-file (s path :external-format '(:utf-8 :replacement #\?))
        (let* ((buf (make-string (min +read-limit+ (file-length s))))
               (n (read-sequence buf s)))
          (subseq buf 0 n)))
    (error () nil)))

(defun in-dir (dir name)
  (merge-pathnames name (uiop:ensure-directory-pathname dir)))

;;; --- lockfiles --------------------------------------------------------------

(defun pypi-name (name)
  "NAME in PEP 503's normal form: lowercase, runs of - _ . as one -."
  (string-downcase (cl-ppcre:regex-replace-all "[-_.]+" name "-")))

(defun toml-package-versions (text package &key (key #'identity))
  "Every version PACKAGE has among TEXT's [[package]] tables (Cargo.lock,
poetry.lock, uv.lock, pdm.lock)."
  (loop for block in (cl-ppcre:split "(?m)^\\[\\[package\\]\\][ \\t]*$" text)
        for name = (cl-ppcre:register-groups-bind (n) ("(?m)^name\\s*=\\s*\"([^\"]+)\"" block) n)
        for version = (cl-ppcre:register-groups-bind (v) ("(?m)^version\\s*=\\s*\"([^\"]+)\"" block) v)
        when (and name version (equal (funcall key name) (funcall key package)))
          collect version))

(defun scan-versions (regex text)
  "The first group of every match of REGEX in TEXT."
  (let ((out '()))
    (cl-ppcre:do-register-groups (v) (regex text) (push v out))
    (nreverse out)))

(defun file-texts (dir names)
  "The text of each of NAMES (globs allowed) that exists in DIR."
  (loop for name in names
        append (loop for path in (directory (in-dir dir name))
                     for text = (read-text path)
                     when text collect text)))

(defun lockfile-versions (dir ecosystem package)
  "Every version of PACKAGE that DIR's lockfiles for ECOSYSTEM pin => strings."
  (let ((q (cl-ppcre:quote-meta-chars package)))
    (remove-duplicates
     (cond
       ((equal ecosystem "crates.io")
        (loop for text in (file-texts dir '("Cargo.lock"))
              append (toml-package-versions text package)))
       ((equal ecosystem "Go")
        ;; go.mod lists every module the build selects (Go 1.17+), direct or not.
        (loop for text in (file-texts dir '("go.mod"))
              append (scan-versions (format nil "(?m)^\\s*(?:require\\s+)?~a\\s+(v[0-9][^\\s]*)" q) text)))
       ((equal ecosystem "Maven")
        (let ((group (subseq package 0 (or (position #\: package) 0)))
              (artifact (subseq package (1+ (or (position #\: package) -1)))))
          (append
           (loop for text in (file-texts dir '("gradle.lockfile" "*/gradle.lockfile" "build.gradle" "build.gradle.kts"
                                               "*/build.gradle" "*/build.gradle.kts"))
                 append (scan-versions (format nil "~a:([0-9][^\\s\"'=:]*)" q) text))
           (loop for text in (file-texts dir '("pom.xml" "*/pom.xml"))
                 append (scan-versions
                         (format nil "<groupId>\\s*~a\\s*</groupId>\\s*<artifactId>\\s*~a\\s*</artifactId>\\s*<version>\\s*([0-9][^<\\s]*)\\s*</version>"
                                 (cl-ppcre:quote-meta-chars group) (cl-ppcre:quote-meta-chars artifact))
                         text)))))
       ((equal ecosystem "RubyGems")
        (loop for text in (file-texts dir '("Gemfile.lock"))
              append (scan-versions (format nil "(?m)^    ~a \\(([0-9][^)]*)\\)" q) text)))
       ((equal ecosystem "PyPI")
        (let ((want (pypi-name package)))
          (append
           (loop for text in (file-texts dir '("poetry.lock" "uv.lock" "pdm.lock"))
                 append (toml-package-versions text package :key #'pypi-name))
           (loop for text in (file-texts dir '("requirements*.txt" "requirements/*.txt"))
                 append (loop for line in (cl-ppcre:split "\\r?\\n" text)
                              for (name v) = (coerce (nth-value 1 (cl-ppcre:scan-to-strings
                                                                   "^\\s*([A-Za-z0-9][A-Za-z0-9._-]*)(?:\\[[^\\]]*\\])?\\s*===?\\s*([0-9][^\\s;#,]*)"
                                                                   line))
                                                     'list)
                              when (and name (equal (pypi-name name) want)) collect v))
           (loop for text in (file-texts dir '("Pipfile.lock"))
                 for json = (handler-case (shasht:read-json text) (error () nil))
                 append (loop for section in '("default" "develop")
                              for deps = (and (hash-table-p json) (gethash section json))
                              when (hash-table-p deps)
                                append (loop for k being the hash-keys of deps using (hash-value v)
                                             when (and (equal (pypi-name k) want) (hash-table-p v)
                                                       (stringp (gethash "version" v)))
                                               collect (string-left-trim "=" (gethash "version" v))))))))
       ((equal ecosystem "npm")
        (let ((v (npm-version dir package))) (and v (list v))))
       ((equal ecosystem "Packagist")
        (let ((v (composer-version dir (list package)))) (and v (list v)))))
     :test #'equal)))

;;; --- GitHub Actions ---------------------------------------------------------

(defun workflow-refs (dir action)
  "Every ref DIR's workflows and local actions pin ACTION to, as (REF COMMENT)."
  (let ((re (format nil "(?m)uses:\\s*[\"']?~a@([^\\s\"'#]+)[\"']?[ \\t]*(?:#\\s*(\\S+))?"
                    (cl-ppcre:quote-meta-chars action)))
        (out '()))
    (dolist (text (file-texts dir '(".github/workflows/*.yml" ".github/workflows/*.yaml"
                                    ".github/actions/*/action.yml" ".github/actions/*/action.yaml"))
             (nreverse out))
      (cl-ppcre:do-register-groups (ref comment) (re text)
        (push (list ref comment) out)))))

(defun judge-action-ref (ref comment check)
  "One pinned REF (COMMENT the version a bot wrote beside a commit pin) =>
(:status :found :why)."
  (let ((v (parse-version (if (cl-ppcre:scan "^[0-9a-f]{40}$" ref) (or comment "") ref))))
    (cond ((and (cl-ppcre:scan "^[0-9a-f]{40}$" ref) (null v))
           (list :status :error :found ref
                 :why (format nil "pinned to commit ~a: its version is not written beside it" (subseq ref 0 12))))
          ((or (null v) (and (= (length v) 1) (not (cl-ppcre:scan "^v?\\d+\\.\\d" ref))))
           ;; v1, release/v1, main: a ref that moves with each release.
           (list :status :ok :found ref :why (format nil "@~a follows the newest release" ref)))
          (t (multiple-value-bind (status why) (judge v check)
               (list :status status :found (version-string v) :why why))))))

;;; --- signs of use -------------------------------------------------------------

(defun expand-sign-path (path dir)
  "PATH with ~/ and {dir} filled in, or NIL when it names {dir} and there is none."
  (cond ((uiop:string-prefix-p "~/" path)
         (concatenate 'string (namestring (home-dir)) (subseq path 2)))
        ((search "{dir}" path) (and dir (fill-in path dir nil)))
        (*root* (concatenate 'string (string-right-trim "/" (namestring *root*)) path))
        (t path)))

(defun on-path-p (program)
  "PROGRAM is a file in one of PATH's folders."
  (loop for folder in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
        thereis (and (plusp (length folder))
                     (probe-file (in-dir folder program)))))

(defun env-sign (name)
  "Whether this process has NAME set, as the sign's text, or NIL. A project's
.env files are paths: {dir}/.env* and {dir}/.env.* (SBCL's .env* matches
.env and .envrc, .env.* matches .env.local), with MATCH naming the variable."
  (when (plusp (length (or (uiop:getenv (coerce name 'simple-string)) "")))
    (format nil "$~a is set" name)))

(defun sign-name (path dir)
  "PATH as a person reads it: inside DIR relative to it, under home as ~/."
  (let ((name (namestring path)))
    (flet ((under (root) (let ((r (and root (namestring (uiop:ensure-directory-pathname root)))))
                           (and r (uiop:string-prefix-p r name) (subseq name (length r))))))
      (cond ((under dir))
            ((under (home-dir)) (concatenate 'string "~/" (under (home-dir))))
            (t name)))))

(defun use-signs (check dir)
  "(VALUES SIGN SKIPPED): the first sign CHECK names that is here, and
whether a {dir} sign went unread for want of DIR."
  (let ((match (field check "match")) (skipped nil))
    (dolist (p (field check "paths"))
      (let ((full (expand-sign-path p dir)))
        (if (null full)
            (setf skipped t)
            (dolist (hit (ignore-errors (directory full)))
              (when (or (null match)
                        (let ((text (read-text hit))) (and text (cl-ppcre:scan match text))))
                (return-from use-signs (values (sign-name hit dir) nil)))))))
    (dolist (name (field check "env"))
      (let ((sign (env-sign name)))
        (when sign (return-from use-signs (values sign nil)))))
    (dolist (program (field check "programs"))
      (when (on-path-p program)
        (return-from use-signs (values (format nil "~a is installed" program) nil))))
    (values nil skipped)))

(defun run-file-check (check &key dir host)
  "One footprint CHECK (lockfile, gh-action, uses) => (:status :found :why)."
  (declare (ignore host))
  (let ((kind (field check "kind")))
    (cond
      ((equal kind "uses")
       (multiple-value-bind (sign skipped) (use-signs check dir)
         (let ((service (field check "service" "it")))
           (cond (sign (list :status :vulnerable :found sign
                             :why (format nil "uses ~a: ~a" service sign)))
                 ((and skipped (null (field check "env")) (null (field check "programs"))
                       (every (lambda (p) (search "{dir}" p)) (field check "paths")))
                  (list :status :skipped :why "needs the site's folder (:dir)"))
                 (t (list :status :absent :why (format nil "no sign of ~a here" service)))))))
      ((null dir) (list :status :skipped :why "needs the site's folder (:dir)"))
      ((equal kind "lockfile")
       (let ((versions (remove nil (mapcar #'parse-version
                                           (lockfile-versions dir (field check "ecosystem") (field check "package"))))))
         (if (null versions)
             (list :status :absent :why "not in this project")
             ;; A lockfile can hold one package at two versions: the worst one speaks.
             (let ((results (loop for v in versions
                                  collect (multiple-value-bind (status why) (judge v check)
                                            (list :status status :found (version-string v) :why why)))))
               (or (find :vulnerable results :key (lambda (r) (getf r :status)))
                   (first results))))))
      ((equal kind "gh-action")
       (let ((refs (workflow-refs dir (field check "action"))))
         (if (null refs)
             (list :status :absent :why "no workflow here uses it")
             (let ((results (loop for (ref comment) in refs collect (judge-action-ref ref comment check))))
               (or (find :vulnerable results :key (lambda (r) (getf r :status)))
                   (find :error results :key (lambda (r) (getf r :status)))
                   (first results))))))
      (t (list :status :error :why (format nil "unknown check kind ~a" kind))))))
