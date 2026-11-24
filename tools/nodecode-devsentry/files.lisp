;;;; files.lisp --- a folder read as data, and nothing written to it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; INSPECT-DIRECTORY reads names, sizes and extensions and never a file's
;;;; contents. Symbolic links are recorded and not followed. A name that looks
;;;; like a credential is marked and its contents are still not read. Directory
;;;; names that are dependencies, build output, caches or version control are
;;;; listed as skipped, with the reason, instead of being descended into.

(in-package #:nodecode-devsentry)

;;; --- what a path is ---------------------------------------------------------------

(defun lstat-mode (path)
  "PATH's lstat mode, or NIL when it cannot be read."
  #+unix (handler-case (sb-posix:stat-mode (sb-posix:lstat (uiop:native-namestring path)))
           (error () nil))
  #-unix (progn path nil))

(defun symlink-p (path)
  "Whether PATH itself is a symbolic link."
  #+unix (let ((mode (lstat-mode path)))
           (and mode (eql (logand mode sb-posix:s-ifmt) sb-posix:s-iflnk)))
  #-unix (let ((probe (probe-file path)))
           (and probe (not (uiop:directory-exists-p probe))
                (not (ignore-errors (uiop:file-exists-p path))))))

(defun plain-directory-p (path)
  "Whether PATH is a directory and not a link to one."
  #+unix (let ((mode (lstat-mode path)))
           (and mode (eql (logand mode sb-posix:s-ifmt) sb-posix:s-ifdir)))
  #-unix (and (uiop:directory-exists-p path) (not (symlink-p path))))

(defun plain-file-p (path)
  "Whether PATH is a regular file and not a link to one."
  #+unix (let ((mode (lstat-mode path)))
           (and mode (eql (logand mode sb-posix:s-ifmt) sb-posix:s-ifreg)))
  #-unix (and (uiop:file-exists-p path) (not (symlink-p path))))

(defun directory-names (folder)
  "Every entry name in FOLDER, dot files included and . and .. left out."
  #+unix (let ((handle (sb-posix:opendir (uiop:native-namestring folder)))
               (names '()))
           (unwind-protect
                (loop for entry = (sb-posix:readdir handle)
                      while (and entry (not (sb-alien:null-alien entry)))
                      for name = (sb-posix:dirent-name entry)
                      unless (member name '("." "..") :test #'equal)
                        do (push name names))
             (sb-posix:closedir handle))
           (nreverse names))
  #-unix (append (mapcar #'file-namestring (uiop:directory-files folder))
                 (mapcar (lambda (path)
                           (car (last (remove "" (uiop:split-string
                                                   (string-right-trim "/\\"
                                                                      (uiop:native-namestring path))
                                                   :separator "/\\")))))
                         (uiop:subdirectories folder))))

(defun file-size (path)
  "PATH's size in bytes, or NIL when it cannot be read."
  (handler-case
      (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (and in (file-length in)))
    (error () nil)))

(defun join-path (prefix name)
  "NAME under PREFIX, '/'-separated, PREFIX's trailing separators taken off."
  (if (or (null prefix) (zerop (length prefix)))
      name
      (format nil "~a/~a" (string-right-trim "/\\" prefix) name)))

;;; --- the one directory a call names -----------------------------------------------

(defun full-path (raw)
  "RAW as an absolute native path string, ~ expanded, relative resolved here."
  (let* ((expanded (if (and (plusp (length raw)) (char= #\~ (char raw 0)))
                        (uiop:native-namestring (nlk:expand-home raw))
                        raw))
         (path (uiop:parse-native-namestring expanded)))
    (uiop:native-namestring
     (if (uiop:absolute-pathname-p path) path (merge-pathnames path (uiop:getcwd))))))

(defun resolve-directory (raw)
  "RAW as an existing readable directory => (values NAMESTRING INFO), or a refusal."
  (unless (and (stringp raw) (plusp (length (string-trim '(#\Space #\Tab) raw))))
    (refuse "invalid_path" "path must be a non-empty string"))
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (when (some (lambda (character) (< (char-code character) 32)) text)
      (refuse "invalid_path" "path contains a control character"))
    (let* ((requested (full-path text))
           (target (or (probe-file requested)
                       (refuse "not_found" "path does not exist: ~s" text)))
           (resolved (string-right-trim "/\\" (uiop:native-namestring (truename target)))))
      (unless (uiop:directory-exists-p target)
        (refuse "not_a_directory" "path is not a directory: ~s" text))
      (handler-case (directory-names resolved)
        (error (condition)
          (declare (ignore condition))
          (refuse "unreadable" "directory is not readable: ~s" text)))
      (values resolved
              (nlk:json-object "requested" text
                                "resolved" resolved
                                "symlink_resolved" (and (symlink-p requested) t))))))

;;; --- the names that are skipped, and why -------------------------------------------

(defparameter +exclusion-reasons+
  '((".git" . "version_control") (".hg" . "version_control") (".svn" . "version_control")
    (".jj" . "version_control") (".bzr" . "version_control")
    ("node_modules" . "dependency") ("bower_components" . "dependency")
    ("jspm_packages" . "dependency") ("vendor" . "dependency") (".venv" . "dependency")
    ("venv" . "dependency") ("__pypackages__" . "dependency") ("site-packages" . "dependency")
    ("pods" . "dependency") ("carthage" . "dependency") ("third_party" . "dependency")
    ("third-party" . "dependency")
    ("dist" . "build") ("build" . "build") ("target" . "build") ("out" . "build")
    ("bin" . "build") ("obj" . "build") (".next" . "build") (".nuxt" . "build")
    (".output" . "build") (".turbo" . "build") ("coverage" . "build") ("htmlcov" . "build")
    (".eggs" . "build")
    ("__pycache__" . "cache") (".pytest_cache" . "cache") (".mypy_cache" . "cache")
    (".ruff_cache" . "cache") (".cache" . "cache") (".tox" . "cache") (".nox" . "cache")
    (".gradle" . "cache") (".parcel-cache" . "cache") (".sass-cache" . "cache")
    (".eslintcache" . "cache") (".hypothesis" . "cache"))
  "Directory names an inventory does not enter, and the reason it does not.")

(defun exclusion-reason (name directory)
  "Why DIRECTORY, named NAME, is not descended into, or NIL."
  (let ((lowered (string-downcase name)))
    (or (cdr (assoc lowered +exclusion-reasons+ :test #'equal))
        (when (uiop:string-suffix-p ".egg-info" lowered) "build")
        ;; `env' is only a virtual environment when it holds a pyvenv.cfg: plenty
        ;; of projects keep their own source in a folder named env.
        (when (equal lowered "env")
          (let ((marker (join-path directory "pyvenv.cfg")))
            (when (and (plain-file-p marker) (not (symlink-p marker))) "dependency"))))))

;;; --- the names that look like secrets ----------------------------------------------

(defparameter +sensitive-names+
  '(".env" ".netrc" ".npmrc" ".pypirc" ".pgpass" ".htpasswd" "id_rsa" "id_dsa"
    "id_ecdsa" "id_ed25519" "credentials.json" "secrets.json" "auth.json"
    "service-account.json" "secrets.yaml" "secrets.yml")
  "File names reported without a byte of their contents read.")

(defparameter +sensitive-suffixes+ '(".pem" ".key" ".p12" ".pfx" ".kdbx" ".keystore" ".jks")
  "Suffixes of a key or a keystore.")

(defparameter +sensitive-tokens+ '("secret" "credential" "password" "passwd"
                                    "private-key" "private_key")
  "Words in a name that make it a credential file.")

(defun sensitive-name-p (name)
  "Whether NAME looks like credentials, a key or a dotenv file."
  (let ((lowered (string-downcase name)))
    (or (member lowered +sensitive-names+ :test #'equal)
        (uiop:string-prefix-p ".env" lowered)
        (some (lambda (suffix) (uiop:string-suffix-p lowered suffix)) +sensitive-suffixes+)
        (some (lambda (token) (search token lowered)) +sensitive-tokens+))))

;;; --- what language an extension is --------------------------------------------------

(defparameter +code-extensions+
  '((".py" . "Python") (".pyi" . "Python") (".js" . "JavaScript") (".mjs" . "JavaScript")
    (".cjs" . "JavaScript") (".jsx" . "JavaScript") (".ts" . "TypeScript")
    (".tsx" . "TypeScript") (".go" . "Go") (".rs" . "Rust") (".java" . "Java")
    (".kt" . "Kotlin") (".kts" . "Kotlin") (".c" . "C") (".h" . "C")
    (".cpp" . "C++") (".cc" . "C++") (".cxx" . "C++") (".hpp" . "C++")
    (".hh" . "C++") (".cs" . "C#") (".rb" . "Ruby") (".php" . "PHP")
    (".swift" . "Swift") (".scala" . "Scala") (".sh" . "Shell") (".bash" . "Shell")
    (".zsh" . "Shell") (".ps1" . "PowerShell") (".psm1" . "PowerShell")
    (".sql" . "SQL") (".html" . "HTML") (".htm" . "HTML") (".css" . "CSS")
    (".scss" . "SCSS") (".vue" . "Vue") (".svelte" . "Svelte") (".lua" . "Lua")
    (".dart" . "Dart") (".ex" . "Elixir") (".exs" . "Elixir") (".erl" . "Erlang")
    (".hs" . "Haskell") (".ml" . "OCaml") (".fs" . "F#") (".clj" . "Clojure")
    (".zig" . "Zig") (".nim" . "Nim") (".pl" . "Perl") (".pm" . "Perl")
    (".proto" . "Protobuf") (".graphql" . "GraphQL") (".gql" . "GraphQL")
    (".tf" . "Terraform") (".lisp" . "Lisp") (".asd" . "Lisp"))
  "Extensions counted as code, and the language they are counted under.")

(defparameter +other-extensions+
  '((".md" . "Markdown") (".json" . "JSON") (".yaml" . "YAML") (".yml" . "YAML")
    (".toml" . "TOML") (".xml" . "XML") (".txt" . "Text") (".rst" . "reStructuredText")
    (".lock" . "Lockfile") (".ini" . "INI") (".cfg" . "Config") (".csv" . "CSV"))
  "Extensions that are files in a project but not code.")

(defparameter +special-files+
  '(("dockerfile" . "Dockerfile") ("makefile" . "Makefile") ("cmakelists.txt" . "CMake")
    ("gemfile" . "Ruby") ("rakefile" . "Ruby") ("go.mod" . "Go")
    ("cargo.toml" . "TOML"))
  "Well-known file names, classified without an extension.")

(defparameter +code-labels+ '("Dockerfile" "Makefile" "CMake" "Ruby" "Go")
  "The +SPECIAL-FILES+ labels that are code and not another kind of file.")

(defun classify-filename (name)
  "NAME's classification => (values LANGUAGE OTHER); at most one is set."
  (let* ((lowered (string-downcase name))
         (special (cdr (assoc lowered +special-files+ :test #'equal))))
    (cond
      ((member special +code-labels+ :test #'equal) (values special nil))
      (special (values nil special))
      (t
       (let* ((dot (position #\. lowered :from-end t))
              (suffix (if (and dot (plusp dot)) (subseq lowered dot) "")))
         (cond
           ((zerop (length suffix)) (values nil nil))
           ((cdr (assoc suffix +code-extensions+ :test #'equal))
            (values (cdr (assoc suffix +code-extensions+ :test #'equal)) nil))
           ((cdr (assoc suffix +other-extensions+ :test #'equal))
            (values nil (cdr (assoc suffix +other-extensions+ :test #'equal))))
           (t (values nil suffix))))))))

;;; --- the inventory ------------------------------------------------------------------

(defun bump-count (table label size)
  "Count one more file under LABEL in TABLE, adding its SIZE in bytes."
  (let ((row (gethash label table)))
    (setf (gethash label table)
          (if row (cons (1+ (car row)) (+ (cdr row) (or size 0))) (cons 1 (or size 0))))))

(defun count-rows (table key)
  "TABLE's rows as JSON objects named by KEY, most files first then by name."
  (let ((rows '()))
    (maphash (lambda (label row)
               (push (nlk:make-json-object key label "files" (car row) "bytes" (cdr row)) rows))
             table)
    (coerce (sort rows (lambda (left right)
                         (let ((left-files (gethash "files" left))
                               (right-files (gethash "files" right)))
                           (if (= left-files right-files)
                               (string< (gethash key left) (gethash key right))
                               (> left-files right-files)))))
            'vector)))

(defparameter *max-skipped* 200
  "How many skipped directory names one inventory lists.")

(defun inspect-directory (root max-depth max-entries)
  "ROOT inventoried to MAX-DEPTH levels and MAX-ENTRIES entries, as a JSON object."
  (let ((files 0) (directories 0) (symlinks 0) (sensitive 0)
        (skipped '()) (entries '()) (truncated nil) (depth-limited nil)
        (skipped-truncated nil)
        (languages (make-hash-table :test #'equal))
        (other (make-hash-table :test #'equal)))
    (labels ((add (object)
               ;; when the entry list is full the walk stops, and says so
               (if (>= (length entries) max-entries)
                   (progn (setf truncated t) nil)
                   (progn (push object entries) t)))
             (walk (directory relative depth)
               (when truncated (return-from walk nil))
               (let ((names (handler-case (sort (directory-names directory) #'string<)
                              (error () (add (nlk:json-object "path" (or relative ".")
                                                              "type" "directory"
                                                              "error" "unreadable"))
                                     nil))))
                 (dolist (name names)
                   (when truncated (return))
                   (let* ((child (join-path directory name))
                          (child-relative (join-path relative name)))
                     (cond
                       ((symlink-p child)
                        (incf symlinks)
                        (add (nlk:json-object "path" child-relative "type" "symlink"
                                              "followed" nil)))
                       ((plain-directory-p child)
                        (let ((reason (exclusion-reason name child)))
                          (if reason
                              (if (< (length skipped) *max-skipped*)
                                  (push (nlk:json-object "path" child-relative "reason" reason)
                                        skipped)
                                  (setf skipped-truncated t))
                              (progn
                                (incf directories)
                                (if (not (add (nlk:json-object "path" child-relative
                                                                "type" "directory")))
                                    (return)
                                    (if (>= depth max-depth)
                                        (setf depth-limited t)
                                        (walk child child-relative (1+ depth))))))))
                       ((plain-file-p child)
                        (incf files)
                        (let* ((size (file-size child))
                               (sensitive-file (sensitive-name-p name))
                               (object (nlk:json-object "path" child-relative "type" "file"
                                                         "bytes" size)))
                          (when sensitive-file
                            (incf sensitive)
                            (setf (gethash "sensitive" object) t
                                  (gethash "content" object) "not_read"))
                          (multiple-value-bind (language label) (classify-filename name)
                            (cond
                              (language
                               (setf (gethash "language" object) language)
                               (bump-count languages language size))
                              (label
                               (setf (gethash "label" object) label)
                               (bump-count other label size))
                              (t (bump-count other "(no extension)" size))))
                          (unless (add object) (return))))
                       (t (add (nlk:json-object "path" child-relative "type" "other")))))))))
      (walk root "" 1)
      (let ((notes (list "Inventory is read-only. File contents were not opened."
                         "Symlinks were recorded and were not followed."
                         "Dependency, build, cache, and version-control directories were not descended into."
                         "Language totals are approximate counts by extension and a few well-known filenames.")))
        (when truncated
          (push "The entry list hit its bound. Totals describe only the scanned portion." notes))
        (when depth-limited
          (push "Some directories were listed and not entered because max_depth was reached." notes))
        (nlk:json-object
         "summary" (nlk:json-object "files" files
                                     "directories" directories
                                     "symlinks" symlinks
                                     "sensitive_files" sensitive
                                     "skipped_directories" (length skipped)
                                     "truncated" truncated
                                     "depth_limited" depth-limited
                                     "approximate" t
                                     "method" "extension_and_filename")
         "languages" (count-rows languages "language")
         "other" (count-rows other "label")
         "skipped" (coerce (reverse skipped) 'vector)
         "skipped_list_truncated" skipped-truncated
         "entries" (coerce (reverse entries) 'vector)
         "notes" (coerce (reverse notes) 'vector))))))
