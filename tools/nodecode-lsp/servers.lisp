;;;; servers.lisp --- which language servers there are, and which one a file gets.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; servers.json is omp's defaults.json verbatim: fifty-five servers, each a
;;;; command, its file types, the markers a project root holds, and what to
;;;; hand it at initialize. The operator's `lsp.servers.<name>' objects are
;;;; laid over it member by member, and a name it does not know is a server
;;;; of their own.
;;;;
;;;; A file gets every enabled server whose file types name it, primary
;;;; servers before linters, as omp orders them, when the server's binary
;;;; resolves (the project's node_modules/.bin or virtualenv first, then
;;;; PATH) and one of its root markers sits above the file. The root a server
;;;; runs in is the outermost directory holding a marker inside the session's
;;;; repository, so a Cargo workspace or a monorepo gets one server at its
;;;; top; outside a repository it is the nearest one.

(in-package #:nodecode-lsp)

(defstruct (spec (:copier nil))
  "One language server as configured."
  (name "" :type string)
  (command "" :type string)
  (args '() :type list)
  (file-types '() :type list)
  (root-markers '() :type list)
  settings init-options language-id linter disabled)

(defparameter +custom-clients+ '("biome" "swiftlint")
  "Servers omp drives through a client of its own rather than LSP; this cell
does not start them.")

(defun member-of (object &rest keys)
  "The first of KEYS OBJECT carries, with its presence."
  (loop for key in keys
        do (multiple-value-bind (value present) (gethash key object)
             (when present (return (values value t))))))

(defun string-list (value what name)
  "VALUE, a JSON array of strings, as a list; refuses anything else."
  (unless (and (vectorp value) (not (stringp value)) (every #'stringp value))
    (nlk:config-error "lsp.servers.~a.~a must be an array of strings" name what))
  (remove "" (coerce value 'list) :test #'string=))

(defun merge-spec (name object &optional base)
  "The spec NAME is: BASE with OBJECT's members laid over it, OBJECT a JSON
object in omp's camelCase or the config's snake_case."
  (flet ((take (&rest keys) (apply #'member-of object keys)))
    (let ((spec (if base
                    (copy-structure base)
                    (make-spec :name name))))
      (multiple-value-bind (command present) (take "command")
        (when present
          (unless (and (stringp command) (plusp (length command)))
            (nlk:config-error "lsp.servers.~a.command must be a non-empty string" name))
          (setf (spec-command spec) command)))
      (multiple-value-bind (args present) (take "args")
        (when present (setf (spec-args spec) (string-list args "args" name))))
      (multiple-value-bind (types present) (take "file_types" "fileTypes")
        (when present (setf (spec-file-types spec) (string-list types "file_types" name))))
      (multiple-value-bind (markers present) (take "root_markers" "rootMarkers")
        (when present (setf (spec-root-markers spec) (string-list markers "root_markers" name))))
      (multiple-value-bind (settings present) (take "settings")
        (when present (setf (spec-settings spec) (nlk:json-value settings :object))))
      (multiple-value-bind (options present) (take "init_options" "initOptions" "initializationOptions")
        (when present (setf (spec-init-options spec) (nlk:json-value options :object))))
      (multiple-value-bind (language present) (take "language_id" "languageId")
        (when present (setf (spec-language-id spec) (nlk:json-value language :text))))
      (multiple-value-bind (linter present) (take "is_linter" "isLinter")
        (when present (setf (spec-linter spec) (eq linter t))))
      (multiple-value-bind (disabled present) (take "disabled")
        (when present (setf (spec-disabled spec) (eq disabled t))))
      (when (or (string= "" (spec-command spec))
                (null (spec-file-types spec))
                (null (spec-root-markers spec)))
        (nlk:config-error "lsp.servers.~a needs command, file_types and root_markers" name))
      spec)))

(defun load-defaults (path)
  "The servers PATH, omp's defaults.json, declares, in its order."
  (let ((table (nlk:decode-json (uiop:read-file-string path :external-format :utf-8)))
        (specs '()))
    (maphash (lambda (name object) (push (merge-spec name object) specs)) table)
    (nreverse specs)))

(defparameter +defaults+
  (load-defaults (asdf:system-relative-pathname "nodecode-lsp" "servers.json"))
  "omp's servers, read once at load.")

(defun configured-specs (servers)
  "The servers the cell runs with: the defaults with SERVERS, the section's
`servers' object (or NIL), laid over them."
  (let ((specs (copy-list +defaults+)))
    (when servers
      (unless (hash-table-p servers)
        (nlk:config-error "lsp.servers must be an object of server name to settings"))
      (maphash (lambda (name object)
                 (unless (hash-table-p object)
                   (nlk:config-error "lsp.servers.~a must be an object" name))
                 (let ((base (find name specs :key #'spec-name :test #'string=)))
                   (if base
                       (setf specs (substitute (merge-spec name object base) base specs))
                       (setf specs (append specs (list (merge-spec name object)))))))
               servers))
    specs))

;;; --- which servers a file gets ------------------------------------------------

(defun file-name (path)
  "PATH's last component."
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun file-extension (path)
  "PATH's extension without its dot, lowercased, or \"\"."
  (let* ((name (file-name path))
         (dot (position #\. name :from-end t)))
    (if (and dot (plusp dot)) (string-downcase (subseq name (1+ dot))) "")))

(defun serves-file-p (spec path)
  "Whether SPEC's file types name PATH: an extension, with or without its
dot, or the whole file name (Dockerfile)."
  (let ((extension (file-extension path))
        (name (string-downcase (file-name path))))
    (some (lambda (type)
            (let ((bare (string-left-trim "." (string-downcase type))))
              (or (string= bare extension) (string= bare name)
                  (string= (string-downcase type) name))))
          (spec-file-types spec))))

(defun candidate-specs (path specs)
  "The enabled servers in SPECS that serve PATH, primary servers first."
  (stable-sort (remove-if (lambda (spec)
                            (or (spec-disabled spec)
                                (member (spec-name spec) +custom-clients+ :test #'string=)
                                (not (serves-file-p spec path))))
                          specs)
               (lambda (a b) (and (not (spec-linter a)) (spec-linter b)))))

(defun glob-match-p (pattern name)
  "Whether NAME matches PATTERN, whose only wildcard is *."
  (ppcre:scan (format nil "^~{~a~^.*~}$"
                      (mapcar #'ppcre:quote-meta-chars (uiop:split-string pattern :separator "*")))
              name))

(defun marker-present-p (directory marker)
  "Whether DIRECTORY (with its trailing slash) holds MARKER, a name or a *
pattern matched against its entries, as omp matches one level deep."
  (if (find #\* marker)
      (let ((here (uiop:parse-native-namestring directory)))
        (or (some (lambda (file) (glob-match-p marker (file-name (native file))))
                  (ignore-errors (uiop:directory-files here)))
            (some (lambda (folder) (glob-match-p marker (car (last (pathname-directory folder)))))
                  (ignore-errors (uiop:subdirectories here)))))
      (probe-file (uiop:parse-native-namestring (concatenate 'string directory marker)))))

(defun parent-directory (directory)
  "DIRECTORY's parent, both with trailing slashes, or NIL at the top."
  (let ((slash (position #\/ directory :end (max 0 (1- (length directory))) :from-end t)))
    (and slash (subseq directory 0 (1+ slash)))))

(defun find-root (path markers bound)
  "The directory, with its trailing slash, a server for PATH runs in: the
outermost one holding one of MARKERS between PATH's own and BOUND, when BOUND
contains PATH, else the nearest one above PATH. NIL when none holds one."
  (let ((bound (and bound (uiop:string-prefix-p bound path) bound))
        (found nil))
    (loop for directory = (parent-directory path) then (parent-directory directory)
          while directory
          do (when (some (lambda (marker) (marker-present-p directory marker)) markers)
               (setf found directory)
               (unless bound (return)))
             (when (and bound (string= directory bound))
               (return)))
    found))

(defparameter +local-bins+
  '((("package.json" "package-lock.json" "yarn.lock" "pnpm-lock.yaml") . "node_modules/.bin/")
    (("pyproject.toml" "ty.toml" "requirements.txt" "setup.py" "setup.cfg" "Pipfile"
      "pyrightconfig.json" "ruff.toml" ".ruff.toml")
     . (".venv/bin/" "venv/bin/" ".env/bin/"))
    (("Gemfile" "Gemfile.lock") . ("vendor/bundle/bin/" "bin/"))
    (("go.mod" "go.sum" "go.work") . "bin/"))
  "Project-local bin folders checked before PATH, each behind the markers that
say the project has one (omp's LOCAL_BIN_PATHS).")

(defun executable-p (path)
  "Whether PATH names an executable regular file."
  (let ((file (probe-file (uiop:parse-native-namestring path))))
    (and file (not (uiop:directory-pathname-p file))
         (ignore-errors (zerop (sb-posix:access (native file) sb-posix:x-ok))))))

(defun resolve-command (command root)
  "The executable COMMAND names for a server rooted at ROOT: a path as given,
else the project's own bins, else the first on PATH. NIL when none is."
  (cond ((find #\/ command)
         (and (executable-p command) command))
        (t (or (loop for (markers . bins) in +local-bins+
                     when (some (lambda (marker) (marker-present-p root marker)) markers)
                       do (loop for bin in (alexandria:ensure-list bins)
                                for candidate = (concatenate 'string root bin command)
                                when (executable-p candidate) do (return-from resolve-command candidate)))
               (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
                     for candidate = (concatenate 'string (string-right-trim "/" directory) "/" command)
                     when (and (plusp (length directory)) (executable-p candidate))
                       do (return candidate))))))

(defun typescript-speaks-lsp-p (tsc)
  "Whether the tsc at TSC is TypeScript 7's native one, which serves LSP
itself and ships no lib/tsserver.js for typescript-language-server to wrap."
  (let* ((real (native (or (probe-file tsc) tsc)))
         (bin (parent-directory real))
         (package (and bin (uiop:string-suffix-p bin "/bin/") (parent-directory bin))))
    (and package
         (probe-file (concatenate 'string package "package.json"))
         (not (probe-file (concatenate 'string package "lib/tsserver.js"))))))

(defun argv (spec command)
  "The command line SPEC runs COMMAND with: omnisharp's $PID is this process."
  (cons command (substitute (princ-to-string (sb-posix:getpid)) "$PID" (spec-args spec)
                            :test #'string=)))

(defun file-servers (path specs bound)
  "The servers PATH gets: ((SPEC ROOT ARGV)...), primary first. => a second
value, why each candidate that is not among them was passed over."
  (let ((chosen '()) (passed '()))
    (dolist (spec (candidate-specs path specs))
      (let ((root (find-root path (spec-root-markers spec) bound)))
        (if (null root)
            (push (format nil "~a needs one of ~{~a~^, ~} above the file"
                          (spec-name spec) (spec-root-markers spec))
                  passed)
            (let ((command (resolve-command (spec-command spec) root)))
              (if command
                  (push (list spec root (argv spec command)) chosen)
                  (push (format nil "~a: ~a is not installed (not on PATH)"
                                (spec-name spec) (spec-command spec))
                        passed))))))
    (setf chosen (nreverse chosen))
    ;; One TypeScript server: typescript-language-server wraps lib/tsserver.js,
    ;; which TypeScript 7 no longer ships, and tsc --lsp exits on older ones.
    (let ((native (find "typescript-native" chosen :key (lambda (c) (spec-name (first c))) :test #'string=)))
      (when native
        (setf chosen (remove (if (typescript-speaks-lsp-p (first (third native)))
                                 "typescript-language-server"
                                 "typescript-native")
                             chosen :key (lambda (c) (spec-name (first c))) :test #'string=))))
    (values chosen (nreverse passed))))

;;; --- what the server is told a document is -------------------------------------

(defparameter +language-ids+
  '(("typescript" "ts" "cts" "mts") ("typescriptreact" "tsx")
    ("javascript" "js" "mjs" "cjs") ("javascriptreact" "jsx")
    ("rust" "rs") ("go" "go") ("c" "c" "h")
    ("cpp" "cpp" "cc" "cxx" "hh" "hpp" "hxx" "cu" "cuh" "ino") ("zig" "zig")
    ("python" "py" "pyi") ("ruby" "rb" "rbw" "gemspec" "rake") ("lua" "lua")
    ("shellscript" "sh" "bash" "zsh" "ksh" "bats") ("fish" "fish") ("perl" "pl" "pm")
    ("php" "php" "phtml") ("java" "java") ("kotlin" "kt" "ktm" "kts")
    ("scala" "scala" "sc" "sbt") ("groovy" "groovy") ("clojure" "clj" "cljc" "cljs" "edn")
    ("csharp" "cs" "csx") ("fsharp" "fs") ("html" "html" "htm" "xhtml") ("css" "css")
    ("scss" "scss") ("sass" "sass") ("less" "less") ("vue" "vue") ("svelte" "svelte")
    ("astro" "astro") ("json" "json") ("jsonc" "jsonc") ("yaml" "yaml" "yml") ("toml" "toml")
    ("xml" "xml" "xsl" "xslt" "svg" "plist") ("markdown" "md" "markdown" "mdx" "mkd")
    ("latex" "tex" "sty" "cls") ("bibtex" "bib") ("sql" "sql") ("graphql" "graphql" "gql")
    ("dockerfile" "dockerfile") ("terraform" "tf") ("hcl" "hcl" "tfvars") ("nix" "nix")
    ("elixir" "ex" "exs") ("erlang" "erl" "hrl") ("haskell" "hs" "lhs")
    ("ocaml" "ml" "mli" "mll" "mly") ("swift" "swift") ("dart" "dart") ("gleam" "gleam")
    ("prisma" "prisma") ("tlaplus" "tla" "tlaplus") ("objective-c" "m") ("objective-cpp" "mm")
    ("odin" "odin") ("vim" "vim") ("helm" "tpl") ("heex" "heex") ("eex" "eex") ("erb" "erb"))
  "The LSP language id of each extension (omp's lang-from-path table, its LSP column).")

(defun language-id (spec path)
  "What SPEC is told PATH's language is."
  (or (spec-language-id spec)
      (let ((name (string-downcase (file-name path))))
        (cond ((or (string= name "dockerfile") (uiop:string-prefix-p "dockerfile." name)
                   (string= name "containerfile"))
               "dockerfile")
              (t (let ((extension (file-extension path)))
                   (or (car (find-if (lambda (row) (member extension (rest row) :test #'string=))
                                     +language-ids+))
                       "plaintext")))))))
