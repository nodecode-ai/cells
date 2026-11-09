;;;; cell.lisp --- a local repository mapped, and the source a task needs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tools the Hermes plugin anthony-maio/cartograph ships (assets/hermes)
;;;; gave a model, rewritten as a Nodecode cell: cartograph_analyze and
;;;; cartograph_context, answering the same two things about a local
;;;; repository -- which files matter and which modules everything imports,
;;;; and which source excerpts a task needs. The plugin ran an installed Node
;;;; CLI over the repository; this cell walks the files, reads their imports
;;;; and ranks them itself, so nothing external is installed and nothing is
;;;; executed.
;;;;
;;;; What it reads is source text, which enters the conversation and rides to
;;;; whatever provider the session is on: repository contents are data, not
;;;; instructions, and the tools never write a file.

(in-package #:nodecode-cartograph)

;;; --- the words the model reads ---------------------------------------------------

(defparameter +primer+
  "Cartograph is available through the nodecode-cartograph cell: a map of a
local repository, and the source excerpts a task needs, as two tools.
  cartograph_analyze {repo: '/abs/path'}   what the repository is made of:
      how many code files, every import edge resolved inside it, the files that
      matter (a PageRank over who imports whom) and the dependency hubs.
      Call it before choosing what to read, or when a change's blast radius
      matters.
  cartograph_context {repo: '/abs/path', task: 'concrete terms'}
      the excerpts to read for one task: files whose path or body carries the
      task's words, plus the tests that cover them, each with its line range.
      Terms are matched lexically: name a module, a symbol, a file or an error
      string; vague prose selects vague files.
The same two are (cartograph:analyze \"/abs/path\") and
(cartograph:context \"/abs/path\" \"task\") from eval, and /cartograph for the
operator. Static and local: nothing is run, no model is called, no key is
needed. Imports are read from source text, so an edge it misses is a miss,
not a fact; the excerpts are source and can be truncated -- read the file
before editing it."
  "What (help :cartograph) answers while the cell runs.")

;;; --- what counts as source -------------------------------------------------------

(defparameter *languages*
  '(("python" "py" "pyi")
    ("javascript" "js" "jsx" "mjs" "cjs")
    ("typescript" "ts" "tsx" "mts" "cts")
    ("lisp" "lisp" "lsp" "cl" "asd" "el")
    ("go" "go")
    ("rust" "rs")
    ("ruby" "rb" "rake")
    ("java" "java")
    ("kotlin" "kt" "kts")
    ("c" "c" "h")
    ("cpp" "cc" "cpp" "cxx" "hpp" "hh" "hxx")
    ("csharp" "cs")
    ("swift" "swift")
    ("php" "php")
    ("scala" "scala")
    ("clojure" "clj" "cljs" "cljc")
    ("elixir" "ex" "exs")
    ("haskell" "hs")
    ("lua" "lua")
    ("shell" "sh" "bash" "zsh")
    ("dart" "dart")
    ("zig" "zig")
    ("vue" "vue")
    ("svelte" "svelte"))
  "Each language this cell ranks, then the file-name types that carry it.")

(defparameter *language-of*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry *languages* table)
      (dolist (type (rest entry))
        (setf (gethash type table) (first entry)))))
  "File-name type to language: the table LANGUAGE-OF reads.")

(defparameter *skipped-directories*
  '(".git" ".hg" ".svn" ".bzr" "node_modules" "bower_components" "jspm_packages"
    "vendor" "third_party" ".venv" "venv" "virtualenv" "env" "__pycache__" ".tox"
    ".mypy_cache" ".pytest_cache" ".ruff_cache" "dist" "build" "out" "target"
    "coverage" "htmlcov" ".next" ".nuxt" ".svelte-kit" ".cache" ".idea"
    ".vscode" ".terraform" ".gradle" ".stack-work" "pods" "deriveddata")
  "Directories the walk never enters: package stores, build output, editor state.")

(defparameter *module-extensions*
  '("" ".py" ".pyi" ".js" ".jsx" ".mjs" ".cjs" ".ts" ".tsx" ".mts" ".cts"
    ".lisp" ".asd" ".cl" ".rb" ".go" ".rs" ".java" ".kt" ".c" ".h" ".cc"
    ".cpp" ".hpp" ".cs" ".ex" ".exs" ".php" ".swift" ".scala" ".lua" ".sh"
    ".dart" ".zig" ".clj" ".vue" ".svelte" ".hs")
  "What a bare module specifier may stand for: the file's own type first,
then the types of the languages this cell ranks.")

(defparameter *index-stems* '("index" "mod" "__init__" "main" "lib")
  "File stems a directory specifier resolves through, tried in this order.")

(defparameter *max-text-chars* 200000
  "How much of one file is read: enough for an excerpt, never a whole dump.")

(defparameter *text-budget* (* 32 1024 1024)
  "How much source text one context read keeps in memory, in characters.")

;;; --- a file, and a repository -----------------------------------------------------

(defstruct (source-file (:constructor make-source-file
                                      (&key path file language lines chars specs text))
                        (:conc-name source-file-))
  "One code file of a repository: PATH relative to the root, the FILE itself,
its LANGUAGE, its size, the module SPECS it asks for and the files IMPORTS
those name; TEXT is kept only for a context read."
  (path "" :type string)
  file
  (language "" :type string)
  (lines 0 :type integer)
  (chars 0 :type integer)
  (specs '())
  (imports '())
  text)

(defstruct (repository (:constructor make-repository (&key root files by-path by-tail))
                       (:conc-name repo-))
  "One repository read whole: its ROOT, its FILES in path order, and the two
lookups an import specifier is resolved through."
  root
  (files '())
  (by-path (make-hash-table :test #'equal))
  (by-tail (make-hash-table :test #'equal)))

;;; --- the walk --------------------------------------------------------------------

(defun language-of (file)
  "The language the extension of FILE names, or NIL for a file this cell does
not read."
  (let ((type (ignore-errors (string-downcase (or (pathname-type file) "")))))
    (and type (plusp (length type)) (gethash type *language-of*))))

(defun skipped-p (directory names)
  "Whether DIRECTORY's own name is one NAMES skips."
  (let ((parts (pathname-directory directory)))
    (and parts (member (string-downcase (car (last parts))) names :test #'equal))))

(defun collect-files (root limit names)
  "The code files under ROOT, at most LIMIT, skipping the directory names
NAMES and never entering a directory twice (a symbolic link back up the
tree). => a list of (FILE . RELATIVE-PATH), in path order."
  (let ((seen (make-hash-table :test #'equal))
        (found '())
        (found-count 0))
    (labels ((visit (directory depth)
               (when (and (< found-count limit) (<= depth 24))
                 (let ((identity (ignore-errors (namestring (truename directory)))))
                   (unless (and identity (gethash identity seen))
                     (when identity (setf (gethash identity seen) t))
                     (dolist (file (sort (ignore-errors (uiop:directory-files directory))
                                         #'string< :key #'namestring))
                       (when (and (< found-count limit) (language-of file))
                         (incf found-count)
                         (push (cons file (enough-namestring file root)) found)))
                     (dolist (sub (sort (ignore-errors (uiop:subdirectories directory))
                                         #'string< :key #'namestring))
                       (unless (skipped-p sub names)
                         (visit sub (1+ depth)))))))))
      (visit root 0))
    (sort found #'string< :key #'cdr)))

(defun read-source (file limit)
  "FILE's text, at most LIMIT characters, or NIL when it does not read as
UTF-8 or at all."
  (handler-case
      (with-open-file (in file :direction :input :external-format :utf-8)
        (let* ((size (or (ignore-errors (file-length in)) limit))
               (buffer (make-string (max 0 (min size limit)))))
          (subseq buffer 0 (read-sequence buffer in))))
    (error () nil)))

(defun count-lines (text)
  "How many lines TEXT holds; 0 for a text with nothing in it."
  (if (zerop (length text)) 0 (1+ (count #\Newline text))))

;;; --- what a file asks for ---------------------------------------------------------

(defun trimmed-line (line)
  "LINE without its leading and trailing blanks."
  (string-trim '(#\Space #\Tab #\Return) line))

(defun quoted-text (line &optional (start 0))
  "The text between the first pair of quotes in LINE past START, or NIL."
  (let* ((open (position-if (lambda (character) (member character '(#\" #\')))
                             line :start (min start (length line))))
         (close (and open (position (char line open) line :start (1+ open)))))
    (and close (subseq line (1+ open) close))))

(defun python-specs (line)
  "What one line of Python asks for: its module, or the modules of its names."
  (let ((text (trimmed-line line)))
    (cond ((and (> (length text) 5) (string= "from " (subseq text 0 5)))
           (let ((at (search " import" text)))
             (and at (> at 5) (list (subseq text 5 at)))))
          ((and (> (length text) 7) (string= "import " (subseq text 0 7)))
           (loop for part in (uiop:split-string (subseq text 7) :separator ",")
                 for name = (car (uiop:split-string (trimmed-line part) :separator '(#\Space)))
                 when (and name (plusp (length name)))
                   collect name))
          (t '()))))

(defun script-specs (line)
  "What one line of JavaScript, TypeScript, Vue or Svelte asks for."
  (let ((specs '()))
    (dolist (token '(" from \"" " from '" "require(\"" "require('" "import(\"" "import('"
                     "import \"" "import '" "export \"" "export '"))
      (let ((at (search token line)))
        (when at
          (let ((spec (quoted-text line at)))
            (when spec (push spec specs))))))
    specs))

(defun go-specs (line)
  "What one line of Go asks for: an IMPORT, or a line of an import block."
  (let ((text (trimmed-line line))
        (spec (quoted-text line)))
    (if (and spec (plusp (length text))
             (or (char= #\" (char text 0)) (char= #\' (char text 0))
                 (search "import" text)))
        (list spec)
        '())))

(defun rust-specs (line)
  "What one line of Rust asks for: a MOD, or the path of a USE."
  (let ((text (trimmed-line line)))
    (cond ((and (> (length text) 5) (string= "mod " (subseq text 0 4))
                (char= #\; (char text (1- (length text)))))
           (let ((module (string-trim '(#\Space) (subseq text 4 (1- (length text))))))
             (and (plusp (length module)) (list (concatenate 'string "./" module)))))
          ((and (> (length text) 4) (string= "use " (subseq text 0 4)))
           (let* ((rest (subseq text 4))
                  (brace (search "{" rest))
                  (rest (if brace (subseq rest 0 brace) rest))
                  (path (string-trim " ;:{}" rest)))
             (and (plusp (length path)) (list path))))
          (t '()))))

(defun keyword-specs (line keywords)
  "The name LINE imports through one of KEYWORDS, or NIL."
  (loop for keyword in keywords
        for at = (search keyword line)
        when at
          do (let* ((rest (subseq line (+ at (length keyword))))
                    (star (search "*" rest))
                    (text (string-trim " ;," (if star (subseq rest 0 star) rest))))
               (return (and (plusp (length text)) (list text))))))

(defun lisp-specs (line)
  "What one line of Lisp loads: (LOAD \"x\"), or an ASDF component (:FILE \"x\")."
  (let ((text (trimmed-line line)))
    (cond ((let ((at (search "(load " text)))
             (and at (quoted-text text at)
                  (list (concatenate 'string "./" (quoted-text text at))))))
          ((let ((at (search "(:file " text)))
             (and at (quoted-text text at) (list (quoted-text text at)))))
          (t '()))))

(defun import-specs (language text)
  "The module specifiers TEXT asks for, by LANGUAGE: a best-effort reading of
each language's import forms, and nothing for a language it does not read."
  (let ((specs '()))
    (dolist (line (nlk:lines text))
      (let ((found (cond ((string= language "python") (python-specs line))
                         ((string= language "go") (go-specs line))
                         ((string= language "rust") (rust-specs line))
                         ((string= language "ruby") (keyword-specs line '("require_relative " "require ")))
                         ((string= language "lisp") (lisp-specs line))
                         ((member language '("java" "kotlin" "scala" "csharp" "elixir")
                                  :test #'string=)
                          (keyword-specs line '("import " "using " "alias ")))
                         ((member language '("javascript" "typescript" "vue" "svelte")
                                  :test #'string=)
                          (script-specs line))
                         ((member language '("c" "cpp") :test #'string=)
                          (let ((at (search "#include " line)))
                            (and at (let ((spec (quoted-text line at)))
                                      (and spec (list spec)))))))))
        (setf specs (nconc (nreverse found) specs))))
    (remove-duplicates specs :test #'equal)))

;;; --- from a specifier to a file ----------------------------------------------------

(defun normalize-path (text)
  "TEXT with ./ and ../ resolved and empty segments dropped, or NIL when it
climbs above its own root."
  (let ((parts '()))
    (dolist (part (uiop:split-string text :separator "/"))
      (cond ((or (string= part "") (string= part ".")) nil)
            ((string= part "..")
             (if parts (pop parts) (return-from normalize-path nil)))
            (t (push part parts))))
    (format nil "~{~a~^/~}" (nreverse parts))))

(defun directory-of (path)
  "The directory part of PATH, relative to the same root; \"\" at the root."
  (let ((at (position #\/ path :from-end t)))
    (if at (subseq path 0 at) "")))

(defun relative-base (spec directory)
  "SPEC resolved against DIRECTORY, both relative to one root."
  (normalize-path (if (string= directory "") spec (concatenate 'string directory "/" spec))))

(defun specifier-forms (spec)
  "The paths SPEC may stand for: as written, with dots or colons for
slashes, and each one's own tails, so a package name finds the module it
names."
  (let* ((variants (remove-duplicates (list spec
                                            (substitute #\/ #\. spec)
                                            (substitute #\/ #\: spec)
                                            (substitute #\/ #\. (substitute #\/ #\: spec)))
                                      :test #'equal))
         (forms (copy-list variants)))
    (dolist (variant variants)
      (let ((segments (uiop:split-string variant :separator "/")))
        (loop for start from 1 below (min 4 (length segments))
              for tail = (format nil "~{~a~^/~}" (nthcdr start segments))
              do (push tail forms))))
    (remove-if (lambda (form) (or (null form) (zerop (length form)))) forms)))

(defun register-file (file repository)
  "FILE into REPOSITORY's lookups: by its whole path, and by its last two to
four segments, so a package specifier finds the module it names."
  (setf (gethash (source-file-path file) (repo-by-path repository)) file)
  (let ((segments (uiop:split-string (source-file-path file) :separator "/")))
    (loop for length from 1 to (min 4 (length segments))
          for tail = (format nil "~{~a~^/~}" (last segments length))
          do (unless (gethash tail (repo-by-tail repository))
               (setf (gethash tail (repo-by-tail repository)) file))))
  file)

(defun candidate-file (base repository)
  "The file BASE names: as it stands with a module suffix, or the index file
of a directory it names."
  (or (loop for extension in *module-extensions*
            for file = (gethash (concatenate 'string base extension)
                                (repo-by-path repository))
            when file return file)
      (loop for stem in *index-stems*
            thereis (loop for extension in *module-extensions*
                          for file = (gethash (concatenate 'string base "/" stem extension)
                                              (repo-by-path repository))
                          when file return file))))

(defun resolve-import (spec from repository)
  "The file SPEC names, read from the file FROM, or NIL when the repository
does not hold it."
  (let* ((directory (directory-of (source-file-path from)))
         (spec (string-trim '(#\Space #\Tab) spec)))
    (when (plusp (length spec))
      (or (when (member (char spec 0) '(#\. #\/) :test #'char=)
            (candidate-file (relative-base spec directory) repository))
          (let ((forms (specifier-forms spec)))
            (or (loop for form in forms
                      thereis (candidate-file form repository))
                (loop for form in forms
                      thereis (or (gethash form (repo-by-tail repository))
                                  (loop for extension in *module-extensions*
                                        for file = (gethash (concatenate 'string form extension)
                                                            (repo-by-tail repository))
                                        when file return file)))))))))

;;; --- reading a repository whole ----------------------------------------------------

(defun scan-repository (root &key (max-files 2000) (ignore '()) keep-text)
  "Read the repository at ROOT: one SOURCE-FILE per code file, at most
MAX-FILES, skipping the directory names IGNORE besides the built-in set, with
every import specifier resolved to a file of the same repository. => its
REPOSITORY."
  (let* ((root (uiop:ensure-directory-pathname root))
         (names (append *skipped-directories* (mapcar #'string-downcase ignore)))
         (repository (make-repository :root root))
         (budget *text-budget*))
    (dolist (pair (collect-files root max-files names))
      (destructuring-bind (file . path) pair
        (let* ((text (read-source file *max-text-chars*))
               (language (language-of file))
               (entry (make-source-file :path path :file file :language language
                                        :lines (count-lines text) :chars (length text)
                                        :specs (import-specs language text))))
          (when (and keep-text text (<= (length text) budget))
            (setf (source-file-text entry) text)
            (decf budget (length text)))
          (push entry (repo-files repository)))))
    (setf (repo-files repository) (nreverse (repo-files repository)))
    (dolist (entry (repo-files repository))
      (register-file entry repository))
    (dolist (entry (repo-files repository))
      (setf (source-file-imports entry)
            (remove-duplicates
             (loop for spec in (source-file-specs entry)
                   for target = (resolve-import spec entry repository)
                   when (and target (not (equal (source-file-path target)
                                                 (source-file-path entry))))
                     collect target)
             :test #'equal)))
    repository))

(defun import-rank (repository)
  "The importance of every file of REPOSITORY -- a PageRank over its own
import edges -- and how many files import it. => (VALUES RANK INCOMING), two
hashes keyed by relative path."
  (let* ((files (repo-files repository))
         (total (max 1 (length files)))
         (rank (make-hash-table :test #'equal))
         (incoming (make-hash-table :test #'equal))
         (outgoing (make-hash-table :test #'equal)))
    (dolist (file files)
      (setf (gethash (source-file-path file) rank) (/ 1.0 total))
      (setf (gethash (source-file-path file) outgoing)
            (max 1 (length (source-file-imports file))))
      (dolist (target (source-file-imports file))
        (incf (gethash (source-file-path target) incoming 0))))
    (loop repeat 14
          do (let ((next (make-hash-table :test #'equal)))
               (dolist (file files)
                 (setf (gethash (source-file-path file) next) 0.15))
               (dolist (file files)
                 (let ((share (* 0.85 (/ (gethash (source-file-path file) rank)
                                         (gethash (source-file-path file) outgoing)))))
                   (dolist (target (source-file-imports file))
                     (incf (gethash (source-file-path target) next) share))))
               (setf rank next)))
    (values rank incoming)))

;;; --- the map the model reads -------------------------------------------------------

(defun clip-text (text limit)
  "TEXT, at most LIMIT characters, saying so when it was cut."
  (if (<= (length text) limit)
      text
      (concatenate 'string (subseq text 0 (max 0 (- limit 60)))
                   "\n[cut at " (princ-to-string limit) " characters]")))

(defun more-important-p (one other rank)
  "Whether ONE ranks above OTHER: the higher score first, then the longer
file, then the path, so one run reads the same twice."
  (let ((a (gethash (source-file-path one) rank))
        (b (gethash (source-file-path other) rank)))
    (cond ((> a b) t)
          ((< a b) nil)
          ((/= (source-file-lines one) (source-file-lines other))
           (> (source-file-lines one) (source-file-lines other)))
          (t (string< (source-file-path one) (source-file-path other))))))

(defun more-imported-p (one other incoming)
  "Whether ONE is imported by more files than OTHER, then by path."
  (let ((a (gethash (source-file-path one) incoming 0))
        (b (gethash (source-file-path other) incoming 0)))
    (cond ((/= a b) (> a b))
          (t (string< (source-file-path one) (source-file-path other))))))

(defun report (repository &key (top 6) (limit 40000))
  "REPOSITORY as cartograph_analyze answers it: its counts, the files that
matter and the modules everything imports. => a string."
  (multiple-value-bind (rank incoming) (import-rank repository)
    (let* ((files (repo-files repository))
           (edges (loop for file in files sum (length (source-file-imports file))))
           (ranked (sort (copy-list files)
                         (lambda (one other) (more-important-p one other rank))))
           (hubs (sort (remove-if-not (lambda (file)
                                        (plusp (gethash (source-file-path file) incoming 0)))
                                      (copy-list files))
                       (lambda (one other) (more-imported-p one other incoming)))))
      (clip-text
       (with-output-to-string (out)
         (format out "cartograph: ~d code file~:p, ~d import edge~:p resolved inside the repository~%"
                 (length files) edges)
         (when (zerop (length files))
           (format out "no code this cell reads: it ranks ~{~a~^, ~}~%"
                   (mapcar #'first *languages*)))
         (format out "ranked by importance (a PageRank over who imports whom):~%")
         (loop for file in ranked
               for index from 1 to top
               do (format out "  ~5,3f  ~6d lines  ~12a  ~a~%"
                          (gethash (source-file-path file) rank)
                          (source-file-lines file) (source-file-language file)
                          (source-file-path file)))
         (when hubs
           (format out "dependency hubs (imported by the most files):~%")
           (loop for file in hubs
                 for index from 1 to top
                 do (format out "  ~4d  ~a~%"
                            (gethash (source-file-path file) incoming 0)
                            (source-file-path file))))
         (format out "static analysis of the files on disk: nothing was run, no model was called.~%"))
       limit))))

;;; --- the excerpts a task needs -----------------------------------------------------

(defparameter *stopwords*
  '("the" "and" "for" "with" "that" "this" "these" "those" "from" "into" "onto"
    "when" "where" "which" "what" "while" "how" "why" "does" "did" "done" "its"
    "it" "is" "are" "was" "were" "be" "been" "being" "to" "of" "in" "on" "at"
    "by" "as" "or" "an" "a" "all" "any" "some" "not" "but" "you" "your" "our"
    "we" "they" "them" "their" "there" "here" "then" "than" "can" "could"
    "should" "would" "will" "just" "also" "about" "over" "under" "between"
    "use" "used" "using" "make" "made" "add" "added" "fix" "fixed" "work"
    "works" "working" "code" "file" "files" "line" "lines" "need" "needs"
    "want" "wants" "must" "may" "might" "one" "two" "new" "old" "get" "set"
    "let" "now" "out" "off" "up" "down" "well" "good" "bad" "part" "case"
    "thing" "things" "way" "still" "only" "even" "very" "much" "more" "most")
  "The words a task states itself in and nothing about: dropped before matching.")

(defun task-terms (task)
  "The words TASK asks about: three letters or more, without the grammar,
keeping an identifier whole and adding the parts a camelCase or snake_case
word is made of."
  (let ((terms '()))
    (labels ((keep (term)
               (when (and (>= (length term) 3)
                          (not (member term *stopwords* :test #'equal)))
                 (pushnew term terms :test #'equal)))
             (parts (word)
               (let ((piece (make-string-output-stream)))
                 (flet ((flush ()
                          (let ((text (get-output-stream-string piece)))
                            (keep text))))
                   (loop for character across word
                         do (cond ((member character '(#\_ #\- #\. #\/ #\:))
                                   (flush))
                                  ((and (upper-case-p character) (plusp (file-position piece)))
                                   (flush)
                                   (write-char (char-downcase character) piece))
                                  (t (write-char (char-downcase character) piece))))
                   (flush)))))
      (dolist (word (uiop:split-string (string-downcase task)
                                        :separator '(#\Space #\Tab #\Newline #\Return
                                                     #\, #\; #\( #\) #\[ #\] #\{ #\}
                                                     #\" #\' #\? #\! #\*)))
        (let ((word (string-trim "._-/" word)))
          (when (plusp (length word))
            (keep word)
            (parts word)))))
    (nreverse terms)))

(defun term-hits (text term &optional (cap 6))
  "How often TERM occurs in TEXT, counted no further than CAP times."
  (do ((start 0) (hits 0 (1+ hits)))
      ((>= hits cap) hits)
    (let ((at (search term text :start2 start)))
      (if at
          (setf start (+ at (length term)))
          (return hits)))))

(defun file-score (file terms rank)
  "How well FILE answers TERMS: its path first, its body second, and a little
for being a file the repository imports. => (VALUES SCORE MATCHED)."
  (let ((path (string-downcase (source-file-path file)))
        (text (string-downcase (or (source-file-text file) "")))
        (score 0)
        (matched '()))
    (dolist (term terms)
      (let ((in-path (term-hits path term 1))
            (in-body (term-hits text term)))
        (when (plusp (+ in-path in-body))
          (push term matched)
          (incf score (+ (* 8 in-path) (* 2 in-body))))))
    (when matched
      (incf score (round (* 12 (or (gethash (source-file-path file) rank) 0)))))
    (values score (nreverse matched))))

(defun test-file-p (file)
  "Whether FILE reads as a test of something else."
  (let ((path (string-downcase (source-file-path file))))
    (or (search "test" (string-downcase (or (pathname-name (source-file-file file)) "")))
        (search "/test" path)
        (search "/spec" path))))

(defun select-files (repository task top)
  "The files that answer TASK: the best matches by path and body, then up to
two tests that name one of them. => a list of (FILE . REASON)."
  (multiple-value-bind (rank) (import-rank repository)
    (let* ((terms (task-terms task))
           (scored '()))
      (dolist (file (repo-files repository))
        (multiple-value-bind (score matched) (file-score file terms rank)
          (when (plusp score)
            (push (list score file matched) scored))))
      (setf scored (sort scored (lambda (one other)
                                  (cond ((/= (first one) (first other))
                                         (> (first one) (first other)))
                                        (t (string< (source-file-path (second one))
                                                    (source-file-path (second other))))))))
      (let* ((chosen (subseq scored 0 (min top (length scored))))
             (selected (mapcar #'second chosen))
             (paths (mapcar #'source-file-path selected))
             (tests '()))
        (dolist (file selected)
          (let ((stem (string-downcase (or (pathname-name (source-file-file file)) ""))))
            (when (plusp (length stem))
              (dolist (candidate (repo-files repository))
                (let ((path (source-file-path candidate)))
                  (when (and (< (length tests) 2)
                             (not (member path paths :test #'equal))
                             (test-file-p candidate)
                             (search stem (string-downcase path)))
                    (push path paths)
                    (push (cons candidate (format nil "a test naming ~a"
                                                  (source-file-path file)))
                          tests)))))))
        (append (loop for entry in chosen
                      collect (cons (second entry)
                                    (format nil "matched ~{~a~^, ~}" (third entry))))
                (nreverse tests))))))

(defun line-hits (line terms)
  "How many of TERMS LINE carries, each counted once."
  (let ((text (string-downcase line)))
    (count-if (lambda (term) (plusp (term-hits text term 1))) terms)))

(defun excerpt (text terms limit)
  "TEXT as it should be shown for TERMS: at most LIMIT characters, from the
line that carries the most of them. => (VALUES EXCERPT START-LINE END-LINE
TRUNCATED-P)."
  (let* ((lines (nlk:lines text))
         (best 0)
         (best-hits 0))
    (loop for line in lines
          for index from 0
          for hits = (line-hits line terms)
          when (> hits best-hits)
            do (setf best index best-hits hits))
    (let* ((start (max 0 (- best 3)))
           (kept '())
           (room limit))
      (loop for line in (nthcdr start lines)
            while (plusp room)
            for shown = (subseq line 0 (min (length line) room))
            do (push shown kept)
               (decf room (1+ (length shown))))
      (let ((end (+ start (length kept))))
        (values (format nil "~{~a~^~%~}" (nreverse kept))
                (1+ start) (max (1+ start) end) (> end (length lines)))))))

(defun context-report (repository task &key (top 6) (limit 40000) (source-chars 8000))
  "REPOSITORY and one TASK as cartograph_context answers them: the files
chosen, each with the excerpt to read, its line range and why it was chosen."
  (let ((terms (task-terms task))
        (selection (select-files repository task top)))
    (if (null selection)
        (format nil "cartograph: no file in this repository answers ~s. Name a module, ~
                     a symbol, a file or an error string." task)
        (let* ((budget (max 400 (floor limit (max 1 (length selection)))))
               (blocks '())
               (tokens 0))
          (dolist (choice selection)
            (destructuring-bind (file . reason) choice
              (multiple-value-bind (text start end truncated)
                  (excerpt (or (source-file-text file) "") terms (min source-chars budget))
                (incf tokens (ceiling (length text) 4))
                (push (format nil "  ~a:~d-~d~:[~; (truncated)~]  ~a~%~{~a~%~}"
                              (source-file-path file) start end truncated reason
                              (mapcar (lambda (line) (concatenate 'string "    " line))
                                      (nlk:lines text)))
                      blocks))))
          (clip-text
           (format nil "cartograph context for ~s: ~d of ~d files, about ~d tokens~%~{~a~}~
                        static lexical matching of the files on disk: verify the source before editing.~%"
                   task (length selection) (length (repo-files repository)) tokens
                   (nreverse blocks))
           limit)))))

;;; --- the tools the model calls -----------------------------------------------------

(nlk:define-contract analyze-call
  (:doc "Map a local repository: the files that matter, ranked, and the modules everything imports. Static analysis of the files on disk -- nothing is run and nothing leaves the machine. Use it before choosing what to read, or when a change's blast radius matters.")
  ("repo" :text :required t
          :doc "absolute path of a local repository or source directory")
  ("top" :integer :default 6
         :doc "how many ranked files and dependency hubs to list, 1 to 20"))

(nlk:define-contract context-call
  (:doc "The source excerpts a coding task needs from a local repository: the files whose path or body carries the task's own terms, plus the tests that name them, each with its line range. Lexical matching only -- verify the source before editing.")
  ("repo" :text :required t
          :doc "absolute path of a local repository or source directory")
  ("task" :text :required t
          :doc "the task in concrete terms: module, symbol, file and error names work; vague prose does not")
  ("top" :integer :default 6
         :doc "how many files to select, 1 to 20"))

(defun refusal-text (condition)
  "CONDITION as the text a failed tool call answers."
  (format nil "ERROR: CARTOGRAPH-ERROR: ~a" condition))

(defun analyze-tool (call)
  "The cartograph_analyze tool: the map of CALL's repository."
  (handler-case (analyze (analyze-call-repo call) :top (analyze-call-top call))
    (cartograph-error (condition) (refusal-text condition))))

(defun context-tool (call)
  "The cartograph_context tool: the excerpts CALL's task needs."
  (handler-case (context (context-call-repo call) (context-call-task call)
                         :top (context-call-top call))
    (cartograph-error (condition) (refusal-text condition))))

;;; --- /cartograph ------------------------------------------------------------------

(defun path-like-p (text)
  "Whether TEXT opens like a path rather than with a task's first word."
  (and (plusp (length text))
       (or (member (char text 0) '(#\/ #\~ #\.) :test #'char=)
           (and (> (length text) 1) (char= #\: (char text 1))))))

(defun session-repository (session-id)
  "Where the session works: its own directory, else the one Nodecode runs in."
  (or (ignore-errors (nlk:find-session-cwd session-id))
      (ignore-errors (uiop:getcwd))
      (fail "no directory to map: type one, /cartograph /path/to/repo")))

(defun run-slash (args session-id)
  "/cartograph [PATH] [TASK]: map the repository at PATH -- the session's own
directory when none is typed -- or the source excerpts TASK needs."
  (let* ((text (nlk:trimmed (or args "")))
         (split (position-if (lambda (character) (member character '(#\Space #\Tab))) text))
         (head (subseq text 0 split))
         (rest (if split (nlk:trimmed (subseq text split)) "")))
    (cond ((zerop (length text)) (analyze (session-repository session-id)))
          ((path-like-p head)
           (if (zerop (length rest))
               (analyze head)
               (context head rest)))
          (t (context (session-repository session-id) text)))))

;;; --- what a repository may be ------------------------------------------------------

(defun permitted-root (directory)
  "DIRECTORY as the repository the settings allow, or a refusal saying why."
  (let ((roots (setting :allowed-roots)))
    (when (eq roots :any) (return-from permitted-root directory))
    (when (null roots)
      (fail "allowed_roots is an empty list, so this cell may read no repository: ~
             name one in the cartograph section of the configuration"))
    (dolist (root roots)
      (let ((root (ignore-errors (truename (uiop:ensure-directory-pathname root)))))
        (when (and root (or (uiop:pathname-equal directory root)
                            (uiop:subpathp directory root)))
          (return-from permitted-root directory))))
    (fail "~a is outside allowed_roots (~{~a~^, ~})" (namestring directory) roots)))

(defun repository-at (path)
  "PATH as a directory this cell may read, or a refusal saying why not."
  (unless (and (stringp path) (plusp (length (nlk:trimmed path))) (<= (length path) 4096))
    (fail "repo must be an absolute local directory path"))
  (when (search "://" path)
    (fail "repo must be an absolute local directory path; URLs are not supported"))
  (let ((probe (ignore-errors (probe-file path))))
    (cond ((null probe) (fail "there is no directory at ~a" path))
          ((not (uiop:directory-exists-p probe)) (fail "~a is a file, not a directory" path))))
  (let ((directory (ignore-errors (truename (uiop:ensure-directory-pathname path)))))
    (unless directory (fail "~a cannot be resolved to a directory" path))
    (permitted-root directory)))

;;; --- the verbs and their settings --------------------------------------------------

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's VALUES, and allowed_roots
read from TABLE so an absent list (any directory) and an empty one (none) are
the different states they are."
  (multiple-value-bind (roots present) (nlk:section-value table "allowed_roots")
    (declare (ignore roots))
    (list :allowed-roots (if present (nlk:config-string-list table "allowed_roots") :any)
          :ignore (getf values :ignore)
          :max-files (getf values :max-files)
          :source-chars (getf values :source-chars)
          :max-output-chars (getf values :max-output-chars))))

(define-verb analyze (repo &key (top 6))
  "Map REPO: the files that matter and the modules everything imports. => a string."
  (let ((repository (scan-repository (repository-at repo)
                                     :max-files (setting :max-files)
                                     :ignore (setting :ignore))))
    (report repository :top (max 1 (min 20 top)) :limit (setting :max-output-chars))))

(define-verb context (repo task &key (top 6))
  "The source excerpts REPO holds for TASK, with their line ranges. => a string."
  (let ((repository (scan-repository (repository-at repo) :max-files (setting :max-files)
                                                           :ignore (setting :ignore)
                                                           :keep-text t)))
    (context-report repository (or task "")
                    :top (max 1 (min 20 top))
                    :limit (setting :max-output-chars)
                    :source-chars (setting :source-chars))))

(nle:define-cell cartograph
  (:section ("cartograph")
    (:guide "no key and nothing to install: it reads the repositories you point it at, locally. allowed_roots narrows the directories it may read; max_files, source_chars and max_output_chars bound one answer")
    ("allowed_roots" :list
     :doc "absolute directories cartograph may read; absent leaves every local directory open, an empty list allows none")
    ("ignore" :list
     :doc "extra directory names to skip besides the built-in set (.git, node_modules, dist, target and the like)")
    ("max_files" :integer :default 2000 :min 10
     :doc "code files read before the walk stops")
    ("source_chars" :integer :default 8000 :min 200
     :doc "the longest excerpt of one file, in characters")
    ("max_output_chars" :integer :default 40000 :min 4000
     :doc "the ceiling on one answer, in characters"))
  (:settings #'read-settings)
  (:tool "cartograph_analyze" (nlk:contract 'analyze-call) 'analyze-tool)
  (:tool "cartograph_context" (nlk:contract 'context-call) 'context-tool)
  (:help :cartograph "cartograph: the files that matter in a local repository, and the excerpts a task needs - (help :cartograph)" +primer+)
  (:command "cartograph" 'run-slash
            :description "Map a local repository, or the source a task needs"))
