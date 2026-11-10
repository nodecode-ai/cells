;;;; cell.lisp --- duplicates, contradictions and dangling references in a home.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A session is told what to do by files, not by code: the home's AGENTS.md,
;;;; SYSTEM.md, SOUL.md and config.jsonc, the project's AGENTS.md, CLAUDE.md,
;;;; .cursorrules and README.md up its folders, and any SKILL.md under the
;;;; home's skills/. Nothing checks that those files agree with each other, and
;;;; a contradiction between two of them is obeyed by whichever the model read
;;;; last. This cell reads them and reports three things, in the original's
;;;; words: near-duplicate directives (the same line in two files, frontmatter
;;;; and metadata lines excluded), contradictions (one file asks for approval
;;;; where another forbids asking, downgraded to a warning when the two sides
;;;; name different scopes), and stale references (a path in an instruction
;;;; file that resolves nowhere, the URL, link, code-block and example
;;;; contexts skipped).
;;;;
;;;; Read-only: nothing here writes, repairs or restarts. (northstar:audit)
;;;; answers the report through eval, /northstar prints it, and the model
;;;; calls northstar_audit.

(in-package #:nodecode-northstar)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Northstar audits the files that tell this session what to do, through the
nodecode-northstar cell: Lisp functions in the northstar: package, called
through eval, each answering a string; a refusal is ERROR: NORTHSTAR-ERROR.
  (northstar:audit)                     every scope below
  (northstar:audit :scope \"project\")    one scope: full, home, project, skills
  (northstar:audit :scope \"home\" :output-format \"json\")
  (northstar:status)                    what the audit reads, and its limit
It reads the home's AGENTS.md, SYSTEM.md, SOUL.md and config.jsonc, the
project's AGENTS.md, CLAUDE.md, .cursorrules and README.md and the same names
in the folders above it, and any SKILL.md under the home's skills/. It reports
near-duplicate directives (the same instruction written twice, in two files), a
contradiction (one file asking for approval where another forbids it; a warning
when the two sides name different scopes), and a stale reference (a path that
resolves nowhere). Read-only: nothing here edits an instruction file, and the
operator runs the same report as /northstar, /northstar project, or with no
argument everything."
  "What (help :northstar) answers while the cell runs.")

;;; --- what this audit knows -----------------------------------------------------

(defparameter *scopes* '("full" "home" "project" "skills")
  "Every scope this cell knows, in reading order: what a bare /northstar runs.")

(defparameter *home-names* '("AGENTS.md" "SYSTEM.md" "SOUL.md" "config.jsonc")
  "The files in the Nodecode home a session is told to obey, each read when
it is there: the operator's rules, the persona, a channel's soul, the config.")

(defparameter *project-names* '("AGENTS.md" "CLAUDE.md" ".cursorrules" "README.md")
  "The files a project leaves for the agents that work in it, in a folder and
in each folder above it; northstar.files adds to this list.")

(defparameter *archived-names* '(".archive" ".curator_backups" ".backup" ".backups"
                                 "archive" "archived" "old" "deprecated")
  "Folder names whose contents are old material, never audited.")

(defparameter *metadata-keys*
  '("name" "version" "description" "author" "license" "homepage" "tags" "category"
    "platforms" "platform" "os" "date" "created" "modified" "updated" "status"
    "type" "kind" "id" "slug" "icon" "logo" "image" "cover" "weight" "order"
    "priority" "sort" "enabled" "active" "visible" "hidden" "dependencies"
    "requires" "provides" "aliases" "synonyms")
  "A `key: value' line whose key is one of these is frontmatter, not a
directive: two files may describe themselves the same way.")

(defparameter *scope-words*
  '("production" "staging" "testing" "test" "development" "local" "release"
    "deploy" "pipeline" "config file" "new file" "autonomous" "routine")
  "The words that say which context a directive governs. Two sides of a
contradiction that name different ones may both be right.")

(defparameter *contradiction-pairs*
  '((("ask for confirmation" "require confirmation" "require approval"
      "request approval" "ask before" "ask permission" "confirm before")
     ("without asking" "without confirmation" "without approval"
      "without permission" "without interrupting" "skip confirmation"))
    (("never edit" "do not edit" "never modify" "do not modify"
      "never write" "do not write")
     ("automatically edit" "automatically modify" "automatically write"))
    (("always confirm" "must confirm" "always verify" "must verify")
     ("skip verification" "without verifying"))
    (("require human approval" "require user approval" "need user approval")
     ("autonomous" "without human" "without user")))
  "Each pair is what one directive allows and what another forbids: a file
holding both, or two files holding one each, contradicts itself. Words, not
regular expressions, so a reader can see what fires.")

(defparameter *path-types* '("md" "yaml" "yml" "json" "jsonc" "toml" "py" "txt" "csv")
  "File extensions a reference in an instruction file names, as the original
listed them: a path to anything else is not a dependency worth reporting.")

(defparameter *example-words*
  '("example" "e.g." "such as" "for instance" "see " "read " "guide" "reference")
  "Words that make a path in an instruction file an illustration rather than
a dependency; the path itself is still reported when it does exist nowhere.")

;;; --- the finding ---------------------------------------------------------------

(defstruct (finding (:constructor make-finding
                                   (category severity file
                                    &key line (evidence "") (explanation "")
                                    (suggestion "") (confidence "medium")))
                    (:copier nil))

  (category "duplicate" :type string)
  (severity "warning" :type string)
  (confidence "medium" :type string)
  (file "" :type string)
  (line nil :type (or null integer))
  (evidence "" :type string)
  (explanation "" :type string)
  (suggestion "" :type string))

;;; --- a source: one file, its text, what kind it is -------------------------------

(defstruct (source (:constructor make-source (path text kind)) (:copier nil))
  (path #p"" :type pathname)
  (text "" :type string)
  (kind "instruction" :type string))

(defun split-lines (text)
  "TEXT's lines, without their newlines."
  (loop with start = 0
        for end = (position #\Newline text :start start)
        collect (subseq text start (or end (length text)))
        while end
        do (setf start (1+ end))))

(defun source-lines (source)
  (split-lines (source-text source)))

(defun kind-of (path)
  "What sort of source PATH is, by its name: a config, a skill, an instruction."
  (let ((name (string-downcase (or (pathname-name path) "")))
        (type (string-downcase (or (pathname-type path) ""))))
    (cond ((member type '("json" "jsonc" "yaml" "yml" "toml") :test #'string=) "config")
          ((string= name "skill") "skill")
          (t "instruction"))))

(defun read-source (path kind)
  "PATH read as a source of KIND, or NIL when it is missing or blank."
  (let ((text (ignore-errors (nlk:read-text path))))
    (when (and (stringp text) (plusp (length (nlk:trimmed text))))
      (make-source (pathname path) text kind))))

(defun archived-p (path)
  "PATH under a folder whose name says the material is old."
  (some (lambda (part)
          (and (stringp part)
               (member (string-downcase part) *archived-names* :test #'string=)))
        (rest (pathname-directory path))))

(defun files-in (directory names)
  "The sources DIRECTORY holds under NAMES, each read once, in NAMES' order."
  (loop for name in names
        for path = (merge-pathnames name directory)
        for source = (and (uiop:file-exists-p path) (read-source path (kind-of path)))
        when source collect source))


(defun directories-up (directory count)
  "DIRECTORY and COUNT folders above it, nearest first, the root last once."
  (loop with seen = '()
        for dir = (uiop:ensure-directory-pathname directory)
          then (uiop:pathname-parent-directory-pathname dir)
        repeat (1+ count)
        while (and dir (not (member (namestring dir) seen :test #'equal)))
        do (push (namestring dir) seen)
        collect dir))

(defun walk-files (directory)
  "Every file under DIRECTORY, recursively, skipping an archived folder."
  (append (ignore-errors (uiop:directory-files directory))
          (loop for sub in (ignore-errors (uiop:subdirectories directory))
                unless (archived-p sub)
                  append (walk-files sub))))

(defun skill-sources (home)
  "Every SKILL.md under the home's skills/, archived folders skipped."
  (let ((directory (merge-pathnames "skills/" home)))
    (when (uiop:directory-exists-p directory)
      (loop for file in (walk-files directory)
            for source = (and (string-equal (or (pathname-name file) "") "skill")
                              (read-source file "skill"))
            when source collect source))))


(defun extra-sources (home)
  "Every file of a text sort in each of northstar.extra_dirs, a relative
name read under the home."
  (loop for name in (setting :extra-dirs)
        for directory = (merge-pathnames (uiop:ensure-directory-pathname name) home)
        when (uiop:directory-exists-p directory)
          append (loop for file in (uiop:directory-files directory)
                       for source = (and (member (string-downcase (or (pathname-type file) ""))
                                                 '("md" "txt" "json" "jsonc" "yaml" "yml" "toml")
                                                 :test #'string=)
                                         (read-source file (kind-of file)))
                       when source collect source)))


(defun dedupe-sources (sources)
  "SOURCES with each file once, by the name it resolves to, archived ones out."
  (let ((seen (make-hash-table :test #'equal)) (kept '()))
    (dolist (source sources (nreverse kept))
      (let ((key (or (ignore-errors (namestring (truename (source-path source))))
                     (namestring (source-path source)))))
        (unless (or (gethash key seen) (archived-p (source-path source)))
          (setf (gethash key seen) t)
          (push source kept))))))

(defun discover-sources (&key (scope "full") directory)
  "Every source SCOPE names under DIRECTORY (the project folder, the process
one when NIL) and the home, each once."
  (let* ((home (nlk:home))
         (project (or directory (uiop:getcwd)))
         (sources '()))
    (when (member scope '("full" "home") :test #'string=)
      (setf sources (append (files-in home *home-names*) sources)))
    (when (member scope '("full" "project") :test #'string=)
      (dolist (dir (directories-up project (setting :max-parents)))
        (setf sources (append (files-in dir (append *project-names* (setting :files)))
                              sources))))
    (when (member scope '("full" "skills") :test #'string=)
      (setf sources (append (skill-sources home) sources)))
    (dedupe-sources (append sources (extra-sources home)))))

;;; --- duplicates ------------------------------------------------------------------

(defun metadata-line-p (line)
  "LINE is a `key: value' frontmatter line, whose key is a metadata field."
  (let ((colon (position #\: line)))
    (and colon
         (let ((key (string-downcase (nlk:trimmed (subseq line 0 colon)))))
           (and (plusp (length key))
                (member key *metadata-keys* :test #'string=))))))

(defun directive-key (line)
  "LINE as a comparison key -- its words, lowercased, punctuation dropped --
or NIL when the line is too short to be a directive."
  (let ((words '()) (start nil))
    (loop for i from 0 below (length line)
          for ch = (char line i)
          do (if (alphanumericp ch)
                 (unless start (setf start i))
                 (when start
                   (push (string-downcase (subseq line start i)) words)
                   (setf start nil)))
          finally (when start (push (string-downcase (subseq line start)) words)))
    (let ((key (format nil "~{~a~^ ~}" (nreverse words))))
      (when (>= (length key) 20)
        (subseq key 0 (min 100 (length key)))))))

(defun frontmatter-end (lines)
  "How many of LINES the YAML frontmatter at the head takes, 0 when none."
  (let ((first (position-if (lambda (line) (plusp (length (nlk:trimmed line)))) lines)))
    (if (and first (string= (nlk:trimmed (nth first lines)) "---"))
        (or (loop for i from (1+ first) below (length lines)
                  when (string= (nlk:trimmed (nth i lines)) "---")
                    return (1+ i))
            (length lines))
        0)))

(defun duplicate-findings (sources)
  "One finding per directive line that two different files both carry."
  (let ((table (make-hash-table :test #'equal)) (order '()))
    (dolist (source sources)
      (let ((lines (source-lines source)))
        (loop for i from (frontmatter-end lines) below (length lines)
              for line = (nth i lines)
              for key = (and (not (metadata-line-p line)) (directive-key line))
              when key
                do (unless (gethash key table) (push key order))
                   (push (cons (source-path source) (1+ i)) (gethash key table)))))
    (loop for key in (nreverse order)
          for hits = (reverse (gethash key table))
          for files = (remove-duplicates (mapcar #'car hits) :test #'equal)
          when (>= (length files) 2)
            collect (let ((first (first hits)))
                      (make-finding
                       "duplicate" "warning" (namestring (first first)) :line (rest first)
                       :evidence key
                       :explanation (format nil "the same directive is written in ~d files: ~{~a~^, ~}"
                                            (length files)
                                            (mapcar (lambda (path) (file-namestring path)) files))
                       :suggestion "keep one copy and let the others point at it")))))

;;; --- contradictions ---------------------------------------------------------------

(defun phrase-hit (text phrases)
  "The first place TEXT holds one of PHRASES => (VALUES PHRASE POSITION), or NIL."
  (let ((best nil) (at nil))
    (dolist (phrase phrases)
      (let ((found (search phrase text :test #'char-equal)))
        (when (and found (or (null at) (< found at)))
          (setf best phrase at found))))
    (values best at)))

(defun line-number-at (text position)
  (1+ (count #\Newline text :end position)))

(defun line-at (text position)
  "The line of TEXT holding POSITION, lowercased."
  (let* ((start (1+ (or (position #\Newline text :end position :from-end t) -1)))
         (end (or (position #\Newline text :start position) (length text))))
    (string-downcase (subseq text start end))))

(defun scopes-named (line)
  "The scope words LINE names."
  (remove-if-not (lambda (word) (search word line :test #'char-equal)) *scope-words*))

(defun scopes-differ-p (one other)
  "ONE and OTHER name scopes, and no scope they name is shared."
  (and one other (null (intersection one other :test #'string=))))

(defun contradiction-finding (file line evidence explanation &key (divided nil) (cross nil))
  "One contradiction, an error unless DIVIDED says the two sides name
different scopes; CROSS says it was found between two files."
  (make-finding
   "contradiction" (if divided "warning" "error") file :line line
   :confidence (if divided "low" "medium")
   :evidence evidence
   :explanation (if divided
                    (format nil "possible contradiction, since ~a" explanation)
                    (format nil "contradiction: ~a" explanation))
   :suggestion "choose one directive and remove the other, or say which scope each governs"))

(defun file-contradictions (source)
  "Every place SOURCE's own text asks for what it forbids."
  (let ((text (source-text source)) (found '()))
    (dolist (pair *contradiction-pairs*)
      (multiple-value-bind (allow allow-at) (phrase-hit text (first pair))
        (multiple-value-bind (deny deny-at) (phrase-hit text (second pair))
          (when (and allow deny)
            (let* ((allowed (scopes-named (line-at text allow-at)))
                   (forbidden (scopes-named (line-at text deny-at)))
                   (divided (scopes-differ-p allowed forbidden)))
              (push (contradiction-finding
                     (namestring (source-path source))
                     (line-number-at text allow-at)
                     (format nil "~s vs ~s" allow deny)
                     (if divided
                         "the two sides name different scopes and both may hold"
                         "one file asks for what it forbids elsewhere")
                     :divided divided)
                    found))))))
    (nreverse found)))

(defun cross-finding (allowing allow allow-at forbidding deny deny-at)
  "One contradiction between ALLOWING and FORBIDDING, said the same way a
single file's is."
  (let* ((allowed (scopes-named (line-at (source-text allowing) allow-at)))
         (forbidden (scopes-named (line-at (source-text forbidding) deny-at)))
         (divided (scopes-differ-p allowed forbidden))
         (one (file-namestring (source-path allowing)))
         (other (file-namestring (source-path forbidding))))
    (contradiction-finding
     (namestring (source-path allowing))
     (line-number-at (source-text allowing) allow-at)
     (format nil "~s in ~a vs ~s in ~a" allow one deny other)
     (if divided
         (format nil "~a and ~a name different scopes and both may hold" one other)
         (format nil "~a allows what ~a forbids" one other))
     :divided divided :cross t)))

(defun pair-contradictions (one other)
  "Every contradiction between TWO instruction sources, in either direction."
  (let ((found '()) (a (source-text one)) (b (source-text other)))
    (dolist (pair *contradiction-pairs*)
      (multiple-value-bind (allow allow-at) (phrase-hit a (first pair))
        (multiple-value-bind (deny deny-at) (phrase-hit b (second pair))
          (when (and allow deny)
            (push (cross-finding one allow allow-at other deny deny-at) found))))
      (multiple-value-bind (allow allow-at) (phrase-hit b (first pair))
        (multiple-value-bind (deny deny-at) (phrase-hit a (second pair))
          (when (and allow deny)
            (push (cross-finding other allow allow-at one deny deny-at) found)))))
    (nreverse found)))

(defun contradiction-findings (sources)
  "Every contradiction inside one instruction file and between two of them."
  (let ((instructions (remove-if-not
                       (lambda (source) (string= "instruction" (source-kind source)))
                       sources)))
    (append (loop for source in instructions append (file-contradictions source))
            (loop for (one . rest) on instructions
                  append (loop for other in rest append (pair-contradictions one other))))))

;;; --- stale references -------------------------------------------------------------

(defun extension-of (token)
  (let ((dot (position #\. token :from-end t)))
    (and dot (< dot (1- (length token)))
         (string-downcase (subseq token (1+ dot))))))

(defun path-start-p (line i)
  "LINE has a path token starting at I: a slash, or a drive letter and its
separator."
  (let ((ch (char line i)))
    (or (char= ch #\/)
        (and (alpha-char-p ch)
             (< (+ i 2) (length line))
             (char= (char line (1+ i)) #\:)
             (find (char line (+ i 2)) "/\\" :test #'char=)))))

(defun path-token (line i)
  "The path token LINE holds from I, when it ends in an extension this audit
reads, or NIL."
  (let ((end i))
    (loop while (and (< end (length line))
                     (let ((ch (char line end)))
                       (or (alphanumericp ch) (find ch "-._/\\" :test #'char=))))
          do (incf end))
    (let ((token (subseq line i end)))
      (when (member (extension-of token) *path-types* :test #'string=)
        token))))

(defun url-before-p (line i)
  "The token at I sits inside a URL, so it names no local file."
  (let ((before (string-downcase (subseq line (max 0 (- i 8)) i))))
    (or (search "http" before)
        (and (plusp i) (char= (char line (1- i)) #\/) (search ":/" before)))))

(defun fence-line-p (line)
  "LINE is a fenced code block's own marker."
  (let ((trimmed (nlk:trimmed line)))
    (and (>= (length trimmed) 3) (string= (subseq trimmed 0 3) "```"))))

(defun example-line-p (line)
  "LINE is prose about an example or a document, not a dependency."
  (let ((lower (string-downcase line)))
    (some (lambda (word) (search word lower)) *example-words*)))

(defun stale-candidates (token source home)
  "Where a reference TOKEN in SOURCE might resolve: as written, beside the
file that names it, and under the home."
  (let ((relative (string-left-trim "/\\" token)))
    (remove-duplicates
     (list token
           (merge-pathnames relative
                             (uiop:pathname-directory-pathname (source-path source)))
           (merge-pathnames relative home))
     :test #'equal)))

(defun scan-tokens (source line number home)
  "Every stale reference on LINE of SOURCE, as findings."
  (let ((found '()))
    (loop with i = 0
          while (< i (length line))
          do (if (path-start-p line i)
                 (let ((token (path-token line i)))
                   (cond (token
                          (when (and (not (url-before-p line i))
                                     (not (and (plusp i) (char= (char line (1- i)) #\()))
                                     (not (oddp (count #\` (subseq line 0 i))))
                                     (not (example-line-p line))
                                     (not (some #'probe-file
                                                (stale-candidates token source home))))
                            (push (make-finding
                                   "stale_reference" "warning"
                                   (namestring (source-path source)) :line number
                                   :confidence "low"
                                   :evidence (format nil "reference to ~s" token)
                                   :explanation "no such file: what this line names is not on disk"
                                   :suggestion "create the file, correct the path, or write it as an example")
                                  found))
                          (incf i (length token)))
                         (t (incf i))))
                 (incf i)))
    (nreverse found)))

(defun source-stale-findings (source home)
  "Every path SOURCE names that resolves nowhere, code blocks and examples aside."
  (let ((fenced nil) (found '()))
    (loop for line in (source-lines source)
          for number from 1
          do (if (fence-line-p line)
                 (setf fenced (not fenced))
                 (unless fenced
                   (setf found (append found (scan-tokens source line number home))))))
    found))


;;; --- the report -----------------------------------------------------------------

(defun severity-count (findings severity)
  (count severity findings :key #'finding-severity :test #'string=))

(defun finding-locator (finding)
  (format nil "~a~@[:~d~]" (finding-file finding) (finding-line finding)))

(defun text-report (sources findings cap)
  "FINDINGS over SOURCES as the report /northstar prints, the first CAP of
them, and the words that say what is left."
  (let ((shown (if (> (length findings) cap) (subseq findings 0 cap) findings)))
    (with-output-to-string (out)
      (format out "Northstar coherence audit: ~d error~:p, ~d warning~:p across ~d source~:p~@[ (the first ~d finding~:p shown)~]"
              (severity-count findings "error") (severity-count findings "warning")
              (length sources) (and (> (length findings) cap) cap))
      (dolist (source sources)
        (format out "~%  ~a (~a, ~d lines)"
                (namestring (source-path source)) (source-kind source)
                (length (source-lines source))))
      (if (null findings)
          (format out "~%Nothing to report: no directive contradicts, repeats or points at a missing file.")
          (dolist (item shown)
            (format out "~%  [~(~a~)] ~a · ~a~%      evidence: ~a~%      ~a~%      -> ~a"
                    (finding-severity item) (finding-category item)
                    (finding-locator item) (finding-evidence item)
                    (finding-explanation item) (finding-suggestion item)))))))

(defun finding-object (finding)
  "FINDING as the JSON object the tool's json format answers with."
  (let ((object (nlk:json-object
                 "category" (finding-category finding)
                 "severity" (finding-severity finding)
                 "confidence" (finding-confidence finding)
                 "source_file" (finding-file finding)
                 "evidence" (finding-evidence finding)
                 "explanation" (finding-explanation finding)
                 "suggestion" (finding-suggestion finding))))
    (when (finding-line finding)
      (setf (gethash "source_line" object) (finding-line finding)))
    object))


(defun json-report (sources findings)
  "The same report as one JSON object, for a caller that reads fields."
  (nlk:encode-json-object
   (nlk:json-object
    "status" "ok"
    "sources_inspected"
    (coerce (loop for source in sources
                  collect (nlk:json-object
                           "path" (namestring (source-path source))
                           "source_type" (source-kind source)
                           "line_count" (length (source-lines source))))
            'vector)
    "findings" (coerce (mapcar #'finding-object findings) 'vector)
    "summary" (nlk:json-object
               "total_findings" (length findings)
               "errors" (severity-count findings "error")
               "warnings" (severity-count findings "warning")
               "info" (severity-count findings "info"))
    "checks_skipped"
    (vector "tool availability (this cell does not read Nodecode's tool list)"
            "model reference validation (no provider is asked)"))))

;;; --- the verbs --------------------------------------------------------------------

(defun checked-scope (scope)
  "SCOPE as one this cell knows, or a refusal naming the ones it does."
  (let ((scope (string-downcase (nlk:trimmed (or scope "full")))))
    (if (member scope *scopes* :test #'string=)
        scope
        (fail "~s is no scope of this cell: ~{~a~^, ~}" scope *scopes*))))

(defun collected-findings (sources)
  "Every check's findings over SOURCES, duplicates first."
  (append (duplicate-findings sources)
          (contradiction-findings sources)
          (loop for source in sources append (source-stale-findings source (nlk:home)))))

(define-verb audit (&key (scope "full") (output-format "text") directory)
  "Audit the instruction files SCOPE names -- full, home, project or skills --
reading the project files under DIRECTORY, the process folder when NIL. =>
the report as text, or as JSON when OUTPUT-FORMAT is \"json\"."
  (let* ((scope (checked-scope scope))
         (sources (discover-sources :scope scope :directory directory))
         (findings (collected-findings sources)))
    (if (string-equal (or output-format "text") "json")
        (json-report sources findings)
        (text-report sources findings (setting :max-findings)))))

(define-verb status ()
  "What this audit reads and how long one report may be, as one line."
  (format nil "northstar: scopes ~{~a~^, ~}; ~d extra file name~:p and ~d extra folder~:p besides the standard ones; at most ~d finding~:p per report"
          *scopes* (length (setting :files)) (length (setting :extra-dirs))
          (setting :max-findings)))

;;; --- the tool ----------------------------------------------------------------------

(nlk:define-contract northstar-request
  (:doc "Audit the instruction files a Nodecode session obeys -- the home's AGENTS.md, SYSTEM.md, SOUL.md and config.jsonc, the project's AGENTS.md, CLAUDE.md, .cursorrules and README.md, and any SKILL.md under the home's skills/ -- and report directives that contradict each other, the same directive written in two files, and paths that resolve nowhere. Read-only: nothing is edited.")
  ("scope" :string :required t :options '("full" "home" "project" "skills")
           :doc "what to read: `full' every folder and the home, `home' the Nodecode home's own instruction files, `project' the project folder's and the folders above it, `skills' the SKILL.md files under the home")
  ("output_format" :string :default "text" :options '("text" "json")
                   :doc "`text' for the report a person reads, `json' for one a caller reads fields from"))

(defun run-audit (request)
  "The model's northstar_audit: REQUEST's scope as the report's text."
  (audit :scope (northstar-request-scope request)
         :output-format (northstar-request-output-format request)))

;;; --- /northstar --------------------------------------------------------------------

(defun run-slash (args session-id)
  "/northstar typed alone: every scope's report. /northstar SCOPE: that one.
/northstar status: what this audit reads. /northstar json: the report as
JSON. => the answer's text."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond ((zerop (length word)) (audit))
          ((string= word "status") (status))
          ((string= word "json") (audit :output-format "json"))
          (t (audit :scope word)))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell northstar
  (:section ("northstar")
    (:guide "nothing here is required; files adds instruction files to read beside the standard ones, extra_dirs more folders, max_parents how far above the project folder to look, max_findings how long one report may be")
    ("files" :list :default '()
              :doc "more file names to read beside AGENTS.md, CLAUDE.md, .cursorrules and README.md, in the project folder and the folders above it")
    ("extra_dirs" :list :default '()
                  :doc "more folders to read, every file of a text sort in one; a relative name is under the home")
    ("max_parents" :integer :default 4 :min 0
                   :doc "how many folders above the project folder to read instruction files from")
    ("max_findings" :integer :default 40 :min 1
                    :doc "how many findings one report carries before it says how many it left out"))
  (:tool "northstar_audit" (nlk:contract 'northstar-request) 'run-audit)
  (:help :northstar "northstar:audit and northstar_audit: instruction files that contradict, repeat or dangle" +primer+)
  (:command "northstar" 'run-slash
            :catches 'northstar-error
            :argument-hint "full | home | project | skills | status"
            :description "Audit the instruction files a session obeys for contradictions"))

