;;;; home.lisp --- which files to look at, and what the walk makes of them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The manifest (world.lisp) says WHERE a home is; the shapes
;;;; (shape.lisp) say what a file HOLDS. This is the third and last piece:
;;;; the conventions that say which of a home's files are worth opening,
;;;; and it is conventions rather than a per-world file list on purpose —
;;;; a table of paths per world is a reader per world wearing different
;;;; clothes, and it misses exactly what a world table always misses. On
;;;; the box this was written against, a convention walk found per-project
;;;; MCP servers inside ~/.claude.json, provider keys in pi's models.json,
;;;; Cline's cron sqlite and Windsurf's memories — none of which anybody
;;;; would have written a row for.
;;;;
;;;; The conventions:
;;;;
;;;;   a config file     .json .jsonc .json5 .yaml .yml .toml, or an
;;;;                     environment file — read whole, classified whole
;;;;   a skill           a directory holding SKILL.md, under a directory
;;;;                     whose name starts with `skills'
;;;;   instructions      markdown named the way the ecosystem names it
;;;;                     (AGENTS, CLAUDE, GEMINI, SOUL, IDENTITY, ...)
;;;;   a memory entry    a .md under a directory named memory or memories,
;;;;                     or one of the named memory files: one named entry
;;;;                     when its frontmatter names it, else split on its
;;;;                     own separator lines; a MEMORY.md of links is the
;;;;                     home's hot index instead
;;;;   a use ledger      usage.jsonl and sightings.jsonl, line by line
;;;;   a conversation    sessions.lisp, given the roots
;;;;
;;;; and the bounds, because this runs on a first frame: a depth, a file
;;;; count, a byte cap per file, and the directories nobody's settings live
;;;; in. Everything read and skipped is counted, and what was seen and not
;;;; understood comes back as the UNMAPPED list — the backlog, and the
;;;; pasteable half of a bug report.

(in-package #:nodecode-import-kit)

(nlk:access (named nlk::agent-world) (world nlk::agent-world))

;;; --- the bounds -------------------------------------------------------------

;;; Five reaches workspace/skills/<name>/SKILL.md and stops.
(defparameter +walk-file-depth+ 5
  "How far below a root the walk goes.")

(defparameter +text-byte-cap+ (* 512 1024)
  "The largest instruction or memory file the walk reads.")

(defparameter +skip-directories+
  '("node_modules" ".git" ".venv" "venv" "__pycache__" "cache" "caches" "tmp" ".tmp"
    "temp" "logs" "log" "bin" "dist" "build" "backups" "backup" "snapshot" "snapshots"
    "tool-output" "trash" "debug" "coverage" ".cache" "target" "vendor" ".github"
    ".husky" "fixtures" "testdata" "extensions" "node_modules" "site-packages")
  "Directory names no harness keeps settings in, skipped in every world.")

(defparameter +memory-directories+ '("memory" "memories")
  "Directory names whose markdown files are each one remembered entry.")

;;; A home's own AGENTS.md sits at its root, or one directory in (a
;;; workspace); deeper than that the file belongs to something else — a
;;; skill's template, a plugin checkout's README, one subagent's brief — and
;;; folding those into the operator's own persona is how an import turns 43k
;;; characters of other people's text into this organism's standing
;;; instructions.
(defparameter +instruction-depth+ 1
  "How far below a root standing instructions are looked for.")

;;; Two kinds of standing text, by what a harness means by the name: its
;;; rules for every session (Claude Code's CLAUDE.md, Codex's AGENTS.md,
;;; Windsurf's global_rules.md), and a persona, the agent's voice (Hermes's
;;; and Openclaw's SOUL.md, Openclaw's IDENTITY.md). They land apart
;;; (plan.lisp PLAN-INSTRUCTIONS).
(defparameter +instruction-names+
  '("AGENTS" "AGENT" "CLAUDE" "GEMINI" "QWEN" "SYSTEM-PROMPT" "SYSTEM_PROMPT" "GLOBAL_RULES"
    "GLOBAL-RULES" "RULES" "INSTRUCTIONS")
  "The names a harness gives its rules for every session.")

(defparameter +persona-names+ '("SOUL" "IDENTITY" "PERSONA")
  "The names a harness gives the agent's persona.")

;;; The manifest table itself is the core's (NLK:*AGENT-WORLDS*, waist/worlds.lisp):
;;; the first frame has to know whether a past exists before any folder
;;; loads, so the question lives there and the readers live here.

;;; --- the home ---------------------------------------------------------------

(nlk:define-record (home (:copier nil))
  "One foreign home as the walk read it."
  ;; WORLD is its manifest, or NIL for a bare path; NAME what the report calls
  ;; it; ROOTS the directories and files walked; FACTS everything the shapes
  ;; made of them; UNMAPPED the files seen and not understood, relative to the
  ;; first root; NOTES one line per file that would not parse.
  world name (roots '()) (facts '()) (unmapped '()) (notes '()))

(defun home-world-name (home) (nlk:if-let (world home.world) world.name home.name))

(defun open-home (source)
  "SOURCE — a world name, a path, or a WORLD — as a HOME with its roots
resolved and nothing read yet."
  (let* ((world (cond ((nlk:agent-world-p source) source)
                      ((and (stringp source) (nlk:find-agent-world source)))))
         (path (cond (world nil)
                     ((pathnamep source) source)
                     ((stringp source) (nlk:expand-home source))
                     (t (fail "an import source is a world name (~{~a~^, ~}) or a path"
                              (nlk:agent-world-names))))))
    (cond
      (world
       (let ((roots (loop for relative in (nlk:agent-world-roots world)
                          when (probe-file (nlk:agent-home-path relative)) collect it)))
         (unless roots
           (fail "no ~a home on this box: looked for ~{~a~^, ~} under ~a"
                 world.label world.roots
                 (namestring (user-homedir-pathname))))
         (make-home :world world :name world.label :roots roots)))
      (t
       (let ((file (probe-file path)))
         (unless file (fail "no home at ~a" (namestring path)))
         ;; A path inside a world this build knows is still that world: the
         ;; manifest's skip list and takeover apply to a home read by path.
         (let ((named (find-if (lambda (world)
                                 (some (lambda (relative)
                                         (let ((root (probe-file (nlk:agent-home-path relative))))
                                           (and root (equal (namestring root) (namestring file)))))
                                       world.roots))
                               nlk:*agent-worlds*)))
           (make-home :world named
                      :name (if named named.label (namestring file))
                      :roots (list file))))))))

;;; --- the walk ---------------------------------------------------------------

(defun skip-directory-p (home name &optional (hidden t) &aux (lower (string-downcase name)))
  "Whether a directory is not worth entering."
  ;; A hidden one never is, unless HIDDEN is NIL or the home's world names it
  ;; (ENTER): inside a home they are archives, caches and the tool's own
  ;; bookkeeping, and the homes that are themselves hidden are reached as
  ;; roots, not walked into.
  (or (and hidden (hidden-name-p name)
           (not (and home.world (member name (nlk:agent-world-enter home.world) :test #'string=))))
      (member lower +skip-directories+ :test #'string=)
      (and home.world (member name (nlk:agent-world-skip home.world) :test #'string-equal))))

(defun read-capped (file cap)
  "FILE's text, or NIL when it is larger than CAP or will not read."
  (and (<= (or (nlk:file-bytes file) 0) cap)
       (ignore-errors (uiop:read-file-string file :external-format :utf-8))))

;;; --- convention: a config file ----------------------------------------------

(defun environment-file-p (file &aux (name (file-namestring file)))
  "Whether FILE is an environment file: `.env', `.env.local', `x.env'."
  (or (uiop:string-prefix-p ".env." name)
      (uiop:string-suffix-p name ".env")))

;;; --- convention: a skill ----------------------------------------------------

(defun skill-fact (file relative world)
  (let* ((directory (uiop:pathname-directory-pathname file))
         (text (read-capped file +text-byte-cap+))
         (declared (and text (ignore-errors (nle::parse-skill-frontmatter text))))
         (slug (string-downcase
                (substitute #\- #\Space
                            (string-trim '(#\Space #\Tab) (or declared (nlk:folder-name directory))))))
         (name (if (and (plusp (length slug))
                        (every (lambda (ch) (or (alphanumericp ch) (member ch '(#\- #\_ #\.)))) slug))
                   slug
                   (nlk:folder-name directory))))
    (make-fact :kind :skill :id name :world world :source relative
               :value (list :name name :directory directory
                            :summary (and text (one-line-of text 80))))))

;;; --- convention: instructions and memory ------------------------------------

(defun markdown-base (file)
  (and (member (string-downcase (or (pathname-type file) "")) '("md" "markdown" "txt")
               :test #'string=)
       (string-upcase (pathname-name file))))

(defun one-line-of (text limit)
  "The first non-empty line of TEXT, whitespace folded, cut to LIMIT."
  (nlk:one-line (find-if (lambda (line)
                           (plusp (length (string-trim '(#\Space #\Tab #\Return #\-) line))))
                         (uiop:split-string (or text "") :separator '(#\Newline)))
                :cap limit :ellipsis "…"))

;;; --- the read ---------------------------------------------------------------

(defun note-unmapped (home relative why) (push (format nil "~a — ~a" relative why) home.unmapped))

(defun read-config-facts (home file relative)
  ;; A settings file is kilobytes: past 2 MB it is a log or a cache wearing a .json name.
  (let* ((text (read-capped file (* 2 1024 1024)))
         (type (string-downcase (or (pathname-type file) "")))
         (tree (and text
                    (handler-case
                        (cond ((environment-file-p file)
                               (alexandria:alist-hash-table (read-env-file text) :test #'equal))
                              ((string= type "toml") (read-toml text))
                              ((member type '("yaml" "yml") :test #'string=) (read-yaml text))
                              ;; JSONC too: half the homes on a real box write `.json' that is JSONC.
                              (t (nlk:decode-json (nlk::normalize-jsonc text))))
                      (error (condition)
                        (push (format nil "~a did not read: ~a" relative condition) home.notes)
                        nil)))))
    (cond ((null text)
           (note-unmapped home relative
                          (format nil "~d bytes, past the cap" (or (nlk:file-bytes file) 0)))
           '())
          ((null tree) '())
          (t (let ((facts (classify-tree tree :source relative :world (home-world-name home))))
               (unless facts (note-unmapped home relative "nothing this build reads"))
               facts)))))

(defun frontmatter (text)
  "=> (values FIELDS BODY): TEXT's leading `---' block as ((KEY . VALUE)
...), a key indented under a bare `key:' line read as `key.child' and a
quoted value unquoted, and the text after the block; NIL and TEXT when it
has none."
  (let* ((lines (nlk:lines text))
         (close (and lines (string= "---" (nlk:trimmed (first lines)))
                     (position "---" (rest lines) :key #'nlk:trimmed :test #'string=))))
    (if (null close)
        (values nil text)
        (let ((section nil) (fields '()))
          (dolist (line (subseq (rest lines) 0 close))
            (ppcre:register-groups-bind (indent key value)
                ("^([ \\t]*)([A-Za-z0-9_.-]+):[ \\t]*(.*?)[ \\t\\r]*$" line)
              (let ((value (ppcre:regex-replace "^([\"'])(.*)\\1$" value "\\2")))
                (cond ((plusp (length indent))
                       (when section (push (cons (format nil "~a.~a" section key) value) fields)))
                      ((string= value "") (setf section key))
                      (t (setf section nil) (push (cons key value) fields))))))
          (values (nreverse fields) (format nil "~{~a~^~%~}" (nthcdr (+ 2 close) lines)))))))

(defun claude-projects ()
  "The project roots Claude Code's ~/.claude.json names, or NIL."
  (let ((config (ignore-errors (nlk:decode-json (uiop:read-file-string
                                                 (nlk:agent-home-path ".claude.json"))))))
    (and (hash-table-p config) (hash-table-p (gethash "projects" config))
         (alexandria:hash-table-keys (gethash "projects" config)))))

(defun trail-project (trail)
  "The project a memory under TRAIL is about when it sits in a folder named
for one -- Claude Code's projects/<root, munged>/memory/: the root
~/.claude.json names whose munged spelling is that folder's, else NIL."
  (let ((munged (second (member "projects" trail :test #'string=))))
    (and munged
         (find-if (lambda (root) (string-equal munged (ppcre:regex-replace-all "[^A-Za-z0-9]" root "-")))
                  (claude-projects)))))

(defun memory-file-fact (home relative trail fields body)
  "One remembered entry from a memory file whose frontmatter names it: its
name, description, type, scope and dates, and its body."
  (flet ((field (key) (cdr (assoc key fields :test #'string-equal))))
    (let ((scope (field "scope"))
          (body (nlk:trimmed body)))
      (make-fact :kind :memory :id (field "name") :world (home-world-name home) :source relative
                 :value (list :name (field "name")
                              :description (or (field "description")
                                               (field "metadata.short-description")
                                               (one-line-of body 100))
                              :type (or (field "type") (field "metadata.type"))
                              :project (if (and scope (uiop:string-prefix-p "project " scope))
                                           (nlk:trimmed (subseq scope 8))
                                           (trail-project trail))
                              :created (field "created") :updated (field "updated")
                              :sources (and (field "sources")
                                            (remove "" (mapcar #'nlk:trimmed
                                                               (uiop:split-string
                                                                (string-trim "[]" (field "sources"))
                                                                :separator ","))
                                                    :test #'string=))
                              :text body)))))

(defun hot-index-facts (home relative text)
  "A MEMORY.md of links, `- [name](name.md) — description', as one :HOT
fact per name it lists: the home's hot index, which the import keeps as a
view of each."
  (let ((names '()))
    (ppcre:do-register-groups (name) ("(?m)^\\s*-\\s*\\[([^\\]]+)\\]\\([^)]*\\.md\\)" text)
      (push name names))
    (mapcar (lambda (name)
              (make-fact :kind :hot :id name :world (home-world-name home) :source relative))
            (nreverse names))))

(defun usage-facts (home file relative)
  "The use a ledger FILE records, as :USAGE facts: a skills library's
usage.jsonl line by line, an experience ledger's helped and harm sightings
by the memory they name and its tool sightings as calls."
  (let ((sightings (string= "sightings.jsonl" (file-namestring file))))
    (loop for line in (nlk:read-json-lines file)
          for index from 0
          for kind = (nlk:json-value line :string "kind")
          for (name use) = (cond ((not sightings) (list (nlk:json-value line :string "name") kind))
                                 ((member kind '("helped" "harm") :test #'equal)
                                  (list (nlk:json-value line :string "line") kind))
                                 ((equal kind "tool") (list (nlk:json-value line :string "tool") "call")))
          when (and name use)
            collect (make-fact :kind :usage :id (format nil "~a#~d" relative index)
                               :world (home-world-name home) :source relative
                               :value (list :name name :kind use
                                            :at (nlk:json-value line :string "at")
                                            :session (nlk:json-value line :string "session"))))))

(defun read-text-facts (home file relative trail)
  "The instruction and memory facts a markdown file yields."
  ;; Instructions are the home's OWN — at its root or one directory in, never
  ;; inside somebody else's checkout; a remembered entry can sit as deep as
  ;; its home files it, which for one harness is projects/<project>/memory/.
  ;; A named one outranks the folder it sits in: Windsurf keeps its
  ;; global_rules.md among its memories.
  (let* ((base (markdown-base file))
         (named (and base
                     (or (member base +instruction-names+ :test #'string=)
                         (member base +persona-names+ :test #'string=))
                     (<= (length trail) +instruction-depth+)
                     (notany (lambda (name)
                               (member name '("skills" "plugins" "marketplaces" "agents"
                                              "subagents" "templates" "commands" "hooks"
                                              "examples" "docs" "extensions" "powers" "recipes")
                                       :test #'string=))
                             trail)))
         (memory (and base (not named)
                      (or (some (lambda (name) (member name +memory-directories+ :test #'string=))
                                trail)
                          (and (member base '("MEMORY" "MEMORIES" "USER") :test #'string=)
                               (<= (length trail) +instruction-depth+)))))
         (wanted (or memory named))
         (text (and wanted (read-capped file +text-byte-cap+))))
    (cond
      ((not wanted) nil)
      ((null text)
       (note-unmapped home relative "past the text cap")
       '())
      ;; A harness's own seed: nothing left once its comments and headings are out.
      ((not (ppcre:scan "(?m)^[ \\t\\r]*[^# \\t\\r\\n]"
                        (ppcre:regex-replace-all "(?s)<!--.*?(?:-->|\\z)" text "")))
       '())
      ;; A home's hot index: links to its memory files, not memories of its own.
      ((and memory (string= base "MEMORY") (ppcre:scan "(?m)^\\s*-\\s*\\[[^\\]]+\\]\\([^)]*\\.md\\)" text))
       (hot-index-facts home relative text))
      ;; A memory file that names itself is one entry, under that name.
      ((and memory (assoc "name" (frontmatter text) :test #'string-equal))
       (multiple-value-bind (fields body) (frontmatter text)
         (list (memory-file-fact home relative trail fields body))))
      (memory
       ;; One entry per run of lines between `§' separator lines, line-end CRs off.
       (let ((type (if (string= base "USER") "user" "project")))
         (loop for piece in (ppcre:split "(?m)^[ \\t\\r]*§[ \\t\\r]*$" text)
               for entry = (nlk:trimmed (ppcre:regex-replace-all "\\r+(?=\\n|\\z)" piece ""))
               when (plusp (length entry))
               collect (make-fact :kind :memory
                                  :id (format nil "~a-~a" (home-world-name home)
                                              (nlk:short-digest entry 10))
                                  :world (home-world-name home) :source relative
                                  :value (list :text entry :type type)))))
      (t
       (list (make-fact :kind :instructions :id base
                        :world (home-world-name home) :source relative
                        :value (list :text text :hash (nlk:short-digest text 10)
                                     :persona (and (member base +persona-names+ :test #'string=)
                                                   t))))))))

(defun read-facts (home &key (sessions t))
  "Everything the home holds, as facts, HOME filled in as it goes."
  ;; SESSIONS nil leaves the conversations alone — what the first frame wants,
  ;; since a transcript store is gigabytes and a provider key is bytes.
  (let ((facts '()) (budget 6000) (first-root (uiop:ensure-directory-pathname (first home.roots))))
    (labels ((visit (file relative trail)
               (cond
                 ((and (string-equal "SKILL.md" (file-namestring file))
                       (some (lambda (name) (uiop:string-prefix-p "skills" name)) trail))
                  (push (skill-fact file relative (home-world-name home)) facts))
                 ((and (not (member (string-downcase (file-namestring file))
                                    '("package.json" "package-lock.json" "bun.lock" "bun.lockb"
                                      "yarn.lock" "tsconfig.json" "jsconfig.json" "composer.lock"
                                      "pnpm-lock.yaml")
                                    :test #'string=))
                       (or (environment-file-p file)
                           (member (string-downcase (or (pathname-type file) ""))
                                   '("json" "jsonc" "json5" "yaml" "yml" "toml") :test #'string=)))
                  (setf facts (revappend (read-config-facts home file relative) facts)))
                 ((markdown-base file)
                  (setf facts (revappend (read-text-facts home file relative trail) facts)))
                 ((member (file-namestring file) '("usage.jsonl" "sightings.jsonl") :test #'string=)
                  (setf facts (revappend (usage-facts home file relative) facts)))))
             ;; A conversation directory is crossed, never read (no settings) nor skipped (memory lives in one).
             (walk (directory trail depth crossing)
               (when (and (plusp budget) (<= depth +walk-file-depth+))
                 (unless crossing
                   (dolist (file (or (ignore-errors (uiop:directory-files directory)) '()))
                     (when (plusp budget)
                       (decf budget)
                       (visit file (enough-namestring file first-root) trail))))
                 (dolist (sub (or (ignore-errors (uiop:subdirectories directory)) '()))
                   (let* ((name (nlk:folder-name sub))
                          (lower (and name (string-downcase name)))
                          (into (cond ((member lower +memory-directories+ :test #'equal) nil)
                                      ;; a memory folder's projects/ holds memories, not transcripts
                                      ((and (member lower '("sessions" "projects" "chats" "history" "storage"
                                                            "conversations" "threads" "rollouts")
                                                    :test #'equal)
                                            (notany (lambda (name) (member name +memory-directories+
                                                                           :test #'equal))
                                                    trail))
                                       t)
                                      (t crossing))))
                     (cond ((null name))
                           ((skip-directory-p home name))
                           (t (walk sub (append trail (list lower)) (1+ depth) into))))))))
      ;; A root named for what it holds -- a memory folder, a skills library --
      ;; is read as what its name says, as a folder of that name inside one is.
      (dolist (root home.roots)
        (if (uiop:directory-pathname-p root)
            (let ((name (string-downcase (nlk:folder-name root))))
              (walk root (and (member name (cons "skills" +memory-directories+) :test #'string=)
                              (list name))
                    0 nil))
            (visit root (file-namestring root) '()))))
    (setf facts (nreverse facts))
    (when sessions
      (setf facts (append facts (session-facts home))))
    (setf home.facts facts home.unmapped (nreverse home.unmapped) home.notes (nreverse home.notes))
    facts))
