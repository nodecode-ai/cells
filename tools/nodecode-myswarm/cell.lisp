;;;; cell.lisp --- the section, the five tools, /myswarm, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of tomkubik/hermes-myswarm: an agents.md read four
;;;; ways (a table, a box-and-arrow diagram, a copy-paste block, a scrubbed
;;;; export) and taken in two (an import with the file it replaced kept, and
;;;; the revert that puts that file back).
;;;;
;;;; Config, a top-level key:
;;;;   "myswarm": {"enabled": true,
;;;;               "workspace": "",                       ; the session's own folder
;;;;               "data_dir": "~/.nodecode/myswarm",    ; required before an import
;;;;               "default_export_filename": "myswarm-export.md",
;;;;               "default_style": "unicode"}
;;;;
;;;; No key, no network, no model: every reading is a regular expression over
;;;; the text of the file, and every write lands in the workspace or data_dir.

(in-package #:nodecode-myswarm)

;;; --- the four arguments a tool call carries --------------------------------------

(nlk:define-contract file-request
  (:doc "Which agents.md to read. Leave path empty to read the one in the workspace this session works in.")
  ("path" :string :default ""
          :doc "an agents.md or AGENTS.md, relative to the workspace or absolute; empty reads the workspace's own"))

(nlk:define-contract export-request
  (:doc "Write a PII-scrubbed copy of the workspace's agents.md as one .md a person can share. It redacts what looks like a secret or a person (emails, tokens, home paths, phone numbers, addresses, the account's own name) on a best-effort basis, and never overwrites agents.md itself.")
  ("output_path" :string :default ""
                 :doc "where the copy goes, relative to the workspace or absolute, and inside the workspace or the cell's data folder; empty uses the configured export name"))

(nlk:define-contract import-request
  (:doc "Take in a shared agents.md (markdown, or a MySwarm copy-paste block, which becomes markdown): show its whole contents, read it for what it could do to a model, keep the agents.md it replaces in the cell's data folder, and write it as the workspace's agents.md. An import is a copy with a backup, never a merge; revert puts the old one back. Preview shows it and writes nothing.")
  ("source_file" :text :required t
                 :doc "the file to import, relative to the workspace or absolute")
  ("preview_only" :boolean :default nil
                  :doc "show the contents and the warnings, and write nothing")
  ("allow_suspicious" :boolean :default nil
                      :doc "import even when a high-severity warning was found; a person decides that, never a guess"))

(nlk:define-contract revert-request
  (:doc "Put back the agents.md an import replaced. When the import created the file, the revert takes it away again.")
  ("note" :string :default ""
          :doc "why the operator wants it back; it is only quoted in the answer"))

(nlk:define-contract nothing-request
  (:doc "What the cell reads, writes and keeps right now: the workspace, the agents.md it found, the agents in it, and whether an import is live.")
  ("note" :string :default "" :doc "anything to quote back in the answer"))

;;; --- the five tools -------------------------------------------------------------

(defun list-verb (arguments)
  (list-agents (let ((path (trimmed (file-request-path arguments))))
          (and (plusp (length path)) path))))

(defun topology-verb (arguments)
  (topology (let ((path (trimmed (file-request-path arguments))))
              (and (plusp (length path)) path))))

(defun export-verb (arguments)
  (export-copy (let ((path (trimmed (export-request-output-path arguments))))
            (and (plusp (length path)) path))))

(defun import-verb (arguments)
  (import-file (import-request-source-file arguments)
          :preview (import-request-preview-only arguments)
          :allow-suspicious (import-request-allow-suspicious arguments)))

(defun revert-verb (arguments)
  (declare (ignore arguments))
  (revert))

(defun status-verb (arguments)
  (declare (ignore arguments))
  (status))

;;; --- /myswarm ---------------------------------------------------------------------

(defparameter +slashes+
  (format nil "/myswarm list            the agents agents.md declares, and their personas and goals~%~
/myswarm ascii [PATH]    the swarm as a box-and-arrow diagram and a copy-paste block~%~
/myswarm export [PATH]   a PII-scrubbed copy of agents.md, written to PATH~%~
/myswarm import [--preview] [--allow] FILE  take in a shared agents.md, keeping the one it replaces~%~
/myswarm revert          put back the agents.md the import replaced~%~
/myswarm status          where the cell reads, what it found, and whether an import is live")
  "What a bare /myswarm answers.")

(defun split-words (text)
  (remove-if #'zerop (cl-ppcre:split "\\s+" (or text "")) :key #'length))

(defun run-slash (args session-id)
  "The /myswarm command: one verb, and what it acts on. => the answer's text."
  (declare (ignore session-id))
  (running-settings)
  (let* ((words (split-words args))
         (verb (string-downcase (or (first words) "")))
         (rest (rest words))
         (preview (member "--preview" rest :test #'string=))
         (allow (member "--allow" rest :test #'string=))
         (operands (remove-if (lambda (word) (member word '("--preview" "--allow") :test #'string=)) rest)))
    (handler-case
        (cond
          ((member verb '("" "help") :test #'string=) +slashes+)
          ((string= verb "list") (list-agents (first operands)))
          ((member verb '("ascii" "topology") :test #'string=) (topology (first operands)))
          ((string= verb "export") (export-copy (first operands)))
          ((string= verb "import") (import-file (first operands) :preview (not (null preview))
                                                     :allow-suspicious (not (null allow))))
          ((string= verb "revert") (revert))
          ((string= verb "status") (status))
          (t (format nil "myswarm: ~a is no verb of this cell.~%~%~a" (first words) +slashes+)))
      (myswarm-error (condition) (format nil "myswarm: ~a" condition)))))

;;; --- the manual -----------------------------------------------------------------

(defparameter +primer+
  "myswarm: reads and shares an agents.md -- the file that declares a workspace's
agents, their personas and their goals -- as tools the model calls, as /myswarm
from a shell, and as Lisp called through eval. No key, no network, no model:
every reading is a regular expression over the file, so the same document reads
the same way whatever model is talking.
  (myswarm:list-agents [PATH])     the table: one row an agent, an asterisk on the
      orchestrator, persona and goal abbreviated to one line.
  (myswarm:topology [PATH]) the box-and-arrow diagram -- orchestrators above
      the specialists they route to, the delegation flows named under it -- and
      the pure 7-bit copy-paste block stamped [Shared via MySwarm].
  (myswarm:export-copy [PATH])   a scrubbed copy written as one .md; it redacts
      emails, tokens and API keys, /Users and /home paths, phone numbers, SSNs,
      IPv4 addresses, Owner/Author/Contact fields and this account's own name,
      and says how many of each it took out.
  (myswarm:import-file FILE &key preview allow-suspicious)  shows the whole file,
      warns about what an agents.md can do to a model -- a prompt injection, a
      piped curl, a destructive command, a hunt for ~/.ssh or ~/.aws, shell-rc
      persistence, zero-width characters -- keeps the agents.md it replaces in
      the data folder and writes the new one. A high-severity warning stops it
      unless a person allows it. A shared copy-paste block imports back as
      markdown.
  (myswarm:revert)          puts the replaced agents.md back.
  (myswarm:status)          where it reads, what it found, whether an import is live.
What it holds to: PII pruning and the import's security reading are both BEST
EFFORT -- say so whenever you hand a user something to share or take in, and tell
them to read it themselves. ALWAYS show the imported file's contents and your
reading of it before a person relies on it, and import only from sources they
trust. An import is a copy with a backup, never a merge, and it writes agents.md
alone. The section myswarm in the config holds workspace (the folder holding
agents.md; empty means this session's own working directory), data_dir (where
the backup and default exports go; required before an import), and
default_export_filename and default_style (unicode or ascii for the diagram)."
  "What (help :myswarm) answers while the cell runs.")

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell myswarm
  (:section ("myswarm")
    (:guide "workspace is the folder holding agents.md, empty for this session's own working directory; data_dir is where an import keeps the agents.md it replaces and where a default export goes, and an import refuses without it; the other two have defaults")
    ("workspace" :path
                 :doc "the folder the cell reads agents.md from and writes into; empty means this session's own working directory")
    ("data_dir" :path :required t
                :doc "the cell's own folder: an import keeps the agents.md it replaced here, and a default export lands here")
    ("default_export_filename" :string :default "myswarm-export.md"
                               :doc "the name an export takes when the call names no path")
    ("default_style" :choice :options '("unicode" "ascii") :default "unicode"
                      :doc "the diagram's frame: unicode box characters, or 7-bit ASCII"))
  (:tool "myswarm_list" (nlk:contract 'file-request) 'list-verb)
  (:tool "myswarm_ascii" (nlk:contract 'file-request) 'topology-verb)
  (:tool "myswarm_export" (nlk:contract 'export-request) 'export-verb)
  (:tool "myswarm_import" (nlk:contract 'import-request) 'import-verb)
  (:tool "myswarm_revert" (nlk:contract 'revert-request) 'revert-verb)
  (:tool "myswarm_status" (nlk:contract 'nothing-request) 'status-verb)
  (:command "myswarm" 'run-slash
            :catches 'myswarm-error
            :description "Read or share an agents.md: list, diagram, export, import, revert"
            :argument-hint "LIST|ASCII|EXPORT|IMPORT|REVERT|STATUS")
  (:help :myswarm
         "myswarm: read and share an agents.md - list, diagram, scrub for sharing, import with a backup, revert"
         +primer+))
