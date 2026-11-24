;;;; cell.lisp --- the tool, /ram-first-context, the manual, and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of ether-btc/ram-first-context, a Hermes Agent plugin
;;;; that registered one tool, mnemosyne_fts_recall: a read-only FTS5 keyword
;;;; search over the live Mnemosyne SQLite database that Hermes's own memory
;;;; provider writes. The rewrite keeps the tool, its name, its arguments and
;;;; its answer -- a JSON array of id, content, source, importance, timestamp,
;;;; memory_type, metadata_json and rank, or of one {"error": ...} record --
;;;; and reads the same two tables with the same two statements.
;;;;
;;;; What it does not carry over is Hermes's own home: Nodecode has no
;;;; Mnemosyne of its own, so the database is a setting the operator names.
;;;; A search never writes to it: SELECTs only, on a connection with PRAGMA
;;;; query_only on.

(in-package #:nodecode-ram-first-context)

;;; --- the boundary the tool reads ------------------------------------------------

(nlk:define-contract recall-arguments
  (:doc "Search a live Mnemosyne memory database (read-only) for keyword matches over working_memory. Every whitespace-separated term must match, as a literal word: FTS5 operators in the text are words, never syntax, and a trailing * searches a word prefix. Results are bm25-ranked and answered as a JSON array of id, content, source, importance, timestamp, memory_type, metadata_json and rank; a LIKE search ordered by importance answers when the index is missing or the file cannot run the FTS statement. Nothing is written to the database.")
  ("query" :string :required t
           :doc "search terms, space-separated, every one required; a trailing * is a prefix")
  ("limit" :integer :default 5
           :doc "how many results to answer, at most the section's max_limit"))

;;; --- the manual ----------------------------------------------------------------

(defparameter +manual+
  "ram-first-context: read-only keyword recall over a live Mnemosyne memory
database -- the SQLite file a Hermes Agent's Mnemosyne provider writes. Point
it at that file with the section's db_path, then search it two ways: the tool
mnemosyne_fts_recall from a call (\"query\", and \"limit\" for how many), or
/ram-first-context TERMS typed at a shell. Both read the same file the same
way and change nothing in it.
What it holds to: every whitespace-separated term must match, and each is a
literal word -- searching AND or a colon character finds memories that carry
them, never an FTS5 syntax error; a trailing * searches a prefix
(terminal* finds terminal and terminals); results come back bm25-ranked, or,
when the FTS index is missing or the file cannot answer, as a LIKE search
ordered by importance then timestamp with rank 0, and the % and _ in the
query stay literal on that path too. The answer is a JSON array of rows --
id, content, source, importance, timestamp, memory_type, metadata_json, rank
-- or a single {\"error\": ...} record when the section names no database or
the file is not there. An empty query answers an empty array. Nothing is
ever written, uploaded or sent anywhere; the database is opened read-only, so
a search cannot change a byte of it. The section ram-first-context also
holds default_limit (how many a command answers), max_limit (the ceiling a
call's limit is held to), busy_timeout_ms (how long to wait on a locked
file), and usage_log (off by default: one local line per search counting the
query's length and the hits, never its words, under this home)."
  "What (help :ram-first-context) answers while the cell runs.")

;;; --- what the command says ------------------------------------------------------

(defun settings-text ()
  "What /ram-first-context says bare: the database it reads and whether it is
there, the limits, and how to search."
  (let ((db (db-file)))
    (format nil "ram-first-context: reads ~a (~:[not there~;there~]); ~d result~:p a search, at most ~d; usage_log is ~:[off~;on~]. /ram-first-context TERMS searches it; every term must match, and a trailing * is a prefix."
            (namestring db) (if (probe-file db) t nil)
            (setting :default-limit) (setting :max-limit) (setting :usage-log))))

(defun run-slash (args session-id)
  "One /ram-first-context invocation: a bare one answers its settings, one with
terms searches the database and answers the matches as lines."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (zerop (length query)) (settings-text) (matches-text query))))

;;; --- the tool ------------------------------------------------------------------

(defun run-recall (arguments)
  "The mnemosyne_fts_recall tool: the search as the JSON array the original answers."
  (recall (recall-arguments-query arguments) :limit (recall-arguments-limit arguments)))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell ram-first-context
  (:section ("ram-first-context")
    (:guide "db_path is the Mnemosyne database to read, a file the provider already writes; the rest have defaults; /ram-first-context TERMS searches it")
    ("db_path" :path :required t
               :doc "the Mnemosyne SQLite database to search, read-only (an existing file)")
    ("default_limit" :integer :default 5 :min 1
                     :doc "how many results /ram-first-context answers")
    ("max_limit" :integer :default 50 :min 1
                 :doc "the ceiling a call's limit is held to")
    ("busy_timeout_ms" :integer :default 5000 :min 0
                       :doc "how long a search waits on a database another process is writing")
    ("usage_log" :boolean :default nil
                 :doc "append one local line per search to plugin-data/ram-first-context/usage.jsonl"))
  (:tool "mnemosyne_fts_recall" (nlk:contract 'recall-arguments) 'run-recall)
  (:command "ram-first-context" 'run-slash
            :description "Search a Mnemosyne memory database, read-only"
            :argument-hint "TERMS")
  (:help :ram-first-context "ram-first-context: read-only keyword recall over a live Mnemosyne SQLite memory database - /ram-first-context TERMS"
         +manual+))
