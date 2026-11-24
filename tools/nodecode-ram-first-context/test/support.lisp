;;;; support.lisp --- the test runner and a Mnemosyne-shaped database.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under a RAM-FIRST-CONTEXT name
;;;; prefix; RUN-RAM-FIRST-CONTEXT-TESTS runs exactly that slice.
;;;;
;;;; Nothing here dials anything: the cell reads no network and needs no key.
;;;; The one fixture is a database written the shape the Mnemosyne memory
;;;; provider writes -- working_memory, its fts_working index and the trigger
;;;; that feeds the index -- under this run's own home.

(in-package #:nodecode.test)

(define-test-slice "ram-first-context" "RAM-FIRST-CONTEXT" :start nodecode-ram-first-context:start-cell)

(defun ram-dir ()
  "A scratch folder under this run's home: where the fixture databases live."
  (ensure-directories-exist (nlk:home "ram-first-context/")))

(defun ram-db (&optional (name "mnemosyne.db"))
  "The fixture database's namestring, as the section's db_path takes it."
  (namestring (merge-pathnames name (ram-dir))))

(defparameter +memories+
  '(("wm1" "the AND gate is a reserved word" 0.9 "2026-10-07T10:00:00")
    ("wm2" "hermes plugin near test" 0.8 "2026-10-07T10:05:00")
    ("wm3" "terminal emulator startup flags" 0.3 "2026-10-07T10:10:00")
    ("wm4" "port 8080:8081 mapping note" 0.6 "2026-10-07T10:15:00")
    ("wm5" "50% of the disk is free" 0.2 "2026-10-07T10:20:00"))
  "Memories the fixtures hold: a reserved word, a near-miss pair of words
(the port line would break an unquoted FTS5 query), a prefix and a percent.")

(defun build-db (path &key (fts t))
  "PATH written as a Mnemosyne database: working_memory and, with FTS, its
fts_working index and the insert trigger. => PATH."
  (when (probe-file path) (delete-file path))
  (sqlite:with-open-database (db path)
    (sqlite:execute-non-query
     db "create table working_memory (id text primary key, content text, source text, importance real, timestamp text, memory_type text, metadata_json text)")
    (when fts
      (sqlite:execute-non-query
       db "create virtual table fts_working using fts5 (content, id unindexed, content='working_memory', content_rowid='rowid')")
      (sqlite:execute-non-query
       db "create trigger wm_ai after insert on working_memory begin insert into fts_working(rowid, id, content) values (new.rowid, new.id, new.content); end"))
    (dolist (memory +memories+)
      (destructuring-bind (id content importance timestamp) memory
        (sqlite:execute-non-query
         db "insert into working_memory (id, content, source, importance, timestamp, memory_type, metadata_json) values (?, ?, 'user', ?, ?, 'note', '{}')"
         id content importance timestamp))))
  path)

(defmacro with-mnemosyne ((&key (fts t) (name "mnemosyne.db")) &body body)
  "BODY over a freshly written Mnemosyne database at the section's db_path;
with (:FTS NIL) the index is left out, so the search falls back to LIKE."
  `(let ((path (build-db (ram-db ,name) :fts ,fts)))
     (declare (ignorable path))
     ,@body))

(defun ram-answer (arguments &optional (id "rf-call"))
  "The mnemosyne_fts_recall tool called with ARGUMENTS (a JSON object string),
its answer decoded: a list of hash tables."
  (let ((answer (nle:message-content
                 (execute-wire-call id "mnemosyne_fts_recall" arguments))))
    (is (stringp answer) "the tool answers text")
    (coerce (cell-json (if (stringp answer) answer "[]")) 'list)))

(defun ram-row (row key)
  "ROW's member KEY as text."
  (let ((value (nlk:json-value row :string key)))
    (or value (princ-to-string (nlk:json-value row :any key)))))

(defun ram-contents (rows)
  "ROWS' contents, in the order they came back."
  (mapcar (lambda (row) (ram-row row "content")) rows))

(defun ram-digest (path)
  "PATH's bytes as one digest, for proving a search changed nothing."
  (nlk:sha256-text (uiop:read-file-string path :external-format :latin-1)))
