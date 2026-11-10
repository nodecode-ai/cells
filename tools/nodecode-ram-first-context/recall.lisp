;;;; recall.lisp --- the read-only query: FTS5 first, LIKE as its fallback.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin's one tool, statement for statement: fts_working
;;;; joined to working_memory and ordered by its bm25 rank, and, when the
;;;; index is missing or the file cannot answer the FTS query, a LIKE search
;;;; over the same table ordered by importance then timestamp.
;;;;
;;;; Every value is bound as a parameter and every term is wrapped as an
;;;; FTS5 string literal, so a word can never become FTS5 syntax or SQL: a
;;;; memory containing "AND" or ":" is found by searching for it.
;;;;
;;;; Read-only by construction: the only statements are SELECTs, on a
;;;; connection of its own with PRAGMA query_only turned on.

(in-package #:nodecode-ram-first-context)

(defparameter +columns+
  '("id" "content" "source" "importance" "timestamp" "memory_type" "metadata_json" "rank")
  "A row's members, in the order the original tool answers them.")

(defparameter +fts-sql+
  "select wm.id, wm.content, wm.source, wm.importance, wm.timestamp,
          wm.memory_type, wm.metadata_json, rank
     from fts_working f
     join working_memory wm on wm.id = f.id
    where fts_working match ?
    order by rank
    limit ?"
  "The FTS5 path: bm25 rank, every term required.")

(defparameter +like-sql+
  "select id, content, source, importance, timestamp, memory_type, metadata_json,
          0 as rank
     from working_memory
    where content like ? escape '\\'
    order by importance desc, timestamp desc
    limit ?"
  "The fallback: a literal substring match, rank reported as zero.")

;;; --- text the SQL is built from --------------------------------------------------

(defun replace-all (text old new)
  "TEXT with every occurrence of OLD replaced by NEW, left to right."
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search old text :start2 start)
          do (cond (at
                    (write-string text out :start start :end at)
                    (write-string new out)
                    (setf start (+ at (length old))))
                   (t
                    (write-string text out :start start)
                    (return))))))

(defun query-terms (query)
  "QUERY's whitespace-separated terms, each (:TEXT TEXT :PREFIX BOOL), in
order; empty when QUERY holds no term. A trailing * makes a term a prefix."
  (loop for word in (uiop:split-string query :separator '(#\Space #\Tab #\Newline #\Return))
        for text = (string-trim '(#\Space #\Tab) word)
        when (plusp (length text))
          collect (if (char= #\* (char text (1- (length text))))
                      (list :text (subseq text 0 (1- (length text))) :prefix t)
                      (list :text text))))

(defun fts-query (terms)
  "TERMS as one FTS5 MATCH expression: each a quoted string literal, joined
by AND, a prefix term with the star the quoting keeps out of the literal."
  (format nil "~{~a~^ AND ~}"
          (loop for term in terms
                collect (concatenate 'string
                                     "\"" (replace-all (getf term :text) "\"" "\"\"") "\""
                                     (if (getf term :prefix) "*" "")))))

(defun like-escape (text)
  "TEXT with the LIKE wildcards -- \\, % and _ -- escaped, so each is data."
  (replace-all (replace-all (replace-all text "\\" "\\\\") "%" "\\%") "_" "\\_"))

(defun like-pattern (terms)
  "TERMS as one LIKE pattern: every term must appear, in any order. The
prefix star is stripped: LIKE has no prefix operator."
  (format nil "%~{~a~^%~}%"
          (loop for term in terms collect (like-escape (getf term :text)))))

;;; --- the database ----------------------------------------------------------------

(defun db-file ()
  "The Mnemosyne database the section names, as a pathname; a refusal naming
db_path when the section has not answered it."
  (let ((named (setting :db-path)))
    (unless (and named (stringp named) (plusp (length (string-trim '(#\Space #\Tab) named))))
      (fail "no db_path to read: answer \"db_path\" in the ram-first-context section ~
             (a Mnemosyne database such as ~/.hermes/mnemosyne/data/mnemosyne.db), in /settings ~
             or in ~/.nodecode/config.jsonc"))
    (let ((path (nlk:expand-home named)))
      (if (uiop:absolute-pathname-p path)
          path
          (merge-pathnames path (uiop:getcwd))))))

(defun available-p ()
  "Whether the section's database file is there: the original's loader check."
  (let ((path (db-file)))
    (and path (probe-file path) t)))

(defun call-with-db (function)
  "FUNCTION called with a read-only connection of its own to the section's
database: PRAGMA query_only refuses any write that would reach the file."
  (let ((path (db-file)))
    (unless (probe-file path)
      (fail "no Mnemosyne database at ~a" (namestring path)))
    (sqlite:with-open-database (db (namestring path) :busy-timeout (setting :busy-timeout-ms))
      (sqlite:execute-non-query db "pragma query_only = 1")
      (funcall function db))))

(defun rows (sql parameters)
  "SQL's rows on a read-only connection as plists keyed like +COLUMNS+."
  (call-with-db
   (lambda (db)
     (loop for row in (apply #'sqlite:execute-to-list db sql parameters)
           collect (loop for key in +columns+
                         for value in row
                         collect (intern (string-upcase (substitute #\- #\_ key)) :keyword)
                         collect value)))))

(defun limit-asked (limit)
  "LIMIT as this section runs it: an integer at least one, at most max_limit,
defaulting to default_limit."
  (let* ((asked (or limit (setting :default-limit)))
         (asked (if (and (integerp asked) (plusp asked)) asked (setting :default-limit))))
    (min asked (setting :max-limit))))

(defun log-usage (query hits)
  "One local line {ts, q_len, hits} when the section turned usage_log on;
nothing at all when it is off, and a broken log never breaks a search."
  (when (setting :usage-log)
    (handler-case
        (let ((path (nlk:home "plugin-data/ram-first-context/usage.jsonl")))
          (ensure-directories-exist path)
          (with-open-file (out path :direction :output :if-exists :append :if-does-not-exist :create)
            (format out "{\"ts\": ~s, \"q_len\": ~d, \"hits\": ~d}~%" (nlk:iso-now) (length query) hits)))
      (error () nil))))

(defun matches (query &key limit)
  "The rows QUERY matches in the section's database: FTS5 first, LIKE when
the index is missing or the FTS statement cannot run. => a list of plists,
empty when QUERY holds no term; signals RAM-FIRST-CONTEXT-ERROR when the
section names no database or the file is not there."
  (let ((terms (query-terms query)))
    (if (null terms)
        '()
        (let ((found (handler-case (rows +fts-sql+ (list (fts-query terms) (limit-asked limit)))
                       (sqlite:sqlite-error () (rows +like-sql+ (list (like-pattern terms) (limit-asked limit)))))))
          (log-usage query (length found))
          found))))

;;; --- the answer ------------------------------------------------------------------

(defun member-text (value)
  "VALUE as a JSON member the original's row carried: text NIL as empty."
  (or value ""))

(defun row-object (row)
  "ROW as the JSON object the original tool answered for it."
  (nlk:json-object "id" (member-text (getf row :id))
                    "content" (member-text (getf row :content))
                    "source" (member-text (getf row :source))
                    "importance" (member-text (getf row :importance))
                    "timestamp" (member-text (getf row :timestamp))
                    "memory_type" (member-text (getf row :memory-type))
                    "metadata_json" (member-text (getf row :metadata-json))
                    "rank" (or (getf row :rank) 0)))

(defun recall (query &key limit)
  "The tool's answer, exactly the original's: a JSON array of rows, or of one
{\"error\": ...} record. Never a signal."
  (handler-case
      (nlk:encode-json-object (coerce (mapcar #'row-object (matches query :limit limit)) 'vector))
    (error (condition)
      (nlk:encode-json-object
       (vector (nlk:json-object "error" (nlk:one-line condition)))))))

;;; --- the command's text -----------------------------------------------------------

(defun matches-text (query)
  "What /ram-first-context QUERY says: the line of the count, then a line a
match; a refusal's own words when the section or the file is not ready."
  (handler-case
      (let ((rows (matches query))
            (db (namestring (db-file))))
        (if (null rows)
            (format nil "ram-first-context: no memory in ~a matches ~s — every term must match, and a trailing * is a prefix"
                    db query)
            (with-output-to-string (out)
              (format out "ram-first-context: ~d match~:p of ~s in ~a"
                      (length rows) query db)
              (dolist (row rows)
                (format out "~&· ~@[~a~]~@[ · ~a~]~@[ · importance ~a~]: ~a"
                        (getf row :timestamp) (getf row :source) (getf row :importance)
                        (nlk:one-line (member-text (getf row :content)) :cap 300))))))
    (ram-first-context-error (condition) (princ-to-string condition))
    (error (condition) (format nil "ram-first-context: the search failed: ~a" (nlk:one-line condition)))))
