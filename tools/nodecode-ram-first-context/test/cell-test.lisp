;;;; cell-test.lisp --- lifecycle; the tool through calls; /ram-first-context.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the start installs the tool, /ram-first-context and
;;;; the manual, and the stop takes all three back; a section of the wrong
;;;; shape refuses; the verbs refuse while the cell is not running; a search
;;;; answers the rows the Mnemosyne tables hold, bm25-ranked, with reserved
;;;; words, a colon and a prefix read as data; a database without the FTS
;;;; index falls back to LIKE, importance-ordered, rank 0, its % literal; a
;;;; section naming no database, or a file that is not there, answers the
;;;; error record instead of raising; a search changes no byte of the file;
;;;; and the usage log is written only when the section turns it on.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "ram-first-context"
  (:fixture with-mnemosyne ())
  (:config "db_path" (ram-db))
  (:help :ram-first-context)
  (:command "ram-first-context")
  (:running
   (is (= 5 (nodecode-ram-first-context::setting :default-limit)) "five results a search by default")
   (is (= 50 (nodecode-ram-first-context::setting :max-limit)) "a call's limit is held to fifty")
   (is (eq t (nodecode-ram-first-context:available-p)) "the database the section names is there"))
  (:refused ("default_limit" 0) ("max_limit" 0) ("busy_timeout_ms" -1))
  (:idle nodecode-ram-first-context:ram-first-context-error
         (nodecode-ram-first-context:matches "gate")))

(deftest ram-first-context-cell-the-tool-answers-ranked-matches ()
  (with-mnemosyne ()
    (with-cell-stop ((ram-first-context-start "db_path" (ram-db)))
      (let ((rows (ram-answer "{\"query\": \"gate\"}")))
        (is (= 1 (length rows)) "one memory carries the word")
        (is (search "AND gate" (ram-row (first rows) "content")) "and it is answered whole")
        (is (plusp (length (ram-row (first rows) "source"))) "with its source")
        (is (plusp (length (ram-row (first rows) "timestamp"))) "and its timestamp"))
      (let ((rows (ram-answer "{\"query\": \"the AND gate\"}" "rf-call-2")))
        (is (= 1 (length rows)) "every term must match"))
      (let ((rows (ram-answer "{\"query\": \"gate\", \"limit\": 1}" "rf-call-3")))
        (is (= 1 (length rows)) "a call's limit is honoured")
        (is (string= "the AND gate is a reserved word" (ram-row (first rows) "content")) "ranked first: the memory carrying the word")))))

(deftest ram-first-context-cell-reserved-words-colons-and-prefixes-are-data ()
  (with-mnemosyne ()
    (with-cell-stop ((ram-first-context-start "db_path" (ram-db)))
      (let ((rows (ram-answer "{\"query\": \"AND\"}" "rf-and")))
        (is (= 1 (length rows)) "a reserved word is searched as a word, not parsed as syntax"))
      (let ((rows (ram-answer "{\"query\": \"8080:8081\"}" "rf-colon")))
        (is-present (row (first rows)) "a colon does not break the FTS query"
          (is (search "8080:8081" (ram-row row "content")) "and finds the memory carrying it")
          (is (minusp (nlk:json-value row :number "rank")) "on the FTS path: a bm25 rank")))
      (let ((rows (ram-answer "{\"query\": \"term*\"}" "rf-prefix")))
        (is-present (row (first rows)) "a trailing star searches a prefix"
          (is (search "terminal emulator" (ram-row row "content")) "finding the word it prefixes"))))))

(deftest ram-first-context-cell-without-the-index-the-search-falls-back-to-like ()
  (with-mnemosyne (:fts nil)
    (with-cell-stop ((ram-first-context-start "db_path" (ram-db)))
      (let ((rows (ram-answer "{\"query\": \"gate\"}" "rf-like")))
        (is-present (row (first rows)) "the memory is found without the FTS index"
          (is (search "AND gate" (ram-row row "content")) "by the words it carries")
          (is (= 0 (nlk:json-value row :number "rank")) "and answers rank 0, as the fallback says")))
      (let ((rows (ram-answer "{\"query\": \"disk\"}" "rf-order")))
        (is (string= "50% of the disk is free" (ram-row (first rows) "content")) "the word is still found")
        (is (= 1 (length rows)) "once"))
      (let ((rows (ram-answer "{\"query\": \"50%\"}" "rf-percent")))
        (is (= 1 (length rows)) "a % in the query stays literal, so only the one memory matches"))
      (let ((rows (ram-answer "{\"query\": \"the\"}" "rf-importance")))
        (is (string= "the AND gate is a reserved word" (ram-row (first rows) "content")) "importance orders the fallback")))))

(deftest ram-first-context-cell-a-missing-database-answers-the-error-record ()
  (with-cell-stop ((ram-first-context-start "db_path" (ram-db "absent.db")))
    (let ((answer (ram-answer "{\"query\": \"gate\"}" "rf-absent")))
      (is (= 1 (length answer)) "one record, not an exception")
      (is (search "no Mnemosyne database at" (ram-row (first answer) "error")) "which names what is missing")
      (is-carrying (text (cell-entry "nodecode-ram-first-context" "ram-first-context" "gate" nil))
        ("no Mnemosyne database at") ("absent.db" "the file the section names")))))

(deftest ram-first-context-cell-a-search-changes-no-byte-and-logs-only-when-asked ()
  (with-mnemosyne ()
    (let ((before (ram-digest (ram-db))))
      (with-cell-stop ((ram-first-context-start "db_path" (ram-db)))
        (ram-answer "{\"query\": \"gate\"}" "rf-read-1")
        (ram-answer "{\"query\": \"term*\"}" "rf-read-2")
        (is (equal before (ram-digest (ram-db))) "the database is the same file the search found")
        (is (null (probe-file (nlk:home "plugin-data/ram-first-context/usage.jsonl"))) "usage_log off writes nothing at all"))
      (with-cell-stop ((ram-first-context-start "db_path" (ram-db) "usage_log" t))
        (ram-answer "{\"query\": \"gate\"}" "rf-read-3")
        (let ((log (probe-file (nlk:home "plugin-data/ram-first-context/usage.jsonl"))))
          (is-present (written (and log (uiop:read-file-string log))) "usage_log on writes one local line"
            (is (search "q_len" written) "counting the query's length")
            (is (not (search "gate" written)) "and never the query's words")))))))

(deftest ram-first-context-cell-the-command-searches-and-reads-its-settings ()
  (with-mnemosyne ()
    (with-cell-stop ((ram-first-context-start "db_path" (ram-db)))
      (is-carrying (text (cell-entry "nodecode-ram-first-context" "ram-first-context" "gate" nil))
        ("ram-first-context: 1 match of \"gate\"") ("the AND gate is a reserved word"))
      (is-carrying (text (cell-entry "nodecode-ram-first-context" "ram-first-context" "unfindable" nil))
        ("no memory in") ("unfindable") ("a trailing * is a prefix"))
      (is-carrying (text (cell-entry "nodecode-ram-first-context" "ram-first-context" "" nil))
        ("ram-first-context: reads") ("mnemosyne.db") ("/ram-first-context TERMS searches it")))))
