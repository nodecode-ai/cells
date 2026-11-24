;;;; cell-test.lisp --- the edit-ledger cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches a store, a model or a network: a turn is a :FRAME op
;;;; carrying the fact the core would publish, so "one turn that changed two
;;;; files is one row of two files" and "a turn that changed nothing is no
;;;; row" are facts a test reads rather than a session's behaviour.

(in-package #:nodecode.test)

(define-test-slice "edit-ledger" "EDIT-LEDGER-CELL-" :start nodecode-edit-ledger:start-cell)

;;; --- the fixture, and one fact's shape -----------------------------------------

(defmacro with-fresh-ledger (&body body)
  "BODY with no ledger on disk and no turn half-remembered."
  `(progn
     (setf nodecode-edit-ledger::*turns* (make-hash-table :test #'equal)
           nodecode-edit-ledger::*flushed* (make-hash-table :test #'equal))
     (ignore-errors (delete-file (ledger-path)))
     ,@body
     (ignore-errors (delete-file (ledger-path)))))

(defun ledger-path () (nlk:home "edit-ledger/edits.jsonl"))

(defun rows-on-disk () (nodecode-edit-ledger::read-ledger (ledger-path)))

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "edit-ledger"
  (:config "keep_rows" 500 "max_bytes" 100000)
  (:hooks :frame)
  (:help :edit-log)
  (:command "edit-log")
  (:refused ("keep_rows" 0) ("max_bytes" "lots"))
  (:fixture with-fresh-ledger))

(defun fact-op (kind &key (session "s1") (turn "t1") payload)
  "One session_stream_event :frame op carrying a durable fact of KIND."
  (list :session-id session
        :kind "session_stream_event"
        :payload (nlk:json-object
                  "frame" (nlk:json-object
                           "turn_id" turn
                           "event" (nlk:json-object
                                    "type" kind
                                    "payload" (or payload (nlk:json-object))
                                    "metadata" (nlk:json-object))))))

(defun diff-payload (&rest paths)
  "A tool result whose workspace diff names PATHS."
  (nlk:json-object
   "metadata" (nlk:json-object
               "workspace_diff" (nlk:json-object
                                 "files" (coerce (loop for path in paths
                                                        collect (nlk:json-object "path" path))
                                                 'vector)))))

(defun receipt-payload (&rest calls)
  "A tool result carrying CALLS as receipts, each (VERB . SOURCE)."
  (nlk:json-object
   "metadata" (nlk:json-object
               "calls" (coerce (loop for (verb . source) in calls
                                      collect (nlk:json-object "verb" verb "source" source))
                               'vector))))

(defun usage-payload (model)
  (nlk:json-object "model" model))

(defun chat (owner op) (cell-publish owner op))

;;; --- what a turn leaves behind ------------------------------------------------

(deftest edit-ledger-cell-a-turn-that-changed-files-is-one-row (with-fresh-ledger)
  (with-cell-stop ((edit-ledger-start))
    (chat "nodecode-edit-ledger"
          (fact-op "turn.tool_result" :payload (diff-payload "/work/a.py" "/work/b.md")))
    (chat "nodecode-edit-ledger" (fact-op "turn.usage" :payload (usage-payload "test-model")))
    (chat "nodecode-edit-ledger" (fact-op "turn.completed"))
    (let ((rows (rows-on-disk)))
      (is (= 1 (length rows)) "one turn that changed files is one row")
      (is (= 2 (nlk:json-value (first rows) :integer "files")) "both files are counted")
      (is (equal "test-model" (nlk:json-value (first rows) :string "model")) "the model the turn's round named")
      (is (= 1 (nlk:json-value (first rows) :integer "types" ".py")) "one .py file")
      (is (= 1 (nlk:json-value (first rows) :integer "types" ".md")) "one .md file")
      (is (string= (nlk:short-digest "s1" 12)
                   (nlk:json-value (first rows) :string "session")) "the session is a short hash, never the id itself")
      (is (null (search "/work/a.py" (nlk:encode-json-object (first rows)))) "no path is kept"))))

(deftest edit-ledger-cell-a-write-file-receipt-counts-without-a-diff (with-fresh-ledger)
  (with-cell-stop ((edit-ledger-start))
    (chat "nodecode-edit-ledger"
          (fact-op "turn.tool_result" :payload (receipt-payload '("write-file" . "/work/x.lisp"))))
    (chat "nodecode-edit-ledger" (fact-op "turn.completed"))
    (let ((rows (rows-on-disk)))
      (is (= 1 (length rows)) "a receipt is a changed file")
      (is (= 1 (nlk:json-value (first rows) :integer "types" ".lisp"))))))

(deftest edit-ledger-cell-a-read-only-turn-is-no-row (with-fresh-ledger)
  (with-cell-stop ((edit-ledger-start))
    (chat "nodecode-edit-ledger" (fact-op "turn.tool_result" :payload (receipt-payload '("help" . "eval"))))
    (chat "nodecode-edit-ledger" (fact-op "turn.completed"))
    (is (null (rows-on-disk)) "a turn that changed nothing leaves no row")))

(deftest edit-ledger-cell-one-file-once-per-turn (with-fresh-ledger)
  (with-cell-stop ((edit-ledger-start))
    (chat "nodecode-edit-ledger" (fact-op "turn.tool_result" :payload (diff-payload "/work/a.py")))
    (chat "nodecode-edit-ledger" (fact-op "turn.tool_result" :payload (diff-payload "/work/a.py")))
    (chat "nodecode-edit-ledger" (fact-op "turn.completed"))
    (let ((rows (rows-on-disk)))
      (is (= 1 (length rows)) "one row")
      (is (= 1 (nlk:json-value (first rows) :integer "files")) "the file counts once"))))

(deftest edit-ledger-cell-a-repeated-terminal-fact-adds-no-row (with-fresh-ledger)
  (with-cell-stop ((edit-ledger-start))
    (chat "nodecode-edit-ledger" (fact-op "turn.tool_result" :payload (diff-payload "/work/a.py")))
    (chat "nodecode-edit-ledger" (fact-op "turn.completed"))
    (chat "nodecode-edit-ledger" (fact-op "turn.completed"))
    (is (= 1 (length (rows-on-disk))) "a replayed terminal fact is not a second turn")))

;;; --- /edit-log ------------------------------------------------------------------

(deftest edit-ledger-cell-the-report-reads-the-ledger (with-fresh-ledger)
  (with-cell-stop ((edit-ledger-start))
    (chat "nodecode-edit-ledger" (fact-op "turn.tool_result" :payload (diff-payload "/work/a.py" "/work/b.py")))
    (chat "nodecode-edit-ledger" (fact-op "turn.usage" :payload (usage-payload "one-model")))
    (chat "nodecode-edit-ledger" (fact-op "turn.completed"))
    (let ((line (cell-entry "nodecode-edit-ledger" "edit-log" "all"))
          (types (cell-entry "nodecode-edit-ledger" "edit-log" "all types"))
          (recent (cell-entry "nodecode-edit-ledger" "edit-log" "all recent 5"))
          (days (cell-entry "nodecode-edit-ledger" "edit-log" "all days")))
      (is (search "one-model" line) line)
      (is (search "2" line) "the turn's two files are counted")
      (is (search "top type" line) "the table names its columns")
      (is (search ".py" types) types)
      (is (search "newest 1 of 1" recent) recent)
      (is (search "day" days) days))))

(deftest edit-ledger-cell-a-window-spans-hours-and-days (with-fresh-ledger)
  (with-cell-stop ((edit-ledger-start "default_hours" 48))
    (chat "nodecode-edit-ledger" (fact-op "turn.tool_result" :payload (diff-payload "/work/a.py")))
    (chat "nodecode-edit-ledger" (fact-op "turn.completed"))
    (is (search "last 7 days" (cell-entry "nodecode-edit-ledger" "edit-log" "7d")) "a window word is the report's title")
    (is (search "last 6 hours" (cell-entry "nodecode-edit-ledger" "edit-log" "6h")) "hours read the same way")
    (is (search "last 48 hours" (cell-entry "nodecode-edit-ledger" "edit-log" "")) "the section's default_hours is the window with no argument")
    (is (null (search "last 48 hours" (cell-entry "nodecode-edit-ledger" "edit-log" "all types"))) "an explicit all is its own window")
    (is (search "all recorded" (cell-entry "nodecode-edit-ledger" "edit-log" "all types")) "named as all recorded")))

(deftest edit-ledger-cell-an-unknown-option-answers-the-help (with-fresh-ledger)
  (with-cell-stop ((edit-ledger-start))
    (let ((answer (cell-entry "nodecode-edit-ledger" "edit-log" "sideways")))
      (is (search "Unknown option: sideways" answer) answer)
      (is (search "/edit-log clear" answer) "the help rides along"))))

(deftest edit-ledger-cell-clear-deletes-the-ledger (with-fresh-ledger)
  (with-cell-stop ((edit-ledger-start))
    (chat "nodecode-edit-ledger" (fact-op "turn.tool_result" :payload (diff-payload "/work/a.py")))
    (chat "nodecode-edit-ledger" (fact-op "turn.completed"))
    (let ((answer (cell-entry "nodecode-edit-ledger" "edit-log" "clear")))
      (is (search "deleted 1 recorded row" answer) answer))
    (is (null (rows-on-disk)) "and the file is gone")
    (is (search "none recorded" (cell-entry "nodecode-edit-ledger" "edit-log" "all")) "a cleared ledger records nothing")))

;;; --- the ledger's own bounds ------------------------------------------------------

(deftest edit-ledger-cell-the-ledger-keeps-its-newest-rows ()
  (with-fresh-ledger
    (dotimes (i 4)
      (nodecode-edit-ledger::append-row
       (ledger-path)
       (nodecode-edit-ledger::make-row :at (+ 1700000000 i) :session (format nil "s~d" i)
                                       :model "test-model" :paths (list "/work/a.py"))
       :keep 2 :max-bytes 1))
    (let ((rows (rows-on-disk)))
      (is (= 2 (length rows)) "the file is trimmed to its newest rows past max_bytes")
      (is (string= (nlk:short-digest "s3" 12)
                   (nlk:json-value (car (last rows)) :string "session")) "and the newest row is the one kept last"))))

(deftest edit-ledger-cell-file-types-read-as-extensions ()
  (is (equal ".py" (nodecode-edit-ledger::file-type "/w/a.py")))
  (is (equal ".tsx" (nodecode-edit-ledger::file-type "w\\src\\b.TSX")) "a path from any system")
  (is (equal "(none)" (nodecode-edit-ledger::file-type "/w/Makefile")))
  (is (equal "(none)" (nodecode-edit-ledger::file-type "/w/.env")) "a dotfile has no type")
  (is (equal "(other)" (nodecode-edit-ledger::file-type "/w/x.veryverylongext"))))
