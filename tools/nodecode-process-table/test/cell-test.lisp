;;;; cell-test.lisp --- lifecycle, the parent judgement, the filters, the wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own data and wiring: which parent
;;;; states the table gives a written-in table, which entries each filter
;;;; leaves, that the ancestor chain comes back as context rows, that the
;;;; tool answers through the real dispatch seam, that the machine's own
;;;; reader answers rows, and that /process-table prints the same table.
;;;; Offline: the table is a fixture, and the one test that reads the real
;;;; machine only reads it.

(in-package #:nodecode.test)

;;; --- a table written here ------------------------------------------------------

(defun process-table-fixture-rows ()
  "A table with one of everything the judgement distinguishes: a root, a live
child, a live grandchild, a process whose parent is gone, one whose parent's
PID was reused by a newer process, and one the account cannot read."
  (let ((now (get-universal-time)))
    (list (list :pid 1 :parent-pid 0 :name "init" :path "/sbin/init"
                :started (- now 100000) :threads 1)
          (list :pid 100 :parent-pid 1 :name "node" :path "/opt/app/bin/node"
                :started (- now 500) :threads 8)
          (list :pid 200 :parent-pid 999 :name "python3" :path "/opt/app/bin/python3"
                :started (- now 400) :threads 2)
          (list :pid 300 :parent-pid 100 :name "gateway" :path "/opt/app/bin/gateway"
                :started (- now 250) :threads 4)
          (list :pid 400 :parent-pid 300 :name "worker" :path "/opt/app/bin/worker"
                :started (- now 600) :threads 1)
          (list :pid 500 :parent-pid 200 :name "agent" :path nil
                :started (- now 100) :threads nil))))

(defmacro with-written-table ((&rest pairs) &body body)
  "Run BODY with the machine's reader replaced by the fixture and the cell
started, so (setting ...) answers."
  `(let ((nodecode-process-table::*row-source* #'process-table-fixture-rows))
     (with-cell-stop ((process-table-start ,@pairs))
       ,@body)))

(defun process-table-names (collected)
  "The names of the entries a COLLECT answered, in order."
  (mapcar #'nodecode-process-table::process-entry-name (getf collected :entries)))

(defun process-table-state (collected name)
  "The parent state COLLECT gave the entry called NAME."
  (nodecode-process-table::process-entry-state
   (find name (getf collected :entries)
         :key #'nodecode-process-table::process-entry-name :test #'string=)))

(defun process-table-note-p (note)
  "Whether NOTE is the one that says an empty answer how to widen the call."
  (and (stringp note) (search "nothing matched" note)))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "process-table"
  (:command "process-table")
  (:help :process-table)
  (:running (is (nle::find-tool "process_table") "the tool is registered")
            (is (equal "auto" (nodecode-process-table::setting :reader)) "the declared default reader")
            (is (= 100 (nodecode-process-table::setting :limit)) "the declared default limit"))
  (:stopped (is (null (nle::find-tool "process_table")) "stop unregisters the tool"))
  (:refused ("reader" "guess")
            ("limit" 0)
            ("timeout_seconds" 0)))

;;; --- the parent judgement ---------------------------------------------------------

(deftest process-table-cell-names-each-parent-state (with-written-table ()
  (let ((collected (nodecode-process-table::collect :include-system t)))
    (is (equal "root" (process-table-state collected "init")) "the first process has no parent")
    (is (equal "alive" (process-table-state collected "node")) "a parent that started before its child is the real one")
    (is (equal "gone" (process-table-state collected "python3")) "a parent that is not in the table is gone")
    (is (equal "reused" (process-table-state collected "worker")) "a parent that started after its child took the PID over")
    (is (= 6 (getf collected :total)) "the whole table is six processes")
    (is (= 1 (getf collected :gone)) "one parent is gone")
    (is (= 1 (getf collected :reused)) "and one PID was reused")
    (is (equal "init" (first (process-table-names collected))) "oldest first"))))

;;; --- the filters -------------------------------------------------------------------

(deftest process-table-cell-filters-cut-the-table (with-written-table ()
  (is (equal '("gateway") (process-table-names (nodecode-process-table::collect :process "GATE"))) "a name substring matches, case ignored")
  (is (equal '("node") (process-table-names (nodecode-process-table::collect :process "/opt/app/bin/node"))) "the path is searched too")
  (is (equal '("node") (process-table-names (nodecode-process-table::collect :parent 1))) "a parent filter leaves that parent's children")
  (let ((orphans (process-table-names (nodecode-process-table::collect :orphans-only t))))
    (is (member "python3" orphans :test #'string=) "a gone parent is an orphan")
    (is (member "worker" orphans :test #'string=) "so is a reused PID")
    (is (not (member "gateway" orphans :test #'string=)) "a live parent is not"))
  (is (not (member "init" (process-table-names (nodecode-process-table::collect :include-system nil)) :test #'string=)) "include_system false leaves the system's own images out")
  (is (not (member "agent" (process-table-names (nodecode-process-table::collect :include-system nil)) :test #'string=)) "and a process with no path to read")
  (is (member "gateway" (process-table-names (nodecode-process-table::collect :pid 300)) :test #'string=) "one PID matches that PID")
  (is (equal '("agent" "python3") (sort (process-table-names (nodecode-process-table::collect :pids (vector 200 500))) #'string<)) "a list of PIDs matches all of them")
  (is (= 2 (length (process-table-names (nodecode-process-table::collect :limit 2)))) "the limit bounds the answer")
  (is (find-if #'process-table-note-p (getf (nodecode-process-table::collect :process "nothing-called-this") :notes)) "an empty answer says how to widen the call")))

(deftest process-table-cell-the-section-limit-is-the-default (with-written-table ("limit" 1)
  (is (= 1 (length (process-table-names (nodecode-process-table::collect)))) "a call with no limit of its own takes the section's")
  (is (= 3 (length (process-table-names (nodecode-process-table::collect :limit 3)))) "and a call that names one, takes its own")))

;;; --- the ancestor chain ---------------------------------------------------------------

(deftest process-table-cell-ancestry-comes-back-as-context (with-written-table ()
  (let ((collected (nodecode-process-table::collect :pid 300 :with-ancestry t)))
    (is (equal '("gateway") (process-table-names collected)) "the match itself")
    (is (equal '("node" "init") (mapcar #'nodecode-process-table::process-entry-name (getf collected :context))) "its parent, then its parent's parent")
    (is (every #'nodecode-process-table::process-entry-context (getf collected :context)) "every one of them is marked as context"))
  (is (null (getf (nodecode-process-table::collect :pid 300) :context)) "no chain unless one is asked for")))

;;; --- the real dispatch seam -------------------------------------------------------------

(deftest process-table-cell-the-tool-answers-through-the-wire (with-written-table ()
  (let* ((result (execute-wire-call "call-process-1" "process_table" "{\"orphans_only\": true, \"limit\": 2}"))
         (answer (nlk:decode-json (nle::message-content result))))
    (is-shape result ("role" "tool") ("tool_call_id" "call-process-1"))
    (is (eq t (nlk:json-value answer :boolean "ok")) "the call answered a table")
    (is (= 2 (nlk:json-value answer :integer "matched")) "the limit is the call's")
    (is (= 6 (nlk:json-value answer :integer "total_processes")) "the whole table is counted")
    (is (= 2 (nlk:json-value answer :integer "orphans")) "the orphans are counted")
    (is (= 2 (length (nlk:json-value answer :array "notes"))) "the notes are a list of lines"))))

(deftest process-table-cell-a-call-that-names-no-limit-still-answers (with-written-table ()
  (let* ((result (execute-wire-call "call-process-2" "process_table" "{}"))
         (answer (nlk:decode-json (nle::message-content result))))
    (is (eq t (nlk:json-value answer :boolean "ok")) "an empty call is the whole table")
    (is (= 6 (nlk:json-value answer :integer "matched")) "every process, under the default limit"))))

;;; --- the machine's own reader ---------------------------------------------------------------

(deftest process-table-cell-the-reader-reads-this-machine (with-cell-stop ((process-table-start "reader" "auto"))
  (let ((rows (nodecode-process-table::read-rows)))
    (is (plusp (length rows)) "the reader answers rows")
    (is (every (lambda (row) (and (integerp (getf row :pid)) (plusp (getf row :pid)))) rows) "each row carries a PID")
    (is (every (lambda (row) (integerp (getf row :parent-pid))) rows) "and a parent PID, 0 for the first process")
    (when (probe-file #p"/proc/stat")
      (is (find (getf (first rows) :pid) (nodecode-process-table::proc-pids) :test #'=) "a PID the reader reported is one /proc lists")))))

;;; --- /process-table ------------------------------------------------------------------------

(deftest process-table-cell-slash-prints-the-table (with-written-table ()
  (let ((text (nodecode-process-table::run-slash "" "s1")))
    (is (search "gateway" text) "the processes are named")
    (is (search "alive" text) "with the state of the parent each points at")
    (is (search "process(es) matched" text) "and the note saying how many of how many"))
  (let ((orphans (nodecode-process-table::run-slash "orphans" "s1")))
    (is (search "python3" orphans) "the orphans word lists only the leftovers")
    (is (not (search "node" orphans)) "and not the live ones"))
  (is (search "node" (nodecode-process-table::run-slash "node" "s1")) "any other word is a name filter")))
