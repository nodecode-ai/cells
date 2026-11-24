;;;; cell-test.lisp --- lifecycle, the five tools, the refusals, the command.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring and its own reading of a
;;;; trace: the section (a bound under its minimum refuses the start), each
;;;; tool through the real dispatch seam, the order and the gaps a timeline
;;;; leaves visible, the hypotheses a diagnosis ranks, a replay that executes
;;;; nothing, a run that survives a restart, and the verbs /rundebug answers.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "run-debugger"
  (:config)
  (:help :run-debugger)
  (:command "rundebug")
  (:running
   (is (nle::find-tool "rundebug_import") "rundebug_import is registered")
   (is (nle::find-tool "rundebug_timeline") "rundebug_timeline is registered")
   (is (nle::find-tool "rundebug_explain") "rundebug_explain is registered")
   (is (nle::find-tool "rundebug_replay") "rundebug_replay is registered")
   (is (nle::find-tool "rundebug_status") "rundebug_status is registered")
   (is (equal 67108864 (nodecode-run-debugger::setting :max-bytes)) "the original plugin's import bound is the default"))
  (:stopped
   (is (null (nle::find-tool "rundebug_import")) "stop unregisters rundebug_import")
   (is (null (nle::find-tool "rundebug_status")) "and rundebug_status"))
  (:refused
   ("max_events" 0)
   ("max_bytes" 1)
   ("max_line_bytes" 1)))

;;; --- the import and the timeline ---------------------------------------------------

(deftest run-debugger-cell-imports-one-file-and-answers-its-run (with-rundebug ()
  (let* ((path (scenario-trace "run-a" "run-a"))
         (text (import-text path)))
    (is (cl:search "imported 1 run" text) text)
    (is (cl:search "run run-a: 3 events, 1 attempt" text) "the answer counts what it read")
    (is (cl:search "run-a" (nodecode-run-debugger:status)) "the run is in the store")
    (let ((timeline (tool-text "rundebug_timeline" (nlk:json-object "run" "run-a"))))
      (is (cl:search "timeline for run run-a" timeline))
      (is (cl:search "status=failed" timeline))
      (is (cl:search "tool_call read_file" timeline) "the tool call is named")
      (let ((tool (cl:search "tool_call" timeline))
            (provider (cl:search "provider_error" timeline)))
        (is (and tool provider (< tool provider)) "the source sequence orders them, parents first"))
      (is (cl:search "wall-clock order is not causation" timeline) "the reading is stated")))))

(deftest run-debugger-cell-a-diagnosis-separates-what-is-reported-from-what-it-may-mean
    (with-rundebug ()
  (scenario-trace "run-b" "run-b")
  (import-text (trace-path "run-b"))
  (let ((explain (tool-text "rundebug_explain" (nlk:json-object "run" "run-b"))))
    (is (cl:search "diagnosis for run run-b" explain))
    (is (cl:search "observed (what the imported evidence reports)" explain))
    (is (cl:search "failure:tool_call" explain) "the failed tool call is observed, not inferred")
    (is (cl:search "[R002]" explain) "the unrecovered tool failure is a hypothesis")
    (is (cl:search "alternative reading" explain) "each hypothesis carries what else it could be")
    (is (cl:search "falsify by" explain) "and how to falsify it")
    (is (cl:search "false_success:suspected" explain) "a success whose text verified nothing")
    (is (cl:search "causal findings: none" explain) "no causal claim is minted"))))

(deftest run-debugger-cell-a-replay-executes-nothing (with-rundebug ()
  (scenario-trace "run-c" "run-c")
  (import-text (trace-path "run-c"))
  (let ((replay (tool-text "rundebug_replay" (nlk:json-object "run" "run-c"))))
    (is (cl:search "inert replay of run run-c (nothing is executed)" replay))
    (is (cl:search "no subprocess, no network" replay))
    (is (cl:search "permission denied" replay) "the imported error text is shown as data")
    (is (cl:search "[reported]" replay) "and keeps its provenance"))))

(deftest run-debugger-cell-a-parent-the-trace-lacks-is-a-gap (with-rundebug ()
  (let ((path (write-trace "run-d"
                            (list (envelope "run-d" "complete")
                                  (attempt-record "run-d" "a1" "completed")
                                  (event-record "run-d" "e1" "tool_call" "failed" 1
                                                :name "write_file" :parents '("e0"))))))
    (import-text path)
    (let ((timeline (tool-text "rundebug_timeline" (nlk:json-object "run" "run-d")))
          (explain (tool-text "rundebug_explain" (nlk:json-object "run" "run-d"))))
      (is (cl:search "missing_parent" timeline) "the missing parent is visible")
      (is (cl:search "names parent e0" timeline))
      (is (cl:search "[R005]" explain) "and it weakens what can be concluded")))))

;;; --- what is refused ----------------------------------------------------------------

(deftest run-debugger-cell-refuses-what-is-not-one-selected-trace (with-rundebug ()
  (let ((directory (ensure-directories-exist (nlk:home "run-debugger-test/folder/"))))
    (is (cl:search "is a directory" (import-text directory)) "a folder is refused"))
  (let ((archive (scratch-file "trace.zip" "not really an archive")))
    (is (cl:search "is an archive" (import-text archive)) (import-text archive)))
  (let ((database (scratch-file "trace.sqlite" "SQLite format 3")))
    (is (cl:search "is a raw database" (import-text database)) (import-text database)))
  (let ((wrong (scratch-file "wrong.jsonl" "{\"kind\":\"note\",\"text\":\"hi\"}")))
    (is (cl:search "not the trace envelope" (import-text wrong)) "another shape is refused"))
  (let ((path (write-trace "run-f" (list (envelope "run-f" "complete")
                                          (nlk:json-object "kind" "event" "event_id" "e1"
                                                           "run_id" "run-f" "attempt_id" "a1"
                                                           "event_kind" "tea_time"
                                                           "reported_status" "failed"
                                                           "semantic_provenance" "reported"
                                                           "source_sequence" 1)))))
    (is (cl:search "tea_time" (import-text path)) "an event kind the format does not name refuses"))))

(deftest run-debugger-cell-a-truncated-source-needs-allow-partial (with-rundebug ()
  (let* ((line (nlk:encode-json-object (envelope "run-g")))
         (path (scratch-file "run-g.jsonl" (format nil "~a~%~a" line
                                                    "{\"kind\":\"attempt\",\"attempt_id\""))))
    (is (cl:search "ends mid-record" (import-text path)) "refused by default")
    (is (cl:search "PARTIAL" (import-text path :allow-partial t)) "and kept, marked PARTIAL, when the caller says so"))))

(deftest run-debugger-cell-a-run-nobody-imported-is-named (with-rundebug ()
  (let ((text (tool-text "rundebug_timeline" (nlk:json-object "run" "run-nowhere"))))
    (is (cl:search "there is no imported run" text))
    (is (cl:search "ERROR:" text) "a tool answers a failure as text the model repairs from"))))

(deftest run-debugger-cell-a-call-without-its-files-never-reaches-the-disk (with-rundebug ()
  (let ((text (tool-text "rundebug_import" (nlk:json-object))))
    (is (cl:search "files" text) "the refusal names the missing member"))))

;;; --- the store, and the command ------------------------------------------------------

(deftest run-debugger-cell-a-run-survives-a-restart (with-rundebug ()
  (scenario-trace "run-h" "run-h")
  (import-text (trace-path "run-h"))
  (is (probe-file (merge-pathnames
                   (format nil "~a.json" (nlk:short-digest "run-h"))
                   (nlk:home "run-debugger/runs/"))) "the run is one JSON file under the home")
  (funcall stop)
  (setf stop nil)
  (setf stop (run-debugger-start))
  (is (cl:search "timeline for run run-h"
                 (tool-text "rundebug_timeline" (nlk:json-object "run" "run-h"))) "a fresh start reads it back")))

(deftest run-debugger-cell-the-command-runs-the-same-verbs (with-rundebug ()
  (scenario-trace "run-i" "run-i")
  (is (cl:search "import" (cell-entry "nodecode-run-debugger" "rundebug" "" "s1")) "typed alone it lists what it does")
  (is (cl:search "run-i" (cell-entry "nodecode-run-debugger" "rundebug"
                                      (format nil "import ~a" (trace-path "run-i")) "s1")))
  (is (cl:search "timeline for run run-i"
                 (cell-entry "nodecode-run-debugger" "rundebug" "timeline run-i" "s1")))
  (is (cl:search "run-i" (cell-entry "nodecode-run-debugger" "rundebug" "runs" "s1")))
  (let ((report (cell-entry "nodecode-run-debugger" "rundebug" "explain run-i" "s1")))
    (is (cl:search "diagnosis for run run-i" report)))
  (let ((path (cell-entry "nodecode-run-debugger" "rundebug" "report run-i" "s1")))
    (is (cl:search "report written to" path))
    (is (probe-file (pathname (subseq path (length "report written to ")))) "the report is a file an operator can read"))))
