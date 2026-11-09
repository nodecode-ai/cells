;;;; cell.lisp --- the section, the five tools, /rundebug, the primer.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original plugin registered five tools, one slash command and one CLI
;;;; subcommand, and no hooks. This cell registers the same five tools under
;;;; the same names, one /rundebug command covering the subcommand's
;;;; subcommands, and no hook at all: nothing observes a running turn, so
;;;; nothing here can slow one down or refuse one.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "run-debugger": {"enabled": true,
;;;;                     "max_bytes": 67108864,
;;;;                     "max_line_bytes": 8388608,
;;;;                     "max_events": 200000,
;;;;                     "allow_partial": false,
;;;;                     "retention_days": 90}
;;;; Every member has the original's default, so the section may be absent.

(in-package #:nodecode-run-debugger)

;;; --- the manual -----------------------------------------------------------------

(defparameter *primer*
  "Agent Run Debugger reads one trace file you name and tells you what it shows, offline:
the same five tools the original plugin gave its harness, in the rundebug: package, callable
through eval, and as the tools rundebug_import, rundebug_timeline, rundebug_explain,
rundebug_replay, rundebug_status.
  (rundebug:import-trace \"FILE\")   import one JSONL trace (the debugger's own format v1:
      a trace envelope, attempt records and event records -- schema
      hermes-run-debugger/trace, version 1). Answers the run id every other verb takes.
      Nothing is executed; directories, archives and raw databases are refused.
  (rundebug:timeline \"RUN\")    the recorded order: causal parents first, source sequence
      as the tie-break, gaps and concurrent pairs left visible. Wall-clock order is not
      causation.
  (rundebug:explain \"RUN\")     what the source REPORTS as failed or interrupted, the
      hypotheses over it ranked with their alternatives and falsifiers, the evidence that
      is missing, and the cost as reported. A completed run is not a passed task.
  (rundebug:replay \"RUN\")      the same order walked with nothing executed: imported
      command and error text is displayed as data.
  (rundebug:report \"RUN\" ?PATH)  the timeline and diagnosis as one markdown file under
      the home; answers its path.
  (rundebug:status)             the runs held, the lanes, the bounds, where they are kept.
A run is kept under the home; rundebug.retention_days deletes only files this cell wrote.
(help :run-debugger) answers this text; /rundebug runs the same verbs."
  "What (help :run-debugger) answers while the cell runs.")

;;; --- the boundaries ---------------------------------------------------------------

(nlk:define-contract import-request
  (:doc "Import one or more trace files you selected explicitly -- the debugger's own trace
format, one JSON object a line. Reads only the files named here: a directory, an archive or a
raw database is refused. Nothing is executed and nothing leaves the machine.")
  ("files" :array :of :string :required t
   :doc "absolute paths of the trace files to import")
  ("format" :string :default "v1" :options '("v1") :slot format-name
   :doc "the source format; v1 is the debugger's own trace format")
  ("allow_partial" :boolean
   :doc "keep the valid prefix of a truncated final line, marked PARTIAL with an import gap;
default off, so a truncated source is refused"))

(nlk:define-contract run-request
  (:doc "Name a run an earlier import answered, to read its timeline, its diagnosis or a replay
of it. Replay re-reads the recorded timeline and executes nothing.")
  ("run" :text :required t
   :doc "the run id rundebug_import answered"))

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object))))

;;; --- the tools ---------------------------------------------------------------------

(defun import-tool (request)
  "The rundebug_import tool: the files a call names, imported."
  (handler-case
      (import-trace (import-request-files request)
                     :format (import-request-format-name request)
                     :allow-partial (import-request-allow-partial request))
    (run-debugger-error (condition) (format nil "ERROR: ~a" condition))))

(defun guarded (function run-id)
  "FUNCTION of RUN-ID, with the cell's one condition rendered for the model."
  (handler-case (funcall function run-id)
    (run-debugger-error (condition) (format nil "ERROR: ~a" condition))))

(defun timeline-tool (request) (guarded #'timeline (run-request-run request)))
(defun explain-tool (request) (guarded #'explain (run-request-run request)))
(defun replay-tool (request) (guarded #'replay (run-request-run request)))

(defun status-tool (arguments)
  (declare (ignore arguments))
  (handler-case (status) (run-debugger-error (condition) (format nil "ERROR: ~a" condition))))

;;; --- the slash command ---------------------------------------------------------------

(defparameter +usage+
  "/rundebug runs one of these:
  /rundebug                       this list, and the runs this cell holds
  /rundebug runs                  every imported run, newest first
  /rundebug import FILE [FILE...] one JSONL trace (format v1) imported, offline
  /rundebug timeline RUN         the recorded order, its gaps and concurrent pairs
  /rundebug explain RUN          observed failures, ranked hypotheses, missing evidence
  /rundebug replay RUN           the same order walked; nothing executed
  /rundebug report RUN [PATH]    timeline and diagnosis as one markdown file
  /rundebug status               lanes, bounds and where runs are kept"
  "What /rundebug says when it does not know the verb, or when it is typed alone.")

(defun words (text)
  "TEXT split on blanks, empties dropped."
  (loop for word in (uiop:split-string (nlk:trimmed (or text "")) :separator " ")
        when (plusp (length word)) collect word))

(defun named-run (args)
  "The run id ARGS names, or a refusal."
  (let ((run-id (first (words args))))
    (unless run-id
      (fail "which run? Name the id rundebug_import answered; /rundebug runs lists them."))
    run-id))

(defun run-slash (args session-id)
  "/rundebug VERB ...: the tools' own verbs, typed."
  (declare (ignore session-id))
  (handler-case
      (let* ((words (words args))
             (verb (string-downcase (or (first words) "")))
             (rest (format nil "~{~a~^ ~}" (rest words))))
        (cond
          ((equal verb "") (format nil "~a~%~%~a" +usage+ (status)))
          ((equal verb "runs")
           (let ((all (runs)))
             (if (null all)
                 "no runs imported yet: /rundebug import FILE"
                 (format nil "~{~a~%~}"
                         (loop for run in all
                               collect (format nil "~a — ~:d event~:p, ~a, imported ~a"
                                               (getf run :run-id) (length (getf run :events))
                                               (getf run :completeness)
                                               (getf run :imported-at)))))))
          ((equal verb "status") (status))
          ((equal verb "import")
           (let ((files (rest words)))
             (unless files (fail "name at least one trace file: /rundebug import FILE"))
             (import-trace files :allow-partial (setting :allow-partial))))
          ((equal verb "timeline") (timeline (named-run rest)))
          ((equal verb "explain") (explain (named-run rest)))
          ((equal verb "replay") (replay (named-run rest)))
          ((equal verb "report")
           (let ((run-id (named-run rest))
                 (path (second (words rest))))
             (format nil "report written to ~a" (report run-id path))))
          (t (format nil "~a~%~%~a" +usage+ (status)))))
    (run-debugger-error (condition) (format nil "ERROR: ~a" condition))))

;;; --- the start ------------------------------------------------------------------------

(defun start ()
  "Read the runs already kept under the home, drop the ones past retention,
and keep the rest in hand."
  (setf *runs* (make-hash-table :test #'equal))
  (prune-runs)
  (dolist (run-id (stored-run-ids))
    (let ((run (run run-id)))
      (when run (setf (gethash run-id *runs*) run))))
  (nle:on-stop (lambda () (setf *runs* nil))))

(nle:define-cell run-debugger
  (:section ("run-debugger")
    (:guide "nothing here is required: every member has the original plugin's default. import
bounds keep a huge source from being read; allow_partial keeps a truncated final line;
retention_days deletes only runs this cell wrote")
    ("max_bytes" :integer :default 67108864 :min 1024
     :doc "hard cap on bytes read from one selected trace")
    ("max_line_bytes" :integer :default 8388608 :min 1024
     :doc "hard cap on one JSONL line, so an embedded snapshot cannot swallow the file")
    ("max_events" :integer :default 200000 :min 1
     :doc "hard cap on records accepted from one trace")
    ("allow_partial" :boolean :default nil
     :doc "keep the valid prefix of a truncated final line, marked PARTIAL with a gap")
    ("retention_days" :integer :default 90 :min 1
     :doc "how long an imported run is kept under the home"))
  (:tool "rundebug_import" (nlk:contract 'import-request) 'import-tool)
  (:tool "rundebug_timeline" (nlk:contract 'run-request) 'timeline-tool)
  (:tool "rundebug_explain" (nlk:contract 'run-request) 'explain-tool)
  (:tool "rundebug_replay" (nlk:contract 'run-request) 'replay-tool)
  (:tool "rundebug_status"
         (no-arguments "Report the debugger's own state: imported runs, which lanes are
implemented, which are refused, the active bounds. No export route and no upload path exists.")
         'status-tool)
  (:help :run-debugger
   "run-debugger: an imported trace's timeline, diagnosis and inert replay, offline - (rundebug:explain \"run-1\")"
   *primer*)
  (:command "rundebug" 'run-slash
            :description "Debug an imported agent run: timeline, diagnosis, inert replay"
            :argument-hint "<import|timeline|explain|replay|report|status>")
  (:start #'start))
