;;;; cell.lisp --- the manual, the tool, /cron-gate, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original fills Hermes cron's pre-run script slot: a script prints
;;;; {"wakeAgent": false} and the job's model run never happens. Nodecode's
;;;; scheduled jobs run a prompt, so the same gate is a verb the prompt calls
;;;; first -- (cron-gate:check "heartbeat") -- plus the tool below, so a
;;;; model that was told to gate a job can define one without writing Lisp.
;;;; While the cell runs, (help :cron-gate) answers the manual and every
;;;; request's help section carries the one line the :HELP clause gives.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "cron-gate": {"api_key": "", "api_key_env": "", "base_url": "",
;;;;                  "model": "", "timeout_seconds": 15, "max_state_chars": 6000}
;;;; Empty means TYPESAFE_API_KEY (or JEV_API_KEY) and
;;;; https://api.typesafe.ai and jev-latest.

(in-package #:nodecode-cron-gate)

(defparameter +manual+
  "A cron gate asks TypeSafe Jev whether a scheduled run would do anything useful, from a small snapshot
of what the job reads, and answers :wake or :skip. It is the nodecode-cron-gate cell: Lisp functions in
the cron-gate: package, called through eval.
  (cron-gate:gate \"heartbeat\" :question \"Running the agent now would make real new progress.\"
                   :sources '((:file \"~/proj/APPROVALS.md\") (:file \"~/proj/log.jsonl\" :tail 900)
                              (:changed (\"~/proj/src\")) (:url \"http://127.0.0.1:8888/state\")
                              (:text \"nightly synthesis\"))
                   :skip-below 0.2 :max-skips 4)
      Define or update the gate. QUESTION is the statement Jev scores, phrased so TRUE means the run is
      worth doing. SOURCES is what Jev sees: a file (or its last N characters with :tail), whether paths
      changed since the last wake, a localhost URL's body (only 127.0.0.1, localhost and ::1 are read;
      any other URL is named and skipped), or static text. The snapshot is capped at max_state_chars,
      keeping the end. Defaults: skip below 0.20, wake anyway after 4 skips in a row.
  (cron-gate:check \"heartbeat\")
      Decide now and answer two values: a line saying what to do, and :wake or :skip. Put it at the top
      of a gated job's prompt: on :skip, answer in one line that there was nothing to do and stop. It
      fails open, so :wake means nothing more than where the run is.
  (cron-gate:gates)    every gate, its runs, its skips and the share skipped, read from the log.
  (cron-gate:forget \"heartbeat\")   drop a gate's definition and counters; the log keeps its lines.
A gate sends the snapshot to TypeSafe with the key, so keep secrets out of what it reads. ERROR:
CRON-GATE-ERROR names the problem: a bad name, a question that is not one, sources of no known kind."
  "What (help :cron-gate) answers while the cell runs.")

;;; --- the tool ----------------------------------------------------------------------------

(defparameter +schema+
  (list :description
        (concatenate 'string
                     "Manage Jev wake gates for scheduled jobs. A gate asks TypeSafe Jev one yes/no question "
                     "('would running the agent now do useful work?') over a small snapshot of files, "
                     "changed-since-last-run checks and localhost URLs; if the answer is under the bar the run "
                     "can skip its work. Actions: define (write a gate, naming its question and sources), "
                     "check (decide now: wake or skip), status (runs and skips per gate), forget (drop a gate).")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "action" (nlk:make-json-object "type" "string"
                                          "enum" (vector "define" "check" "status" "forget")
                                          "description" "what to do")
          "name" (nlk:make-json-object
                  "type" "string"
                  "description" "the gate, 1 to 40 of a-z 0-9 _ -; one gate per job")
          "question" (nlk:make-json-object
                      "type" "string"
                      "description" (concatenate 'string
                                                  "define only. The statement Jev scores, phrased so TRUE means "
                                                  "the run is worth doing, naming what 'nothing to do' looks like."))
          "sources" (nlk:make-json-object
                     "type" "array"
                     "description" (concatenate 'string
                                                 "define only. What Jev sees: {\"file\": PATH, \"tail\": N}, "
                                                 "{\"changed\": [PATHS]} (modified since the last wake?), "
                                                 "{\"url\": \"http://127.0.0.1:PORT/...\"}, {\"text\": \"...\"}.")
                     "items" (nlk:make-json-object "type" "object"))
          "skip_below" (nlk:make-json-object
                         "type" "number"
                         "description" "define only. Skip when P(useful) is under this. Default 0.2.")
          "max_consecutive_skips" (nlk:make-json-object
                                    "type" "integer"
                                    "description" "define only. Force a wake after this many skips in a row. Default 4."))
         "required" (vector "action")))
  "The one tool: the original's jev_cron_gate, its setup/test/status/remove.")

(defun argument (arguments name)
  "ARGUMENTS' member NAME as JSON gives it, or NIL."
  (and arguments (nlk:json-value arguments :any name)))

(defun text-argument (arguments name)
  "ARGUMENTS' member NAME when it is a string with something in it."
  (let ((value (argument arguments name)))
    (and (stringp value) (plusp (length (nlk:trimmed value))) value)))

(defun cron-gate-tool (arguments)
  "cron_gate: define, check, status or forget one gate, answering text."
  (handler-case
      (let ((action (string-downcase (princ-to-string (or (argument arguments "action") "")))))
        (cond
          ((string= action "define")
           (gate (argument arguments "name")
                 :question (text-argument arguments "question")
                 :sources (argument arguments "sources")
                 :skip-below (or (argument arguments "skip_below") +default-skip-below+)
                 :max-skips (or (argument arguments "max_consecutive_skips") +default-max-skips+)))
          ((string= action "check")
           (check (argument arguments "name")))
          ((string= action "status")
           (let ((name (text-argument arguments "name")))
             (if name
                 (format nil "cron-gate ~a: ~a" (gate-name name) (stats-line (gate-name name)))
                 (gates))))
          ((string= action "forget")
           (forget (argument arguments "name")))
          (t (format nil "ERROR: CRON-GATE-ERROR: action is define, check, status or forget; ~s is none" action))))
    (cron-gate-error (condition) (format nil "ERROR: CRON-GATE-ERROR: ~a" condition))
    (error (condition) (format nil "ERROR: CRON-GATE-ERROR: ~a" condition))))

;;; --- /cron-gate ------------------------------------------------------------------------------

(defun slash (args session-id)
  "One /cron-gate: no argument lists the gates; a gate's name decides it now."
  (declare (ignore session-id))
  (handler-case
      (let ((name (nlk:trimmed (or args ""))))
        (if (zerop (length name))
            (gates)
            (multiple-value-bind (text decision) (check name)
              (format nil "~a~%~a" text (stats-line (gate-name name))))))
    (cron-gate-error (condition) (format nil "cron-gate: ~a" condition))))

;;; --- the cell ---------------------------------------------------------------------------------

(nle:define-cell cron-gate
  (:section ("cron-gate")
    (:guide "a TypeSafe key is what a gate asks with; without one every gate wakes (fails open)")
    ("api_key" :secret :doc "TypeSafe key for Jev; blank reads api_key_env, then the environment")
    ("api_key_env" :env :doc "environment variable holding the key (TYPESAFE_API_KEY by default)")
    ("base_url" :string :default "" :doc "TypeSafe's root; blank is https://api.typesafe.ai")
    ("model" :string :default "" :doc "the judgment model; blank is jev-latest")
    ("timeout_seconds" :integer :default 15 :min 1 :doc "one request's, and one source's, ceiling")
    ("max_state_chars" :integer :default 6000 :min 200 :doc "how much of the snapshot Jev is sent, from the end"))
  (:tool "cron_gate" +schema+ #'cron-gate-tool)
  (:help :cron-gate "cron-gate: a Jev gate before a scheduled run - (cron-gate:check \"heartbeat\") answers :wake or :skip" +manual+)
  (:command "cron-gate" 'slash
            :description "Gates before scheduled runs: each one's skips, or check one now"
            :argument-hint "[NAME]"))
