;;;; cell.lisp --- a folder a scan reads, a report a person reads, nothing run.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Hermes plugin mauricemohr88-debug/hermes-plugin-guard ships: a
;;;; local static review of a candidate plugin -- its manifest, source,
;;;; dependencies, likely secrets and repository hygiene -- where the target
;;;; code is read as data and never imported or executed. This folder is
;;;; that scanner rewritten for Nodecode: the same subject and the same rule
;;;; ids' meanings under a PG prefix, one tool the model calls and one
;;;; command the operator types, over a folder of any plugin or cell.
;;;;
;;;; What is left out, and why: the plugin's native Hermes surface (its
;;;; subprocess worker in a resource-bounded sandbox, the opaque
;;;; location-id projection that kept absolute paths out of the model's
;;;; context, the JSON and SARIF reporters, the fail-on exit codes, the
;;;; enabled/disabled activation state of a Hermes install). A scan here is
;;;; one plain function in this image, its report already carries only
;;;; in-tree paths, and the operator's shell is the exit code.

(in-package #:nodecode-plugin-guard)

(nlk:define-peripheral plugin-guard :not-running t)

;;; --- the words --------------------------------------------------------------------

(defparameter *primer*
  "plugin-guard reviews a plugin or cell folder before it is trusted: it reads
the folder's files as text -- never importing, compiling or running any of
them -- and reports what a person should look at: a manifest that is missing,
dynamic execution, unsafe deserialization, process execution, sensitive paths,
a disabled TLS check, outbound requests, undeclared secret environment reads,
destructive filesystem calls, privileged registration surfaces, committed
credentials, unpinned or unbounded dependencies, a remote script piped to a
shell, a missing license, security policy or tests. Call plugin_guard_scan
with a folder (absolute, or relative to the project) and it answers the
report: the counts, FAIL or ok at a threshold, then one line a finding, with
its rule id, path and line. The folder's own sources are what prove it; the
report is a first pass, a review aid, not a verdict, and passing it is not
permission to trust the code."
  "What (help :plugin-guard) answers while the cell runs.")

;;; --- the verbs --------------------------------------------------------------------

(define-verb scan (path &key min-severity)
  "Review the plugin or cell folder PATH: static rules over its files, its
manifest, its dependencies and likely secrets, nothing in it executed. =>
the report, as REPORT-TEXT writes it."
  (report-text
   (scan-directory path
                   :min-severity (or min-severity (setting :min-severity))
                   :max-findings (or (setting :max-findings) 20)
                   :max-files (or (setting :max-files) 500)
                   :exclude (setting :exclude))))

(defun run-slash (args session-id)
  "/plugin-guard PATH: the operator's own review of one folder;
/plugin-guard rules lists what the scan looks for."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (cond ((zerop (length text))
           (format nil "plugin-guard: /plugin-guard PATH reviews a plugin or cell folder before you ~
                        trust it -- static rules over its files, manifest, dependencies and secrets, ~
                        and nothing in it is run. /plugin-guard rules lists the rules. The model ~
                        has plugin_guard_scan for the same review."))
          ((string-equal text "rules") (rules-text))
          (t (scan text)))))

;;; --- the boundary -----------------------------------------------------------------

(nlk:define-contract plugin-guard-scan
  (:doc "Review a plugin or cell folder before it is trusted: static, read-only checks over its manifest, sources, dependencies, likely secrets and repository hygiene. Nothing in the folder is imported, compiled or run. A review aid, not a verdict: read the code the report points at before you trust it.")
  ("path" :text :required t
          :doc "the folder to review, absolute or relative to the project")
  ("min_severity" :string :default "" :options '("info" "low" "medium" "high" "critical" "")
   :doc "the threshold the summary judges at; empty uses the cell's setting"))

(defun scan-tool (call)
  "The plugin_guard_scan tool: the report on CALL's folder."
  (let ((given (plugin-guard-scan-min-severity call)))
    (scan (plugin-guard-scan-path call)
          :min-severity (and given (plusp (length given)) given))))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell plugin-guard
  (:section ("plugin-guard")
    (:guide "nothing here is required; min_severity is the line the report fails at, exclude leaves rule ids out, max_findings and max_files bound one review")
    ("min_severity" :choice :options '("info" "low" "medium" "high" "critical")
                    :default "high"
                    :doc "the severity the report's FAIL or ok judges at")
    ("exclude" :list
               :doc "rule ids to leave out of every report, e.g. PG203")
    ("max_findings" :integer :default 20 :min 1
                    :doc "the longest list of findings one report prints")
    ("max_files" :integer :default 500 :min 1
                 :doc "how many files one review reads"))
  (:tool "plugin_guard_scan" (nlk:contract 'plugin-guard-scan) 'scan-tool)
  (:command "plugin-guard" 'run-slash
            :catches 'plugin-guard-error
            :description "Review a plugin or cell folder before you trust it"
            :argument-hint "PATH")
  (:help :plugin-guard
         "plugin-guard: static review of a plugin or cell folder, read as text and never run; plugin_guard_scan"
         *primer*))
