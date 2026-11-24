;;;; contracts.lisp --- the three tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the body
;;;; sees it. They load before cell.lisp, whose bodies read the records.
;;;;
;;;; The arguments and their meanings are the original plugin's; the ranges
;;;; the original clamped are said in a member's :DOC, because a clamp is a
;;;; rule of the verb rather than a shape.

(in-package #:nodecode-windows-eventlog)

(nlk:define-contract crash-events-request
  (:doc "Read Windows crash evidence for a time window: Application Error / Application Hang / Windows Error Reporting / .NET Runtime records from the Application log, with the faulting module, its path, the fault offset and the exception code decoded to its debugger name (0xC0000005 ACCESS_VIOLATION), plus groups that collapse repeated crashes of the same application and signature. Use it when a program, an app or a gateway vanished, hung, or was reported as crashed: on Windows the Application log is the durable record of the faulting module and offset. Read-only; Windows only.")
  ("hours" :number :default 24
   :doc "how far back to look, in hours: 0.1-720, clamped, default 24")
  ("process" :string :default ""
   :doc "case-insensitive filter on the crashing image name or path (hermes, python.exe, codex); omit for every application")
  ("kinds" :array :of :string
   :doc "limit to these kinds: application_error, application_hang, wer, dotnet; omit for all of them")
  ("limit" :integer :default 25
   :doc "how many records to answer, 1-200, default 25"))

(nlk:define-contract shutdown-events-request
  (:doc "Read the System event log for machine-level interruptions: Kernel-Power 41 unexpected restarts, bugchecks (WER-SystemErrorReporting 1001, decoded to names like DRIVER_POWER_STATE_FAILURE), EventLog 6008 dirty shutdowns, 1074 operator-initiated restarts with the process and the comment, and the 6005/6006/12/13 start and stop markers. Use it when a process died together with the machine, after sleep or hibernate, or when sessions stop without an exit record. Read-only; Windows only.")
  ("hours" :number :default 168
   :doc "how far back to look, in hours: 0.1-720, clamped, default 168 (one week)")
  ("limit" :integer :default 30
   :doc "how many records to answer, 1-200, default 30"))

(nlk:define-contract eventlog-query-request
  (:doc "Query any Windows event log channel with filters (event ids, levels, exact provider, a substring of the payload) and get structured records instead of raw XML. Use it for what the two crash tools do not cover: Service Control Manager 7031/7034 service crashes, Task Scheduler, Kernel-Power, application Operational logs, or 'which event mentions this path or port'. Filters are applied in the query and again on what comes back. Read-only; Windows only; channels that need elevation (Security) answer ok=false with the exit code.")
  ("channel" :string :default "Application"
   :doc "log name: Application (default), System, or Microsoft-Windows-TaskScheduler/Operational")
  ("hours" :number :default 24
   :doc "how far back to look, in hours: 0.1-720, clamped, default 24")
  ("event_ids" :array :of :integer
   :doc "only these event ids, e.g. [7031, 7034]")
  ("levels" :array :of :integer
   :doc "only these levels: 1 critical, 2 error, 3 warning, 4 information, 5 verbose")
  ("provider" :string :default ""
   :doc "the exact provider name, e.g. Service Control Manager")
  ("contains" :string :default ""
   :doc "case-insensitive substring the payload must contain")
  ("include_details" :boolean
   :doc "include the provider's own named data fields (default false)")
  ("limit" :integer :default 20
   :doc "how many records to answer, 1-200, default 20"))
