;;;; cell.lisp --- nodecode-process-table: what is running, and who started it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of hermes-windows-process. One tool, one command,
;;;; one manual topic, and nothing running in the background.
;;;;
;;;; The tool answers one JSON object: the processes the call's filters
;;;; left, oldest first, each with its parent, its start time, its thread
;;;; count, its image path, and the state of the parent it points at
;;;; (`alive', `gone', `reused', `root' or `unknown'), plus the count of
;;;; orphans in the whole table and the notes that explain what a caller is
;;;; looking at. It is the answer to "what is still running from the thing I
;;;; just quit", "which of these thirty node processes is the live one",
;;;; "did this start before or after the process I am debugging".
;;;;
;;;; READ-ONLY is the whole contract with the system: /proc read, `ps'
;;;; asked, Win32_Process queried -- no process is signalled, opened for
;;;; writing, or started, and no file is written.

(in-package #:nodecode-process-table)

;;; --- the call --------------------------------------------------------------

(nlk:define-contract process-table-call
  (:doc "Read the machine's process table: every process with its parent,
start time, thread count and image path, and a flag saying whether the
parent it points at is alive, gone, or a newer process that took the PID
over. Read-only; nothing is signalled or started.")
  ("pid" :integer :doc "A single process ID to match.")
  ("pids" :array :of :integer :doc "Several process IDs to match (combined with pid).")
  ("process" :string
   :doc "Case-insensitive substring of the image name or path, e.g. 'node', 'python', 'gateway'.")
  ("parent" :integer :doc "Only processes whose parent PID is this value.")
  ("orphans_only" :boolean
   :doc "Only processes whose parent is gone, or whose parent PID now belongs to a process that started later than they did (default false).")
  ("with_ancestry" :boolean
   :doc "Also return the ancestor chain of each matched process, marked as context rows (default false).")
  ("include_system" :boolean :default t
   :doc "Include system session and bootstrap processes (images under the system's own directories) and processes this account cannot open, which come back without a path or a start time (default true).")
  ("limit" :integer :doc "Maximum processes to return (default the section's limit, max 500)."))

(defun process-table-tool (call)
  "The tool's body: one contract record in, one JSON answer out."
  (report :pid (process-table-call-pid call)
          :pids (process-table-call-pids call)
          :process (process-table-call-process call)
          :parent (process-table-call-parent call)
          :orphans-only (process-table-call-orphans-only call)
          :with-ancestry (process-table-call-with-ancestry call)
          :include-system (process-table-call-include-system call)
          :limit (process-table-call-limit call)))

;;; --- /process-table ---------------------------------------------------------

(defun run-slash (args session-id)
  "/process-table [NAME | orphans]: the same table a person reads.
=> the answer's text."
  (declare (ignore session-id))
  (let ((words (words (nlk:trimmed (or args "")))))
    (cond ((null words) (describe-table))
          ((string-equal (first words) "orphans") (describe-table :orphans-only t))
          (t (describe-table :process (first words))))))

;;; --- the manual -------------------------------------------------------------

(defparameter +manual+
  "process-table: what is running on this machine, what started it, and what
outlived its parent. The tool `process_table` answers JSON; /process-table
answers the same rows as text, and (process-table:describe-table) reads them from eval.
reads them from eval.
A process holds its parent's PID, not a handle: once the parent exits that
number is free again, and a child that keeps running then points at an
unrelated newer process. Each entry's parent_state says which it is --
alive, gone (no such PID now), reused (the PID belongs to a process that
started after the child), root (PID 1's own child, parent 0) or unknown
(no start time to compare). Orphans are the gone and reused ones; they are
what is left over from a session that quit.
Filters: pid, pids, process (a substring of the name or path), parent,
orphans_only, include_system (false leaves out the system's own session and
bootstrap processes, whose parent exits by design), with_ancestry (also
return the parent chain, as context rows) and limit (max 500).
The table is read, never touched: /proc on Linux, the ps command on macOS
and the BSDs, Win32_Process on Windows. A process this account cannot open
comes back with no path and no start time instead of failing the call.")

;;; --- the declaration --------------------------------------------------------

(nle:define-cell process-table
  (:section ("process-table")
    (:guide "nothing here is required; the tool reads the machine's own process table and reports it")
    ("reader" :choice :options '("auto" "proc" "ps" "powershell") :default "auto"
     :doc "where the table is read: auto (the system's own: /proc, ps or Win32_Process), or one named reader")
    ("limit" :integer :default 100 :min 1
     :doc "how many processes one call answers when it names no limit of its own")
    ("clock_ticks" :integer :default 100 :min 1
     :doc "clock ticks a second, for the start times /proc reports; Linux's own value is 100")
    ("timeout_seconds" :integer :default 10 :min 1
     :doc "how long the ps or powershell reader may take")
    ("system_prefixes" :list
     :doc "path prefixes that mark an image as the system's own, replacing the built-in list"))
  (:tool "process_table" (nlk:contract 'process-table-call) 'process-table-tool)
  (:help :process-table
   "process-table: what is running, who started it, and what outlived its parent"
   +manual+)
  (:command "process-table" 'run-slash
            :description "What is running, with the parent each process points at"
            :argument-hint "NAME | orphans"))
