;;;; cell.lisp --- two tools, one command, one manual topic.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the cell contributes is declared once, in the NLE:DEFINE-CELL form
;;;; at the bottom: the `windows-services' section, the two tools the model
;;;; calls, /windows-services for the operator, and (help :windows-services).
;;;; The verbs above it are the same answers reached from eval, which is how
;;;; a layer, a test and an operator all read the same text.

(in-package #:nodecode-windows-services)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Windows services: what runs on this machine, why one died, and what depends on what,
read from the service manager and the registry. Read-only: nothing is started, stopped,
reconfigured or deleted.
The model calls two tools; eval calls the same two answers as (windows-services:table ...)
and (windows-services:detail NAME).
  win_service_table(query, state, start, nonzero_exit, limit)
      Every registered service: state (running, stopped, paused), start mode (boot, system,
      auto, manual, disabled, with the delayed flag folded in as 'auto, delayed'), the pid
      it runs under, the account and binary it runs as, the services it waits for and the
      ones that wait for it. query is a substring of the name or display name; state and
      start narrow by value; nonzero_exit=true keeps only the failures. Ask it for 'what
      died after the last update', for what an uninstalled program left registered, and for
      the dependency order before a restart.
  win_service_detail(name)
      One service in full: the same row, plus what Windows does when it fails (restart the
      service, restart the computer, run a command, after how many seconds, and when the
      count resets), whether the registered binary is still on disk, and the Service
      Control Manager's own recent events naming it -- 7000 failed to start, 7009 a start
      request that timed out, 7024 a service-specific error, 7031 an unexpected end, 7045
      an install.
An exit code is a Win32 error number: 1077 has not tried since the last boot (not a
failure), 1053 did not answer in time, 1075 waits on a dependency that failed first, 2
its binary is gone, 5 access was denied, 1067 the process ended unexpectedly. Dependency
edges and the failure policy come from the registry, because CIM's DependOnService is
never populated and FailureActions is in CIM at all.
The output names services, accounts, command lines and paths, so treat it like a Services
window opened on this machine."
  "What (help :windows-services) answers while the cell runs.")

;;; --- what the model hands over -----------------------------------------------

(nlk:define-contract service-table-call
  (:doc "The filters of one service listing: what runs on this Windows machine, and what it waits for.")
  ("query" :string :default ""
   :doc "case-insensitive substring of the service name or display name, e.g. wuauserv, update, hermes")
  ("state" :string :options +states+
   :doc "only services in this state")
  ("start" :string :options +start-modes+
   :doc "only services with this start mode; a delayed Automatic service matches auto")
  ("nonzero_exit" :boolean :default nil
   :doc "only services whose exit code is a failure (1077, never started, is not one)")
  ("limit" :integer :default 100
   :doc "rows to return, 1 to 400"))

(nlk:define-contract service-detail-call
  (:doc "One service in full, for 'why is this one not running'.")
  ("name" :text :required t
   :doc "the service's name, not its display name, e.g. wuauserv; win_service_table lists them all"))

;;; --- the verbs, and the tools over them ---------------------------------------

(defun bounded-limit (limit)
  "LIMIT inside what one answer may carry: 1 to 400."
  (max 1 (min (or limit 100) 400)))

(defun table (&key query state start nonzero-exit (limit nil))
  "Every service on this machine as the text a model reads, with the filters
named; LIMIT defaults to the section's max_rows."
  (running-settings)
  (table-text (rows-with-edges)
              :limit (bounded-limit (or limit (getf (running-settings) :max-rows)))
              :query query :state state :start start :nonzero-exit nonzero-exit))

(defun detail (name)
  "One service in full, or a sentence saying no service has that name."
  (running-settings)
  (detail-text (nlk:trimmed (princ-to-string name))))

(defun tool-table (call)
  "The win_service_table tool: the filters the model sent, one listing back."
  (table :query (service-table-call-query call)
         :state (service-table-call-state call)
         :start (service-table-call-start call)
         :nonzero-exit (service-table-call-nonzero-exit call)
         :limit (service-table-call-limit call)))

(defun tool-detail (call)
  "The win_service_detail tool: one name in, one service in full out."
  (detail (service-detail-call-name call)))

;;; --- what the operator sees ---------------------------------------------------

(defun summary-text ()
  "What /windows-services with no argument answers: the counts, then the
services that are actually in a failed state."
  (let* ((rows (rows-with-edges))
         (failing (remove-if-not (function failed-p) rows))
         (running (count-if (lambda (row) (equal "running" (getf row :state))) rows)))
    (with-output-to-string (out)
      (format out "~d services registered, ~d running, ~d in a failed state~%"
              (length rows) running (length failing))
      (if failing
          (progn
            (format out "~&carrying a non-zero exit code:~%")
            (dolist (row (subseq failing 0 (min (length failing) 12)))
              (format out "  ~a  ~a~@[ - ~a~]  ~a~%"
                      (column (getf row :name) 28)
                      (getf row :state)
                      (getf row :display)
                      (let ((exit (exit-of (getf row :exit-code))))
                        (format nil "exit ~a: ~a" (getf exit :code) (getf exit :meaning))))))
          (format out "none of them carries a failed exit code~%"))
      (format out "~&/windows-services NAME for one service in full; win_service_table and ~
win_service_detail for the filters."))))

(defun run-command (arguments session-id)
  "One /windows-services invocation: no argument says what is failing on this
machine, a name says one service in full."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or arguments ""))))
    (if (zerop (length text))
        (summary-text)
        (detail text))))

;;; --- the section, and what start installs ------------------------------------

(defun read-settings (values table)
  "The settings the cell runs on: which PowerShell to run, how long one query
may take, and how long a listing may be. No PowerShell and none named is a
refusal naming the setting to fix."
  (declare (ignore table))
  (let ((exe (find-powershell (getf values :powershell))))
    (unless exe
      (error 'nlk:config-refusal
             :detail (format nil "windows-services: no PowerShell found (~{~a~^, ~}) on PATH, ~
and the windows-services section names none. This cell reads the Windows service manager ~
through PowerShell, so it needs Windows or a PowerShell install: set `powershell` in the ~
windows-services section, or put powershell.exe (or pwsh) on PATH, then (restart-cells \"nodecode-windows-services\")."
                            +powershell-names+)))
    (list :powershell exe
          :timeout-seconds (getf values :timeout-seconds)
          :max-rows (getf values :max-rows))))

(nle:define-cell windows-services
  (:section ("windows-services")
    (:guide "nothing is required on Windows: the cell finds powershell.exe or pwsh.exe itself. Name powershell only when it lives somewhere else; timeout_seconds bounds one query; max_rows bounds one listing")
    ("powershell" :string
     :doc "the PowerShell to run; empty finds powershell.exe or pwsh.exe on PATH")
    ("timeout_seconds" :integer :default 90 :min 1
     :doc "one query's ceiling, the listing or the event log")
    ("max_rows" :integer :default 100 :min 1
     :doc "how many services one listing lays out before the rest are counted only"))
  (:settings (function read-settings))
  (:help :windows-services
         "windows-services: what runs, what failed and what depends on what - (help :windows-services)"
         +primer+)
  (:command "windows-services" (function run-command)
            :description "Windows services: what is failing here, or one service in full"
            :argument-hint "NAME"
            :session nil)
  (:tool "win_service_table" (nlk:contract 'service-table-call) (function tool-table))
  (:tool "win_service_detail" (nlk:contract 'service-detail-call) (function tool-detail)))
