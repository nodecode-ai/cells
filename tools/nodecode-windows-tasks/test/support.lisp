;;;; support.lisp --- the Windows Tasks test runner and its fixtures.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry as every cell's
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; WINDOWS-TASKS-CELL- name prefix; RUN-WINDOWS-TASKS-TESTS runs exactly
;;;; that slice.
;;;;
;;;; The one outside thing this cell runs is powershell.exe, so that is what the
;;;; tests stub: a special variable they rebind to a Lisp function that answers
;;;; the JSON PowerShell would have printed, and records every call's argv. The
;;;; fixtures below are that JSON: a listing as ConvertTo-Json writes it, and a
;;;; definition as Export-ScheduledTask writes it. No test needs Windows, and
;;;; none of them starts a real PowerShell.

(in-package #:nodecode.test)

(define-test-slice "windows-tasks" "WINDOWS-TASKS-CELL-" :start nodecode-windows-tasks:start-cell
  :defaults (("program" (windows-tasks-stub-program))))

(defvar *runs* '()
  "The (PROGRAM ARGS TIMEOUT) of every run this test stubbed, newest first.")

(defmacro with-powershell-stub ((&key (stdout "") (stderr "") (exit 0) (timed-out nil) replies) &body body)
  "BODY with PowerShell stubbed: it answers STDOUT without running anything, and
  every run is pushed on *RUNS* as (PROGRAM ARGS TIMEOUT). REPLIES, when given,
  is a list of answers taken one a run (the last one standing for every run
  after it), which is what a detail needs: one answer for the listing, one for
  the export."
  `(let ((*runs* '())
         (pending (copy-list (or ,replies (list ,stdout)))))
     ;; the seam is bound INSIDE the let that holds PENDING: a special bound in the
     ;; same LET as the closure over it would leave the closure reading an outer
     ;; binding, and every call would answer an unbound-variable error.
     (let ((nodecode-windows-tasks::*run*
             (lambda (program args timeout)
               (push (list program args timeout) *runs*)
               (values (if (rest pending) (pop pending) (first pending))
                       ,stderr ,exit ,timed-out))))
       ,@body)))

(defun last-run ()
  "The (PROGRAM ARGS TIMEOUT) of the newest run."
  (first *runs*))

(defun last-script ()
  "The PowerShell script of the newest run: the last of its argv."
  (car (last (second (last-run)))))

(defun windows-tasks-script (path &rest lines)
  "Write LINES to PATH after a /bin/sh line, make it executable, and answer its
  native namestring."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-line "#!/bin/sh" out)
    (dolist (line lines) (write-line line out)))
  (uiop:run-program (list "chmod" "755" (uiop:native-namestring path))
                     :output nil :error-output nil)
  (uiop:native-namestring path))

(defparameter *stub-program* nil
  "The stand-in program's path, made once a run.")

(defun windows-tasks-stub-program ()
  "A program the cell accepts as powershell.exe: it exits at once, so a start
  finds something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (windows-tasks-script (merge-pathnames "powershell-stub/powershell.exe"
                                                    (uiop:temporary-directory))
                                   "exit 0"))))

;;; --- the fixtures -------------------------------------------------------------------

(defun a-task (&key (path "\\Backup\\Offsite")
                     (state "Ready")
                     (user "a-machine\\someone")
                     (group nil)
                     (run-level "Limited")
                     (action "C:\\Tools\\copy.exe --to offsite")
                     (action-count 1)
                     (triggers '("MSFT_TaskLogonTrigger"))
                     (last-run "2026-03-01T09:00:00.0000000-08:00")
                     (last-result #x80070002)
                     (next-run "2026-03-02T02:15:00.0000000-08:00")
                     (missed-runs 0))
  "One task as the listing script writes it."
  (nlk:make-json-object
   "path" path "state" state "author" "a machine" "user" user "group" group
   "run_level" run-level "action" action "action_count" action-count
   "triggers" (coerce triggers 'vector)
   "last_run" last-run "last_result" last-result
   "next_run" next-run "missed_runs" missed-runs))

(defparameter +working-task+
  (a-task :path "\\Backup\\Offsite" :state "Ready" :last-result #x00000000)
  "A task whose last run worked.")

(defparameter +failing-task+
  (a-task :path "\\Backup\\Nightly" :state "Ready" :action "C:\\Old\\gone.exe"
          :last-result #x80070002)
  "A task whose last run failed: the program is gone.")

(defparameter +microsoft-task+
  (a-task :path "\\Microsoft\\Windows\\Defrag\\ScheduledDefrag" :state "Disabled"
          :user "S-1-5-18" :run-level "Highest"
          :triggers '("MSFT_TaskWeeklyTrigger") :last-result #x00041303)
  "One of Windows' own tasks.")

(defun listing-json (&rest tasks)
  "The listing script's answer for TASKS."
  (nlk:encode-json-object (coerce tasks 'vector)))

(defparameter +listing+ (listing-json +working-task+ +failing-task+ +microsoft-task+)
  "Three tasks: one that works, one that fails, and one of Windows' own.")

(defparameter +definition+
  "<?xml version=\"1.0\" encoding=\"UTF-16\"?>
<Task version=\"1.2\" xmlns=\"http://schemas.microsoft.com/windows/2004/02/mit/task\">
  <RegistrationInfo>
    <Author>a machine</Author>
    <Description>Keeps the off-site copy current.</Description>
    <URI>\\Backup\\Offsite</URI>
  </RegistrationInfo>
  <Triggers>
    <LogonTrigger>
      <Enabled>true</Enabled>
      <UserId>a-machine\\someone</UserId>
    </LogonTrigger>
    <CalendarTrigger>
      <StartBoundary>2026-03-01T03:00:00</StartBoundary>
      <Repetition>
        <Interval>PT1H</Interval>
        <Duration>PT4H</Duration>
      </Repetition>
      <ScheduleByDay>
        <DaysInterval>1</DaysInterval>
      </ScheduleByDay>
    </CalendarTrigger>
  </Triggers>
  <Principals>
    <Principal id=\"Author\">
      <UserId>a-machine\\someone</UserId>
      <LogonType>InteractiveToken</LogonType>
      <RunLevel>LeastPrivilege</RunLevel>
    </Principal>
  </Principals>
  <Settings>
    <MultipleInstancesPolicy>IgnoreNew</MultipleInstancesPolicy>
    <DisallowStartIfOnBatteries>true</DisallowStartIfOnBatteries>
    <StopIfGoingOnBatteries>true</StopIfGoingOnBatteries>
    <StartWhenAvailable>true</StartWhenAvailable>
    <ExecutionTimeLimit>PT1H</ExecutionTimeLimit>
  </Settings>
  <Actions Context=\"Author\">
    <Exec>
      <Command>C:\\Tools\\copy.exe</Command>
      <Arguments>--to offsite</Arguments>
      <WorkingDirectory>C:\\Tools</WorkingDirectory>
    </Exec>
  </Actions>
</Task>"
  "A definition as Export-ScheduledTask writes it: a logon trigger, a daily
  trigger that repeats, and one action.")

(defparameter +maintenance-definition+
  "<Task><RegistrationInfo><Author>a machine</Author></RegistrationInfo>
<Triggers />
<Settings>
  <MaintenanceSettings><Period>P1D</Period><Deadline>P7D</Deadline></MaintenanceSettings>
</Settings>
<Actions Context=\"Author\"><Exec><Command>C:\\Tools\\tidy.exe</Command></Exec></Actions>
</Task>"
  "A definition with no triggers and maintenance settings: a task that runs
  during automatic maintenance.")

(defun export-json (&optional (xml +definition+))
  "What the definition script answers for XML."
  (nlk:encode-json-object (nlk:make-json-object "xml" xml)))

(defun export-error-json (message)
  "What the definition script answers when the export failed."
  (nlk:encode-json-object (nlk:make-json-object "error" message)))

;;; --- calling the tools ---------------------------------------------------------------

(defun answered (name arguments)
  "Tool NAME called as the model calls it -- its own registration, its own
contract decoding ARGUMENTS, a JSON object string -- in this thread, so the stub
the test binds is the one the tool sees. => the text it answers."
  (let ((entry (nle::find-tool name)))
    (unless entry (error "no tool ~a is registered" name))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (nlk:decode-json arguments :whole t)))))

(defun answered-json (name arguments)
  "The JSON tool NAME answers for ARGUMENTS (a JSON object string)."
  (let ((text (answered name arguments)))
    (handler-case (nlk:decode-json text :whole t)
      (error () text))))

(defun field (payload key &rest path)
  "PAYLOAD's member at KEY and PATH, read leniently."
  (apply #'nlk:json-value payload :any key path))

(defun entries (payload)
  "PAYLOAD's entries as a list."
  (coerce (or (nlk:json-value payload :array "entries") #()) 'list))

(defun notes (payload)
  "PAYLOAD's notes, one string."
  (let ((notes (nlk:json-value payload :array "notes")))
    (format nil "~{~a~^ | ~}" (coerce (or notes #()) 'list))))

(defun windows-tasks-refusal (thunk)
  "What THUNK refuses with, as the text a person would read."
  (handler-case (funcall thunk)
    (error (condition) (princ-to-string condition))))
