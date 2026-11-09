;;;; cell.lisp --- what this machine runs on its own, when, and whether it worked.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes plugin windows-tasks: two read-only tools that read
;;;; Windows' Task Scheduler -- the listing with each task's state, action,
;;;; triggers, last and next run and the meaning of its last result code, and one
;;;; task's definition in full. The Scheduler's cmdlets are asked through
;;;; powershell.exe, as the plugin asked them, and their JSON answer is read
;;;; here. Nothing is created, started, changed or deleted: nothing here writes
;;;; a task, and no script does either.
;;;;
;;;; Config, a top-level key of its own:
;;;;   "windows-tasks": {"program": "powershell.exe", "timeout_seconds": 90}

(in-package #:nodecode-windows-tasks)

;;; --- the words --------------------------------------------------------------------

(defparameter *primer*
  "Windows starts most of what it does, and what installed software wants it to do,
through Task Scheduler: at logon, at startup, every hour, or during automatic
maintenance. These tools read that list, and answer 'what does this machine run on
its own, and did it work'. Both are read-only and both call the Scheduler's own
cmdlets through powershell.exe: no key, no service, no network.
  (windows-tasks:tasks)                       every task, as JSON: its path, state,
      the account it runs as, its first action's command line, its triggers in
      words, its last and next run, how many runs it missed, and its last result
      code in hex with what it means (0x80070002 a program that is no longer
      there, 0x800710E0 a task that needed a desktop it did not have). Filter by
      path substring (:query), :state, :failed-only (a task that has never run is
      not a failure) and :include-microsoft (Windows' own tasks, left out by
      default); :limit stops the listing at 100.
  (windows-tasks:task-detail \"\\\\Microsoft\\\\Windows\\\\Defrag\\\\ScheduledDefrag\")
      one task in full: every trigger with its start boundary, repetition and
      schedule, every action with its arguments and working directory, the
      principal, and the settings that decide whether it runs on battery or when
      late. A task with no triggers but maintenance settings runs during
      automatic maintenance, and the notes say so.
The win_scheduled_tasks and win_scheduled_task_detail tools are the same two calls
from the model's side, and /windows-tasks is the operator's. Windows only."
  "What (help :windows-tasks) answers while the cell runs.")

(defparameter *result-meanings*
  '((#x00000000 . "success")
    (#x00041300 . "ready to run")
    (#x00041301 . "running now")
    (#x00041302 . "disabled")
    (#x00041303 . "has not run yet")
    (#x00041304 . "no more runs are scheduled")
    (#x00041305 . "a required property is not set")
    (#x00041306 . "terminated by the user")
    (#x80070002 . "the program or script could not be found")
    (#x80070003 . "the path could not be found")
    (#x80070005 . "access was denied")
    (#x8007000E . "not enough memory")
    (#x80070020 . "the file is in use by another process")
    (#x8007010B . "the working directory does not exist")
    (#x80040154 . "the COM class it runs is not registered")
    (#x40010004 . "it was terminated (stopped or timed out)")
    (#x8004131F . "the task is already running")
    (#x80041321 . "the task's definition is corrupt or was tampered with")
    (#x800710E0 . "it needs an interactive desktop and ran without one"))
  "What the Scheduler writes into LastTaskResult, in the words a person needs.")

(defparameter *trigger-names*
  '(("MSFT_TaskBootTrigger" . "at startup")
    ("MSFT_TaskLogonTrigger" . "at logon")
    ("MSFT_TaskTimeTrigger" . "at a time")
    ("MSFT_TaskDailyTrigger" . "daily")
    ("MSFT_TaskWeeklyTrigger" . "weekly")
    ("MSFT_TaskMonthlyTrigger" . "monthly")
    ("MSFT_TaskMonthlyDOWTrigger" . "monthly by weekday")
    ("MSFT_TaskEventTrigger" . "on event")
    ("MSFT_TaskIdleTrigger" . "when idle")
    ("MSFT_TaskRegistrationTrigger" . "at registration")
    ("MSFT_TaskSessionStateChangeTrigger" . "on session change"))
  "The cmdlet's trigger classes, in the words a listing can carry.")

(defparameter *not-a-failure* '(#x00000000 #x00041300 #x00041301 #x00041303)
  "\"No result at all\" rather than a failure: a task nobody has run yet.")

(defparameter *states* '("ready" "running" "disabled" "queued" "unknown")
  "The task states a listing can filter on.")

(defparameter *max-rows* 500
  "The most tasks one listing is asked for, whatever limit says.")

(defparameter *action-chars* 240
  "How much of a task's first command line a listing carries.")

;;; --- the program ------------------------------------------------------------------

(defun program-path (name)
  "NAME as a program: the path itself when it names one, else the first file of
that name on PATH; NIL when there is none. A directory of that name is no
program."
  (when (and (stringp name) (plusp (length name)))
    (flet ((use (path)
             (when (and path (not (uiop:directory-pathname-p path)))
               (uiop:native-namestring path))))
      (if (or (find #\/ name) (find #\\ name))
          (use (ignore-errors (probe-file name)))
          (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                               :separator '(#\: #\;))
                for path = (ignore-errors
                            (probe-file
                             (merge-pathnames name
                                              (uiop:ensure-directory-pathname
                                               (if (plusp (length dir)) dir ".")))))
                thereis (use path))))))

(defvar *serial* 0
  "A counter that keeps one call's temporary files apart from the next's.")

(defun temporary (kind)
  "A pathname in the system's temporary directory that nothing else is using."
  (merge-pathnames (format nil "windows-tasks-~d-~d.~a"
                            (get-universal-time) (incf *serial*) kind)
                   (uiop:temporary-directory)))

(defun file-text (path)
  "PATH's contents as text, or \"\" when it holds none."
  (if (probe-file path)
      (handler-case (uiop:read-file-string path :external-format :utf-8)
        (error () ""))
      ""))

(defun program-run (program args timeout)
  "Run PROGRAM with ARGS, standard input at /dev/null, for at most TIMEOUT
seconds. => (values STDOUT STDERR EXIT-CODE TIMED-OUT-P). The streams are read
back whole from files, and a program past its deadline is killed, so a Scheduler
that never answers cannot hold the call forever."
  (let ((out (temporary "out")) (err (temporary "err")))
    (unwind-protect
         (let* ((process (sb-ext:run-program program args
                                             :search nil
                                             :input nil
                                             :output out
                                             :error err
                                             :if-output-exists :supersede
                                             :wait nil))
                (deadline (+ (get-internal-real-time)
                             (* timeout internal-time-units-per-second)))
                (timed-out nil))
           (loop
             (unless (sb-ext:process-alive-p process) (return))
             (when (>= (get-internal-real-time) deadline)
               (setf timed-out t)
               (ignore-errors (sb-ext:process-kill process 15))
               (sleep 0.1)
               (when (sb-ext:process-alive-p process)
                 (ignore-errors (sb-ext:process-kill process 9)))
               (return))
             (sleep 0.02))
           (sb-ext:process-wait process)
           (values (file-text out) (file-text err)
                   (ignore-errors (sb-ext:process-exit-code process))
                   timed-out))
      (ignore-errors (delete-file out))
      (ignore-errors (delete-file err)))))

(defvar *run* #'program-run
  "The one seam that runs the program: (PROGRAM ARGS TIMEOUT) => (values STDOUT
STDERR EXIT-CODE TIMED-OUT-P). Tests replace it; nothing else does.")

;;; --- asking the Scheduler ------------------------------------------------------------

(defparameter *header*
  "$ErrorActionPreference='Stop'; [Console]::OutputEncoding=[Text.Encoding]::UTF8;"
  "The two lines every script opens with, as the plugin's did: a cmdlet that
fails ends the call, and the answer is UTF-8 whatever the console's code page.")

(defparameter *list-script*
  "
$info = @{}
try {
  foreach ($row in (Get-ScheduledTask | Get-ScheduledTaskInfo)) {
    $info[$row.TaskPath + $row.TaskName] = $row
  }
} catch {
  # a task whose state cannot be read loses its last/next run, not the whole listing
}
$tasks = foreach ($task in Get-ScheduledTask) {
  $detail = $info[$task.TaskPath + $task.TaskName]
  [pscustomobject]@{
    path = $task.TaskPath + $task.TaskName
    state = [string]$task.State
    author = $task.Author
    user = $task.Principal.UserId
    group = $task.Principal.GroupId
    run_level = [string]$task.Principal.RunLevel
    action = (($task.Actions | ForEach-Object { ($_.Execute + ' ' + $_.Arguments).Trim() }) | Select-Object -First 1)
    action_count = @($task.Actions).Count
    triggers = @($task.Triggers | ForEach-Object { $_.CimClass.CimClassName })
    last_run = if ($detail -and $detail.LastRunTime) { $detail.LastRunTime.ToString('o') } else { $null }
    last_result = if ($detail) { $detail.LastTaskResult } else { $null }
    next_run = if ($detail -and $detail.NextRunTime) { $detail.NextRunTime.ToString('o') } else { $null }
    missed_runs = if ($detail) { $detail.NumberOfMissedRuns } else { $null }
  }
}
$tasks | ConvertTo-Json -Depth 5 -Compress"
  "Every task, with its info, in one call: Get-ScheduledTaskInfo is one call per
task, so the info rows are collected once into a table rather than asked for
again inside the loop. ConvertTo-Json gives field names and values that do not
change with the display language.")

(defparameter *definition-script*
  "try { @{ xml = (Export-ScheduledTask -TaskPath ~a -TaskName ~a) }} | ConvertTo-Json -Compress } catch { @{ error = $_.Exception.Message } | ConvertTo-Json -Compress }"
  "One task's definition, as Export-ScheduledTask writes it: the same document
the definition file holds, without needing a right to read that file. It takes
the folder and the name, each a single-quoted PowerShell string.")

(defun powershell-argv (script)
  "The argv the plugin ran: no profile, not interactive, a bypassed execution
policy, and the script as one command."
  (list "-NoProfile" "-NonInteractive" "-ExecutionPolicy" "Bypass" "-Command" script))

(defun nonblank (text &optional (default ""))
  "TEXT without its surrounding whitespace, or DEFAULT when nothing is left."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (if (and text (plusp (length text))) text default)))

(defun without-bom (text)
  "TEXT with a byte-order mark, which a pipe written by PowerShell may carry, and
its leading whitespace dropped."
  (string-left-trim (list #\Space #\Tab #\Newline #\Return (code-char #xFEFF)) text))

(defun ask (script)
  "SCRIPT's answer from PowerShell, as the JSON it printed; a refusal when the
program is missing, slow, or answered something that is no JSON."
  (let* ((program (setting :program))
         (timeout (setting :timeout-seconds))
         (found (program-path program)))
    (unless found
      (fail "windows-tasks.program is ~s: no such program on PATH -- these tools read Windows' Task Scheduler through PowerShell, so they need a Windows machine, or the path of powershell.exe here"
            program))
    (multiple-value-bind (stdout stderr exit timed-out)
        (funcall *run* found (powershell-argv script) timeout)
      (cond
        (timed-out
         (fail "PowerShell did not answer within ~as" timeout))
        ((not (eql 0 exit))
         (fail "PowerShell exited ~a: ~a" exit
               (nlk:clip (nlk:one-line (nonblank stderr "no message")) 400)))
        ((null (nonblank stdout))
         (fail "PowerShell answered nothing"))
        (t
         (let ((text (without-bom stdout)))
           (handler-case (nlk:decode-json text :whole t)
             (error ()
               (fail "PowerShell's answer was not JSON: ~a"
                     (nlk:clip (nlk:one-line text) 200))))))))))

(defun quote-powershell (text)
  "TEXT as a PowerShell single-quoted string: ' is written twice, nothing else
is special."
  (format nil "'~a'" (with-output-to-string (out)
                        (loop for char across (princ-to-string text)
                              do (when (char= char #\') (write-char #\' out))
                                 (write-char char out)))))

(defun split-path (path)
  "\\A\\B\\Task as the two parameters the cmdlets want: \\A\\B\\ and Task."
  (let* ((cleaned (format nil "\\~a" (string-left-trim "\\" (princ-to-string path))))
         (cut (position #\\ cleaned :from-end t)))
    (unless (and cut (> (length cleaned) (1+ cut)))
      (fail "the path names no task: ~s" path))
    (values (subseq cleaned 0 (1+ cut)) (subseq cleaned (1+ cut)))))

(defun raw-tasks ()
  "Every task the Scheduler knows, as the objects PowerShell answered."
  (let ((answer (ask (concatenate 'string *header* *list-script*))))
    (cond
      ((hash-table-p answer) (list answer)) ; -Compress writes one object, not a one-row array
      ((and (vectorp answer) (not (stringp answer)))
       (remove-if-not #'hash-table-p (coerce answer 'list)))
      (t (fail "expected a list of tasks from PowerShell, got ~a"
               (type-of answer))))))

(defun definition (path)
  "PATH's definition as a plist: :XML when the export worked, :ERROR when it did not."
  (multiple-value-bind (folder name) (split-path path)
    (let ((answer (ask (concatenate 'string *header*
                                    (format nil *definition-script*
                                            (quote-powershell folder)
                                            (quote-powershell name))))))
      (unless (hash-table-p answer)
        (fail "expected an object with the definition, got ~a" (type-of answer)))
      (list :xml (nlk:json-value answer :string "xml")
            :error (nlk:json-value answer :string "error")))))

;;; --- one task as a row ---------------------------------------------------------------

(defun trigger-word (class)
  "A trigger class in words, or the class itself when it is not one this knows."
  (or (cdr (assoc class *trigger-names* :test #'string=)) class))

(defun one-line (text)
  "TEXT with its runs of whitespace as one space, clipped at *ACTION-CHARS*."
  (when (stringp text)
    (let ((text (nlk:one-line text)))
      (if (> (length text) *action-chars*)
          (concatenate 'string (subseq text 0 *action-chars*) "...")
          text))))

(defun result-of (code)
  "A last result as a plist of :CODE, :HEX and what it :MEANS, or NIL."
  (when (integerp code)
    (let* ((unsigned (ldb (byte 32 0) code))
           (meaning (cdr (assoc unsigned *result-meanings*))))
      (list :code unsigned
            :hex (format nil "0x~8,'0X" unsigned)
            :meaning (or meaning "no known meaning")))))

(defun task-row (raw)
  "One task as the plist a listing and a detail both read."
  (let ((triggers (nlk:json-value raw :array "triggers")))
    (list :path (nlk:json-value raw :string "path")
          :state (nlk:json-value raw :string "state")
          :run-as (or (nlk:json-value raw :string "user")
                      (nlk:json-value raw :string "group"))
          :run-level (nlk:json-value raw :string "run_level")
          :action (one-line (nlk:json-value raw :string "action"))
          :action-count (nlk:json-value raw :integer "action_count")
          :triggers (loop for class across (or triggers #())
                          collect (trigger-word (princ-to-string class)))
          :last-run (nlk:json-value raw :string "last_run")
          :last-result (nlk:json-value raw :integer "last_result")
          :next-run (nlk:json-value raw :string "next_run")
          :missed-runs (nlk:json-value raw :integer "missed_runs"))))

(defun failing-p (row)
  "Whether ROW's last run ended with a failure; never run is not a failure."
  (let ((code (getf row :last-result)))
    (and (integerp code) (not (member (ldb (byte 32 0) code) *not-a-failure*)))))

(defun inside-microsoft-p (row)
  "Whether ROW is one of Windows' own tasks, the ones under \\Microsoft\\..."
  (let ((path (string-downcase (or (getf row :path) ""))))
    (or (uiop:string-prefix-p "\\microsoft\\" path)
        (string= path "\\microsoft"))))

;;; --- JSON out ---------------------------------------------------------------------

(defun plist-key (key)
  "KEY as the JSON member it names: :RUN-AS reads \"run_as\"."
  (string-downcase (substitute #\_ #\- (symbol-name key))))

(defun plist-json (plist)
  "PLIST as a JSON object, a nested plist as a nested object; NIL is an empty one."
  (apply #'nlk:make-json-object
         (loop for (key value) on plist by #'cddr
               append (list (plist-key key)
                            (if (and value (consp value)) (plist-json value) value)))))

(defun merge-json (&rest objects)
  "ONE JSON object over OBJECTS, a later member standing in an earlier's place."
  (apply #'nlk:make-json-object
         (loop for object in objects
               when (hash-table-p object)
                 append (loop for key being the hash-keys of object using (hash-value value)
                              collect key collect value))))

(defun result-json (result)
  "RESULT-OF's plist as a JSON object, or NIL when there is no code."
  (and result (plist-json result)))

(defun row-json (row)
  "One task as a listing shows it."
  (nlk:json-object
   (:getf row :opt "path" "state" "run_as" "run_level" "action" "action_count"
          "last_run" "next_run" "missed_runs")
   "triggers" (coerce (getf row :triggers) 'vector)
   "last_result" (or (result-json (result-of (getf row :last-result)))
                      (nlk:make-json-object))))

(defun answer-json (payload)
  "PAYLOAD as the text a tool answers: compact JSON."
  (nlk:encode-json-object payload))

(defun failed-answer (message &key (code "failed") suggestions hint)
  "The answer a call that could not read the Scheduler gives, as the plugin gave it."
  (answer-json (merge-json
                 (nlk:make-json-object "ok" nil "error" message "code" code)
                 (and suggestions (nlk:make-json-object "suggestions" (coerce suggestions 'vector)))
                 (and hint (nlk:make-json-object "hint" hint)))))

;;; --- the listing ------------------------------------------------------------------

(defun clamp-limit (limit)
  "LIMIT as the listing reads it: 1 to *MAX-ROWS*, 100 when it is not a number."
  (let ((value (cond ((integerp limit) limit)
                     ((and limit (ignore-errors (parse-integer (princ-to-string limit)))))
                     (t 100))))
    (max 1 (min value *max-rows*))))

(defun list-answer (query state failed-only include-microsoft limit)
  "Every task, filtered as ASKED, as the JSON a listing answers."
  (let ((rows (handler-case (mapcar #'task-row (raw-tasks))
                (windows-tasks-error (condition)
                  (return-from list-answer
                    (failed-answer (princ-to-string condition) :code "tasks_unavailable")))))
        (limit (clamp-limit limit))
        (query (string-downcase (nlk:trimmed (or query ""))))
        (state (string-downcase (nlk:trimmed (or state "")))))
    (let* ((matching (remove-if-not
                       (lambda (row)
                         (and (or include-microsoft (not (inside-microsoft-p row)))
                              (or (zerop (length query))
                                  (search query (string-downcase (or (getf row :path) ""))))
                              (or (zerop (length state))
                                  (string= state (string-downcase (or (getf row :state) ""))))
                              (or (not failed-only) (failing-p row))))
                       rows))
           (listed (if (> (length matching) limit) (subseq matching 0 limit) matching))
           (failures (loop for row in rows when (failing-p row) collect (getf row :path)))
           (notes (list (format nil "~d of ~d matching task(s) listed, ~d registered"
                                (length listed) (length matching) (length rows)))))
      (when (> (length matching) (length listed))
        (setf notes (append notes (list (format nil "stopped at limit=~d; raise it or narrow the query"
                                                limit)))))
      (when failures
        (setf notes (append notes (list (format nil "~d task(s) last ended with a failure; failed_only=true shows them"
                                                (length failures))))))
      (unless include-microsoft
        (setf notes (append notes (list "Windows' own tasks are left out; set include_microsoft=true to see them"))))
      (when (null listed)
        (setf notes (append notes (list "nothing matched; drop the query or the state filter"))))
      (when (and (plusp (length state)) (not (member state *states* :test #'string=)))
        (setf notes (append notes (list "state is one of ready, running, disabled, queued, unknown"))))
      (answer-json
       (nlk:json-object
        "ok" t
        "matched" (length listed)
        "candidates" (length matching)
        "registered_total" (length rows)
        "failing_total" (length failures)
        "entries" (coerce (mapcar #'row-json listed) 'vector)
        "notes" (coerce notes 'vector))))))

;;; --- one task in full --------------------------------------------------------------

(defun normalise (path)
  "A task path with one leading backslash and no trailing one: \\A\\B."
  (let ((text (nlk:trimmed (substitute #\\ #\/ (princ-to-string (or path ""))))))
    (if (zerop (length text))
        ""
        (format nil "\\~a" (string-trim "\\" text)))))

(defun describing (items describe)
  "ITEMS (plists) as JSON objects, each with DESCRIBE's sentence beside its fields."
  (coerce (mapcar (lambda (item)
                    (merge-json (plist-json item)
                                (nlk:make-json-object "description" (funcall describe item))))
                  items)
          'vector))

(defun detail-answer (name)
  "One task in full, as the JSON a detail answers."
  (let* ((requested (normalise name))
         (rows (handler-case (mapcar #'task-row (raw-tasks))
                 (windows-tasks-error (condition)
                   (return-from detail-answer
                     (failed-answer (princ-to-string condition) :code "tasks_unavailable")))))
         (wanted (string-downcase requested))
         (row (find wanted rows :test #'string=
                              :key (lambda (candidate) (string-downcase (or (getf candidate :path) ""))))))
    (unless row
      (let ((near (loop for candidate in rows
                        for path = (or (getf candidate :path) "")
                        when (search (string-trim "\\" wanted) (string-downcase path))
                          collect path)))
        (return-from detail-answer
          (failed-answer (format nil "no scheduled task is registered at ~a" requested)
                         :code "not_registered"
                         :suggestions (subseq near 0 (min 5 (length near)))
                         :hint "win_scheduled_tasks lists every registered path"))))
    (let ((notes '())
          (parsed nil))
      (let ((exported (handler-case (definition requested)
                        (windows-tasks-error (condition)
                          (list :error (princ-to-string condition))))))
        (let ((xml (getf exported :xml)))
          (if (and xml (plusp (length (nlk:trimmed xml))))
              (setf parsed (handler-case (parse-definition xml)
                             (windows-tasks-error (condition)
                               (setf notes (append notes
                                                   (list (princ-to-string condition))))
                               nil)))
              (setf notes (append notes
                                  (list (format nil "the definition could not be exported (the task may run as another account): ~a"
                                                (or (getf exported :error) "no reason given"))))))))
      (let* ((triggers (getf parsed :triggers))
             (settings (getf parsed :settings)))
        (when (and parsed (null triggers) (getf settings :maintenance))
          (setf notes (append notes
                              (list "no triggers: this task runs during automatic maintenance, which is why it has MaintenanceSettings instead"))))
        (answer-json
         (merge-json
          (nlk:json-object
           (:getf row :opt "path" "state" "run_as" "run_level" "last_run" "next_run" "missed_runs" "action")
           "ok" t
           "last_result" (or (result-json (result-of (getf row :last-result)))
                              (nlk:make-json-object)))
          (nlk:make-json-object
           "registration" (plist-json (getf parsed :registration))
           "principal" (plist-json (getf parsed :principal))
           "settings" (plist-json (getf parsed :settings)))
          (nlk:make-json-object
           "triggers" (describing triggers #'describe-trigger)
           "actions" (describing (getf parsed :actions) #'describe-action)
           "notes" (coerce notes 'vector))))))))

;;; --- the verbs --------------------------------------------------------------------

(define-verb tasks (&key query state failed-only include-microsoft (limit 100))
  "Every scheduled task on this Windows machine, with its state, the account it
runs as, its first action, its triggers in words, its last and next run, its
missed runs and what its last result code means. Filter by path substring
(QUERY), STATE, FAILED-ONLY (a task that has never run is not a failure) and
INCLUDE-MICROSOFT (Windows' own tasks are left out by default). Answers JSON;
read-only, and Windows only."
  (list-answer query state failed-only include-microsoft limit))

(define-verb task-detail (name)
  "One scheduled task in full: every trigger with its start boundary, repetition
and schedule, every action with its arguments and working directory, the
principal, and the settings that decide whether it runs on battery or when late,
beside its state and last and next run. NAME is the task's path as the listing
prints it; a leading backslash is optional. Answers JSON; read-only."
  (detail-answer name))

;;; --- the boundaries ---------------------------------------------------------------

(nlk:define-contract scheduled-tasks-call
  (:doc "What this Windows machine runs on its own, and did it work: every Task Scheduler task with its state (Ready, Running, Disabled), the account it runs as, its first action's command line, its triggers in words (at logon, daily, weekly, at a time, on event), its last and next run with the machine's offset, how many runs it missed, and what its last result code means -- 0x80070002 is a program that is no longer there, 0x800710E0 a task that needed a desktop it did not have, 0x40010004 one that was stopped. Use it for 'why does this keep failing', for finding what an uninstalled program left behind at logon, and for telling a disabled task from a broken one. Read-only; Windows only.")
  ("query" :string :default ""
           :doc "case-insensitive substring of the task's path, e.g. 'wps', 'update', 'backup'")
  ("state" :string :default ""
           :doc "only tasks in this state: ready, running, disabled, queued or unknown")
  ("failed_only" :boolean :default nil
                  :doc "only tasks whose last run ended with a failure; a task that has never run is not a failure")
  ("include_microsoft" :boolean :default nil
                        :doc "include Windows' own tasks, the ones under \\Microsoft\\...")
  ("limit" :integer :default 100
           :doc "how many tasks to list, default 100, at most 500"))

(defun tasks-tool (call)
  "The win_scheduled_tasks tool: the listing CALL asks for."
  (list-answer (scheduled-tasks-call-query call)
                (scheduled-tasks-call-state call)
                (scheduled-tasks-call-failed-only call)
                (scheduled-tasks-call-include-microsoft call)
                (scheduled-tasks-call-limit call)))

(nlk:define-contract scheduled-task-call
  (:doc "One Windows scheduled task in full: every trigger with its start boundary, repetition and schedule, every action with its arguments and working directory, the principal it runs as, and the settings that decide whether it runs on battery or when late, alongside its state and last and next run. A task with no triggers but maintenance settings runs during automatic maintenance, and the notes say so. Use it after win_scheduled_tasks named the task whose command line or schedule you need to see. Read-only; Windows only.")
  ("name" :text :required t
          :doc "the task's full path as win_scheduled_tasks prints it, e.g. '\\Microsoft\\Windows\\Defrag\\ScheduledDefrag'; a leading backslash is optional"))

(defun task-detail-tool (call)
  "The win_scheduled_task_detail tool: the one task CALL names."
  (detail-answer (scheduled-task-call-name call)))

;;; --- /windows-tasks ---------------------------------------------------------------

(defun run-slash (args session-id)
  "/windows-tasks: the tasks whose last run failed, or with a task path, that one
task in full."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (let* ((answer (list-answer "" "" t nil 25))
               (payload (nlk:decode-json answer))
               (entries (nlk:json-array payload "entries")))
          (if (zerop (length entries))
              (format nil "windows-tasks: no task's last run is a failure. ~
                           /windows-tasks with a task's path shows one in full; the model has win_scheduled_tasks.~%")
              (with-output-to-string (out)
                (format out "windows-tasks: ~d task(s) whose last run failed~%" (length entries))
                (loop for entry across entries
                      do (format out "  ~a  ~a  ~a~%"
                                 (or (nlk:json-value entry :string "path") "")
                                 (or (nlk:json-value entry :string "state") "")
                                 (or (nlk:json-value (nlk:json-value entry :object "last_result") :string "meaning") "")))
                (format out "a task's path after /windows-tasks shows it in full~%"))))
        (detail-answer text))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell windows-tasks
  (:section ("windows-tasks")
    (:guide "Windows only, and no key: these tools ask the Task Scheduler through powershell.exe, which Windows ships; name its path in program when it is not on PATH, and timeout_seconds bounds one call")
    ("program" :string :default "powershell.exe"
               :doc "the Windows PowerShell program: its name on PATH, or a path to it")
    ("timeout_seconds" :integer :default 90 :min 1
                       :doc "how long one call may take, in seconds"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((program (getf values :program)))
                 (unless (program-path program)
                   (nlk:config-error
                    "windows-tasks.program is ~s: no such program on PATH -- these tools read Windows' Task Scheduler, so they need Windows, or the path of powershell.exe here"
                    program))
                 values)))
  (:tool "win_scheduled_tasks" (nlk:contract 'scheduled-tasks-call) 'tasks-tool)
  (:tool "win_scheduled_task_detail" (nlk:contract 'scheduled-task-call) 'task-detail-tool)
  (:command "windows-tasks" 'run-slash
            :catches 'windows-tasks-error
            :description "What Windows runs on its own, and whether it worked"
            :argument-hint "TASK-PATH")
  (:help :windows-tasks "windows-tasks: Windows' scheduled tasks, read-only - (windows-tasks:tasks :failed-only t)"
         *primer*))
