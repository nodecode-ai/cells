;;;; cell-test.lisp --- the listing, the detail, the command, every refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed PowerShell: that
;;;; its start installs both tools, the command and the manual and its stop takes
;;;; them back; that a listing reads the rows the Scheduler's cmdlets printed and
;;;; filters them as asked; that a detail reads an exported definition into its
;;;; triggers, actions, principal and settings in words; that every way the
;;;; Scheduler cannot be read says why; and that a missing program refuses the
;;;; start. One test runs a real /bin/sh stand-in, for the argv a stub cannot
;;;; prove. Nothing here needs Windows or a real PowerShell.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "windows-tasks"
  (:help :windows-tasks)
  (:command "windows-tasks")
  (:running
   (is (nle::find-tool "win_scheduled_tasks") "the listing tool is registered")
   (is (nle::find-tool "win_scheduled_task_detail") "and the one-task detail")
   (is (= 90 (nodecode-windows-tasks::setting :timeout-seconds)) "a call may take 90 seconds")
   (is (plusp (length (nodecode-windows-tasks::setting :program))) "and it names a program"))
  (:stopped
   (is (null (nle::find-tool "win_scheduled_tasks")) "and stop takes both back")
   (is (null (nle::find-tool "win_scheduled_task_detail"))))
  (:refused ("program" "definitely-no-such-program-xyz") ("timeout_seconds" 0))
  (:idle nodecode-windows-tasks:windows-tasks-error (windows-tasks:tasks)))

;;; --- the listing -------------------------------------------------------------------

(deftest windows-tasks-cell-the-listing-reads-the-scheduler-and-answers-its-rows ()
  (with-cell-stop ((windows-tasks-start))
    (with-powershell-stub (:stdout +listing+)
      (let* ((payload (answered-json "win_scheduled_tasks" "{}"))
             (first (first (entries payload))))
        (is (eq t (field payload "ok")) "the answer says it worked")
        (is (= 3 (field payload "registered_total")) "three tasks are registered")
        (is (= 2 (length (entries payload))) "Windows' own task is left out by default")
        (is (= 1 (field payload "failing_total")) "one of them last ended with a failure")
        (is (string= "\\Backup\\Offsite" (field first "path")) "a row names its path")
        (is (string= "Ready" (field first "state")) "and its state")
        (is (string= "a-machine\\someone" (field first "run_as")) "and the account it runs as")
        (is (equal '("at logon") (coerce (nlk:json-value first :array "triggers") 'list)) "its trigger class is carried in words")
        (is (string= "0x00000000" (field first "last_result" "hex")) "the result code in hex")
        (is (string= "success" (field first "last_result" "meaning")) "with what it means")
        (is (search "Get-ScheduledTask" (last-script)) "the script asks the Scheduler's cmdlets")
        (is (equal '("-NoProfile" "-NonInteractive" "-ExecutionPolicy" "Bypass" "-Command")
                   (butlast (second (last-run)))) "with the plugin's own argv")))))

(deftest windows-tasks-cell-the-filters-and-the-limit-say-what-they-left-out ()
  (with-cell-stop ((windows-tasks-start))
    (with-powershell-stub (:stdout +listing+)
      (let* ((failing (answered-json "win_scheduled_tasks" "{\"failed_only\": true}"))
             (only (entries failing)))
        (is (= 1 (length only)) "failed_only answers the one that failed")
        (is (string= "\\Backup\\Nightly" (field (first only) "path")))
        (is (string= "the program or script could not be found"
                     (field (first only) "last_result" "meaning")) "and says what 0x80070002 means"))
      (let* ((all (answered-json "win_scheduled_tasks" "{\"include_microsoft\": true,\"limit\": 2}")))
        (is (= 2 (length (entries all))) "the limit stops the listing")
        (is (search "stopped at limit=2" (notes all)) "and says it did")
        (is (search "1 task(s) last ended with a failure" (notes all)) "a failure is counted"))
      (let ((none (answered-json "win_scheduled_tasks" "{\"query\": \"nothing-matches-this\"}")))
        (is (null (entries none)) "a query that matches nothing answers no row")
        (is (search "nothing matched" (notes none)) "and says so"))
      (let ((bad (answered-json "win_scheduled_tasks" "{\"state\": \"broken\"}")))
        (is (search "state is one of ready" (notes bad)) "an unknown state is named")))))

;;; --- one task in full --------------------------------------------------------------

(deftest windows-tasks-cell-the-detail-reads-an-exported-definition ()
  (with-cell-stop ((windows-tasks-start))
    (with-powershell-stub (:replies (list +listing+ (export-json)))
      (let* ((payload (answered-json "win_scheduled_task_detail"
                                      "{\"name\": \"\\\\Backup\\\\Offsite\"}"))
             (triggers (nlk:json-value payload :array "triggers"))
             (actions (nlk:json-value payload :array "actions")))
        (is (eq t (field payload "ok")) "the detail answers")
        (is (= 2 (length triggers)) "both triggers of the definition are there")
        (is (string= "at logon of a-machine\\someone" (field (aref triggers 0) "description")) "a logon trigger in words")
        (is (string= "every 1 day(s) at 03:00, repeating every PT1H for PT4H"
                     (field (aref triggers 1) "description")) "a daily trigger with its start time and repetition")
        (is (string= "C:\\Tools\\copy.exe --to offsite" (field (aref actions 0) "description")) "the action as the command line a person would run")
        (is (string= "C:\\Tools" (field (aref actions 0) "working_directory")))
        (is (eq t (field payload "settings" "disallow_start_if_on_batteries")) "whether it runs on battery is carried")
        (is (string= "a-machine\\someone" (field payload "principal" "user_id")))
        (is (string= "Keeps the off-site copy current." (field payload "registration" "description")))
        (is (search "Export-ScheduledTask" (last-script)) "the definition is exported, not guessed")
        (is (search "-TaskName 'Offsite'" (last-script)) "with the task's folder and name quoted")
        (is (search "-TaskPath '\\Backup\\'" (last-script)))))))

(deftest windows-tasks-cell-a-task-with-no-triggers-runs-in-maintenance ()
  (with-cell-stop ((windows-tasks-start))
    (with-powershell-stub (:replies (list +listing+ (export-json +maintenance-definition+)))
      (let ((payload (answered-json "win_scheduled_task_detail"
                                     "{\"name\": \"\\\\Backup\\\\Offsite\"}")))
        (is (string= "P1D" (field payload "settings" "maintenance" "period")))
        (is (search "runs during automatic maintenance" (notes payload)) "an empty trigger list is explained rather than left to be misread")))))

;;; --- every way the Scheduler answers nothing ---------------------------------------

(deftest windows-tasks-cell-every-way-the-scheduler-cannot-be-read-says-why ()
  (with-cell-stop ((windows-tasks-start))
    (with-powershell-stub (:exit 1 :stderr "Access is denied")
      (let ((payload (answered-json "win_scheduled_tasks" "{}")))
        (is (null (field payload "ok")) "a failed call answers ok false")
        (is (string= "tasks_unavailable" (field payload "code")))
        (is (search "Access is denied" (field payload "error")) "with PowerShell's own message")))
    (with-powershell-stub (:stdout "not json at all")
      (is (search "not JSON" (field (answered-json "win_scheduled_tasks" "{}") "error")) "an answer that is no JSON says so"))
    (with-powershell-stub (:timed-out t)
      (is (search "did not answer within" (field (answered-json "win_scheduled_tasks" "{}") "error")) "a call past its deadline says so"))
    (with-powershell-stub (:stdout +listing+)
      (let ((payload (answered-json "win_scheduled_task_detail"
                                     "{\"name\": \"\\\\Backup\\\\Nigh\"}")))
        (is (null (field payload "ok")) "a path nothing is registered at is no answer")
        (is (string= "not_registered" (field payload "code")))
        (is (string= "win_scheduled_tasks lists every registered path" (field payload "hint")))
        (is (string= "\\Backup\\Nightly"
                     (aref (or (nlk:json-value payload :array "suggestions") #()) 0)) "a path like it is suggested")))
    (with-powershell-stub (:replies (list +listing+ (export-json "<NotATask><oops></NotATask>")))
      (let ((payload (answered-json "win_scheduled_task_detail"
                                     "{\"name\": \"\\\\Backup\\\\Offsite\"}")))
        (is (search "could not be read" (notes payload)) "a definition that is no task is named")))))

(deftest windows-tasks-cell-an-export-that-failed-is-named-in-the-notes ()
  (with-cell-stop ((windows-tasks-start))
    (with-powershell-stub (:replies (list +listing+ (export-error-json "Access is denied")))
      (let ((payload (answered-json "win_scheduled_task_detail"
                                     "{\"name\": \"\\\\Backup\\\\Offsite\"}")))
        (is (eq t (field payload "ok")) "the listing's own row still answers")
        (is (search "Access is denied" (notes payload)) "and the note carries why the export failed")))))

;;; --- the command -------------------------------------------------------------------

(deftest windows-tasks-cell-the-command-lists-what-failed-and-one-task-in-full ()
  (with-cell-stop ((windows-tasks-start))
    (with-powershell-stub (:stdout +listing+)
      (let ((answer (cell-entry "nodecode-windows-tasks" "windows-tasks" "")))
        (is (search "task(s) whose last run failed" answer) "typed alone it names what failed")
        (is (search "\\Backup\\Nightly" answer) "with the task's path")
        (is (search "the program or script could not be found" answer) "and what its code means")))
    (with-powershell-stub (:replies (list +listing+ (export-json)))
      (let ((answer (cell-entry "nodecode-windows-tasks" "windows-tasks" "\\Backup\\Offsite")))
        (is (search "every 1 day(s) at 03:00" answer) "with a path it answers that task in full")))))

;;; --- the real program ---------------------------------------------------------------

(deftest windows-tasks-cell-the-program-is-really-run-and-its-answer-read ()
  (let ((directory (merge-pathnames "windows-tasks-running/" (uiop:temporary-directory))))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree directory :validate t)))
      (let ((program (windows-tasks-script
                      (merge-pathnames "powershell.exe" directory)
                      (format nil "printf '%s\\n' \"$@\" > ~s"
                              (namestring (merge-pathnames "argv" directory)))
                      (format nil "printf '%s' ~s" +listing+))))
        (with-cell-stop ((windows-tasks-start "program" program))
          (let ((payload (answered-json "win_scheduled_tasks" "{}")))
            (is (= 3 (field payload "registered_total")) "the program's answer is the listing"))
          (let ((argv (uiop:read-file-string (merge-pathnames "argv" directory))))
            (is (search "-NoProfile" argv) "it was asked for no profile")
            (is (search "Get-ScheduledTask" argv) "and to run the Scheduler's own script")))))))
