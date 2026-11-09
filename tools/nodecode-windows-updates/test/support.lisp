;;;; support.lisp --- the test slice: a canned PowerShell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; WINDOWS-UPDATES-CELL- name prefix; RUN-WINDOWS-UPDATES-TESTS runs exactly
;;;; that slice. The one seam that leaves the image is QUERY, the cell's only
;;;; way to start a process: WITH-POWERSHELL replaces it with a canned answer
;;;; and records every call, so a test asserts on what was asked and nothing
;;;; ever runs a command.

(in-package #:nodecode.test)

(define-test-slice "windows-updates" "WINDOWS-UPDATES-CELL-"
  :start nodecode-windows-updates:start-cell)

;;; --- the canned answers -----------------------------------------------------------

(defparameter +history+
  "{\"total\": 723, \"entries\": [
    {\"date\": \"2026-09-09T10:11:00Z\", \"title\": \"2026-09 Cumulative Update for Windows 11 (KB5039212)\", \"result_code\": 2, \"hresult\": 0, \"support_url\": \"https://support.example/help/5039212\", \"categories\": [\"Security Updates\"]},
    {\"date\": \"2026-09-08T03:02:01Z\", \"title\": \"Security Intelligence Update for Microsoft Defender Antivirus - KB2267602 (Version 1.417.1.0)\", \"result_code\": 4, \"hresult\": -2145124300, \"support_url\": \"https://support.example/help/2267602\", \"categories\": [\"Definition Updates\", \"Microsoft Defender Antivirus\"]},
    {\"date\": null, \"title\": \"Intel - System - 31.0.101.4502\", \"result_code\": 5, \"hresult\": -2145116156, \"support_url\": null, \"categories\": [\"Drivers\"]}]}"
  "Three history entries as the history script writes them: one that succeeded,
one that failed with the signed HRESULT the Agent hands over, and one aborted
entry that carries no date.")

(defparameter +hotfixes+
  "[{\"id\": \"KB5039212\", \"description\": \"Security Update\", \"installed_on\": \"2026-09-01\", \"installed_by\": \"NT AUTHORITY\\\\SYSTEM\"},
    {\"id\": \"KB2267602\", \"description\": \"Definition Update\", \"installed_on\": null, \"installed_by\": \"NT AUTHORITY\\\\SYSTEM\"},
    {\"id\": \"KB5041585\", \"description\": \"Security Update\", \"installed_on\": \"2026-09-09\", \"installed_by\": \"DOMAIN\\\\operator\"}]"
  "Three quick-fix rows out of order, one of them without an install date.")

;;; --- the stub ---------------------------------------------------------------------

(defvar *answers* '(:history "" :hotfix "")
  "The stdout the stubbed QUERY gives, by script: :history for the update
history, :hotfix for the quick-fix list.")

(defvar *stderr* ""
  "The answer the stubbed QUERY gives as its stderr.")

(defvar *status* 0
  "The status the stubbed QUERY answers with: an exit code, or :TIMEOUT.")

(defvar *calls* '()
  "Every call the stub took, newest first: (:script :environment).")

(defun stub-query (script &key environment)
  (push (list :script script :environment environment) *calls*)
  (values (getf *answers* (if (search "QuickFixEngineering" script) :hotfix :history) "")
          *stderr* *status*))

(defmacro with-powershell ((&key (history "") (hotfix "") (stderr "") (status 0))
                           &body body)
  "Run BODY with the cell's one process seam stubbed and its calls recorded."
  `(let ((*answers* (list :history ,history :hotfix ,hotfix))
         (*stderr* ,stderr) (*status* ,status) (*calls* '()))
     (with-stubbed-fdefinition (nodecode-windows-updates::query (script &key environment)
                                (stub-query script :environment environment))
       ,@body)))

(defmacro with-updates ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS and a canned PowerShell, stopped on unwind."
  `(with-cell-stop ((windows-updates-start ,@pairs))
     ,@body))

;;; --- calling a tool the way a provider does ---------------------------------------

(defun wu-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun answer-of (text)
  "TEXT, a tool answer, as the JSON object it carries."
  (nlk:decode-json text))

(defun value-of (text key)
  "KEY's value in the tool answer TEXT, or NIL."
  (let ((object (answer-of text)))
    (and (hash-table-p object) (gethash key object))))

(defun rows-of-answer (text key)
  "The rows under KEY in the tool answer TEXT, as a list of objects."
  (let ((value (value-of text key)))
    (cond ((vectorp value) (coerce value 'list))
          ((hash-table-p value) (list value))
          (t '()))))

(defun text-of (object key)
  "KEY's value in the JSON OBJECT as a string, or NIL."
  (let ((value (and (hash-table-p object) (gethash key object))))
    (and value (princ-to-string value))))

(defun stub-call ()
  "The newest call the stub took."
  (first *calls*))
