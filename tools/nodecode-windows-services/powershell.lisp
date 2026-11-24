;;;; powershell.lisp --- the two queries, and the one way they run.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything this cell knows comes from Windows' own two answers: one
;;;; Get-CimInstance Win32_Service call, whose property names and enum
;;;; values do not change with the display language (State is Stopped on a
;;;; Chinese install too, while sc.exe's query labels and status words are
;;;; localized), and the registry under HKLM\SYSTEM\CurrentControlSet\Services,
;;;; which is where the dependency edges and the failure policy really live:
;;;; CIM carries a DependOnService property it never populates, and
;;;; FailureActions exists nowhere in CIM at all.
;;;;
;;;; The listing is one child process for the whole machine; the event query
;;;; is a second one, only for a detail. The service name reaches it through
;;;; the environment, not the command line, so there is no quoting, escaping
;;;; or injection surface at all.

(in-package #:nodecode-windows-services)

(defparameter +powershell-names+ '("powershell.exe" "pwsh.exe" "pwsh")
  "The PowerShell the cell looks for, in order, when the section names none:
Windows PowerShell 5.1 first, then PowerShell 7.")

(defun clip (text chars)
  "TEXT as one line, at most CHARS characters; anything else unchanged."
  (if (stringp text)
      (let ((text (nlk:trimmed (substitute #\Space #\Newline (substitute #\Space #\Return text)))))
        (if (<= (length text) chars) text (concatenate 'string (subseq text 0 chars) "...")))
      text))

;;; --- finding it -------------------------------------------------------------

(defun path-separator ()
  "The character PATH entries are split on here."
  (if (uiop:os-windows-p) #\; #\:))

(defun probe-native (path)
  "PATH, a native namestring, as a truename when a file is there."
  (ignore-errors (probe-file (uiop:parse-native-namestring path))))

(defun executable-on-path (name)
  "NAME's file in the first PATH directory holding it -- under .exe too on
Windows, where a bare name is how a program is written -- or NIL."
  (let* ((windows (uiop:os-windows-p))
         (names (if (and windows (not (find #\. name)))
                    (list name (concatenate 'string name ".exe"))
                    (list name)))
         (separator (string (path-separator))))
    (dolist (directory (uiop:split-string (or (uiop:getenv "PATH") "") :separator separator))
      (when (plusp (length directory))
        (dolist (candidate names)
          (let ((found (probe-native
                        (format nil "~a~a~a"
                                (string-right-trim "/\\" directory)
                                (if windows "\\" "/")
                                candidate))))
            (when found
              (return-from executable-on-path (uiop:native-namestring found)))))))))

(defun find-powershell (&optional configured)
  "The PowerShell to run: CONFIGURED when the section names one, else the
first of +POWERSHELL-NAMES+ on PATH, else NIL."
  (or (and configured (plusp (length (nlk:trimmed configured))) (nlk:trimmed configured))
      (loop for name in +powershell-names+
            thereis (executable-on-path name))))

;;; --- running one ------------------------------------------------------------

(defparameter +header+
  "$ErrorActionPreference='Stop'; [Console]::OutputEncoding=[Text.Encoding]::UTF8;"
  "What every script opens with: stop on the first error, and answer UTF-8.")

(defun strip-bom (text)
  "TEXT without a byte-order mark PowerShell may have led with."
  (if (and (plusp (length text)) (char= (char text 0) #\uFEFF))
      (subseq text 1)
      text))

(defun run-powershell (script &key environment)
  "SCRIPT through PowerShell, one child process, ENVIRONMENT (NAME=value
strings) laid over this process's; its stdout as a string. A child that
exits non-zero, or does not exit in time, is a refusal naming what it said."
  (let* ((settings (running-settings))
         (exe (getf settings :powershell))
         (seconds (getf settings :timeout-seconds)))
    (multiple-value-bind (output error status)
        (nlk:run-bounded (list exe "-NoProfile" "-NonInteractive" "-Command"
                               (concatenate 'string +header+ (string #\Newline) script))
                         :seconds seconds
                         :environment environment
                         :output :string :error-output :string)
      (cond ((eq status :timeout)
             (fail "PowerShell did not answer in ~a s; raise timeout_seconds in the windows-services section"
                   seconds))
            ((not (eql status 0))
             (fail "the PowerShell query failed (~a): ~a"
                   status (clip (or error "") 400)))
            (t (strip-bom (or output "")))))))

;;; --- the listing ------------------------------------------------------------

(defparameter +list-script+
  "$svcs = Get-CimInstance -ClassName Win32_Service
$out = foreach ($s in $svcs) {
  $deps = @(); $delayedReg = $false; $actions = @()
  try {
    $reg = Get-ItemProperty -LiteralPath ('HKLM:\SYSTEM\CurrentControlSet\Services\' + $s.Name) -ErrorAction Stop
    if ($reg.DependOnService) { $deps = @($reg.DependOnService) }
    if ($reg.DelayedAutostart -eq 1) { $delayedReg = $true }
    if ($reg.FailureActions) { $actions = @($reg.FailureActions) }
  } catch { }
  [pscustomobject]@{
    name = $s.Name
    display = $s.DisplayName
    state = [string]$s.State
    start = [string]$s.StartMode
    delayed = [bool]($s.DelayedAutoStart -or $delayedReg)
    pid = [int]$s.ProcessId
    exit_code = [int]$s.ExitCode
    account = $s.StartName
    binary = $s.PathName
    description = $s.Description
    depends_on = $deps
    failure_actions = $actions
  }
}
ConvertTo-Json -InputObject @($out) -Depth 4 -Compress"
  "Every service, with the facts CIM tells the truth about and the two the
registry does: the dependency edges, and the failure-action blob -- a byte
array there, an array of numbers in the JSON.")

(defparameter +events-script+
  "$name = $env:NODECODE_SVC_NAME
$ids = @(7000, 7001, 7009, 7011, 7023, 7024, 7031, 7034, 7045)
$events = Get-WinEvent -FilterHashtable @{LogName='System'; ProviderName='Service Control Manager'} `
  -MaxEvents 400 -ErrorAction SilentlyContinue |
  Where-Object { $ids -contains $_.Id -and $_.Message -match [regex]::Escape($name) } |
  Select-Object -First 12
$out = foreach ($e in $events) {
  [pscustomobject]@{
    time = $e.TimeCreated.ToString('o')
    id = [int]$e.Id
    what = $e.Message
  }
}
if ($out) { ConvertTo-Json -InputObject @($out) -Depth 4 -Compress } else { '[]' }"
  "The Service Control Manager's own record behind a failure -- 7000, 7009,
7024, 7031, 7034, 7045 and their neighbours, newest first, the service named
in the environment. Routine noise (7036 state changes, 7040 start-type)
stays out on purpose.")

;;; --- reading the answers ----------------------------------------------------

(defun parse-objects (text &key (what "service listing"))
  "TEXT, PowerShell's JSON, as a list of decoded objects; one object counts
as one row, and an empty answer as no rows."
  (let ((text (nlk:trimmed (or text ""))))
    (when (plusp (length text))
      (let ((data (handler-case (nlk:decode-json text)
                    (error () (fail "the PowerShell answer was not JSON: ~a" (clip text 200))))))
        (cond ((hash-table-p data) (list data))
              ((and (vectorp data) (not (stringp data)))
               (coerce (remove-if-not (function hash-table-p) data) 'list))
              (t (fail "the PowerShell answer was not a ~a" what)))))))

(defun fetch-services ()
  "Every registered service, as the objects the listing script answers."
  (parse-objects (run-powershell +list-script+)))

(defun fetch-events (name)
  "The events naming service NAME, newest first."
  (parse-objects (run-powershell +events-script+
                                  :environment (list (format nil "NODECODE_SVC_NAME=~a" name)))
                  :what "event list"))
