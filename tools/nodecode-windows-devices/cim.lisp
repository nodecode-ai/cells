;;;; cim.lisp --- one seam to PowerShell, and the two CIM tables read through it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin asks CIM through powershell.exe twice: Win32_PnPEntity
;;;; for the device tree, Win32_PnPSignedDriver for the driver behind a device.
;;;; Both scripts are the original's, carried here as data. The arguments go to
;;;; the process as a list, never through a shell, so no text a caller wrote is
;;;; ever script: the only variable part of a query is nothing at all.
;;;;
;;;; Win32_PnPEntity answers with property names, enum values and ids that do
;;;; not change with the display language; the human-facing names (Name,
;;;; Manufacturer) localize, but they are data carried through, never parsed.
;;;; Both queries run unelevated, and nothing here writes: no CIM call sets a
;;;; device, no file is opened, no network is touched.

(in-package #:nodecode-windows-devices)

;;; --- the platform ---------------------------------------------------------------

(defparameter *platform* (or (ignore-errors (nlk:platform)) :unix)
  "The platform this image runs on, as NLK:PLATFORM answers it; a test rebinds it.")

(defun windows-p ()
  "Whether this image runs on Windows."
  (eq *platform* :windows))

;;; --- the one process -------------------------------------------------------------

(defparameter +header+
  "$ErrorActionPreference='Stop'; [Console]::OutputEncoding=[Text.Encoding]::UTF8;"
  "Said before every script: a failing cmdlet raises, and the answer comes back UTF-8.")

(defparameter +devices-script+
  "$devs = Get-CimInstance Win32_PnPEntity
$out = foreach ($d in $devs) {
  [pscustomobject]@{
    name = $d.Name
    class = $d.PNPClass
    status = [string]$d.Status
    code = [int]$d.ConfigManagerErrorCode
    manufacturer = $d.Manufacturer
    device_id = $d.DeviceID
    present = if ($d.Present) { $true } else { $false }
  }
}
$out | ConvertTo-Json -Depth 4 -Compress
"
  "The device tree, read-only: one object a device, its id and its problem code.")

(defparameter +drivers-script+
  "$drv = Get-CimInstance Win32_PnPSignedDriver
$out = foreach ($x in $drv) {
  [pscustomobject]@{
    device_id = $x.DeviceID
    device_name = $x.DeviceName
    class = $x.DeviceClass
    version = $x.DriverVersion
    date = if ($x.DriverDate) { $x.DriverDate.ToString('o') } else { $null }
    signer = $x.Signer
    inf = $x.InfName
    signed = if ($x.IsSigned) { $true } else { $false }
    provider = $x.DriverProviderName
  }
}
$out | ConvertTo-Json -Depth 4 -Compress
"
  "The signed-driver table: version, date, signer and INF per device.")

(nlk:define-error cim-refusal (error)
  ((code) (detail))
  (:report "~a" detail)
  (:documentation "A CIM query, or the device one was asked about, that could not be had.
CODE is the word the answer carries for why: windows_only, devices_unavailable, not_found."))

(defun refuse (code format-control &rest arguments)
  "Signal CIM-REFUSAL with CODE and a formatted DETAIL."
  (error 'cim-refusal :code code :detail (apply #'format nil format-control arguments)))

(defun last-chars (text &optional (limit 400))
  "TEXT trimmed, cut to its last LIMIT characters: a child's own words, bounded."
  (let ((text (nlk:trimmed (or text ""))))
    (if (> (length text) limit) (subseq text (- (length text) limit)) text)))

(defun strip-bom (text)
  "TEXT without the byte-order mark a pipe written by PowerShell may start with."
  (if (and (stringp text) (plusp (length text)) (char= (char text 0) (code-char #xFEFF)))
      (subseq text 1)
      text))

(defun powershell-executable ()
  "The PowerShell to run: the `powershell' setting when it names one, else the one
this platform has."
  (let ((configured (nlk:trimmed (or (setting :powershell) ""))))
    (cond ((plusp (length configured)) configured)
          ((windows-p) "powershell.exe")
          (t "pwsh"))))

(defun powershell-argv (script)
  "ARGV that runs SCRIPT: the executable first, the script one argument."
  (list (powershell-executable) "-NoProfile" "-NonInteractive" "-Command"
        (concatenate 'string +header+ (string #\Newline) script)))

(defparameter +windows-only+
  "this machine is not Windows: CIM answers nothing here. Set `powershell' in the
windows-devices section to a PowerShell that can reach a Windows host, or run the cell there."
  "What a query says on a platform with no CIM, so no process is spawned for nothing.")

(defun run-powershell (script)
  "SCRIPT through PowerShell, answering its standard output.
 The one function every CIM query goes through, so a test stubs it and nothing
 spawns a process."
  (when (and (not (windows-p)) (zerop (length (nlk:trimmed (or (setting :powershell) "")))))
    (refuse "windows_only" "~a" +windows-only+))
  (multiple-value-bind (out err status)
      (nlk:run-bounded (powershell-argv script) :seconds (setting :timeout))
    (cond ((eq status :timeout)
           (refuse "devices_unavailable"
                   "the CIM query did not finish in ~d s; its whole process tree was killed. Raise `timeout' in the windows-devices section."
                   (setting :timeout)))
          ((not (and (integerp status) (zerop status)))
           (refuse "devices_unavailable" "the CIM query failed: ~a"
                   (or (last-chars err) "no reason given")))
          (t (strip-bom out)))))

;;; --- what an answer is ------------------------------------------------------------

(defun cim-list (text what)
  "TEXT, a ConvertTo-Json answer, as a list of JSON objects.
 WHAT names the table in a refusal. No device at all is the empty string, and
 one device is a single object rather than a list: both read as the list they mean."
  (let ((text (nlk:trimmed (strip-bom (or text "")))))
    (if (zerop (length text))
        '()
        (let ((data (handler-case (nlk:decode-json text)
                      (error (problem)
                        (refuse "devices_unavailable" "the CIM answer for ~a was not JSON: ~a"
                                what problem)))))
          (cond ((hash-table-p data) (list data))
                ((and (vectorp data) (not (stringp data))) (coerce data 'list))
                ((listp data) data)
                (t (refuse "devices_unavailable" "the CIM answer for ~a was not a listing" what)))))))

(defun device-of (object)
  "One Win32_PnPEntity answer as a plist; a field it does not carry reads NIL."
  (multiple-value-bind (present given) (nlk:json-value object :boolean "present")
    (list :name (nlk:json-value object :string "name")
          :class (nlk:json-value object :string "class")
          :status (nlk:json-value object :string "status")
          :code (nlk:json-value object :integer "code")
          :manufacturer (nlk:json-value object :string "manufacturer")
          :device-id (nlk:json-value object :string "device_id")
          :present (if given present t))))

(defun driver-of (object)
  "One Win32_PnPSignedDriver answer as a plist."
  (list :device-id (nlk:json-value object :string "device_id")
        :name (nlk:json-value object :string "device_name")
        :class (nlk:json-value object :string "class")
        :version (nlk:json-value object :string "version")
        :date (nlk:json-value object :string "date")
        :provider (nlk:json-value object :string "provider")
        :signer (nlk:json-value object :string "signer")
        :inf (nlk:json-value object :string "inf")
        :signed (nlk:json-value object :boolean "signed")))

(defun devices ()
  "Every PnP device this machine knows, present or historical, as plists."
  (mapcar #'device-of (cim-list (run-powershell +devices-script+) "devices")))

(defun drivers ()
  "The signed-driver table, a row a device, as plists."
  (mapcar #'driver-of (cim-list (run-powershell +drivers-script+) "drivers")))

;;; --- the problem codes ------------------------------------------------------------

(defparameter +code-meanings+
  '((0 . "working")
    (1 . "configured incorrectly")
    (10 . "cannot start")
    (12 . "not enough free resources for it")
    (14 . "needs a restart to finish")
    (18 . "its driver must be reinstalled")
    (21 . "Windows is removing it")
    (22 . "disabled (the person, or a policy, switched it off)")
    (24 . "not present or not working")
    (28 . "no driver is installed for it")
    (31 . "not working properly")
    (37 . "its driver failed to initialize")
    (39 . "its driver is missing or corrupt")
    (43 . "Windows stopped it after it reported a failure"))
  "What a device's ConfigManagerErrorCode means, in words a person can act on.")

(defparameter +intentional+
  '(0 22)
  "Codes that are an intentional state rather than a breakage: working, and switched off.")

(defun code-meaning (code)
  "CODE as the words a person can act on."
  (or (cdr (assoc code +code-meanings+)) "no known meaning"))

(defun broken-p (code)
  "Whether CODE is a device that needs help; a disabled one does not."
  (and (integerp code) (not (member code +intentional+))))
