;;;; cell.lisp --- two read-only device tools, a command and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Device Manager question, answered without Device Manager: a yellow mark
;;;; on a device is a number nobody translates, and these two tools translate
;;;; it. Everything they reach is CIM through PowerShell, unelevated and
;;;; read-only; the section only says which PowerShell and how long one query
;;;; may take.

(in-package #:nodecode-windows-devices)

(nlk:define-peripheral windows-devices :not-running t)

(defparameter +manual+
  "Which device on this Windows machine is unhappy, and what it needs, through the
nodecode-windows-devices cell: CIM read by PowerShell, never Device Manager.

  win_device_table   every present PnP device -- name, class, status, manufacturer,
      device id, and its problem code in words. Arguments: query (a substring of
      the name or the id, e.g. 'camera' or 'LENOVO'), class ('Net', 'HIDClass',
      'Display'), broken_only, include_absent (historical devices), limit (1 to
      300, 100 by default). The codes: 28 no driver is installed, 10 cannot
      start, 43 Windows stopped it after it reported a failure, 22 the person
      switched it off on purpose (not broken), 1 configured incorrectly.
  win_device_detail  one device in full: everything the table shows, plus the
      driver behind it -- name, version, date, provider, signer, INF file and
      whether it is signed -- or none at all, which for code 28 is the same fact
      said twice. Argument: name, a substring or the exact DeviceID.
  /windows-devices    the same reading for a person: typed alone, what is here
      and what needs attention, with a row that opens one; /windows-devices NAME
      prints one device in full.

From eval: (windows-devices:devices) is the raw table as plists and
(windows-devices:detail-of \"camera\") a device with the driver behind it.

Read-only: nothing is enabled, disabled, updated or reinstalled; no network, no
file written. Windows only; the section's `powershell' names the executable when
it is not on PATH (pwsh off Windows, for a host that reaches one) and `timeout'
bounds one query."
  "What (help :windows-devices) answers while the cell runs.")

(nle:define-cell windows-devices
  (:section ("windows-devices")
    (:guide "nothing here is required on Windows: the device tree is read through powershell.exe, unelevated; set powershell only when the executable is elsewhere or a pwsh reaches a Windows host; timeout bounds one query")
    ("powershell" :string :default ""
     :doc "the PowerShell to run a CIM query with; empty uses powershell.exe on Windows and pwsh elsewhere")
    ("timeout" :integer :default 120 :min 10
     :doc "seconds one CIM query may take before its whole process tree is killed"))
  (:tool "win_device_table" (nlk:contract 'table-query) 'win-device-table)
  (:tool "win_device_detail" (nlk:contract 'detail-query) 'win-device-detail)
  (:help :windows-devices
         "windows-devices: which Windows device is unhappy, what its problem code means and what driver is behind it"
         +manual+)
  (:command "windows-devices" 'run-command
            :description "Windows devices: what needs attention, or one device in full"))
