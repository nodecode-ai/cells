;;;; package.lisp --- the cell's one package, under the name the folder carries.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-windows-devices
  (:nicknames #:windows-devices)
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:windows-devices-error #:windows-devices-error-detail
           #:cim-refusal #:cim-refusal-code #:cim-refusal-detail
           #:win-device-table #:win-device-detail
           #:decode-table-query #:decode-detail-query
           #:devices #:drivers #:detail-of #:code-meaning #:broken-p
           #:run-powershell #:powershell-argv #:powershell-executable #:cim-list
           #:table-report #:detail-report #:run-command))

(in-package #:nodecode-windows-devices)
