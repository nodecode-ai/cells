;;;; package.lisp --- the NODECODE-LFG-ALARM package: the MCP wire and the cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed LFG-ALARM so an eval form reads
;;;; (lfg-alarm:call-remote-tool ...). NLK:DEFINE-PERIPHERAL gives this
;;;; package its condition, FAIL, *LFG-ALARM* and RUNNING-SETTINGS; SETTING
;;;; and START-CELL come from the NLE:DEFINE-CELL declaration in cell.lisp,
;;;; which loads last.

(defpackage #:nodecode-lfg-alarm
  (:use #:cl)
  (:nicknames #:lfg-alarm)
  (:export #:start-cell
           #:stop-cell
           #:setting
           #:connect
           #:remote-tools
           #:call-remote-tool
           #:lfg-alarm-error
           #:lfg-alarm-error-detail))

(in-package #:nodecode-lfg-alarm)

(nlk:define-peripheral lfg-alarm :not-running t)
