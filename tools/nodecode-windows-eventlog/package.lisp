;;;; package.lisp --- the WINDOWS-EVENTLOG package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; WINDOWS-EVENTLOG is a nickname, so an eval form
;;;; reads (windows-eventlog:crash-events ...). SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; the files
;;;; between read the section through it. FAIL and WINDOWS-EVENTLOG-ERROR
;;;; come from NLK:DEFINE-PERIPHERAL, and are what a tool says when the log
;;;; could not be read at all.
;;;;
;;;; The three verbs are the original plugin's three tools, exported so a
;;;; person can call any one from eval, as the model does through the tool of
;;;; the same name; each takes the record its contract declares.

(defpackage #:nodecode-windows-eventlog
  (:use #:cl)
  (:nicknames #:windows-eventlog)
  (:export #:windows-eventlog-error
           #:windows-eventlog-error-detail
           #:crash-events #:shutdown-events #:eventlog-query
           #:parse-xml-elements #:event-record #:record-provider #:record-event-id
           #:record-time-seconds #:record-iso-time #:record-level))

(in-package #:nodecode-windows-eventlog)

(nlk:define-peripheral windows-eventlog :not-running t)
