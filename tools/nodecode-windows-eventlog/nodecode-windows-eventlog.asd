;;;; nodecode-windows-eventlog.asd --- Windows crash forensics from the Event Log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `windows-eventlog' section, registers the three tools and puts
;;;; the manual on (help :windows-eventlog).
;;;;
;;;; Rewritten from Yicheng-Mark's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:run-bounded runs
;;;; wevtutil, nlk:define-contract types each tool's arguments, and xml.lisp
;;;; reads the XML wevtutil renders.

(defsystem "nodecode-windows-eventlog"
  :description "Windows event log: why a process died - crash, hang, WER and .NET records, restarts and bugchecks, and a filtered query of any channel"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "xml")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-windows-eventlog/test"))))

(defsystem "nodecode-windows-eventlog/test"
  :description "Windows event log tests. Registered into the shared nodecode.test registry; RUN-WINDOWS-EVENTLOG-TESTS filters by the WINDOWS-EVENTLOG-CELL- name prefix."
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-windows-eventlog" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-windows-eventlog-tests)))
