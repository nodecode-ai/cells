;;;; nodecode-windows-tasks.asd --- what Windows runs on its own, and whether it worked.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which reads
;;;; the `windows-tasks' section, registers the two scheduled-task tools and the
;;;; /windows-tasks command, and puts the manual on (help :windows-tasks).
;;;;
;;;; The one thing outside this folder is Windows' own powershell.exe, which the
;;;; tools run to ask the Task Scheduler's cmdlets; it is on PATH on every
;;;; Windows. See NOTICE: this folder is a rewrite of
;;;; Yicheng-Mark/hermes-windows-plugins' windows-tasks plugin.

(defsystem "nodecode-windows-tasks"
  :description "Windows Tasks: what Task Scheduler runs on its own, and whether it worked"
  :license "MIT"
  :version "0.1.0"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "xml")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-windows-tasks/test"))))

(defsystem "nodecode-windows-tasks/test"
  :description "Windows Tasks tests. Registered into the shared nodecode.test registry; RUN-WINDOWS-TASKS-TESTS filters by the WINDOWS-TASKS-CELL- name prefix."
  :license "MIT"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :depends-on ("nodecode-windows-tasks" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-windows-tasks-tests)))
