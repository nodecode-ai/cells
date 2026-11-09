;;;; nodecode-process-table.asd --- what is running, and who started it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of Yicheng-Mark/hermes-windows-plugins' windows-process
;;;; plugin. Optional ASDF cell, NOT part of the organism core: nothing in src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `process-table' section and registers the
;;;; process_table tool, /process-table and (help :process-table).
;;;;
;;;; The .asd sits INSIDE its folder: a cell is a directory carrying its
;;;; .asd, and it is installed by putting -- or symlinking -- that directory
;;;; under ~/.nodecode/cells/. Presence is enabled.
;;;;
;;;; Every dependency is already in the serving image: the readers are
;;;; /proc read with the standard library, `ps' or `powershell' through
;;;; NLK:RUN-BOUNDED, and nothing else.

(defsystem "nodecode-process-table"
  :description "What is running, and who started it"
  :license "MIT"
  :version "0.1.0"
  :author "Yicheng-Mark (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "read")
               (:file "table")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-process-table/test"))))

(defsystem "nodecode-process-table/test"
  :description "Process-table tests. Registered into the shared nodecode.test registry; RUN-PROCESS-TABLE-TESTS filters by the PROCESS-TABLE- name prefix."
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-process-table" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-process-table-tests)))
