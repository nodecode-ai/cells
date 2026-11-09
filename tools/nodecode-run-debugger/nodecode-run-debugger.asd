;;;; nodecode-run-debugger.asd --- a local, offline debugger for one agent run.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `run_debugger' section, registers the five tools the original
;;;; plugin gave its harness, puts /rundebug and the manual topic on, and
;;;; prunes the runs past their retention.
;;;;
;;;; See NOTICE: this folder is a rewrite of Sahil-SS9/agent-run-debugger.

(defsystem "nodecode-run-debugger"
  :description "Agent Run Debugger: an imported trace's timeline, diagnosis and inert replay, offline"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "trace")
               (:file "analyse")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-run-debugger/test"))))

(defsystem "nodecode-run-debugger/test"
  :description "run-debugger tests. Registered into the shared nodecode.test registry; RUN-RUN-DEBUGGER-TESTS filters by the RUN-DEBUGGER-CELL- name prefix."
  :license "Apache-2.0"
  :version "0.1.0"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-run-debugger" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-run-debugger-tests)))
