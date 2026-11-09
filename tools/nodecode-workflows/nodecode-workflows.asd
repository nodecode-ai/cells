;;;; nodecode-workflows.asd --- agent-authored workflow graphs, run in the background.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `workflows' section, loads the runs this home
;;;; holds, starts one worker thread, registers the `workflow' tool, the
;;;; /workflow command and the manual topic (help :workflow).
;;;;
;;;; A rewrite of jacobhausler/hermes-workflows, MIT. Dependencies are the
;;;; ones the serving image already carries.

(defsystem "nodecode-workflows"
  :description "Agent-authored workflow graphs run in the background: after-edges, fan-outs, gates, resume"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "graph")
               (:file "run")
               (:file "runner")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-workflows/test"))))

(defsystem "nodecode-workflows/test"
  :description "Workflow tests. Registered into the shared nodecode.test registry; RUN-WORKFLOWS-TESTS filters by the WORKFLOW-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-workflows" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "graph-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-workflows-tests)))
