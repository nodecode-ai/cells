;;;; nodecode-mflow.asd --- mFlow boards, tasks, Knowledge Base and coding agent, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `mflow' section and registers the mflow_* tools, /mflow and
;;;; (help :mflow).
;;;;
;;;; A Nodecode rewrite of vladyslavpatrick/mflow-hermes (see NOTICE).

(defsystem "nodecode-mflow"
  :description "mFlow boards, tasks, Knowledge Base and coding agent, through mFlow's own MCP server"
  :license "MIT"
  :version "0.1.0"
  :author "vladyslavpatrick (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mflow/test"))))

(defsystem "nodecode-mflow/test"
  :description "mFlow cell tests. Registered into the shared nodecode.test registry; RUN-MFLOW-TESTS filters by the MFLOW-CELL- name prefix."
  :license "MIT"
  :author "vladyslavpatrick (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mflow" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mflow-tests)))
