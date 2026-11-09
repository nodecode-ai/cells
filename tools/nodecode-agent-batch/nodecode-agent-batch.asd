;;;; nodecode-agent-batch.asd --- a task list phased into parallel groups, dispatched to GitHub Actions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `agent-batch' section, registers the five agent_batch_* tools and
;;;; /agent-batch, and puts the workflow on the manual as (help :agent-batch).
;;;;
;;;; A Nodecode rewrite of raaaas/hermes-agent-batch (see NOTICE). The plan it
;;;; reads and writes is the original's own JSON, in (nlk:home "agent-batch/").

(defsystem "nodecode-agent-batch"
  :description "Agent Batch: a task list phased into parallel groups and dispatched to GitHub Actions agents, with the runs and pull requests they open"
  :author "raaaas (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "plan")
               (:file "github")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agent-batch/test"))))

(defsystem "nodecode-agent-batch/test"
  :description "Agent Batch tests. Registered into the shared nodecode.test registry; RUN-AGENT-BATCH-TESTS filters by the AGENT-BATCH-CELL- name prefix."
  :license "MIT"
  :author "raaaas (original), Nodecode (rewrite)"
  :depends-on ("nodecode-agent-batch" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agent-batch-tests)))
