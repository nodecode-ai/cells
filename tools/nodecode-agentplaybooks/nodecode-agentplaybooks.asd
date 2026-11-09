;;;; nodecode-agentplaybooks.asd --- AgentPlaybooks memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `agentplaybooks' section and registers the six apb_memory_*
;;;; tools, /agentplaybooks, the manual topic, and the round that carries
;;;; what the turn's own words recall.
;;;;
;;;; A Nodecode rewrite of matebenyovszky/agentplaybooks' Hermes memory
;;;; provider (see NOTICE).

(defsystem "nodecode-agentplaybooks"
  :description "AgentPlaybooks memory: recall, explicit writes, archive and history in a private playbook"
  :license "MIT"
  :version "0.1.0"
  :author "matebenyovszky (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agentplaybooks/test"))))

(defsystem "nodecode-agentplaybooks/test"
  :description "AgentPlaybooks tests. Registered into the shared nodecode.test registry; RUN-AGENTPLAYBOOKS-TESTS filters by the AGENTPLAYBOOKS-CELL- name prefix."
  :license "MIT"
  :author "matebenyovszky (original), Nodecode (rewrite)"
  :depends-on ("nodecode-agentplaybooks" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agentplaybooks-tests)))
