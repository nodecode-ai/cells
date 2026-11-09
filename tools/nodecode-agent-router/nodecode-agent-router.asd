;;;; nodecode-agent-router.asd --- the one agent that fits a task, and why.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes Agent plugin jev-agent-router (see NOTICE).
;;;; Optional ASDF cell, not part of the organism core: the folder loader loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/, and the
;;;; gateway calls START-CELL after recovery, which reads the `agent-router'
;;;; section, registers the agent_route tool, /agents and (help :agent-router).

(defsystem "nodecode-agent-router"
  :description "Agent router: chooses one agent for a task, explains the choice, abstains or falls back once"
  :license "MIT"
  :author "Pinutss (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "router")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agent-router/test"))))

(defsystem "nodecode-agent-router/test"
  :description "Agent router tests. Registered into the shared nodecode.test registry; RUN-AGENT-ROUTER-TESTS filters by the AGENT-ROUTER-CELL- name prefix."
  :license "MIT"
  :author "Pinutss (original), Nodecode (rewrite)"
  :depends-on ("nodecode-agent-router" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agent-router-tests)))
