;;;; nodecode-agentplaybooks.asd --- AgentPlaybooks' Hermes tools skill, as a
;;;; Nodecode tool cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: the folder loader loads it at boot
;;;; and the gateway calls START-CELL, which reads the `agentplaybooks' section
;;;; and puts the four tools, the /apb command and the (help :agentplaybooks)
;;;; topic on the organism that cell.lisp lists.
;;;;
;;;; Ported from the Hermes agent plugin of matebenyovszky/agentplaybooks (one
;;;; prose skill driving the `apb' CLI). It shells out to the CLI the operator
;;;; installs themselves, through nlk:run-bounded: no Node, no Python, no
;;;; bundled copy of the CLI.

(defsystem "nodecode-agentplaybooks"
  :description "AgentPlaybooks: playbook skills, MCP servers and federated OpenAPI tools in the harness, through the apb CLI"
  :license "MIT"
  :author "matebenyovszky (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cli")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agentplaybooks/test"))))

(defsystem "nodecode-agentplaybooks/test"
  :description "AgentPlaybooks cell tests: every apb call stubbed, no process started, no project read."
  :license "MIT"
  :author "matebenyovszky (original), Nodecode (rewrite)"
  :depends-on ("nodecode-agentplaybooks" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agentplaybooks-tests)))
