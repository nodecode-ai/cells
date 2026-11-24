;;;; nodecode-island.asd --- play an agent-inception voxel island over its MCP world.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of ethanwiegert/agent-inception's Hermes plugin: that
;;;; plugin was a manifest wiring the game's MCP server plus a playbook, so this
;;;; cell speaks the game's MCP endpoint itself and carries the playbook as
;;;; (help :island). No part of the original runs here.

(defsystem "nodecode-island"
  :description "Play an agent-inception voxel island over the game's MCP world"
  :license "MIT"
  :author "ethanwiegert (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-island/test"))))

(defsystem "nodecode-island/test"
  :description "Island tests. Registered into the shared nodecode.test registry; RUN-ISLAND-TESTS filters by the ISLAND-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-island" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-island-tests)))
