;;;; nodecode-mcp-serve.asd --- this Nodecode as an MCP server, for other agents.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional cell, NOT part of the organism core: nothing in src names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the mcp_serve section, puts one MCP route on the gateway's own
;;;; port, registers /mcp-serve and the (help :mcp-serve) topic, and takes
;;;; the route back on stop.
;;;;
;;;; The one dependency is already in the serving image. The .asd sits
;;;; INSIDE its folder: presence is enabled.

(defsystem "nodecode-mcp-serve"
  :description "Run this Nodecode as an MCP server other agents can read and send through"
  :license "MIT"
  :version "0.1.0"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mcp-serve/test"))))

(defsystem "nodecode-mcp-serve/test"
  :description "MCP-serve cell tests. Registered into the shared nodecode.test registry; RUN-MCP-SERVE-TESTS filters by the MCP-SERVE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-mcp-serve" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mcp-serve-tests)))
