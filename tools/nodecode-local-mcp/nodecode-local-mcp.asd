;;;; nodecode-local-mcp.asd --- LMCP: the Mac apps an agent may act on, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `local-mcp' section, starts the LMCP app's MCP server over
;;;; stdio, reads its tool catalog, and registers local_mcp_tools,
;;;; local_mcp_call and /local-mcp, and puts (help :local-mcp) on the manual.
;;;;
;;;; A Nodecode rewrite of colibird-ai/local-mcp-hermes-plugin (see NOTICE).

(defsystem "nodecode-local-mcp"
  :description "LMCP: Mail, Calendar, Teams, Slack, files, a browser and the terminal as tools, through the LMCP app"
  :license "MIT"
  :version "0.1.0"
  :author "colibird-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-local-mcp/test"))))

(defsystem "nodecode-local-mcp/test"
  :description "LMCP tests. Registered into the shared nodecode.test registry; RUN-LOCAL-MCP-TESTS filters by the LOCAL-MCP-CELL- prefix."
  :license "MIT"
  :version "0.1.0"
  :author "colibird-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-local-mcp" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-local-mcp-tests)))
