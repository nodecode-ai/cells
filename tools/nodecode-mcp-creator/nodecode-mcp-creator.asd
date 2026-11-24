;;;; nodecode-mcp-creator.asd --- an MCP server made into a portable skill.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the mcp-creator section, registers the nine
;;;; mcp_creator_* tools, /mcp-creator and (help :mcp-creator), and hooks the
;;;; :TOOL point so a call the model makes to an MCP server is one line in
;;;; this folder's own log.
;;;;
;;;; A rewrite of tonbistudio's tonbistudio/hermes-mcp-plugin-creator (see
;;;; NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-mcp-creator"
  :description "An MCP server's tools as a portable skill: introspect, mine, scaffold, validate, install, drift, review"
  :license "MIT"
  :author "tonbistudio (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "shasht")
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "mcp")
               (:file "build")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mcp-creator/test"))))

(defsystem "nodecode-mcp-creator/test"
  :description "mcp-creator tests. Registered into the shared nodecode.test registry; RUN-MCP-CREATOR-TESTS filters by the MCP-CREATOR-CELL- name prefix."
  :license "MIT"
  :author "tonbistudio (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mcp-creator" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mcp-creator-tests)))
