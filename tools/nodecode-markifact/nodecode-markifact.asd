;;;; nodecode-markifact.asd --- Markifact's hosted MCP server, as a Nodecode tool cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: the folder loader loads it at boot,
;;;; and the gateway calls START-CELL, which reads the `markifact' section and
;;;; puts the two tools, the /markifact command and the (help :markifact) topic
;;;; on the organism that cell.lisp lists.
;;;;
;;;; Ported from the Hermes agent plugin of markifact/markifact-mcp (a portable
;;;; Agent Plugins v1 package: plugin.json, mcp.json and ten skills). The plugin
;;;; ships no code: it points an MCP client at one hosted server. This cell is
;;;; that client, in Lisp, so the same tools answer without an MCP runtime.

(defsystem "nodecode-markifact"
  :description "Markifact: run Google, Meta, TikTok and other ad operations through Markifact's hosted MCP server"
  :license "MIT"
  :author "markifact (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-markifact/test"))))

(defsystem "nodecode-markifact/test"
  :description "Markifact cell tests: every exchange stubbed, no sign-in, no network."
  :license "MIT"
  :author "markifact (original), Nodecode (rewrite)"
  :depends-on ("nodecode-markifact" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-markifact-tests)))
