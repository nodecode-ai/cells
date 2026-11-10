;;;; nodecode-parrotnotes.asd --- ParrotNotes' hosted notes server, and its workflow.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ascetx/parrot-notes-hermes-plugin: the original has no
;;;; code, only a manifest pointing an agent at ParrotNotes' hosted MCP
;;;; server and a skill that makes the tools usable. Nodecode already speaks
;;;; the manifest's wire (the mcp cell: Streamable HTTP, OAuth 2.0 with
;;;; dynamic client registration), so this cell carries the other half --
;;;; the workflow as a manual on (help :parrotnotes) -- and the one write
;;;; the manifest made, the mcp.servers entry, as /parrotnotes connect.

(defsystem "nodecode-parrotnotes"
  :description "ParrotNotes: your in-person meeting notes, and how to work them"
  :license "MIT"
  :version "0.1.0"
  :author "ascetx (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-parrotnotes/test"))))

(defsystem "nodecode-parrotnotes/test"
  :description "ParrotNotes cell tests. Registered into the shared nodecode.test registry; RUN-PARROTNOTES-TESTS filters by the PARROTNOTES-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-parrotnotes" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-parrotnotes-tests)))
