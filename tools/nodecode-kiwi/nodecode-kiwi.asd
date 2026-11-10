;;;; nodecode-kiwi.asd --- the Kiwi buyer runtime as a source-and-negotiate cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `kiwi' section, registers the buyer runtime's tools, puts /kiwi
;;;; and the manual topic on, and hands the model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: an Agent Plugins v1 manifest that
;;;; registers one MCP server (`npx -y @harrylabsj/kiwi@0.12.24 mcp serve')
;;;; and ships a sourcing skill beside it. This cell starts the same pinned
;;;; runtime itself, over JSON-RPC on its stdio, so nothing has to be added to
;;;; an MCP server list, and the skill's workflow is the cell's primer.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-kiwi"
  :description "Source suppliers and negotiate purchases on the Kiwi network: search, RFQs, quotes, agreements"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "harrylabsj (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kiwi/test"))))

(defsystem "nodecode-kiwi/test"
  :description "Kiwi tests. Registered into the shared nodecode.test registry; RUN-KIWI-TESTS filters by the KIWI-CELL- name prefix."
  :license "Apache-2.0"
  :version "0.1.0"
  :author "harrylabsj (original), Nodecode (rewrite)"
  :depends-on ("nodecode-kiwi" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kiwi-tests)))
