;;;; nodecode-frihet.asd --- Frihet's MCP as a cell: the checks, and one call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `frihet' section, registers the one tool the original plugin
;;;; stood on (a call to a Frihet MCP operation), puts /frihet and the manual
;;;; topic on, and hooks :tool so an irreversible operation waits for a person.
;;;;
;;;; The original is a Hermes Agent plugin: a thin layer over the Frihet MCP
;;;; that added no tool of its own -- a skill, a /frihet command and a
;;;; pre_tool_call gate. This cell speaks the same MCP itself (JSON-RPC over
;;;; HTTPS: initialize, tools/list, tools/call), so it needs no host MCP
;;;; client, no Python and no dependency of its own.

(defsystem "nodecode-frihet"
  :description "Frihet: invoices, expenses and tax filings through Frihet's MCP, with a gate over irreversible operations"
  :license "MIT"
  :version "0.1.0"
  :author "Frihet-io (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "frihet")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-frihet/test"))))

(defsystem "nodecode-frihet/test"
  :description "Frihet tests. Registered into the shared nodecode.test registry; RUN-FRIHET-TESTS filters by the FRIHET-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Frihet-io (original), Nodecode (rewrite)"
  :depends-on ("nodecode-frihet" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-frihet-tests)))
