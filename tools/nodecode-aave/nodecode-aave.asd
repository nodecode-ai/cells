;;;; nodecode-aave.asd --- Aave V3 and V4 through Aave's own MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `aave' section and registers the Aave tools and /aave that
;;;; cell.lisp declares. Ported from aave/skills (see NOTICE): the tool set
;;;; is served by Aave's server at run time, so the cell asks it rather than
;;;; pinning the wire, and carries the workflow its skills teach as a primer.

(defsystem "nodecode-aave"
  :description "Aave V3/V4 markets, positions, health factor and unsigned transactions through Aave's MCP server"
  :license "MIT"
  :version "0.1.0"
  :author "aave (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-aave/test"))))

(defsystem "nodecode-aave/test"
  :description "Aave cell tests. Registered into the shared nodecode.test registry; RUN-AAVE-TESTS filters by the AAVE-CELL- name prefix."
  :license "MIT"
  :author "aave (original), Nodecode (rewrite)"
  :depends-on ("nodecode-aave" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-aave-tests)))
