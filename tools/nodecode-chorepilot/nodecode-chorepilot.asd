;;;; nodecode-chorepilot.asd --- ChorePilot's house and chore board as tools.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `chorepilot' section, registers the seven tools and
;;;; /chorepilot, and puts the house's rules on (help :chorepilot).
;;;;
;;;; Every dependency is already in the serving image.

(defsystem "nodecode-chorepilot"
  :description "ChorePilot's house and chore board through its own MCP server"
  :license "Apache-2.0"
  :author "WestWaters (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-chorepilot/test"))))

(defsystem "nodecode-chorepilot/test"
  :description "ChorePilot cell tests. Registered into the shared nodecode.test registry; RUN-CHOREPILOT-TESTS filters by the CHOREPILOT-CELL- name prefix."
  :license "Apache-2.0"
  :author "WestWaters (original), Nodecode (rewrite)"
  :depends-on ("nodecode-chorepilot" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-chorepilot-tests)))
