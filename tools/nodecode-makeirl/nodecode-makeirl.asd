;;;; nodecode-makeirl.asd --- MakeIRL's KiCad release gate, through its MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `makeirl' section, registers the two tools and /makeirl,
;;;; and puts the release-gate workflow on (help :makeirl).
;;;;
;;;; Every dependency is already in the serving image.

(defsystem "nodecode-makeirl"
  :description "MakeIRL: ERC, DRC, BOM and export checks on a KiCad 9 board before manufacturing"
  :license "MIT"
  :author "MelMayssonOwen (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-makeirl/test"))))

(defsystem "nodecode-makeirl/test"
  :description "MakeIRL cell tests. Registered into the shared nodecode.test registry; RUN-MAKEIRL-TESTS filters by the MAKEIRL-CELL- name prefix."
  :license "MIT"
  :author "MelMayssonOwen (original), Nodecode (rewrite)"
  :depends-on ("nodecode-makeirl" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-makeirl-tests)))
