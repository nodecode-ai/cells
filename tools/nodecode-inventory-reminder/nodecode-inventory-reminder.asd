;;;; nodecode-inventory-reminder.asd --- do I already have X? answered from a digest.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. A Nodecode rewrite of ether-btc/inventory-reminder, whose two
;;;; tools answer "do I already have X?" from a cached, read-only inventory of
;;;; the host. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `inventory-reminder' section, registers /inventory-reminder and
;;;; the two tools, and puts (help :inventory-reminder) on the manual.

(defsystem "nodecode-inventory-reminder"
  :description "Do I already have X? Search a cached host inventory: repos, skills, MCP, CLIs, models"
  :license "MIT"
  :version "0.1.0"
  :author "ether-btc (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-inventory-reminder/test"))))

(defsystem "nodecode-inventory-reminder/test"
  :description "Inventory reminder tests. Registered into the shared nodecode.test registry; RUN-INVENTORY-REMINDER-TESTS filters by the INVENTORY-REMINDER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-inventory-reminder" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-inventory-reminder-tests)))
