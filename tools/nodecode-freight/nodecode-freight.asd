;;;; nodecode-freight.asd --- freight bills reconciled against trips and rates, locally.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of harrylabsj/hermes-plugin-freight-reconciliation (MIT).
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode's
;;;; src names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which reads the `freight' section, registers the five
;;;; freight tools and /freight, and puts the manual on (help :freight).
;;;; A case is one JSON file under the section's data_dir, so the arithmetic
;;;; survives a restart and nothing here dials anything.

(defsystem "nodecode-freight"
  :description "Freight bills reconciled against trip ledgers and contracted rates"
  :license "MIT"
  :version "0.1.0"
  :author "harrylabsj (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-freight/test"))))

(defsystem "nodecode-freight/test"
  :description "Freight reconciliation tests. Registered into the shared nodecode.test registry; RUN-FREIGHT-TESTS filters by the FREIGHT-CELL- name prefix."
  :license "MIT"
  :author "harrylabsj (original), Nodecode (rewrite)"
  :depends-on ("nodecode-freight" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-freight-tests)))
