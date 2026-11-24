;;;; nodecode-hindsight.asd --- Hindsight long-term memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `hindsight' section, registers hindsight_retain,
;;;; hindsight_recall and hindsight_reflect, puts /hindsight and (help
;;;; :hindsight) up, and hooks the :round point so a turn's input is recalled
;;;; into its own request. Every dependency is already in the serving image.

(defsystem "nodecode-hindsight"
  :description "Hindsight long-term memory: recall, retain and reflect over a memory bank, cloud or local"
  :author "vectorize-io (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell")
               (:file "status"))
  :in-order-to ((test-op (test-op "nodecode-hindsight/test"))))

(defsystem "nodecode-hindsight/test"
  :description "Hindsight tests. Registered into the shared nodecode.test registry; RUN-HINDSIGHT-TESTS filters by the HINDSIGHT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-hindsight" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hindsight-tests)))
