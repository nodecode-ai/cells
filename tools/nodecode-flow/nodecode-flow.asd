;;;; nodecode-flow.asd --- recorded agent flows, their handoffs and tools.
;;;;
;;;; SPDX-License-Identifier: MIT

(defsystem "nodecode-flow"
  :description "Recorded agent flows: lineage, handoffs and tool use in a tree, compared with trusted paths"
  :license "MIT"
  :author "rewanthtammana (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "model")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-flow/test"))))

(defsystem "nodecode-flow/test"
  :description "Flow tests. Registered into the shared nodecode.test registry; RUN-FLOW-TESTS filters by the FLOW-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-flow" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-flow-tests)))
