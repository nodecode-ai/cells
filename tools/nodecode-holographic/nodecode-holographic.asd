;;;; nodecode-holographic.asd --- facts kept on this machine, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `holographic' section, registers fact_store and
;;;; fact_feedback, puts /holographic and (help :holographic) up, and hooks
;;;; the :round point so each turn's own message recalls the facts it holds
;;;; before the request goes out. Nothing here needs a key or a network.

(defsystem "nodecode-holographic"
  :description "Holographic memory: facts kept on this machine, recalled into each turn"
  :author "itpartypattaya (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-holographic/test"))))

(defsystem "nodecode-holographic/test"
  :description "Holographic memory tests. Registered into the shared nodecode.test registry; RUN-HOLOGRAPHIC-TESTS filters by the HOLOGRAPHIC-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-holographic" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-holographic-tests)))
