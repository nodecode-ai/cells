;;;; nodecode-memory-shield.asd --- the memory policy, in front of the one tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of churnast/hermes-memory-shield (MIT), credited in
;;;; NOTICE. Optional cell, not part of the organism core: the folder loader
;;;; loads it at boot when its directory sits under the cells folder, and the
;;;; gateway calls START-CELL, which hooks the :TOOL point.

(defsystem "nodecode-memory-shield"
  :description "Keeps the agent from rewriting or wiping what it remembers about its operator"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-memory-shield/test"))))

(defsystem "nodecode-memory-shield/test"
  :description "memory-shield tests: the policy levels, the reads of a call, and the refusal path."
  :license "MIT"
  :depends-on ("nodecode-memory-shield" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-memory-shield-tests)))
