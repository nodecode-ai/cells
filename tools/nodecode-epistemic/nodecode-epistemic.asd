;;;; nodecode-epistemic.asd --- a case record for an inquiry that could change its mind.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `epistemic' section, loads the cases file, and registers
;;;; the two tools, /case, (help :epistemic) and the two hooks. Every
;;;; dependency is already in the serving image.

(defsystem "nodecode-epistemic"
  :description "Records a prediction before a check, compares the result and keeps the case on this machine"
  :author "quangphucqp (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-epistemic/test"))))

(defsystem "nodecode-epistemic/test"
  :description "Epistemic tests. Registered into the shared nodecode.test registry; RUN-EPISTEMIC-TESTS filters by the EPISTEMIC-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-epistemic" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-epistemic-tests)))
