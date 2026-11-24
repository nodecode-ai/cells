;;;; nodecode-pocket.asd --- the operator's phone, through Notify by belweave.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `pocket' section and registers three tools, one command
;;;; and the manual on (help :pocket). Its tests are the FOLDER/TEST system.

(defsystem "nodecode-pocket"
  :description "Updates and questions on the operator's phone, through Notify"
  :license "MIT"
  :author "Belweave (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pocket/test"))))

(defsystem "nodecode-pocket/test"
  :description "Pocket tests: the slice POCKET-CELL-*, run by RUN-POCKET-TESTS."
  :license "MIT"
  :author "Belweave (original), Nodecode (rewrite)"
  :depends-on ("nodecode-pocket" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pocket-tests)))
