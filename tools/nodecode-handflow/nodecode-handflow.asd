;;;; nodecode-handflow.asd --- a visible step plan the agent marks as it works.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `handflow' section, registers the handflow tool, /handflow
;;;; and (help :handflow), and carries the active plan in every request.
;;;;
;;;; State is plain JSON under the Nodecode home, written atomically, read by
;;;; the tool and the command alike. Nothing here calls a model or a network.

(defsystem "nodecode-handflow"
  :description "A visible step plan the agent writes before it works and marks as it goes"
  :author "DECRUX9812 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-handflow/test"))))

(defsystem "nodecode-handflow/test"
  :description "Handflow tests. Registered into the shared nodecode.test registry; RUN-HANDFLOW-TESTS filters by the HANDFLOW-CELL- name prefix."
  :author "DECRUX9812 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-handflow" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-handflow-tests)))
