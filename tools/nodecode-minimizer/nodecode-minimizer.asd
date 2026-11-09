;;;; nodecode-minimizer.asd --- the simplest complete change, at every turn's start.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral: one :ROUND hook carrying the policy, one manual topic and
;;;; one /minimizer command that reads it back. The folder loader loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `minimizer' section and
;;;; installs the hook, the topic and the command. Every dependency is
;;;; already in the serving image.

(defsystem "nodecode-minimizer"
  :description "Keeps a coding agent on the smallest complete change: reuse first, no speculative layers, prove it"
  :author "nosliwhtes (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-minimizer/test"))))

(defsystem "nodecode-minimizer/test"
  :description "The cell's test slice: the guidance, the two modes, the cap, and the command that reads them back."
  :license "MIT"
  :version "0.1.0"
  :author "nosliwhtes (original), Nodecode (rewrite)"
  :depends-on ("nodecode-minimizer" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-minimizer-tests)))
