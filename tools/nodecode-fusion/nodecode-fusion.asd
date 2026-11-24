;;;; nodecode-fusion.asd --- what this machine's harnesses can call, tiered into rosters.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery. It is
;;;; a rewrite of a Hermes Agent plugin -- see NOTICE.

(defsystem "nodecode-fusion"
  :description "The models this machine's agent harnesses can call, tiered into Frontier / Middle Manager / Frontier rosters, with a plan-packet validator"
  :license "MIT"
  :author "BoredSexyJordan (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fusion/test"))))

(defsystem "nodecode-fusion/test"
  :description "Fusion tests. Registered into the shared nodecode.test registry; RUN-FUSION-TESTS filters by the FUSION- name prefix."
  :license "MIT"
  :author "BoredSexyJordan (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fusion" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fusion-tests)))
