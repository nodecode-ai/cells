;;;; nodecode-muster.asd --- the board of subagent flights.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of jollyroger1480/muster. Optional ASDF cell, not part of the
;;;; organism core: the folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/, and the gateway calls START-CELL, which
;;;; reads the `muster' config section, observes the :FRAME point for the
;;;; fork progress frames a running call publishes, and starts one sweeping
;;;; thread. /muster, /muster-digest and the two tools read the same rows.

(defsystem "nodecode-muster"
  :description "The board of subagent flights: working, stuck, failing on repeat or gone, with stop and a message"
  :license "MIT"
  :author "jollyroger1480 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "state")
               (:file "store")
               (:file "board")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-muster/test"))))

(defsystem "nodecode-muster/test"
  :description "Muster tests. Registered into the shared nodecode.test registry; RUN-MUSTER-TESTS filters by the MUSTER-CELL- name prefix."
  :license "MIT"
  :author "jollyroger1480 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-muster" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-muster-tests)))
