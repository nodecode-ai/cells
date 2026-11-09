;;;; nodecode-spectator.asd --- a live, masked page of one session for guests.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A flow cell, not part of the organism core: nothing in src/ names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `spectator' section, registers the /spectator command, the
;;;; spectator_share tool and (help :spectator), and puts the guest page on
;;;; the gateway's own port under /spectator/. Every dependency is already in
;;;; the serving image.

(defsystem "nodecode-spectator"
  :description "Share a live, masked page of one session with invited guests, who may suggest prompts"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-spectator/test"))))

(defsystem "nodecode-spectator/test"
  :description "Spectator tests. Registered into the shared nodecode.test registry; RUN-SPECTATOR-TESTS filters by the SPECTATOR prefix."
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-spectator" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-spectator-tests)))
