;;;; nodecode-penguin.asd --- time across zones, and the windows a whole team shares.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery. It is
;;;; a rewrite of a Hermes Agent plugin -- see NOTICE.

(defsystem "nodecode-penguin"
  :description "Deterministic IANA time conversion and the windows where a whole team's meeting fits"
  :license "MIT"
  :author "Crystal32378 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-penguin/test"))))

(defsystem "nodecode-penguin/test"
  :description "Tests for the penguin cell; RUN-PENGUIN-TESTS filters the shared registry by the PENGUIN- prefix."
  :license "MIT"
  :author "Crystal32378 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-penguin" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-penguin-tests)))
