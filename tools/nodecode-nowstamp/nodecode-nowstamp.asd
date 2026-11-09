;;;; nodecode-nowstamp.asd --- the real clock and the real gap, every turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery. It is
;;;; a rewrite of a Hermes Agent plugin -- see NOTICE.

(defsystem "nodecode-nowstamp"
  :description "The real clock and the real gap since the conversation last moved, in every turn's standing context"
  :license "MIT"
  :author "zakiyys (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-nowstamp/test"))))

(defsystem "nodecode-nowstamp/test"
  :description "Tests for the nowstamp cell; RUN-NOWSTAMP-TESTS filters the shared registry by the NOWSTAMP- prefix."
  :license "MIT"
  :author "zakiyys (original), Nodecode (rewrite)"
  :depends-on ("nodecode-nowstamp" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-nowstamp-tests)))
