;;;; nodecode-muse-harness.asd --- one Muse Code turn over MSP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `muse' section, registers the muse_turn tool
;;;; and /muse, and puts the manual on (help :muse).
;;;;
;;;; A rewrite of SnoobieJunes/hermes-muse-harness. The whole client is
;;;; cell.lisp: NLK:SPAWN-PROGRAM starts the user's own `muse' executable and
;;;; the cell speaks Muse Session Protocol on its stdin and stdout. Every
;;;; dependency is already in the serving image.

(defsystem "nodecode-muse-harness"
  :description "One Muse Code turn over MSP, from the local muse executable"
  :license "MIT"
  :author "SnoobieJunes (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-muse-harness/test"))))

(defsystem "nodecode-muse-harness/test"
  :description "Muse harness tests. Registered into the shared nodecode.test registry; RUN-MUSE-HARNESS-TESTS filters by the MUSE-HARNESS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-muse-harness" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-muse-harness-tests)))
