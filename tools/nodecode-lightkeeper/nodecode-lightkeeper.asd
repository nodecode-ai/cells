;;;; nodecode-lightkeeper.asd --- a knowledge vault kept outside the repo, read at the turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `lightkeeper' section, registers three tools and the /lightkeeper
;;;; command, and hangs two hooks: the vault's primer on a session's first
;;;; round, and the owning note's pointer on the result of a write.
;;;;
;;;; A rewrite of Light5566/claude-lightkeeper's hook scripts (see NOTICE).

(defsystem "nodecode-lightkeeper"
  :description "A map of how your codebase works, kept in a vault of notes outside the repo, and the pointers to it a turn reads"
  :license "MIT"
  :author "Light5566 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-lightkeeper/test"))))

(defsystem "nodecode-lightkeeper/test"
  :description "Lightkeeper tests. Registered into the shared nodecode.test registry; RUN-LIGHTKEEPER-TESTS filters by the LIGHTKEEPER-CELL- prefix."
  :license "MIT"
  :depends-on ("nodecode-lightkeeper" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-lightkeeper-tests)))
