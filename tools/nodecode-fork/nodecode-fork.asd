;;;; nodecode-fork.asd --- a turn forks itself: its independent tasks run at once,
;;;; each in a fork of the session that holds its whole context.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `fork' section, watches the :FRAME point for a
;;;; fork's turn ending, bounds a fork's turn (NLE:TURN-BUDGET), keeps forks
;;;; unnamed (NLE:NAME-SESSION) and puts the manual on (help :fork).
;;;;
;;;; A fork is a session in the event log like any other, anchored to the
;;;; turn that made it. Every dependency is already in the serving image.

(defsystem "nodecode-fork"
  :description "A turn forks itself: its independent tasks run at once, each in a fork of the session that holds its whole context"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fork/test"))))

(defsystem "nodecode-fork/test"
  :description "Fork tests. Registered into the shared nodecode.test registry; RUN-FORK-TESTS filters by the FORK-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-fork" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fork-tests)))
