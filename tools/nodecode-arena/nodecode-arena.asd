;;;; nodecode-arena.asd --- a knockout tournament of sub-agents over one task.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `arena' section, puts (help :arena) on the manual, registers
;;;; the arena tool and /arena.
;;;;
;;;; A rewrite of J182Razor/hermes-arena-skill (see NOTICE). Every dependency
;;;; rides with nodecode; skill/ holds the original's prose, rubric and
;;;; strategy cards, unchanged.

(defsystem "nodecode-arena"
  :description "A knockout tournament of sub-agents over one task: attack, defend and judge until one answer wins"
  :license "MIT"
  :author "J182Razor (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "bracket")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-arena/test"))))

(defsystem "nodecode-arena/test"
  :description "Arena tests. Registered into the shared nodecode.test registry; RUN-ARENA-TESTS filters by the ARENA-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-arena" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-arena-tests)))
