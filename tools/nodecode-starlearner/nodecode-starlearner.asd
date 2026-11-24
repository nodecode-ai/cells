;;;; nodecode-starlearner.asd --- GitHub stars as categorized skill notes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `starlearner' section, registers the three tools and puts the
;;;; primer on (help :starlearner).
;;;;
;;;; A Nodecode rewrite of BigBossRabbit's Hermes Agent plugin of the same
;;;; name; see NOTICE. Everything rides with nodecode: nlk:http speaks to
;;;; api.github.com, nlk:json reads the answers, and the ledger, the keyword
;;;; rules and the assessment are this folder's own Common Lisp.

(defsystem "nodecode-starlearner"
  :description "GitHub stars: fetch what you starred, categorize and assess it, and write a skill note for each new repository"
  :author "BigBossRabbit (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-starlearner/test"))))

(defsystem "nodecode-starlearner/test"
  :description "StarLearner tests. Registered into the shared nodecode.test registry; RUN-STARLEARNER-TESTS filters by the STARLEARNER-CELL- name prefix."
  :author "BigBossRabbit (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-starlearner" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-starlearner-tests)))
