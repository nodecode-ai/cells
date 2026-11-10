;;;; nodecode-doctrine.asd --- the Axiom Doctrine as a standing section and a report.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `doctrine' section, puts the doctrine on every request as a
;;;; standing harness section, and registers /doctrine and the manual topic.
;;;;
;;;; The original is a Hermes Agent plugin: it appended nine articles of agent
;;;; conduct to each model turn and answered /doctrine with a table of which
;;;; article an enforcer plugin covers. This cell carries the same prose -- the
;;;; original's axiom_agent/DOCTRINE.md, shipped unchanged beside it as
;;;; doctrine.md -- and the same table, with the enforcers read as cells of
;;;; this harness. No Python, no network, no subprocess, nothing written.

(defsystem "nodecode-doctrine"
  :description "Axiom Doctrine: nine conduct rules in every request, and which enforcer cells are installed"
  :license "MIT"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "doctrine")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-doctrine/test"))))

(defsystem "nodecode-doctrine/test"
  :description "Doctrine tests. Registered into the shared nodecode.test registry; RUN-DOCTRINE-TESTS filters by the DOCTRINE-CELL- name prefix."
  :license "MIT"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode-doctrine" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-doctrine-tests)))
