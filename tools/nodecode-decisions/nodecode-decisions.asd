;;;; nodecode-decisions.asd --- predicate decisions, as two tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `decisions' section, puts (help :decisions) on the manual and
;;;; registers decisions_submit and decisions_status.

(defsystem "nodecode-decisions"
  :description "Decisions: yes/no probability answers for a text, from an OpenAI-shaped Decisions endpoint"
  :license "MIT"
  :version "0.1.0"
  :author "eve-ai-dev (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-decisions/test"))))

(defsystem "nodecode-decisions/test"
  :description "The decisions cell's tests. Registered into the shared nodecode.test registry; RUN-DECISIONS-TESTS filters by the DECISIONS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-decisions" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-decisions-tests)))
