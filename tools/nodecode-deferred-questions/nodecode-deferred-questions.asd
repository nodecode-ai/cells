;;;; nodecode-deferred-questions.asd --- the agent asks, and moves on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of web3blind/hermes-deferred-questions (MIT).
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode's
;;;; src names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which reads the `deferred_questions' section, registers
;;;; the deferred_questions tool, /dq, puts the manual on (help :deferred-questions)
;;;; and hooks the :ROUND point so the answers that arrived between turns reach
;;;; the session at its next request. The ledger is one JSON file under the
;;;; home. Every dependency is already in the serving image.

(defsystem "nodecode-deferred-questions"
  :description "Ask a question and move on: the answer comes back at the next turn"
  :license "MIT"
  :version "0.1.0"
  :author "web3blind (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-deferred-questions/test"))))

(defsystem "nodecode-deferred-questions/test"
  :description "Deferred-question tests. Registered into the shared nodecode.test registry; RUN-DEFERRED-QUESTIONS-TESTS filters by the DEFERRED-QUESTIONS-CELL- name prefix."
  :license "MIT"
  :author "web3blind (original), Nodecode (rewrite)"
  :depends-on ("nodecode-deferred-questions" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-deferred-questions-tests)))
