;;;; nodecode-crashtest.asd --- one armed turn of injected tool failures,
;;;; and a star rating of what the model did about them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in the
;;;; release names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which hooks the :ROUND and :TOOL points, advises the
;;;; assistant-message record the scorecard rides, and registers /crashtest.

(defsystem "nodecode-crashtest"
  :description "Crash test the agent: one armed turn of failing tools, then a 1-5 star rating of how it coped"
  :license "MIT"
  :author "salch-cred (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-crashtest/test"))))

(defsystem "nodecode-crashtest/test"
  :description "Crash test tests. Registered into the shared nodecode.test registry; RUN-CRASHTEST-TESTS filters by the CRASHTEST-CELL- name prefix."
  :license "MIT"
  :author "salch-cred (original), Nodecode (rewrite)"
  :depends-on ("nodecode-crashtest" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-crashtest-tests)))
