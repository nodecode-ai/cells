;;;; nodecode-plan-mode.asd --- enforced plan mode: plan first, edit after approval.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of 100yenadmin/hermes-plan-mode: the same enforced
;;;; plan mode, on Nodecode's one seam instead of Hermes's twelve hooks. See
;;;; README.md beside this file and NOTICE for what came from where.

(defsystem "nodecode-plan-mode"
  :description "Enforced plan mode: the agent plans, you approve, then it edits"
  :license "MIT"
  :version "0.1.0"
  :author "100yenadmin (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-plan-mode/test"))))

(defsystem "nodecode-plan-mode/test"
  :description "Plan mode tests. Registered into the shared nodecode.test registry; RUN-PLAN-MODE-TESTS filters by the PLAN-MODE- name prefix."
  :license "MIT"
  :depends-on ("nodecode-plan-mode" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-plan-mode-tests)))
