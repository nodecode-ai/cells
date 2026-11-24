;;;; nodecode-approvals.asd --- a shell command judged APPROVE, DENY or ESCALATE.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `approvals' section, puts (help :approvals) on the manual and
;;;; registers /approvals.
;;;;
;;;; A rewrite of anpicasso's hermes-jev-approvals (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-approvals"
  :description "Approvals: a shell command judged APPROVE, DENY or ESCALATE by TypeSafe's Jev"
  :author "anpicasso (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "policy")
               (:file "judge")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-approvals/test"))))

(defsystem "nodecode-approvals/test"
  :description "Approvals tests. Registered into the shared nodecode.test registry; RUN-APPROVALS-TESTS filters by the APPROVALS-CELL- name prefix."
  :author "anpicasso (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-approvals" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-approvals-tests)))
