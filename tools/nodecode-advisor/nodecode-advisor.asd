;;;; nodecode-advisor.asd --- a second model that reviews each turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ljubomirj/hermes-advisor-plugin. Optional quicklisp/ASDF
;;;; cell, not part of the organism core: the folder loader loads it at boot
;;;; when its directory sits under ~/.nodecode/cells/, and the gateway calls
;;;; START-CELL after recovery, which reads the `advisor' config section,
;;;; observes the :FRAME point for finished turns, starts one reviewing
;;;; thread and registers /advisor and the advisor_review tool.

(defsystem "nodecode-advisor"
  :description "A persistent second model reviews each turn and posts tagged advice"
  :license "MIT"
  :author "ljubomirj (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "review")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-advisor/test"))))

(defsystem "nodecode-advisor/test"
  :description "Advisor tests. Registered into the shared nodecode.test registry; RUN-ADVISOR-TESTS filters by the ADVISOR-CELL- name prefix."
  :license "MIT"
  :author "ljubomirj (original), Nodecode (rewrite)"
  :depends-on ("nodecode-advisor" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-advisor-tests)))
