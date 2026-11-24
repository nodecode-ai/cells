;;;; nodecode-verifier.asd --- a second model that verifies each turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of am423/hermes-verifier: the folder loader loads it at boot
;;;; when its directory sits under ~/.nodecode/cells/, and the gateway calls
;;;; START-CELL after recovery, which reads the `verifier' config section,
;;;; observes the :FRAME point for finished turns, hooks :TOOL for the phase
;;;; gate, and registers /verify and the verifier_review tool.

(defsystem "nodecode-verifier"
  :description "A second model reviews each turn and can veto a risky call before it runs"
  :license "MIT"
  :author "am423 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-verifier/test"))))

(defsystem "nodecode-verifier/test"
  :description "Verifier tests. Registered into the shared nodecode.test registry; RUN-VERIFIER-TESTS filters by the VERIFIER-CELL- name prefix."
  :license "MIT"
  :author "am423 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-verifier" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-verifier-tests)))
