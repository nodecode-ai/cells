;;;; nodecode-proofgate.asd --- no proof, no "done".
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Whydiriki700/proofgate for Nodecode: a tool, a command and
;;;; one :TOOL point hook that holds the model to the evidence its own tool
;;;; calls left in this session.

(defsystem "nodecode-proofgate"
  :description "Checks a reply's completion claims against the session's tool evidence"
  :license "MIT"
  :author "Whydiriki700 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-proofgate/test"))))

(defsystem "nodecode-proofgate/test"
  :description "ProofGate tests: the checker, the hook, the tool and the ledger, offline."
  :license "MIT"
  :author "Whydiriki700 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-proofgate" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-proofgate-tests)))
