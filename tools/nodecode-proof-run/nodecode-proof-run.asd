;;;; nodecode-proof-run.asd --- record a demonstrated run, then gate its reuse.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of raffieeey/hermes-proof-run for Nodecode: the /proof-run
;;;; command, a proof_status tool and one :TOOL point hook that is both the
;;;; original's recorder and its pre_tool_call gate.

(defsystem "nodecode-proof-run"
  :description "Records a demonstrated run and gates its reuse on exact arguments and fresh reads"
  :license "MIT"
  :author "raffieeey (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-proof-run/test"))))

(defsystem "nodecode-proof-run/test"
  :description "Proof Run tests: the recording, the gate, the command and the contract file, offline."
  :license "MIT"
  :author "raffieeey (original), Nodecode (rewrite)"
  :depends-on ("nodecode-proof-run" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-proof-run-tests)))
