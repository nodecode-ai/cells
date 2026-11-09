;;;; nodecode-aux-ledger.asd --- what the model calls behind a turn cost, one row each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `aux_ledger' section, advises NLE:COMPLETE (the one side
;;;; call) and CALL-PROVIDER under it, registers /aux and the aux_ledger tool,
;;;; and puts the manual on (help :aux).
;;;;
;;;; The ledger is a local JSONL file the section names: identifiers, timings
;;;; and token counts, never a prompt or an answer.

(defsystem "nodecode-aux-ledger"
  :description "A local ledger of the auxiliary LLM calls a session makes, and /aux"
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "ledger")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-aux-ledger/test"))))

(defsystem "nodecode-aux-ledger/test"
  :description "Auxiliary ledger tests. Registered into the shared nodecode.test registry; RUN-AUX-LEDGER-TESTS filters by the AUX-LEDGER-CELL- name prefix."
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-aux-ledger" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-aux-ledger-tests)))
