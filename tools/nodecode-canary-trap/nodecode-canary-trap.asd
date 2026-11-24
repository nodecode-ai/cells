;;;; nodecode-canary-trap.asd --- catch prompt injection by the secret it tries to steal.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode cell, NOT part of the organism core: nothing in Nodecode names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which issues the decoy, hooks the :ROUND point (the paragraph that rides
;;;; the request) and the :TOOL point (the block and the lockdown), and
;;;; registers canary_status, /canary and the manual on (help :canary-trap).
;;;;
;;;; Rewritten from Tarekkharsa/hermes-canary-trap (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-canary-trap"
  :description "Canary Trap: a decoy credential each turn; a tool call carrying it is blocked and its session locked"
  :license "Apache-2.0"
  :author "Tarekkharsa (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-canary-trap/test"))))

(defsystem "nodecode-canary-trap/test"
  :description "Canary Trap tests. Registered into the shared nodecode.test registry; RUN-CANARY-TRAP-TESTS filters by the CANARY-TRAP-CELL- name prefix."
  :license "Apache-2.0"
  :author "Tarekkharsa (original), Nodecode (rewrite)"
  :depends-on ("nodecode-canary-trap" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-canary-trap-tests)))
