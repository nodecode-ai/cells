;;;; nodecode-account-pool.asd --- several Claude and Codex accounts in one pool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `account-pool' section, registers the account_pool tool and
;;;; /account-pool, and puts (help :account-pool) on the manual.
;;;;
;;;; Ported from the Hermes plugin hermes-account-pool (see NOTICE).

(defsystem "nodecode-account-pool"
  :description "Account Pool: several Claude and Codex accounts in one pool, with usage, best, park and wake"
  :license "MIT"
  :author "roqianjas (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-account-pool/test"))))

(defsystem "nodecode-account-pool/test"
  :description "Account Pool tests. Registered into the shared nodecode.test registry; RUN-ACCOUNT-POOL-TESTS filters by the ACCOUNT-POOL-CELL- name prefix."
  :license "MIT"
  :author "roqianjas (original), Nodecode (rewrite)"
  :depends-on ("nodecode-account-pool" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-account-pool-tests)))
