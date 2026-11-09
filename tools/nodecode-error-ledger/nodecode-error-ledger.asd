;;;; nodecode-error-ledger.asd --- the provider calls that failed, and why.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `error-ledger' section, registers the error_ledger tool,
;;;; /error-ledger and (help :error-ledger).
;;;;
;;;; Rewritten from olric3015/hermes-plugins (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-error-ledger"
  :description "Error ledger: the provider calls that failed, by model, provider, cause and day"
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-error-ledger/test"))))

(defsystem "nodecode-error-ledger/test"
  :description "Error ledger tests. Registered into the shared nodecode.test registry; RUN-ERROR-LEDGER-TESTS filters by the ERROR-LEDGER-CELL- name prefix."
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-error-ledger" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-error-ledger-tests)))
