;;;; nodecode-recall-vault.asd --- a policy vault and a memory for the agent's money.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes Agent plugin tydavid5/recall-vault (see NOTICE).
;;;; Optional ASDF cell, not part of the organism core: the folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/, and
;;;; the gateway calls START-CELL after recovery, which reads the
;;;; `recall-vault' section, registers the five recall_vault_* tools, /recall-vault
;;;; and (help :recall-vault), hooks the :TOOL point so a payment-like call is
;;;; held to the rules before it runs, and rides the harness sections so the
;;;; rules are in front of the model at every request.

(defsystem "nodecode-recall-vault"
  :description "A money policy vault: caps, an allowlist and a ledger over payment-like tool calls"
  :license "MIT"
  :author "tydavid5 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "policy")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-recall-vault/test"))))

(defsystem "nodecode-recall-vault/test"
  :description "Recall vault tests. Registered into the shared nodecode.test registry; RUN-RECALL-VAULT-TESTS filters by the RECALL-VAULT-CELL- name prefix."
  :license "MIT"
  :author "tydavid5 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-recall-vault" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-recall-vault-tests)))
