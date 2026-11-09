;;;; nodecode-vault.asd --- a local encrypted secret store as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `vault' section, registers vault_list, vault_get, vault_put,
;;;; vault_remove and vault_run, and puts /vault and (help :vault) up.
;;;; AES-256-GCM is ironclad's, the one crypto system the serving image names.

(defsystem "nodecode-vault"
  :description "A local secret store sealed with AES-256-GCM: hv: refs, a mapping to the environment, and a command run under it"
  :author "Guerilla-ops (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "ironclad" "ironclad/aead/gcm" "ironclad/cipher/aes")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-vault/test"))))

(defsystem "nodecode-vault/test"
  :description "Vault cell tests. Registered into the shared nodecode.test registry; RUN-VAULT-TESTS filters by the VAULT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-vault" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-vault-tests)))
