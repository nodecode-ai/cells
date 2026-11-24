;;;; nodecode-constraint-vault.asd --- exact constraints, delivered verbatim.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of jusst01/constraint-vault as a Nodecode cell: a few facts are
;;;; pinned in one JSON file under the home as the operator's own words, and a
;;;; standing harness section carries them verbatim at the head of every
;;;; request, with each delivery logged by the block's SHA-256.

(defsystem "nodecode-constraint-vault"
  :description "Exact operator constraints, delivered verbatim on every request"
  :license "MIT"
  :version "0.1.0"
  :author "jusst01 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-constraint-vault/test"))))

(defsystem "nodecode-constraint-vault/test"
  :description "Constraint vault tests. Registered into the shared nodecode.test registry; RUN-CONSTRAINT-VAULT-TESTS filters by the CONSTRAINT-VAULT-CELL- name prefix."
  :license "MIT"
  :author "jusst01 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-constraint-vault" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-constraint-vault-tests)))
