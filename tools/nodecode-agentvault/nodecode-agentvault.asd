;;;; nodecode-agentvault.asd --- Agent Vault's archived conversations, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes Agent plugin agentvault (see NOTICE). Optional
;;;; ASDF cell, not part of the organism core: the folder loader loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/, and the gateway
;;;; calls START-CELL after recovery, which reads the `vault' section and
;;;; registers search_sessions, get_session, vault_status, /vault and
;;;; (help :vault). The app's own MCP server is the archive's only door, and
;;;; this cell is its client.

(defsystem "nodecode-agentvault"
  :description "Agent Vault: search the archived coding conversations, and read one exact revision"
  :license "MIT"
  :author "synnada-ai (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agentvault/test"))))

(defsystem "nodecode-agentvault/test"
  :description "Agent Vault tests. Registered into the shared nodecode.test registry; RUN-AGENTVAULT-TESTS filters by the AGENTVAULT-CELL- name prefix."
  :license "MIT"
  :author "synnada-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-agentvault" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agentvault-tests)))
