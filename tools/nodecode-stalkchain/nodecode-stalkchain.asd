;;;; nodecode-stalkchain.asd --- StalkChain's crypto research as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `stalkchain' section and registers the six stalkchain_* tools,
;;;; the /stalkchain command and the (help :stalkchain) topic.
;;;;
;;;; A Nodecode rewrite of StalkC/stalkchain-plugin (see NOTICE).

(defsystem "nodecode-stalkchain"
  :description "StalkChain: crypto research on coins, traders and wallets over its MCP server"
  :license "MIT"
  :version "0.1.0"
  :author "StalkC (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-stalkchain/test"))))

(defsystem "nodecode-stalkchain/test"
  :description "StalkChain tests. Registered into the shared nodecode.test registry; RUN-STALKCHAIN-TESTS filters by the STALKCHAIN-CELL- name prefix."
  :license "MIT"
  :author "StalkC (original), Nodecode (rewrite)"
  :depends-on ("nodecode-stalkchain" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-stalkchain-tests)))
