;;;; nodecode-basekit.asd --- read-only Base (chain 8453) reads, as six tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `basekit' section and registers the six tools cell.lisp lists.
;;;;
;;;; A rewrite of D0xedDevi0/hermes-basekit (see NOTICE): the original's six
;;;; agent tools and its /basekit command, reimplemented against Base's public
;;;; JSON-RPC endpoints, DefiLlama and Coinbase spot pricing. No key needed,
;;;; nothing signed, nothing sent.

(defsystem "nodecode-basekit"
  :description "Read-only Base (chain 8453) intelligence: network and gas in USD, wallet portfolios, transaction receipts, token facts, transfers and approvals, as six tools"
  :author "D0xedDevi0 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "chain")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-basekit/test"))))

(defsystem "nodecode-basekit/test"
  :description "basekit cell tests. Registered into the shared nodecode.test registry; RUN-BASEKIT-TESTS filters by the BASEKIT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-basekit" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-basekit-tests)))
