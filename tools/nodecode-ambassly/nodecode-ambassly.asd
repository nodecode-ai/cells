;;;; nodecode-ambassly.asd --- Ambassly's hosted MCP server, spoken natively.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of MelMayssonOwen's `ambassly' plugin for Hermes Agent (see
;;;; NOTICE). A cell, not part of the organism core: nothing in Nodecode names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `ambassly' section and registers the two tools, the
;;;; /ambassly command and the (help :ambassly) topic cell.lisp lists.

(defsystem "nodecode-ambassly"
  :description "Ambassly: Stripe affiliate programs, links, referrals and commissions through its hosted MCP server"
  :license "MIT"
  :author "MelMayssonOwen (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ambassly/test"))))

(defsystem "nodecode-ambassly/test"
  :description "Ambassly tests: the lifecycle, the wire handshake over a stubbed endpoint, the tools and /ambassly."
  :license "MIT"
  :author "MelMayssonOwen (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ambassly" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ambassly-tests)))
