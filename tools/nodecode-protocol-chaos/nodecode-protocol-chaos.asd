;;;; nodecode-protocol-chaos.asd --- Protocol Chaos as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `protocol-chaos' section, puts the overview and the related
;;;; matters into each round, registers the three protocol_chaos_* tools and
;;;; /protocol-chaos, and starts the worker that records a finished turn.
;;;;
;;;; A Nodecode rewrite of chaos-xxl's Protocol Chaos Hermes plugin (see NOTICE).

(defsystem "nodecode-protocol-chaos"
  :description "Protocol Chaos: the user's long-lived matters, on their own server, in context before the model answers and recorded after"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "chaos-xxl (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-protocol-chaos/test"))))

(defsystem "nodecode-protocol-chaos/test"
  :description "Protocol Chaos tests. Registered into the shared nodecode.test registry; RUN-PROTOCOL-CHAOS-TESTS filters by the PROTOCOL-CHAOS-CELL- name prefix."
  :license "Apache-2.0"
  :author "chaos-xxl (original), Nodecode (rewrite)"
  :depends-on ("nodecode-protocol-chaos" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-protocol-chaos-tests)))
