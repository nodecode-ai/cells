;;;; nodecode-broadband-deals.asd --- FindBroadband's UK broadband deals, as four tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `broadband-deals' section, registers the four findbroadband_*
;;;; tools and /broadband-deals, and puts (help :broadband-deals) on the
;;;; manual.
;;;;
;;;; A Nodecode rewrite of FindBroadband/mcp's findbroadband-deals plugin
;;;; (see NOTICE). Every dependency rides with nodecode: the cell speaks
;;;; MCP's streamable HTTP with nlk:http and nlk:read-events.

(defsystem "nodecode-broadband-deals"
  :description "FindBroadband: UK broadband deals, whole-contract priced, and an overpaying check, at a postcode"
  :license "MIT"
  :version "0.1.0"
  :author "FindBroadband (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-broadband-deals/test"))))

(defsystem "nodecode-broadband-deals/test"
  :description "Broadband deals tests. Registered into the shared nodecode.test registry; RUN-BROADBAND-DEALS-TESTS filters by the BROADBAND-DEALS-CELL- name prefix."
  :license "MIT"
  :author "FindBroadband (original), Nodecode (rewrite)"
  :depends-on ("nodecode-broadband-deals" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-broadband-deals-tests)))
