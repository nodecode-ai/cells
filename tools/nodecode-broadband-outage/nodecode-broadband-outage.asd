;;;; nodecode-broadband-outage.asd --- FindBroadband's UK broadband outage checker, as three tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `broadband-outage' section, registers the three
;;;; findbroadband_outage_* tools and /broadband-outage, and puts
;;;; (help :broadband-outage) on the manual.
;;;;
;;;; A Nodecode rewrite of FindBroadband/mcp's internet-outage-checker plugin
;;;; (see NOTICE). Every dependency rides with nodecode: the cell speaks
;;;; MCP's streamable HTTP with nlk:http and nlk:read-events.

(defsystem "nodecode-broadband-outage"
  :description "FindBroadband: whether a UK broadband provider is down, where, and its outages over 30 days"
  :license "MIT"
  :version "0.1.0"
  :author "FindBroadband (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-broadband-outage/test"))))

(defsystem "nodecode-broadband-outage/test"
  :description "Broadband outage tests. Registered into the shared nodecode.test registry; RUN-BROADBAND-OUTAGE-TESTS filters by the BROADBAND-OUTAGE-CELL- name prefix."
  :license "MIT"
  :author "FindBroadband (original), Nodecode (rewrite)"
  :depends-on ("nodecode-broadband-outage" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-broadband-outage-tests)))
