;;;; nodecode-covision.asd --- Covision's connector as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `covision' section, registers covision_tools and covision_call,
;;;; handshakes with Covision's connector and registers the read-only tools it
;;;; lists, and keeps the original's skill as a standing section.
;;;;
;;;; A Nodecode rewrite of Covision-Consultants/covision-agent-plugin (see NOTICE).

(defsystem "nodecode-covision"
  :description "Covision: read-only tasks, meetings and published knowledge through the vendor's MCP connector"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "Covision-Consultants (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-covision/test"))))

(defsystem "nodecode-covision/test"
  :description "Covision tests. Registered into the shared nodecode.test registry; RUN-COVISION-TESTS filters by the COVISION-CELL- name prefix."
  :license "Apache-2.0"
  :version "0.1.0"
  :author "Covision-Consultants (original), Nodecode (rewrite)"
  :depends-on ("nodecode-covision" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-covision-tests)))
