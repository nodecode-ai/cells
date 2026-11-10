;;;; nodecode-affiliateo.asd --- Affiliateo's MCP server as two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `affiliateo' section, registers the two tools and puts the
;;;; manual on (help :affiliateo) and its three deeper guides beside it.
;;;;
;;;; Rewritten from affiliateo's Hermes Agent plugin; see NOTICE. Every
;;;; dependency rides with nodecode: nlk:http speaks MCP's streamable HTTP
;;;; to Affiliateo's own origin, quri reads the one address.

(defsystem "nodecode-affiliateo"
  :description "Affiliateo: affiliates, payouts, revenue, ads, invoices and support through its hosted MCP server"
  :author "affiliateo (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-affiliateo/test"))))

(defsystem "nodecode-affiliateo/test"
  :description "Affiliateo tests. Registered into the shared nodecode.test registry; RUN-AFFILIATEO-TESTS filters by the AFFILIATEO-CELL- name prefix."
  :author "affiliateo (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-affiliateo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-affiliateo-tests)))
