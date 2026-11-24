;;;; nodecode-birta.asd --- Birta's published pages, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `birta' section, registers the two tools cell.lisp lists and puts
;;;; the plugin's sixteen skills on the manual as (help :birta).
;;;;
;;;; A rewrite of birtadev's birtadev/plugins (see NOTICE): the original
;;;; shipped Birta's hosted MCP server through a portable manifest. This cell
;;;; speaks that MCP protocol in Common Lisp, so it needs no MCP client beyond
;;;; itself.

(defsystem "nodecode-birta"
  :description "Birta: publish pages, sites and documents at a link, read the feedback left on them, and call every Birta tool the connected server offers"
  :author "birtadev (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-birta/test"))))

(defsystem "nodecode-birta/test"
  :description "Birta cell tests. Registered into the shared nodecode.test registry; RUN-BIRTA-TESTS filters by the BIRTA-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-birta" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-birta-tests)))
