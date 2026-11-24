;;;; nodecode-maqami-travel.asd --- MAQAMI's travel MCP server, as two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `maqami' section and registers the two tools cell.lisp lists.
;;;;
;;;; A rewrite of negm17111995's mcp-server (see NOTICE): the original shipped
;;;; MAQAMI's hosted Streamable HTTP endpoint through a Node stdio bridge and a
;;;; bundled booking skill. The endpoint is the same; the bridge is wire.lisp
;;;; and the skill is the manual topic.

(defsystem "nodecode-maqami-travel"
  :description "MAQAMI travel: live hotel and flight rates, and a checkout link on book.maqami.co"
  :author "negm17111995 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-maqami-travel/test"))))

(defsystem "nodecode-maqami-travel/test"
  :description "MAQAMI travel cell tests. Registered into the shared nodecode.test registry; RUN-MAQAMI-TRAVEL-TESTS filters by the MAQAMI-TRAVEL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-maqami-travel" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-maqami-travel-tests)))
