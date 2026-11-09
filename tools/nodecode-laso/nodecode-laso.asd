;;;; nodecode-laso.asd --- Laso Finance over its MCP server, as model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL, which reads the `laso'
;;;; section and registers the two tools, the /laso command and the manual
;;;; topic cell.lisp declares.
;;;;
;;;; Ported from LasoFinance's Hermes plugin (see NOTICE). Nodecode carries
;;;; dexador and quri, so the two HTTP exchanges ride nlk:http.

(defsystem "nodecode-laso"
  :description "Laso Finance: spend USDC on prepaid cards, gift cards and payouts, from chat"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "catalog")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-laso/test"))))

(defsystem "nodecode-laso/test"
  :description "Laso tests. Registered into the shared nodecode.test registry; RUN-LASO-TESTS filters by the LASO-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-laso" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-laso-tests)))
