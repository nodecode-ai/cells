;;;; nodecode-dsers.asd --- the DSers remote MCP service, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral: it registers two tools, one command and one manual topic,
;;;; and keeps the DSers account's tool list and calls behind one HTTP seam.
;;;; See src/CELLS.md, "Declaring a cell".

(defsystem "nodecode-dsers"
  :description "DSers dropshipping tools: supplier search, listings, prices, orders and parcels, through the DSers MCP server"
  :license "MIT"
  :version "0.1.0"
  :author "dsers (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-dsers/test"))))

(defsystem "nodecode-dsers/test"
  :description "The dsers cell's own tests, offline."
  :license "MIT"
  :depends-on ("nodecode-dsers" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-dsers-tests)))
