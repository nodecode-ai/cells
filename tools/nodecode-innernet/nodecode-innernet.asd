;;;; nodecode-innernet.asd --- innernet memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `innernet' section and registers the server's tools and
;;;; /innernet.
;;;;
;;;; A Nodecode rewrite of your-innernet/innernet-mcp's Hermes plugin (see
;;;; NOTICE).

(defsystem "nodecode-innernet"
  :description "innernet: one hosted memory across AI tools - projects, decisions, tasks and handoffs"
  :license "MIT"
  :version "0.1.0"
  :author "your-innernet (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-innernet/test"))))

(defsystem "nodecode-innernet/test"
  :description "innernet tests. Registered into the shared nodecode.test registry; RUN-INNERNET-TESTS filters by the INNERNET-CELL- name prefix."
  :license "MIT"
  :author "your-innernet (original), Nodecode (rewrite)"
  :depends-on ("nodecode-innernet" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-innernet-tests)))
