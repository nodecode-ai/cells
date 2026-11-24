;;;; nodecode-postel.asd --- Postel: research, drafts, and publishing, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `postel' section and registers the tools cell.lisp lists.
;;;;
;;;; A rewrite of robinfaraj's postel-mcp agent plugin (see NOTICE). The
;;;; original was a manifest pointing the harness at Postel's hosted MCP
;;;; server, plus a Markdown skill that taught the order of work; Nodecode has
;;;; no MCP client of its own to inherit here, and Postel publishes a REST API
;;;; with the same operations and an API key, so the tools call that.

(defsystem "nodecode-postel"
  :description "Postel: what is going viral in your niche, and your drafts, schedule and published posts"
  :author "robinfaraj (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "quri")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-postel/test"))))

(defsystem "nodecode-postel/test"
  :description "Postel cell tests. Registered into the shared nodecode.test registry; RUN-POSTEL-TESTS filters by the POSTEL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-postel" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-postel-tests)))
