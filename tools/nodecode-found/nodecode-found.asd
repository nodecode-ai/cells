;;;; nodecode-found.asd --- FOUND's AI SDR as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `found' section, registers the six found_* tools and /found,
;;;; and talks to FOUND's hosted MCP server over HTTP.
;;;;
;;;; A Nodecode rewrite of Utkarshkapil's FOUND plugin for Hermes (see NOTICE).

(defsystem "nodecode-found"
  :description "FOUND: warm leads, pipeline numbers, account research and drafts from the AI SDR"
  :license "MIT"
  :version "0.1.0"
  :author "Utkarshkapil (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-found/test"))))

(defsystem "nodecode-found/test"
  :description "FOUND cell tests. Registered into the shared nodecode.test registry; RUN-FOUND-TESTS filters by the FOUND-CELL- name prefix."
  :license "MIT"
  :author "Utkarshkapil (original), Nodecode (rewrite)"
  :depends-on ("nodecode-found" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-found-tests)))
