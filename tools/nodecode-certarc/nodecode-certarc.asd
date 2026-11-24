;;;; nodecode-certarc.asd --- CertArc's free exam practice, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `certarc' section, registers the four certarc_* tools and
;;;; /certarc, and puts (help :certarc) on the manual.
;;;;
;;;; A Nodecode rewrite of the Hermes plugin in CertArc/certarc-mcp (see
;;;; NOTICE): one hosted MCP endpoint, four read-only tools, no account.

(defsystem "nodecode-certarc"
  :description "CertArc's free CISM, CISSP, CISA, AAISM and AAIA practice questions, exam outlines and guide links"
  :license "MIT"
  :version "0.1.0"
  :author "CertArc (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-certarc/test"))))

(defsystem "nodecode-certarc/test"
  :description "CertArc cell tests. Registered into the shared nodecode.test registry; RUN-CERTARC-TESTS filters by the CERTARC-CELL- name prefix."
  :license "MIT"
  :author "CertArc (original), Nodecode (rewrite)"
  :depends-on ("nodecode-certarc" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-certarc-tests)))
