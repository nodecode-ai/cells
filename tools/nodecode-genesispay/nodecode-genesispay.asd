;;;; nodecode-genesispay.asd --- the GenesisPay catalog, as four tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `genesispay' section, registers the four tools cell.lisp lists
;;;; and puts (help :genesispay) on the manual.
;;;;
;;;; A rewrite of GenesisTechAT's hermes-genesispay-catalog plugin (see
;;;; NOTICE): the same four tools and the same bundled skill, spoken to the
;;;; same server -- the public, read-only GenesisPay Catalog MCP endpoint --
;;;; with no MCP runtime in between: the cell is a small JSON-RPC client, so
;;;; the tools are ordinary Nodecode tools and the skill is one help topic.

(defsystem "nodecode-genesispay"
  :description "GenesisPay Catalog: search products and stores and read verified-purchase reviews, from the public read-only catalog"
  :author "GenesisTechAT (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-genesispay/test"))))

(defsystem "nodecode-genesispay/test"
  :description "GenesisPay catalog cell tests. Registered into the shared nodecode.test registry; RUN-GENESISPAY-TESTS filters by the GENESISPAY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-genesispay" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-genesispay-tests)))
