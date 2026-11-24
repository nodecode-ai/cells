;;;; nodecode-hireko.asd --- Hireko's recruiting tools, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `hireko' section, registers the seven hireko_ tools,
;;;; /hireko, and puts (help :hireko) on the manual.
;;;;
;;;; A Nodecode rewrite of Hireko's Hermes MCP gateway (see NOTICE). Every
;;;; dependency rides with nodecode: the HTTP calls are NLK:HTTP's.

(defsystem "nodecode-hireko"
  :description "Hireko recruiting: sign in, create candidate screening interview links, read match scores"
  :license "MIT"
  :version "0.1.0"
  :author "HireKoAI (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hireko/test"))))

(defsystem "nodecode-hireko/test"
  :description "Hireko tests. Registered into the shared nodecode.test registry; RUN-HIREKO-TESTS filters by the HIREKO-CELL- name prefix."
  :license "MIT"
  :author "HireKoAI (original), Nodecode (rewrite)"
  :depends-on ("nodecode-hireko" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hireko-tests)))
