;;;; nodecode-patsnap.asd --- Patsnap patent and literature search as ten tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `patsnap' section, registers the nine server tools and
;;;; the schema lookup, puts /patsnap and the manual on (help :patsnap).
;;;;
;;;; One seam reaches outside: NLK:HTTP, to Patsnap's hosted MCP server, in
;;;; an apikey header. Nothing is installed, and no dependency is added.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting - or symlinking -
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-patsnap"
  :description "Patsnap patent and literature search through Patsnap's hosted MCP server, as tools"
  :license "MIT"
  :version "0.1.0"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-patsnap/test"))))

(defsystem "nodecode-patsnap/test"
  :description "Patsnap tests. Registered into the shared nodecode.test registry; RUN-PATSNAP-TESTS filters by the PATSNAP-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode-patsnap" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-patsnap-tests)))
