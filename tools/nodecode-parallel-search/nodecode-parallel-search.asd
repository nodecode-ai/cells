;;;; nodecode-parallel-search.asd --- Parallel Search as two model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `parallel_search' section, registers the two tools the original
;;;; package gave its harness (web_search, web_fetch), puts /parallel and the
;;;; manual topic on, and hands the model a primer.
;;;;
;;;; The original is a configuration-only Agent Plugins package: one remote MCP
;;;; server at https://search.parallel.ai/mcp, spoken to in streamable HTTP.
;;;; This cell speaks that protocol itself, over the image's own HTTP call; no
;;;; Node, no MCP client library, nothing downloaded at run time.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting - or symlinking - the
;;;; directory under ~/.nodecode/cells/.

(defsystem "nodecode-parallel-search"
  :description "Parallel Search: keyless web search and page fetch, as two tools"
  :license "MIT"
  :version "0.1.0"
  :author "parallel-web (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-parallel-search/test"))))

(defsystem "nodecode-parallel-search/test"
  :description "Parallel Search tests. Registered into the shared nodecode.test registry; RUN-PARALLEL-SEARCH-TESTS filters by the PARALLEL-SEARCH-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "parallel-web (original), Nodecode (rewrite)"
  :depends-on ("nodecode-parallel-search" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-parallel-search-tests)))
