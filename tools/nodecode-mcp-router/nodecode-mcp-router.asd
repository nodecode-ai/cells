;;;; nodecode-mcp-router.asd --- MCP Router: pick the MCP tools that fit a
;;;; query inside a context budget, locally.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `mcp-router' section, registers the mcp_select tool, puts
;;;; /mcp-router and the manual topic on, and hands the model a primer.
;;;;
;;;; The original is an Agent Plugins package whose one stdio MCP server
;;;; exposes one tool, mcp_select: given a query and a catalog of MCP tool
;;;; descriptions, it ranks them, fits them into a token budget, explains the
;;;; choice, abstains when nothing is safe, and allows one fallback hop. It
;;;; never executes the tools and, in its default local mode, never dials.
;;;; This cell is that selector in Lisp, over the image's own JSON waist: no
;;;; Node, no Python, no MCP client, nothing downloaded at run time.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting - or symlinking - the
;;;; directory under ~/.nodecode/cells/.

(defsystem "nodecode-mcp-router"
  :description "MCP Router: pick the MCP tools that fit a query inside a context budget, locally"
  :license "MIT"
  :version "0.1.0"
  :author "Pinutss (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "select")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mcp-router/test"))))

(defsystem "nodecode-mcp-router/test"
  :description "MCP Router tests. Registered into the shared nodecode.test registry; RUN-MCP-ROUTER-TESTS filters by the MCP-ROUTER-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Pinutss (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mcp-router" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mcp-router-tests)))
