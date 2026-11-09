;;;; nodecode-keenable.asd --- Keenable web search and page fetch as model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `keenable' section, registers the two tools the original
;;;; plugin gave its harness (search_web_pages and fetch_page_content, here
;;;; keenable_search and keenable_fetch), puts /keenable and the manual
;;;; topic on, and hands the model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: a manifest, one remote MCP
;;;; connection over streamable HTTP and two Markdown skills. This cell
;;;; speaks the same JSON-RPC to the same server through the image's own
;;;; HTTP call -- no Python, no MCP client library, no dependency of its own
;;;; -- and answers the server's text as the model reads it. The skills'
;;;; guidance becomes the primer (help :keenable).
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-keenable"
  :description "Keenable: web search and page fetch through its MCP server; no key needed"
  :license "MIT"
  :version "0.1.0"
  :author "keenableai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "rpc")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-keenable/test"))))

(defsystem "nodecode-keenable/test"
  :description "Keenable tests. Registered into the shared nodecode.test registry; RUN-KEENABLE-TESTS filters by the KEENABLE-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "keenableai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-keenable" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-keenable-tests)))
