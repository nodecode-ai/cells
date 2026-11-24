;;;; nodecode-corpus.asd --- Corpus as model tools: US law search and a
;;;; company-formation intake.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `corpus' section, registers the five tools the original package
;;;; gave its harness, puts /corpus and the manual topic on, and hands the
;;;; model a primer.
;;;;
;;;; The original is a configuration-only Agent Plugins package: one remote MCP
;;;; server at https://corpuslaw.us/api/mcp, spoken to in streamable HTTP, and
;;;; two prose skills that teach the search-before-you-answer procedure and
;;;; the formation intake. This cell speaks that protocol itself, over the
;;;; image's own HTTP call; no Node, no MCP client library, nothing downloaded
;;;; at run time.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting - or symlinking - the
;;;; directory under ~/.nodecode/cells/.

(defsystem "nodecode-corpus"
  :description "Corpus: current US law with verbatim citations, and a US company-formation intake"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "teakesdev (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-corpus/test"))))

(defsystem "nodecode-corpus/test"
  :description "Corpus tests. Registered into the shared nodecode.test registry; RUN-CORPUS-TESTS filters by the CORPUS-CELL- name prefix."
  :license "Apache-2.0"
  :version "0.1.0"
  :author "teakesdev (original), Nodecode (rewrite)"
  :depends-on ("nodecode-corpus" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-corpus-tests)))
