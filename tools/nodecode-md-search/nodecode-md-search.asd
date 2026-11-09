;;;; nodecode-md-search.asd --- full-text search over your own markdown folders.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `md-search' section and registers the md_search tool and the
;;;; /md-search command.
;;;;
;;;; A Nodecode rewrite of kuehnberger/hermes-qdrant-memory's md_search (see
;;;; NOTICE).

(defsystem "nodecode-md-search"
  :description "Markdown memory: full-text search over your own markdown folders, ranked by BM25"
  :license "MIT"
  :version "0.1.0"
  :author "kuehnberger (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-md-search/test"))))

(defsystem "nodecode-md-search/test"
  :description "Markdown search tests, registered into the shared nodecode.test registry; RUN-MD-SEARCH-TESTS filters by the MD-SEARCH-CELL- prefix."
  :license "MIT"
  :version "0.1.0"
  :author "kuehnberger (original), Nodecode (rewrite)"
  :depends-on ("nodecode-md-search" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-md-search-tests)))
