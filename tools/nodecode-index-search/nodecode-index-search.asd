;;;; nodecode-index-search.asd --- one Meilisearch index, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes plugin `index-search' by TakeshiTGAL: three tools
;;;; (index_search, index_health, index_watch) and one slash command that
;;;; talk to a Meilisearch server the operator runs. The only peer is that
;;;; server's own origin; no key is baked in, and nothing else is dialled.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting the directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-index-search"
  :description "Search and health-check a Meilisearch index you run, and watch one saved query"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "api")
               (:file "watch")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-index-search/test"))))

(defsystem "nodecode-index-search/test"
  :description "Index-search tests. They register into the shared nodecode.test registry; RUN-INDEX-SEARCH-TESTS runs the INDEX-SEARCH-CELL- slice."
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-index-search" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-index-search-tests)))
