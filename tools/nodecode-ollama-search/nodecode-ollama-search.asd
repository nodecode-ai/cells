;;;; nodecode-ollama-search.asd --- Ollama Cloud web search, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ollama-search' section, registers the two tools cell.lisp
;;;; declares and puts (help :ollama-search) on the manual.
;;;;
;;;; A rewrite of rriggs/hermes-plugin-ollama-cloud-search (see NOTICE).

(defsystem "nodecode-ollama-search"
  :description "Web search and page fetch through Ollama Cloud, reusing an existing Ollama API key"
  :license "MIT"
  :version "0.1.0"
  :author "rriggs (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ollama-search/test"))))

(defsystem "nodecode-ollama-search/test"
  :description "Ollama Cloud search tests. Registered into the shared nodecode.test registry; RUN-OLLAMA-SEARCH-TESTS filters by the OLLAMA-SEARCH-CELL- name prefix."
  :license "MIT"
  :author "rriggs (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ollama-search" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ollama-search-tests)))
