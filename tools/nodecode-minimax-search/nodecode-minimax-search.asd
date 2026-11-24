;;;; nodecode-minimax-search.asd --- MiniMax Token Plan web search, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `minimax-search' section, registers the one tool cell.lisp
;;;; declares and puts (help :minimax-search) on the manual.
;;;;
;;;; A rewrite of pdonizete/hermes-minimax-search (see NOTICE).

(defsystem "nodecode-minimax-search"
  :description "MiniMax web search through a Token Plan key: titles, links and snippets as a tool"
  :license "MIT"
  :version "0.1.0"
  :author "pdonizete (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-minimax-search/test"))))

(defsystem "nodecode-minimax-search/test"
  :description "MiniMax search tests. Registered into the shared nodecode.test registry; RUN-MINIMAX-SEARCH-TESTS filters by the MINIMAX-SEARCH-CELL- name prefix."
  :license "MIT"
  :author "pdonizete (original), Nodecode (rewrite)"
  :depends-on ("nodecode-minimax-search" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-minimax-search-tests)))
