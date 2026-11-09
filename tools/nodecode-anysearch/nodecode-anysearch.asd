;;;; nodecode-anysearch.asd --- AnySearch web search and page extract as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `anysearch' section, registers anysearch_search,
;;;; anysearch_extract and /anysearch, and puts (help :anysearch) on the
;;;; manual.
;;;;
;;;; A Nodecode rewrite of the web providers in vollegrewar/hermes-plugin-anysearch
;;;; (see NOTICE).

(defsystem "nodecode-anysearch"
  :description "AnySearch web search and page extract, with the MCP extract fallback beside the REST one"
  :license "MIT"
  :version "0.1.0"
  :author "vollegrewar (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-anysearch/test"))))

(defsystem "nodecode-anysearch/test"
  :description "AnySearch cell tests. Registered into the shared nodecode.test registry; RUN-ANYSEARCH-TESTS filters by the ANYSEARCH-CELL- name prefix."
  :license "MIT"
  :author "vollegrewar (original), Nodecode (rewrite)"
  :depends-on ("nodecode-anysearch" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-anysearch-tests)))
