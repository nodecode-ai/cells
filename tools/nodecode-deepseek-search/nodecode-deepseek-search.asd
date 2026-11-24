;;;; nodecode-deepseek-search.asd --- DeepSeek's Search & Fetch APIs as two verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `deepseek-search' section, registers the deepseek_search and
;;;; deepseek_fetch tools and puts (help :deepseek-search) on the manual.
;;;;
;;;; A Nodecode rewrite of JasonSwindle's hermes-deepseek-search-fetch plugin (see
;;;; NOTICE). The plugin was a web provider in Hermes; here it is two verbs
;;;; and two tools, which is the shape a search backend takes in Nodecode.

(defsystem "nodecode-deepseek-search"
  :description "DeepSeek Search & Fetch: web search and page fetch on DeepSeek's own APIs, with your key"
  :license "MIT"
  :version "0.1.0"
  :author "JasonSwindle (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-deepseek-search/test"))))

(defsystem "nodecode-deepseek-search/test"
  :description "DeepSeek Search & Fetch tests. Registered into the shared nodecode.test registry; RUN-DEEPSEEK-SEARCH-TESTS filters by the DEEPSEEK-SEARCH-CELL- name prefix."
  :license "MIT"
  :author "JasonSwindle (original), Nodecode (rewrite)"
  :depends-on ("nodecode-deepseek-search" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-deepseek-search-tests)))
