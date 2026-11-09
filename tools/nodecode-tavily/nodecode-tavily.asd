;;;; nodecode-tavily.asd --- Tavily search, extract, map, crawl and research, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `tavily' section, registers the five tools cell.lisp lists and
;;;; puts the plugin's skills on the manual as (help :tavily).
;;;;
;;;; A rewrite of tavily-ai's tavily-hermes-plugin (see NOTICE): the original
;;;; shipped Tavily's hosted MCP server through a portable manifest. Every tool
;;;; it discovered wrapped one Tavily REST endpoint, so this cell posts to them
;;;; directly and needs no MCP transport.

(defsystem "nodecode-tavily"
  :description "Tavily web search, page extraction, site map, crawl and research, as five tools the model calls"
  :author "tavily-ai (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tavily/test"))))

(defsystem "nodecode-tavily/test"
  :description "Tavily cell tests. Registered into the shared nodecode.test registry; RUN-TAVILY-TESTS filters by the TAVILY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-tavily" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tavily-tests)))
