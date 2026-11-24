;;;; nodecode-web-search-plus.asd --- multi-provider web search and page reading.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL, which reads the
;;;; websearch_plus section, registers the two tools and puts the primer on
;;;; (help :web-search-plus).
;;;;
;;;; Rewritten from robbyczgw-cla's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the
;;;; providers, shasht reads and writes their JSON, quri encodes the query,
;;;; ppcre strips a page.

(defsystem "nodecode-web-search-plus"
  :description "Web Search Plus: multi-provider search with routing and fallback, and pages read as text"
  :author "robbyczgw-cla (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "web")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-web-search-plus/test"))))

(defsystem "nodecode-web-search-plus/test"
  :description "Web Search Plus tests, registered into the shared nodecode.test registry; RUN-WEB-SEARCH-PLUS-TESTS filters by the WEB-SEARCH-PLUS-CELL- prefix."
  :author "robbyczgw-cla (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-web-search-plus" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-web-search-plus-tests)))
