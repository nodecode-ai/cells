;;;; nodecode-serpapi.asd --- SerpApi search, maps, news, shopping, travel and extraction.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `serpapi' section, registers the eight tools cell.lisp lists and
;;;; puts the plugin's own guidance on the manual as (help :serpapi).
;;;;
;;;; A rewrite of SerpApi's serpapi-hermes-plugin (see NOTICE): the original
;;;; registered one web-search provider and seven tools against SerpApi's
;;;; single endpoint. Every one of them is one parameterised GET, so this cell
;;;; calls the endpoint directly and needs neither Hermes nor an HTTP client of
;;;; its own.

(defsystem "nodecode-serpapi"
  :description "SerpApi web search, Maps, News, Shopping, Hotels, Flights, Travel Explore and page extraction, as eight tools the model calls"
  :author "serpapi (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-serpapi/test"))))

(defsystem "nodecode-serpapi/test"
  :description "SerpApi cell tests. Registered into the shared nodecode.test registry; RUN-SERPAPI-TESTS filters by the SERPAPI-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-serpapi" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-serpapi-tests)))
