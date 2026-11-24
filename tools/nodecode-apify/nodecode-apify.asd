;;;; nodecode-apify.asd --- Apify Actors as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `apify' section and registers the three apify_* tools, the
;;;; /apify command and the (help :apify) topic.
;;;;
;;;; A Nodecode rewrite of apify/apify-hermes-agent-plugin (see NOTICE).

(defsystem "nodecode-apify"
  :description "Apify Actors: discover one in the Store, start runs, collect results, plus Apify web search and page fetch"
  :license "MIT"
  :version "0.1.0"
  :author "apify (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-apify/test"))))

(defsystem "nodecode-apify/test"
  :description "Apify tests. Registered into the shared nodecode.test registry; RUN-APIFY-TESTS filters by the APIFY-CELL- name prefix."
  :license "MIT"
  :author "apify (original), Nodecode (rewrite)"
  :depends-on ("nodecode-apify" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-apify-tests)))
