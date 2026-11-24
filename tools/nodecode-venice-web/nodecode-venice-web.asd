;;;; nodecode-venice-web.asd --- Venice AI web search and page extract as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `venice' section, registers venice_search, venice_extract and
;;;; /venice, and puts (help :venice) on the manual.
;;;;
;;;; A Nodecode rewrite of the web provider in MnrGreg/hermes-venice-web (see
;;;; NOTICE).

(defsystem "nodecode-venice-web"
  :description "Venice AI web search and page extract: privacy-first search and Markdown fetch, on a key"
  :license "MIT"
  :version "0.1.0"
  :author "MnrGreg (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-venice-web/test"))))

(defsystem "nodecode-venice-web/test"
  :description "Venice web tests. Registered into the shared nodecode.test registry; RUN-VENICE-WEB-TESTS filters by the VENICE-WEB-CELL- name prefix."
  :license "MIT"
  :author "MnrGreg (original), Nodecode (rewrite)"
  :depends-on ("nodecode-venice-web" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-venice-web-tests)))
