;;;; nodecode-crawl4ai.asd --- Crawl4AI web extraction as one tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `crawl4ai' section and registers the crawl4ai_extract tool, the
;;;; /crawl command and the manual topic cell.lisp lists.
;;;;
;;;; A rewrite of mblauser/hermes-plugin-crawl4ai (see NOTICE). The server is
;;;; the operator's own Crawl4AI instance: this cell never installs, starts or
;;;; stops it, and every byte it sends goes to the address the section names.
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-crawl4ai"
  :description "Crawl4AI: turn pages into clean markdown through your own self-hosted instance"
  :license "MIT"
  :author "mblauser (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-crawl4ai/test"))))

(defsystem "nodecode-crawl4ai/test"
  :description "Crawl4AI tests. Registered into the shared nodecode.test registry; RUN-CRAWL4AI-TESTS filters by the CRAWL4AI-CELL- name prefix."
  :license "MIT"
  :author "mblauser (original), Nodecode (rewrite)"
  :depends-on ("nodecode-crawl4ai" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-crawl4ai-tests)))

