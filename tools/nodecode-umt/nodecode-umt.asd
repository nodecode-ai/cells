;;;; nodecode-umt.asd --- Hacker News, arXiv and npm lookups, the three read-only servers of UMT.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `umt' section and puts (help :umt) on the manual.
;;;;
;;;; Ported from Markgatcha's Universal MCP Toolkit (see NOTICE): the three
;;;; zero-config MCP servers the plugin ships -- Hacker News, arXiv and the
;;;; npm registry -- as eleven tools and the same lookups as Lisp verbs.
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-umt"
  :description "Hacker News, arXiv and npm lookups: the zero-config servers of Universal MCP Toolkit, as one cell"
  :license "MIT"
  :version "0.1.0"
  :author "Markgatcha (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "hn")
               (:file "arxiv")
               (:file "npm")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-umt/test"))))

(defsystem "nodecode-umt/test"
  :description "UMT tests. Registered into the shared nodecode.test registry; RUN-UMT-TESTS filters by the UMT-CELL- name prefix."
  :license "MIT"
  :author "Markgatcha (original), Nodecode (rewrite)"
  :depends-on ("nodecode-umt" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-umt-tests)))
