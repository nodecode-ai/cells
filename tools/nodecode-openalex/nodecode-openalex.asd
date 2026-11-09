;;;; nodecode-openalex.asd --- OpenAlex scholarly search, as ten tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openalex' section, registers the tools cell.lisp lists and puts
;;;; (help :openalex) on the manual.
;;;;
;;;; A rewrite of Adolanium's hermes-plugin-openalex (see NOTICE): the
;;;; original's ten tools and its /openalex command, against the same REST API,
;;;; with the same cost model -- the price of a call is known before it is
;;;; sent, and the ledger refuses the one that would exceed the budget.

(defsystem "nodecode-openalex"
  :description "OpenAlex scholarly search: works, authors, sources and citation graphs, with the price of each call"
  :author "Adolanium (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "fields")
               (:file "shape")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openalex/test"))))

(defsystem "nodecode-openalex/test"
  :description "OpenAlex cell tests. Registered into the shared nodecode.test registry; RUN-OPENALEX-TESTS filters by the OPENALEX-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-openalex" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openalex-tests)))
