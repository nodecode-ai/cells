;;;; nodecode-federated-search.asd --- one query across every search backend you configured.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `federated-search' section, registers the federated_search tool
;;;; and the /federated-search command, and puts the manual on (help
;;;; :federated-search).
;;;;
;;;; A rewrite of kumaxs's hermes-federated-search (see NOTICE): the original
;;;; was a Hermes web-search provider that fanned one query out to several
;;;; backends. This cell asks the same backends itself, through one tool.

(defsystem "nodecode-federated-search"
  :description "Federated search: one query across every search backend you configured, merged, deduplicated and ranked"
  :author "kumaxs (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "lines")
               (:file "search")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-federated-search/test"))))

(defsystem "nodecode-federated-search/test"
  :description "Federated search cell tests. Registered into the shared nodecode.test registry; RUN-FEDERATED-SEARCH-TESTS filters by the FEDERATED-SEARCH-CELL- name prefix."
  :license "Apache-2.0"
  :depends-on ("nodecode-federated-search" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-federated-search-tests)))
