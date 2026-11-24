;;;; nodecode-citation-guard.asd --- references checked against the registries.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `citation_guard' section, registers the verify_citations tool,
;;;; puts /citecheck and the manual on, and derives its stop from them.
;;;;
;;;; One seam reaches outside: NLK:HTTP, to api.crossref.org, doi.org,
;;;; eutils.ncbi.nlm.nih.gov and export.arxiv.org. Nothing is installed.

(defsystem "nodecode-citation-guard"
  :description "Citation check: every DOI, PMID, arXiv id and reference entry checked against Crossref, PubMed and arXiv"
  :license "MIT"
  :version "0.1.0"
  :author "ChienNguyen23 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "extract")
               (:file "sources")
               (:file "verify")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-citation-guard/test"))))

(defsystem "nodecode-citation-guard/test"
  :description "Citation guard tests. Registered into the shared nodecode.test registry; RUN-CITATION-GUARD-TESTS filters by the CITATION-GUARD-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "ChienNguyen23 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-citation-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-citation-guard-tests)))
