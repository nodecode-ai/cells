;;;; nodecode-financial-datasets.asd --- Financial Datasets' stock-market data as 28 tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `financial-datasets' section and registers the 28 read-only
;;;; tools cell.lisp lists.
;;;;
;;;; One seam reaches outside: NLK:HTTP, to api.financialdatasets.ai, with the
;;;; key in an X-API-KEY header. Nothing is installed, and no dependency is
;;;; added beyond what nodecode already loads.

(defsystem "nodecode-financial-datasets"
  :description "Financial Datasets: statements, prices, SEC filings, ownership, KPIs, news, macro data and a screener, as 28 tools"
  :license "MIT"
  :version "0.1.0"
  :author "financial-datasets (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "api")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-financial-datasets/test"))))

(defsystem "nodecode-financial-datasets/test"
  :description "Financial Datasets tests. Registered into the shared nodecode.test registry; RUN-FINANCIAL-DATASETS-TESTS filters by the FINANCIAL-DATASETS-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "financial-datasets (original), Nodecode (rewrite)"
  :depends-on ("nodecode-financial-datasets" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-financial-datasets-tests)))
