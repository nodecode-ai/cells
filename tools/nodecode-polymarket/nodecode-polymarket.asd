;;;; nodecode-polymarket.asd --- Polymarket prediction markets, read-only.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `polymarket' section, registers the six polymarket_* tools, puts
;;;; (help :polymarket) on the manual and /polymarket in the slash catalog.
;;;;
;;;; Rewritten from HATWHY4041's prediction-market-scout (see NOTICE).
;;;; Every dependency, HTTP included, rides with nodecode.

(defsystem "nodecode-polymarket"
  :description "Polymarket: live odds, 24h movers, attention signals, a price-alert watchlist and a briefing, read-only"
  :license "MIT"
  :version "0.1.0"
  :author "HATWHY4041 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "analysis")
               (:file "watchlist")
               (:file "render")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-polymarket/test"))))

(defsystem "nodecode-polymarket/test"
  :description "Polymarket tests. Registered into the shared nodecode.test registry; RUN-POLYMARKET-TESTS filters by the POLYMARKET-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "HATWHY4041 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-polymarket" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-polymarket-tests)))
