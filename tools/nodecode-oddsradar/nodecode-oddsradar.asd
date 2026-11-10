;;;; nodecode-oddsradar.asd --- Polymarket odds, a watchlist and its alerts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes plugin `oddsradar' by WV629: five tools the model
;;;; calls (odds_search, odds_market, odds_watch, odds_check, odds_movers), one
;;;; slash command (/oddsradar) and one help topic, all on one engine. The only
;;;; peers are the two public Polymarket hosts, asked read-only and keyless;
;;;; the watchlist is one JSON file in this home.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting the directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-oddsradar"
  :description "Polymarket odds, a persistent watchlist and its alerts, keyless"
  :license "MIT"
  :author "WV629 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "api")
               (:file "markets")
               (:file "watch")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-oddsradar/test"))))

(defsystem "nodecode-oddsradar/test"
  :description "Odds Radar tests. They register into the shared nodecode.test registry; RUN-ODDSRADAR-TESTS runs the ODDSRADAR-CELL- slice."
  :license "MIT"
  :author "WV629 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-oddsradar" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-oddsradar-tests)))
