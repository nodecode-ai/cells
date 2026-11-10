;;;; nodecode-prediction-markets.asd --- the six calculators, as model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `prediction-markets' section and registers the six tools the
;;;; plugin's offline build offers -- calculate_ev, kelly_size, bayes_update,
;;;; convert_probability, base_rate_gap, combo_edge -- with
;;;; /prediction-markets and the manual topic (help :prediction-markets).
;;;;
;;;; The original is a Hermes Agent plugin, configuration only: an mcp.json
;;;; pointing at a hosted MCP server, and one skill. Its own bridge also
;;;; ships those six calculators as a self-contained offline build, and that
;;;; build is what this cell is: arithmetic in Lisp, no network, no key, no
;;;; dependency of its own.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-prediction-markets"
  :description "Prediction-market calculators: EV, Kelly sizing, Bayesian updating, odds conversion, base-rate gaps and combo edge"
  :author "predictionmarketspicks (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "calc")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-prediction-markets/test"))))

(defsystem "nodecode-prediction-markets/test"
  :description "Prediction-markets cell tests: offline, nothing to stub but the clock."
  :author "predictionmarketspicks (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-prediction-markets" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-prediction-markets-tests)))
