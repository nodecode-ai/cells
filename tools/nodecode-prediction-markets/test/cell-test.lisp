;;;; cell-test.lisp --- lifecycle, the six tools, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every calculator is arithmetic on numbers the caller passes, so there is
;;;; no seam to stub: what is proved here is the cell's own wiring -- the
;;;; section, the six tools through the dispatch the model calls them by, the
;;;; slash command, and the refusals (a setting of the wrong shape, a call
;;;; missing a required member, a probability passed as a decimal fraction).

(in-package #:nodecode.test)

(define-test-slice "prediction-markets" "PREDICTION-MARKETS-CELL-"
  :start nodecode-prediction-markets:start-cell)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "prediction-markets"
  (:config "bankroll" 2500 "default_fraction" "quarter")
  (:help :prediction-markets)
  (:command "prediction-markets")
  (:running
   (is (nle::find-tool "calculate_ev") "calculate_ev is registered")
   (is (nle::find-tool "kelly_size") "kelly_size is registered")
   (is (nle::find-tool "bayes_update") "bayes_update is registered")
   (is (nle::find-tool "convert_probability") "convert_probability is registered")
   (is (nle::find-tool "base_rate_gap") "base_rate_gap is registered")
   (is (nle::find-tool "combo_edge") "combo_edge is registered")
   (is (= 2500 (nodecode-prediction-markets::setting :bankroll)) "the section's bankroll is in force")
   (is (equal "quarter" (nodecode-prediction-markets::setting :default-fraction)) "and its fraction"))
  (:stopped
   (is (null (nle::find-tool "calculate_ev")) "stop unregisters the tools")
   (is (null (nle::find-tool "combo_edge")) "every one of them"))
  (:refused
   ("default_fraction" "third")
   ("bankroll" "lots")))

;;; --- the dispatch the model calls the tools by ------------------------------------

(defun prediction-markets-call (name arguments)
  "NAME called through the model's own dispatch seam with ARGUMENTS as the JSON
text a provider sends, answering the text the model reads back."
  (nle::message-content
   (execute-wire-call "prediction-markets-call" name arguments)))

(defmacro is-answering (name arguments &body checks)
  "The tool NAME, called with ARGUMENTS, carries every CHECK."
  `(is-carrying (answer (prediction-markets-call ,name ,arguments)) ,@checks))

(defmacro with-prediction-markets (&body body)
  "Run BODY with the cell started on its defaults, stopped on unwind."
  `(with-cell-stop ((prediction-markets-start))
     ,@body))

;;; --- calculate_ev ---------------------------------------------------------------

(deftest prediction-markets-cell-ev-answers-the-edge-and-a-buy ()
  (with-prediction-markets
    (is-answering "calculate_ev" "{\"market_price\": 40, \"your_probability\": 55}"
      ("+37.5%" "the edge is relative to the implied probability")
      ("BUY" "more than five points of edge is a BUY")
      ("underpriced" "with the plain-English read")
      ("40 cents" "the price it was given comes back"))
    (is-answering "calculate_ev" "{\"market_price\": 60, \"your_probability\": 55}"
      ("-8.33%" "a price above your estimate is a negative edge")
      ("SELL"))))

;;; --- kelly_size -----------------------------------------------------------------

(deftest prediction-markets-cell-kelly-sizes-the-stake ()
  (with-prediction-markets
    (is-answering "kelly_size" "{\"win_probability\": 60, \"market_price\": 45, \"bankroll\": 1000}"
      ("$136" "half-Kelly of a thousand dollars")
      ("Optimal" "and the rating a 13.6% stake carries"))
    (is-answering "kelly_size" "{\"win_probability\": 60, \"market_price\": 45, \"bankroll\": 1000, \"fraction\": \"quarter\"}"
      ("$68" "a quarter-Kelly stake is half of that"))))

(deftest prediction-markets-cell-kelly-falls-back-on-the-section ()
  (with-cell-stop ((prediction-markets-start "bankroll" 2000 "default_fraction" "full"))
    (is-answering "kelly_size" "{\"win_probability\": 60, \"market_price\": 45}"
      ("$545" "full Kelly of the section's two thousand dollars")
      ("full-Kelly stake" "and the section's fraction, named in the answer"))))

;;; --- bayes_update ---------------------------------------------------------------

(deftest prediction-markets-cell-bayes-chains-the-evidence ()
  (with-prediction-markets
    (is-answering "bayes_update" "{\"prior\": 50, \"evidence\": [{\"label\": \"poll\", \"likelihood_if_true\": 70, \"likelihood_if_false\": 30}]}"
      ("posterior 70" "one update takes 50% to 70%")
      ("1. poll" "the evidence keeps its label")
      ("+20.0pp" "and the change is named"))
    (is-answering "bayes_update" "{\"prior\": 50, \"evidence\": [{\"likelihood_if_true\": 70, \"likelihood_if_false\": 30}, {\"likelihood_if_true\": 70, \"likelihood_if_false\": 30}]}"
      ("2 update(s)" "two pieces are applied in order")
      ("Evidence 2" "an unlabelled piece is numbered"))))

;;; --- convert_probability ---------------------------------------------------------

(deftest prediction-markets-cell-convert-answers-all-three ()
  (with-prediction-markets
    (is-answering "convert_probability" "{\"value\": 62, \"format\": \"probability\"}"
      ("62.00%" "the implied probability")
      ("-163" "the American odds")
      ("1.613" "and the decimal multiplier, all at once"))
    (is-answering "convert_probability" "{\"value\": -150, \"format\": \"american\"}"
      ("60.00%" "American odds become the probability and the multiplier"))
    (is-answering "convert_probability" "{\"value\": 2.5, \"format\": \"decimal\"}"
      ("40.00%" "the implied probability")
      ("+150" "and the American odds"))))

;;; --- base_rate_gap ---------------------------------------------------------------

(deftest prediction-markets-cell-base-rate-gaps-against-history ()
  (with-prediction-markets
    (is-answering "base_rate_gap" "{\"market_price\": 60, \"base_rate_id\": \"fed_hold_unemp_below_4\"}"
      ("Fed holds rates" "the curated entry, by label")
      ("+14.0pp" "the gap in points")
      ("Strong BUY" "and the signal a fourteen-point gap carries")
      ("medium" "with the sample quality")
      ("n=18" "and its size"))
    (is-answering "base_rate_gap" "{\"market_price\": 30, \"base_rate_value\": 38}"
      ("custom base rate" "a caller's own base rate works without an id")
      ("+8.0pp" "and the gap is computed from it")
      ("BUY" "at the seven points an eight-point gap clears"))))

;;; --- combo_edge -----------------------------------------------------------------

(deftest prediction-markets-cell-combo-grades-only-against-a-quote ()
  (with-prediction-markets
    (is-answering "combo_edge" "{\"leg_prices\": [50, 45], \"true_win_probability\": 22}"
      ("2-leg combo" "the legs are counted")
      ("fair value" "the band is reported")
      ("Supply offered_odds" "and a quote is asked for")
      ("trap" "a joint under the product of the legs is flagged"))
    (is-answering "combo_edge" "{\"leg_prices\": [50, 45], \"true_win_probability\": 30, \"offered_odds\": 4.0}"
      ("SMASH" "a quote well above fair is worth playing")
      ("EV +20.0%" "and its EV is reported"))
    (is-answering "combo_edge" "{\"leg_prices\": [50, 45], \"true_win_probability\": 30, \"offered_odds\": -150}"
      ("EV -50.0%" "an American quote is read as a multiplier")
      ("RUN" "and fifty points against is the original's RUN"))))

;;; --- refusals -------------------------------------------------------------------

(deftest prediction-markets-cell-a-fraction-is-refused ()
  (with-prediction-markets
    (is-answering "calculate_ev" "{\"market_price\": 40, \"your_probability\": 0.71}"
      ("decimal fraction" "0.71 is refused rather than read as 0.71%")
      ("Pass 71" "and the fix is named"))))

(deftest prediction-markets-cell-a-call-missing-a-required-member-refuses ()
  (with-prediction-markets
    (is-answering "kelly_size" "{\"market_price\": 40}"
      ("win_probability" "the member the call left out is named"))))

(deftest prediction-markets-cell-the-percent-guard-can-be-turned-off ()
  (with-cell-stop ((prediction-markets-start "percent_guard" nil))
    (is-answering "calculate_ev" "{\"market_price\": 40, \"your_probability\": 0.71}"
      ("-98.23%" "with the guard off, 0.71 reads as 0.71%")
      (:absent "decimal fraction"))))

;;; --- the command -----------------------------------------------------------------

(deftest prediction-markets-cell-the-command-runs-the-calculators ()
  (with-prediction-markets
    (is-carrying (answer (cell-entry "nodecode-prediction-markets" "prediction-markets" "ev 40 55"))
      ("+37.5%" "ev PRICE PROB is the calculate_ev calculator"))
    (is-carrying (answer (cell-entry "nodecode-prediction-markets" "prediction-markets" "ids"))
      ("fed_cut_cpi_above_3" "ids lists the base rates"))
    (is-carrying (answer (cell-entry "nodecode-prediction-markets" "prediction-markets" ""))
      ("/prediction-markets" "with no argument it says how to use it")))
  (is-carrying (answer (refusal-text nodecode-prediction-markets:prediction-markets-error
                         (nodecode-prediction-markets:calculate-ev
                          :market-price 40 :your-probability 0.71)))
    ("decimal fraction" "the verb refuses outside a command too")))
