;;;; cell.lisp --- the calculator table, the six boundaries, the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Hermes package was an mcp.json pointing at a hosted MCP server plus a
;;;; skill. The six tools that server also ships in its own offline build --
;;;; calculate_ev, kelly_size, bayes_update, convert_probability,
;;;; base_rate_gap, combo_edge -- are pure arithmetic with no key and no
;;;; network, and they are what this cell answers: the same names, the same
;;;; formulas, the same thresholds, as six tools the model calls, six verbs
;;;; the operator calls, and one /prediction-markets command.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "prediction-markets": {"bankroll": 1000, "default_fraction": "half",
;;;;                          "percent_guard": true}
;;;; Nothing is required: the calculators are arithmetic. A vetoed section
;;;; ("enabled": false) installs nothing.

(in-package #:nodecode-prediction-markets)

;;; --- the manual ---------------------------------------------------------------

(defparameter *primer*
  "Prediction-market calculators from PredictionMarketsPicks, in the
nodecode-prediction-markets cell: six tools, the same six calls in the
PREDICTION-MARKETS package from eval, all offline arithmetic on numbers you
supply. Nothing here reads a live price; ask for the numbers, or take them
from a market you fetched.
  calculate_ev        (prediction-markets:calculate-ev :market-price 40 :your-probability 55)
      the EV edge in percent from the contract's price in cents and your own
      probability, with a BUY / SELL / SKIP read at +-5%.
  kelly_size          (prediction-markets:kelly-size :win-probability 60 :market-price 45 :bankroll 1000 :fraction \"half\")
      the Kelly stake in dollars, full / half / quarter / eighth, with a risk
      rating; bankroll defaults to the section's, fraction to its default.
  bayes_update        (prediction-markets:bayes-update :prior 50 :evidence '((:label \"poll\" :if-true 70 :if-false 30)))
      a prior updated by one or more pieces of evidence, with each step.
  convert_probability (prediction-markets:convert-probability :value -150 :format \"american\")
      probability, American odds and decimal odds from any one of the three.
  base_rate_gap       (prediction-markets:base-rate-gap :market-price 60 :base-rate-id \"fed_hold_unemp_below_4\")
      a market price against a historical base rate: the gap in points, a
      signal and the sample quality; 12 curated base rates, or your own.
  combo_edge          (prediction-markets:combo-edge :leg-prices '(50 45) :true-win-probability 22 :offered-odds -150)
      a same-game combo's fair-value band, and - given the price a venue
      quotes - its EV and a SMASH / PLAY / LEAN / RISK / NO_VALUE / PASS / RUN
      verdict.
Rules the calculators ask for:
- Percent, not a fraction: pass 71 for 71%. A value between 0 and 1 is
  refused as ambiguous rather than guessed at (percent_guard turns that off).
- A combo is never graded off the product of its leg prices; no venue pays
  that on legs that settle together. Supply the quote you were offered.
- These are arithmetic, not advice: say what the numbers are, and name the
  price and probability they came from.
Every refusal is ERROR: PREDICTION-MARKETS-ERROR naming the argument.
The operator's own door: /prediction-markets ev|kelly|convert|bayes|base-rate|combo."
  "What (help :prediction-markets) answers while the cell runs.")

;;; --- the base rates the original curated -----------------------------------------
;;; id, label, base rate in percent, sample size, source. The gap each
;;; answers is arithmetic; the rate and its sample are the original's data.

(defparameter *base-rates*
  '(("incumbent_reelected" "Incumbent president reelected" 68 22 "U.S. presidential election records")
    ("fed_hold_unemp_below_4" "Fed holds rates (unemployment < 4%)" 74 18 "Federal Reserve FOMC meeting records")
    ("fed_cut_cpi_above_3" "Fed cuts rates when CPI > 3%" 12 14 "Federal Reserve FOMC meeting records")
    ("recession_called_12mo" "Recession declared within 12 months" 28 15 "NBER business cycle dating")
    ("sp500_positive_year" "S&P 500 positive calendar year return" 72 74 "S&P 500 annual return data")
    ("bitcoin_above_100k_eoy" "Bitcoin > $100k by year-end" 33 5 "CoinGecko historical pricing")
    ("gdp_growth_above_2" "Real GDP growth > 2% (annualized)" 62 28 "Bureau of Economic Analysis")
    ("cpi_above_3" "YoY CPI inflation > 3%" 38 26 "Bureau of Labor Statistics")
    ("senate_incumbent_wins_primary" "Senate incumbent wins primary" 91 44 "Congressional election records")
    ("vix_below_20_eoy" "VIX closes below 20 on year-end" 58 34 "CBOE VIX historical data")
    ("interest_rate_cut_next_meeting" "Fed cuts rates at next meeting" 31 42 "Federal Reserve FOMC meeting records")
    ("major_sports_upset" "Heavy underdog wins championship" 22 30 "Sports reference historical odds"))
  "The original's curated base rates: id, label, percent, sample size, source.")

(defun base-rate-entry (id)
  "The entry ID names, or NIL."
  (assoc id *base-rates* :test #'equal))

(defun base-rate-table ()
  "Every base rate this cell carries, one per line: id, label, percent, n."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (entry)
                    (destructuring-bind (id label rate size source) entry
                      (format nil "~a - ~a: ~a% (n=~d, ~a)" id label rate size source)))
                  *base-rates*)))

;;; --- text the answers carry -----------------------------------------------------

(defun plain-text (value)
  "VALUE as text, whole numbers without a trailing .0: 62 rather than 62.0."
  (let ((v (f value)))
    (if (= v (round v)) (format nil "~d" (round v)) (format nil "~f" v))))

(defun refuse-fraction (name value)
  "Refuse VALUE as an ambiguous decimal fraction, naming the percent it would
be and the fix, as the original's guard did."
  (let ((percent (round-to (* (f value) 100) 2)))
    (fail "~a=~a looks like a decimal fraction, but these calculators take percent (0-100). ~
Pass ~a for ~a%. For a genuine sub-1% estimate round to 1: signals are not meaningful below that resolution."
          name (plain-text (round-to value 2)) (plain-text percent) (plain-text percent))))

(defun signed-number-text (value)
  "VALUE as text with its sign when it is up: +5.5, -2.25, 0.0."
  (let ((text (number-text value)))
    (if (> (f value) 0) (concatenate 'string "+" text) text)))

(defun percent-guard-p ()
  "The section's percent_guard, on while the cell is not started."
  (if *prediction-markets* (setting :percent-guard) t))

(defun bankroll-setting ()
  "The section's bankroll, or a thousand dollars while the cell is not started."
  (if *prediction-markets* (setting :bankroll) 1000))

(defun fraction-setting ()
  "The section's default_fraction, or half while the cell is not started."
  (if *prediction-markets* (setting :default-fraction) "half"))

;;; --- calculate_ev ---------------------------------------------------------------

(defun calculate-ev (&key market-price your-probability)
  "The expected-value edge on a contract: MARKET-PRICE in cents (1-99, the
implied probability) against YOUR-PROBABILITY in percent (0-100). Answers the
edge in percent, a BUY / SELL / SKIP signal at +-5%, and a plain-English
read."
  (check-range "market_price" market-price 1 99)
  (when (and (percent-guard-p) (fraction-like-p your-probability))
    (refuse-fraction "your_probability" your-probability))
  (check-range "your_probability" your-probability 0 100)
  (let* ((edge (ev-edge market-price your-probability))
         (signal (ev-signal edge))
         (interpretation (ev-interpretation edge signal)))
    (format nil "EV edge: ~a% - ~a. ~a. Market price ~a cents (implies ~a%); your estimate ~a%."
            (signed-number-text edge) signal interpretation
            (plain-text market-price) (plain-text market-price)
            (plain-text (round-to your-probability 4)))))

;;; --- kelly_size -----------------------------------------------------------------

(defun kelly-size (&key win-probability market-price bankroll fraction)
  "The Kelly stake for a contract: WIN-PROBABILITY in percent, MARKET-PRICE in
cents, BANKROLL in dollars (defaults to the section's) and FRACTION, one of
full / half / quarter / eighth (defaults to the section's). Answers the stake,
the share of the bankroll it is, full Kelly and a risk rating."
  (check-range "win_probability" win-probability 0 100)
  (check-range "market_price" market-price 1 99)
  (let* ((bankroll (or bankroll (bankroll-setting)))
    (fraction (or fraction (fraction-setting)))
    (payout (payout-ratio market-price))
    (full (kelly-fraction win-probability payout))
    (adjusted (* full (kelly-multiplier fraction))))
    (multiple-value-bind (rating message) (kelly-rating adjusted)
      (declare (ignore rating))
      (format nil "~a-Kelly stake: ~a of a ~a bankroll (~,1f% of bankroll). Full-Kelly f* = ~,1f%, ~
payout ratio ~,2f:1. ~a."
              fraction (format-currency (* adjusted (f bankroll))) (format-currency bankroll)
              (* adjusted 100.0d0) (* full 100.0d0) payout message))))

;;; --- bayes_update ---------------------------------------------------------------

(defun bayes-update (&key prior evidence)
  "PRIOR in percent, updated by EVIDENCE: a list of (:label \"...\"
:if-true N :if-false N) pieces, applied in order. Answers the posterior and
every step."
  (when (and (percent-guard-p) (fraction-like-p prior))
    (refuse-fraction "prior" prior))
  (check-range "prior" prior 0 100)
  (let ((current (f prior)) (steps '()))
    (unless (plusp (length evidence))
      (fail "bayes_update needs at least one piece of evidence"))
    (loop for item in evidence
          for index from 1
          for if-true = (getf item :if-true)
          for if-false = (getf item :if-false)
          do (check-range (format nil "evidence[~d].likelihood_if_true" (1- index)) if-true 0 100)
             (check-range (format nil "evidence[~d].likelihood_if_false" (1- index)) if-false 0 100)
             (let* ((posterior (bayes-posterior current if-true if-false))
                    (change (round-to (- posterior current) 2))
                    (label (or (getf item :label) (format nil "Evidence ~d" index))))
               (push (format nil "  ~d. ~a: ~a% -> ~a% (~app)"
                             index label (plain-text current) (number-text posterior)
                             (signed-number-text change))
                     steps)
               (setf current posterior)))
    (format nil "Prior ~a% -> posterior ~a% after ~d update(s):~%~{~a~^~%~}"
            (plain-text prior) (number-text current) (length evidence) (nreverse steps))))

;;; --- convert_probability ---------------------------------------------------------

(defun convert-probability (&key value format)
  "VALUE in FORMAT -- probability (0-100), american (-200, +150) or decimal
(2.5) -- answered as all three."
  (let* ((format (string-downcase (or format "probability")))
         (prob (cond ((equal format "probability") (/ (f value) 100.0d0))
                     ((equal format "american") (american-to-prob value))
                     ((equal format "decimal") (decimal-to-prob value))
                     (t (fail "format must be probability, american or decimal; got ~a" format)))))
    (unless (and (> prob 0.0d0) (< prob 1.0d0))
      (fail "value is out of range for format ~a: probability must be 0-100 (exclusive), ~
American odds nonzero, decimal odds above 1" format))
    (format nil "~,2f% implied probability = ~a American = ~,3f decimal odds."
            (* prob 100.0d0) (format-american (prob-to-american prob))
            (round-to (prob-to-decimal prob) 3))))

;;; --- base_rate_gap --------------------------------------------------------------

(defun base-rate-gap (&key market-price base-rate-id base-rate-value)
  "MARKET-PRICE in percent against a historical BASE-RATE-ID from
`base-rate-table', or your own BASE-RATE-VALUE in percent. Answers the gap in
points, a signal and the sample quality."
  (check-range "market_price" market-price 0 100)
  (let ((entry (and base-rate-id (base-rate-entry base-rate-id))))
    (when (and base-rate-id (null entry))
      (fail "unknown base_rate_id ~s; the ids are ~{~a~^, ~}" base-rate-id (mapcar #'first *base-rates*)))
    (multiple-value-bind (rate size label source)
        (cond (entry (destructuring-bind (id entry-label base sample origin) entry
                       (declare (ignore id))
                       (values base sample entry-label origin)))
              ((numberp base-rate-value) (values (f base-rate-value) nil "custom base rate" nil))
              (t (fail "provide either a base_rate_id or a base_rate_value")))
      (let* ((gap (base-rate-gap-value rate market-price))
             (quality (base-rate-quality size)))
        (multiple-value-bind (signal signal-label) (base-rate-signal gap)
          (declare (ignore signal))
          (format nil "~a: base rate ~a% vs market ~a% -> gap ~app - ~a. ~a. Sample quality: ~a~@[ (n=~d)~]~@[, source: ~a~]."
                  label (plain-text rate) (plain-text market-price) (signed-number-text gap)
                  signal-label (base-rate-interpretation gap rate)
                  quality size source))))))

;;; --- combo_edge -----------------------------------------------------------------

(defun combo-edge (&key leg-prices true-win-probability offered-odds)
  "A same-game combo: LEG-PRICES (each leg's YES price in cents),
TRUE-WIN-PROBABILITY (your correlation-aware joint, in percent) and, when you
have it, OFFERED-ODDS (American odds or a decimal multiplier). Without a
quote it answers the fair-value band; with one it grades the EV and the
verdict."
  (let ((prices (mapcar #'f (if (listp leg-prices) leg-prices (coerce leg-prices 'list)))))
    (unless (>= (length prices) 2)
      (fail "combo_edge needs at least two leg prices"))
    (loop for price in prices
          for index from 0
          do (check-range (format nil "leg_prices[~d]" index) price 1 99))
    (check-range "true_win_probability" true-win-probability 0 100)
    (when (and (percent-guard-p) (fraction-like-p true-win-probability))
      (refuse-fraction "true_win_probability" true-win-probability))
    (let* ((true-prob (/ (f true-win-probability) 100.0d0))
           (leg-count (length prices))
           (independent (reduce (lambda (acc price) (* acc (/ price 100.0d0))) prices :initial-value 1.0d0))
           (fair-american (prob-to-american true-prob))
           (trap (< true-prob independent))
           (band (combo-band true-prob leg-count))
           (expected-prob (min (+ true-prob +maker-margin+) 0.99d0))
           (offered (and (numberp offered-odds) (offered-to-decimal offered-odds))))
      (unless (plusp independent)
        (fail "provide two or more leg prices (1-99 cents) and a true win probability in 0-100"))
      (if (null offered)
          (format nil "~d-leg combo - fair value ~a (joint ~a%). Fair-value play: between ~,1fx and ~,1fx (~a to ~a). ~
A maker will likely quote near ~,1fx (~a). Supply offered_odds - the price your platform quotes - to grade EV and get a verdict. ~
The product of the leg prices (~,1f%) is a theoretical assemble ceiling only; no venue pays it.~a"
                  leg-count (format-american fair-american) (plain-text true-win-probability)
                  (getf band :low-decimal) (getf band :high-decimal)
                  (format-american (decimal-to-american (getf band :low-decimal)))
                  (format-american (decimal-to-american (getf band :high-decimal)))
                  (/ 1.0d0 expected-prob) (format-american (decimal-to-american (/ 1.0d0 expected-prob)))
                  (* independent 100.0d0)
                  (if trap " Negative-correlation trap: your joint is below the naive product." ""))
          (let* ((ev (* (- (* true-prob offered) 1.0d0) 100.0d0))
                 (verdict (combo-verdict-for-offered ev true-prob trap offered band)))
            (format nil "~d-leg combo - ~a. EV ~a% at your quote of ~a. Fair value ~a; fair-value play between ~,1fx and ~,1fx (~a to ~a). ~
Your joint ~a%.~a"
                    leg-count verdict (signed-number-text (round-to ev 2))
                    (format-american (decimal-to-american offered))
                    (format-american fair-american)
                    (getf band :low-decimal) (getf band :high-decimal)
                    (format-american (decimal-to-american (getf band :low-decimal)))
                    (format-american (decimal-to-american (getf band :high-decimal)))
                    (plain-text true-win-probability)
                    (if trap " Negative-correlation trap: your joint is below the naive product - the price is a mirage." "")))))))

;;; --- the six boundaries the model calls -------------------------------------------

(nlk:define-contract ev-query
  (:doc "The expected-value edge on a prediction-market contract: the percent
edge and a BUY / SELL / SKIP read. Use it for 'is this contract mispriced',
'what is my edge', 'should I take this position'.")
  ("market_price" :number :required t
   :doc "current contract price in cents (1-99), which is the implied probability in percent")
  ("your_probability" :number :required t
   :doc "your own estimate that the contract resolves YES, in percent (0-100); 71 for 71%, not 0.71"))

(nlk:define-contract kelly-query
  (:doc "The Kelly position size for a contract: the dollar stake, its share of
the bankroll, full Kelly, and a risk rating. Use it for 'how much should I
stake', 'what is my position size'.")
  ("win_probability" :number :required t
   :doc "your probability the contract resolves YES, in percent (0-100); 60 for 60%, not 0.6")
  ("market_price" :number :required t
   :doc "contract price in cents (1-99); it sets the payout ratio")
  ("bankroll" :number
   :doc "the bankroll to size against, in dollars; omitted, the section's bankroll is used")
  ("fraction" :string :options '("full" "half" "quarter" "eighth")
   :doc "the Kelly fraction to apply: full, half, quarter or eighth; omitted, the section's default_fraction, which is half until it says otherwise"))

(nlk:define-contract evidence-item
  (:doc "One piece of evidence for a Bayesian update: how likely it is when the
hypothesis is true, and when it is false.")
  ("label" :text
   :doc "a short label for this evidence")
  ("likelihood_if_true" :number :required t
   :doc "P(observing this evidence | the hypothesis is true), in percent (0-100)")
  ("likelihood_if_false" :number :required t
   :doc "P(observing this evidence | the hypothesis is false), in percent (0-100)"))

(nlk:define-contract bayes-query
  (:doc "A prior probability updated by one or more pieces of evidence, with
the posterior and every step. Use it for 'update my estimate with this news',
'what is the posterior', 'how does this change the odds'.")
  ("prior" :number :required t
   :doc "the prior probability the hypothesis is true, in percent (0-100)")
  ("evidence" :array :of evidence-item :required t
   :doc "one or more pieces of evidence, applied in order"))

(nlk:define-contract convert-query
  (:doc "One probability in any of the three formats, answered as all three:
implied probability in percent, American odds and decimal odds.")
  ("value" :number :required t
   :doc "the number to convert, in the format named below")
  ("format" :string :required t :options '("probability" "american" "decimal") :slot format-name
   :doc "what value is: probability (0-100), american (-200 or +150) or decimal (2.5)"))


(nlk:define-contract base-rate-query
  (:doc "A market price against a historical base rate for a class of events:
the gap in percentage points, a signal and the sample quality. Use it for
'how does this price compare to history'.")
  ("market_price" :number :required t
   :doc "the market price in cents, which is the implied probability in percent (0-100)")
  ("base_rate_id" :string
   :options '("incumbent_reelected" "fed_hold_unemp_below_4" "fed_cut_cpi_above_3"
               "recession_called_12mo" "sp500_positive_year" "bitcoin_above_100k_eoy"
               "gdp_growth_above_2" "cpi_above_3" "senate_incumbent_wins_primary"
               "vix_below_20_eoy" "interest_rate_cut_next_meeting" "major_sports_upset")
   :doc "one of the curated base rates, by id; base_rate-table lists them")
  ("base_rate_value" :number
   :doc "your own base rate in percent (0-100), used when no base_rate_id is given"))

(nlk:define-contract combo-query
  (:doc "A same-game combo graded against its fair value. Without offered_odds
it answers the fair-value band; with the quote a venue gave you it answers the
EV and a verdict. Never grade a combo off the product of its leg prices.")
  ("leg_prices" :array :of :number :required t
   :doc "each leg's YES price in cents (1-99); two or more")
  ("true_win_probability" :number :required t
   :doc "your correlation-aware estimate that every leg hits, in percent (0-100)")
  ("offered_odds" :number
   :doc "the combo price your venue quotes: American odds (-150, 988) or a decimal multiplier (10.7); omit for fair value only"))

(defun calculate-ev-tool (query)
  "The calculate_ev tool."
  (calculate-ev :market-price (ev-query-market-price query)
                 :your-probability (ev-query-your-probability query)))

(defun kelly-size-tool (query)
  "The kelly_size tool."
  (kelly-size :win-probability (kelly-query-win-probability query)
               :market-price (kelly-query-market-price query)
               :bankroll (kelly-query-bankroll query)
               :fraction (kelly-query-fraction query)))

(defun bayes-update-tool (query)
  "The bayes_update tool."
  (bayes-update
   :prior (bayes-query-prior query)
   :evidence (map 'list (lambda (item)
                           (list :label (evidence-item-label item)
                                 :if-true (evidence-item-likelihood-if-true item)
                                 :if-false (evidence-item-likelihood-if-false item)))
                   (bayes-query-evidence query))))

(defun convert-probability-tool (query)
  "The convert_probability tool."
  (convert-probability :value (convert-query-value query)
                        :format (convert-query-format-name query)))

(defun base-rate-gap-tool (query)
  "The base_rate_gap tool."
  (base-rate-gap :market-price (base-rate-query-market-price query)
                  :base-rate-id (base-rate-query-base-rate-id query)
                  :base-rate-value (base-rate-query-base-rate-value query)))

(defun combo-edge-tool (query)
  "The combo_edge tool."
  (combo-edge :leg-prices (combo-query-leg-prices query)
              :true-win-probability (combo-query-true-win-probability query)
              :offered-odds (combo-query-offered-odds query)))

;;; --- /prediction-markets ---------------------------------------------------------

(defparameter +usage+
  "prediction-markets: ev PRICE PROB | kelly PROB PRICE [BANKROLL] | convert VALUE FORMAT |
base-rate PRICE [ID|VALUE] | bayes PRIOR [IF-TRUE IF-FALSE] | combo PRICES PROB [OFFERED]
PRICE is cents (1-99), PROB a percent (0-100). /prediction-markets ids lists the base rates."
  "What the command answers with no argument.")

(defun parse-number-text (text)
  "TEXT as a double-float, or NIL when it is not one number."
  (when (and (stringp text) (plusp (length text)))
    (multiple-value-bind (value position)
        (ignore-errors (read-from-string text nil nil))
      (when (and (numberp value) (= position (length text)))
        (f value)))))

(defun run-slash (args session-id)
  "One /prediction-markets invocation: the calculators, from the operator's
own hands. Answers the text, and no dialog."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words (or args "")))
         (verb (string-downcase (or (first words) "")))
         (rest (rest words)))
    (flet ((need (index name)
             (or (parse-number-text (nth index rest))
                 (fail "~a is needed as a number: ~a" name +usage+))))
      (values
       (cond
         ((equal verb "ev")
          (calculate-ev :market-price (need 0 "a price") :your-probability (need 1 "a probability")))
         ((equal verb "kelly")
          (kelly-size :win-probability (need 0 "a probability") :market-price (need 1 "a price")
                      :bankroll (and (nth 2 rest) (need 2 "a bankroll"))))
         ((equal verb "convert")
          (convert-probability :value (need 0 "a value") :format (or (second rest) "probability")))
         ((equal verb "bayes")
          (let ((prior (need 0 "a prior")))
            (if (nth 2 rest)
                (bayes-update :prior prior
                              :evidence (list (list :label "evidence"
                                                    :if-true (need 1 "P(evidence | true)")
                                                    :if-false (need 2 "P(evidence | false)"))))
                (format nil "prior ~a%: give P(evidence | true) and P(evidence | false) to update it"
                        (plain-text prior)))))
         ((equal verb "base-rate")
          (let ((price (need 0 "a price")) (second-arg (second rest)))
            (base-rate-gap :market-price price
                            :base-rate-id (and second-arg (not (parse-number-text second-arg)) second-arg)
                            :base-rate-value (and second-arg (parse-number-text second-arg)))))
         ((equal verb "combo")
          (let ((prices (and (first rest)
                             (mapcar (lambda (word) (or (parse-number-text word) (fail "~a" +usage+)))
                                     (uiop:split-string (first rest) :separator ",")))))
            (combo-edge :leg-prices prices
                        :true-win-probability (need 1 "a joint probability")
                        :offered-odds (and (nth 2 rest) (need 2 "an offered price")))))
         ((member verb '("ids" "base-rates") :test #'equal) (base-rate-table))
         (t +usage+))
       nil))))

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell prediction-markets
  (:section ("prediction-markets")
    (:guide "nothing here is required: the six calculators are offline arithmetic. bankroll is the stake base /prediction-markets kelly sizes against; default_fraction is the Kelly fraction it applies when none is named; percent_guard refuses a probability passed as a fraction (0.71) instead of a percent (71)")
    ("bankroll" :integer :default 1000 :min 1
     :doc "the bankroll the kelly command sizes against, in whole dollars")
    ("default_fraction" :choice :options '("full" "half" "quarter" "eighth") :default "half"
     :doc "the Kelly fraction the kelly command applies when it names none")
    ("percent_guard" :boolean :default t
     :doc "refuse a probability passed as a decimal fraction (0.71) instead of percent (71)"))
  (:tool "calculate_ev" (nlk:contract 'ev-query) #'calculate-ev-tool)
  (:tool "kelly_size" (nlk:contract 'kelly-query) #'kelly-size-tool)
  (:tool "bayes_update" (nlk:contract 'bayes-query) #'bayes-update-tool)
  (:tool "convert_probability" (nlk:contract 'convert-query) #'convert-probability-tool)
  (:tool "base_rate_gap" (nlk:contract 'base-rate-query) #'base-rate-gap-tool)
  (:tool "combo_edge" (nlk:contract 'combo-query) #'combo-edge-tool)
  (:help :prediction-markets
   "prediction-markets: EV, Kelly, Bayesian, odds, base-rate and combo-edge calculators for Kalshi and Polymarket contracts"
   *primer*)
  (:command "prediction-markets" 'run-slash
            :catches 'prediction-markets-error
            :description "Prediction-market calculators: EV, Kelly, odds, base rates, combos"
            :argument-hint "ev|kelly|convert|bayes|base-rate|combo ..."))
