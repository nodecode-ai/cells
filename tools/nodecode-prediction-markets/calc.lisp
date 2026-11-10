;;;; calc.lisp --- the six calculators' arithmetic, and nothing else.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original vendored six stateless calculators (src/lib/formulas.js and
;;;; src/lib/combo-price.js) so that its --local bridge could answer with no
;;;; network at all. These are those two files as functions: the same
;;;; rounding, the same thresholds, the same signals. Nothing here reaches
;;;; for a network, a file or a setting.

(in-package #:nodecode-prediction-markets)

;;; --- numbers, rounded and printed the way the original did ------------------

(defun f (value)
  "VALUE as a double-float, so every sum below is one."
  (coerce value 'double-float))

(defun round-to (value decimals)
  "VALUE rounded to DECIMALS places, halves up as the original's Math.round
rounds them: 2.345 is 2.35, -2.345 is -2.34."
  (let* ((factor (expt 10.0d0 decimals))
         (scaled (* (f value) factor)))
    (/ (floor (+ scaled 0.5d0)) factor)))

(defun round-int (value)
  "VALUE rounded to a whole number, halves up."
  (floor (+ (f value) 0.5d0)))

(defun number-text (value)
  "VALUE as the text the model reads: 0.71, 5.5, 37.5 -- ~F, because ~A on a
SBCL double carries its own d0 exponent marker into the answer."
  (format nil "~f" (f value)))

(defun signed-text (value &optional (decimals 2))
  "VALUE with its sign, DECIMALS places: +5.00 rather than 5.00 when it is up."
  (if (> (f value) 0)
      (format nil "+~,vf" decimals (f value))
      (format nil "~,vf" decimals (f value))))

(defun format-currency (value)
  "VALUE in whole dollars, as the original's toLocaleString printed it."
  (format nil "$~:d" (round-int value)))

(defun format-american (american)
  "AMERICAN odds with their sign, or a dash when there are none."
  (if (or (null american) (zerop (f american)))
      "-"
      (let ((n (round-int american)))
        (if (> n 0) (format nil "+~d" n) (format nil "~d" n)))))

;;; --- the percent-scale guard -----------------------------------------------
;;; Agents habitually pass 0.71 meaning 71%. On a 0-100 scale that reads as
;;; 0.71% and yields a confidently wrong signal, so the ambiguous open
;;; interval (0, 1) is refused rather than guessed at.

(defun fraction-like-p (value)
  "True when VALUE reads as a decimal fraction: 0.71 for 71%."
  (and (numberp value) (> (f value) 0) (< (f value) 1)))

(defun check-range (name value low high)
  "Refuse VALUE when it is outside LOW..HIGH, naming what the calculator wants."
  (unless (and (numberp value) (<= (f low) (f value) (f high)))
    (fail "~a must be a number from ~a to ~a; got ~a" name low high
          (if (numberp value) (number-text value) value)))
  (f value))

;;; --- expected value --------------------------------------------------------

(defun ev-edge (market-price your-probability)
  "The expected-value edge in percent: how far YOUR-PROBABILITY sits from the
price's implied probability, relative to it."
  (let ((price (f market-price)))
    (if (<= price 0)
        0.0d0
        (round-to (* (/ (- (f your-probability) price) price) 100.0d0) 2))))

(defun ev-signal (edge)
  "BUY past +5%, SELL past -5%, SKIP in between."
  (cond ((> (f edge) 5) "BUY")
        ((< (f edge) -5) "SELL")
        (t "SKIP")))

(defun ev-interpretation (edge signal)
  "SIGNAL's plain-English read of EDGE."
  (let ((abs (abs (f edge))))
    (cond ((equal signal "BUY") (format nil "Market is ~,1f% underpriced vs. your estimate" abs))
          ((equal signal "SELL") (format nil "Market is ~,1f% overpriced vs. your estimate" abs))
          (t "Market is fairly priced within your confidence interval"))))

;;; --- Kelly sizing ----------------------------------------------------------

(defun payout-ratio (market-price)
  "The payout b a contract at MARKET-PRICE cents pays: (1 - p) / p, zero at
or beyond the edges."
  (let ((p (/ (f market-price) 100.0d0)))
    (if (or (<= p 0.0d0) (>= p 1.0d0)) 0.0d0 (/ (- 1.0d0 p) p))))

(defun kelly-fraction (win-probability payout)
  "Kelly's f* = (p*b - q) / b, floored at zero."
  (let* ((p (/ (f win-probability) 100.0d0))
         (q (- 1.0d0 p))
         (b (f payout)))
    (if (<= b 0.0d0) 0.0d0 (max 0.0d0 (/ (- (* p b) q) b)))))

(defparameter +kelly-multipliers+
  '(("full" . 1.0d0) ("half" . 0.5d0) ("quarter" . 0.25d0) ("eighth" . 0.125d0))
  "The four fractions the original offers, and full-Kelly's share of each.")

(defun kelly-multiplier (fraction)
  "FRACTION's share of full Kelly; half when it names none of the four."
  (let ((entry (assoc fraction +kelly-multipliers+ :test #'equal)))
    (if entry (cdr entry) 0.5d0)))

(defun kelly-rating (adjusted-fraction)
  "ADJUSTED-FRACTION as (values RATING MESSAGE), at the original's two edges."
  (let ((pct (* (f adjusted-fraction) 100.0d0)))
    (cond ((> pct 15) (values "too_aggressive" "Aggressive - variance will be high"))
          ((< pct 3) (values "conservative" "Conservative - low variance"))
          (t (values "optimal" "Optimal - sharp money standard")))))

;;; --- Bayesian updating -----------------------------------------------------

(defun bayes-posterior (prior if-true if-false)
  "PRIOR updated by one piece of evidence: P(evidence | true) is IF-TRUE and
P(evidence | false) is IF-FALSE, all three in percent."
  (let* ((p (/ (f prior) 100.0d0))
         (likelihood-true (/ (f if-true) 100.0d0))
         (likelihood-false (/ (f if-false) 100.0d0))
         (numerator (* likelihood-true p))
         (denominator (+ numerator (* likelihood-false (- 1.0d0 p)))))
    (if (zerop denominator)
        (f prior)
        (round-to (* (/ numerator denominator) 100.0d0) 2))))

;;; --- probability, American and decimal odds --------------------------------

(defun prob-to-decimal (prob)
  "PROB (a 0-1 fraction) as decimal odds; zero outside 0..1."
  (let ((p (f prob)))
    (if (or (<= p 0.0d0) (> p 1.0d0)) 0.0d0 (/ 1.0d0 p))))

(defun decimal-to-prob (decimal)
  "DECIMAL odds as a 0-1 probability; zero at or below zero."
  (let ((d (f decimal)))
    (if (<= d 0.0d0) 0.0d0 (/ 1.0d0 d))))

(defun prob-to-american (prob)
  "PROB (a 0-1 fraction) as American odds, the favorite signed minus; zero at
the edges."
  (let ((p (f prob)))
    (cond ((or (<= p 0.0d0) (>= p 1.0d0)) 0)
          ((>= p 0.5d0) (- (round-int (* (/ p (- 1.0d0 p)) 100.0d0))))
          (t (round-int (* (/ (- 1.0d0 p) p) 100.0d0))))))

(defun american-to-prob (american)
  "AMERICAN odds as a 0-1 probability; zero for odds of zero."
  (let ((a (f american)))
    (cond ((zerop a) 0.0d0)
          ((> a 0) (/ 100.0d0 (+ a 100.0d0)))
          (t (/ (- a) (+ (- a) 100.0d0))))))

(defun american-to-decimal (american)
  "AMERICAN odds as decimal odds; zero for odds of zero."
  (let ((a (f american)))
    (cond ((zerop a) 0.0d0)
          ((> a 0) (+ (/ a 100.0d0) 1.0d0))
          (t (+ (/ 100.0d0 (- a)) 1.0d0)))))

(defun decimal-to-american (decimal)
  "DECIMAL odds as American odds; zero at or below even money."
  (let ((d (f decimal)))
    (if (<= d 1.0d0) 0 (prob-to-american (/ 1.0d0 d)))))

;;; --- base rates ------------------------------------------------------------

(defun base-rate-gap-value (base-rate market-price)
  "The gap in percentage points between BASE-RATE and MARKET-PRICE."
  (round-to (- (f base-rate) (f market-price)) 2))

(defun base-rate-signal (gap)
  "GAP's (values SIGNAL LABEL) at the original's two thresholds."
  (let ((gap (f gap)))
    (cond ((> gap 10) (values "strong_buy" "Strong BUY"))
          ((> gap 5) (values "buy" "BUY"))
          ((< gap -10) (values "strong_sell" "Strong SELL"))
          ((< gap -5) (values "sell" "SELL"))
          (t (values "neutral" "NEUTRAL")))))

(defun base-rate-quality (sample-size)
  "SAMPLE-SIZE as the original's quality word."
  (cond ((null sample-size) "unknown")
        ((>= sample-size 20) "high")
        ((>= sample-size 10) "medium")
        (t "low")))

(defun base-rate-interpretation (gap base-rate)
  "GAP and BASE-RATE in one sentence."
  (format nil "Market pricing ~,1fpp ~a the ~d% historical average"
          (abs (f gap))
          (if (> (f gap) 0) "lower than" "higher than")
          (round-int base-rate)))

;;; --- combo edge ------------------------------------------------------------
;;; A combo is graded against a fair-value BAND, never against the product of
;;; its leg prices: no venue pays that product on legs that settle together.

(defparameter +leg-uncertainty+ 0.03d0 "Model error seeded per leg.")
(defparameter +min-uncertainty+ 0.06d0 "The band never narrows past this.")
(defparameter +max-uncertainty+ 0.25d0 "Nor widens past this.")
(defparameter +maker-margin+ 0.017d0 "What a maker quotes above fair, on the joint.")
(defparameter +combo-epsilon+ 1d-9)

(defun combo-band (joint-probability leg-count)
  "The fair-value band around JOINT-PROBABILITY for LEG-COUNT legs, as a
plist of :uncertainty :p-low :p-high :low-decimal :high-decimal."
  (let* ((legs (max 1 leg-count))
         (u (min +max-uncertainty+
                 (max +min-uncertainty+
                      (- 1.0d0 (expt (- 1.0d0 +leg-uncertainty+) legs)))))
         (p-low (* (f joint-probability) (- 1.0d0 u)))
         (p-high (min (* (f joint-probability) (+ 1.0d0 u)) 0.99d0)))
    (list :uncertainty u :p-low p-low :p-high p-high
          :low-decimal (if (> p-high 0.0d0) (/ 1.0d0 p-high) 0.0d0)
          :high-decimal (if (> p-low 0.0d0) (/ 1.0d0 p-low) 0.0d0))))

(defun combo-verdict (ev-pct win-probability trap)
  "The original's 7-tier verdict, off EV, the win probability and the trap flag."
  (let ((ev (f ev-pct)) (win (f win-probability)))
    (cond ((or trap (<= ev -5.0d0)) "RUN")
          ((>= ev 1.5d0)
           (cond ((< win 0.25d0) "RISK")
                 ((>= ev 12.0d0) "SMASH")
                 ((>= ev 5.0d0) "PLAY")
                 (t "LEAN")))
          ((>= win 0.5d0) "NO_VALUE")
          (t "PASS"))))

(defun combo-verdict-for-offered (ev-pct win-probability trap offered-decimal band)
  "COMBO-VERDICT, narrowed by where OFFERED-DECIMAL falls in BAND: an
unplayable quote is PASS, a good one inside the band is LEAN."
  (let ((verdict (combo-verdict ev-pct win-probability trap))
        (offered (f offered-decimal))
        (low (getf band :low-decimal))
        (high (getf band :high-decimal)))
    (cond ((<= offered (+ low +combo-epsilon+))
           (if (or (equal verdict "RUN") (equal verdict "NO_VALUE")) verdict "PASS"))
          ((<= offered (+ high +combo-epsilon+))
           (if (member verdict '("SMASH" "PLAY" "RISK") :test #'equal) "LEAN" verdict))
          (t verdict))))

(defun offered-to-decimal (value)
  "The combo price VALUE as a decimal multiplier: American odds (a whole number
of 100 and up, or a negative), else a multiplier above 1.0; NIL when neither."
  (when (numberp value)
    (let ((v (f value)))
      (cond ((or (minusp v) (and (= v (round v)) (>= (abs v) 100.0d0)))
             (let ((d (american-to-decimal v)))
               (and (> d 1.0d0) d)))
            ((> v 1.0d0) v)
            (t nil)))))
