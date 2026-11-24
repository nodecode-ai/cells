;;;; stats.lisp --- the small-sample statistics the verdict is measured on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A pilot eval runs on 40-100 real cases, not 10,000, and at that size
;;;; the point estimate is not the answer: the interval is. Wilson score
;;;; intervals because they stay inside [0, 1] and behave sanely at small n;
;;;; a sample size at which a rate would clear a target; a risk-coverage
;;;; curve for the confidence threshold that routes the uncertain cases to a
;;;; person; expected calibration error; and McNemar's test on paired case
;;;; outcomes.

(in-package #:nodecode-gonogo)

;;; --- the normal quantile ------------------------------------------------------

(defparameter +z-values+
  '((0.80d0 . 1.2815515655446004d0)
    (0.90d0 . 1.6448536269514722d0)
    (0.95d0 . 1.959963984540054d0)
    (0.99d0 . 2.5758293035489004d0))
  "z for a two-sided interval, at the levels people actually ask for.")

(defun z-for (level)
  "The z value of a two-sided interval at LEVEL, as a double."
  (let ((known (assoc (coerce level 'double-float) +z-values+
                       :test (lambda (a b) (< (abs (- a b)) 1d-9)))))
    (if known
        (cdr known)
        (normal-quantile (/ (+ 1d0 (coerce level 'double-float)) 2d0)))))

(defun normal-quantile (p)
  "The standard normal quantile at P (Acklam's rational approximation)."
  (unless (< 0d0 p 1d0)
    (fail "confidence level must be in (0, 1), got ~s" p))
  (let ((a #(-3.969683028665376d1 2.209460984245205d2 -2.759285104469687d2
             1.383577518672690d2 -3.066479806614716d1 2.506628277459239d0))
        (b #(-5.447609879822406d1 1.615858368580409d2 -1.556989798598866d2
             6.680131188771972d1 -1.328068155288572d1))
        (c #(-7.784894002430293d-3 -3.223964580411365d-1 -2.400758277161838d0
             -2.549732539343734d0 4.374664141464968d0 2.938163982698783d0))
        (d #(7.784695709041462d-3 3.224671290700398d-1 2.445134137142996d0
             3.754408661907416d0))
        (low 0.02425d0)
        (high (- 1d0 0.02425d0)))
    (labels ((horner (v q) (reduce (lambda (acc coefficient) (+ (* acc q) coefficient))
                                   v :start 1 :initial-value (aref v 0)))
             (ratio (q) (/ (horner c q) (+ (* (horner d q) q) 1d0))))
      (cond ((< p low) (ratio (sqrt (* -2d0 (log p)))))
            ((> p high) (- (ratio (sqrt (* -2d0 (log (- 1d0 p)))))))
            (t (let* ((q (- p 0.5d0))
                      (r (* q q)))
                 (/ (* (horner a r) q) (+ (* (horner b r) r) 1d0))))))))

;;; --- an interval, and how it prints -------------------------------------------

(defstruct (interval (:constructor %interval))
  "A proportion with a confidence interval."
  (point nil) (low 0d0) (high 1d0) (level 0.95d0) (n 0))

(defun interval-text (interval)
  "INTERVAL as an agent quotes it: 94.0% [83.8%, 97.9%]."
  (format nil "~,1f% [~,1f%, ~,1f%]"
          (* 100d0 (interval-point interval))
          (* 100d0 (interval-low interval))
          (* 100d0 (interval-high interval))))

(defun wilson (successes n &optional (level 0.95d0))
  "The Wilson score interval for SUCCESSES out of N at LEVEL."
  (unless (and (integerp successes) (integerp n) (not (minusp successes)) (not (minusp n)))
    (fail "successes and n must be non-negative integers, got ~s of ~s" successes n))
  (when (> successes n)
    (fail "successes (~d) cannot exceed n (~d)" successes n))
  (if (zerop n)
      (%interval :point nil :low 0d0 :high 1d0 :level level :n 0)
      (let* ((z (z-for level))
             (p (/ (coerce successes 'double-float) n))
             (denominator (+ 1d0 (/ (* z z) n)))
             (center (/ (+ p (/ (* z z) (* 2.0d0 n))) denominator))
             (half (/ (* z (sqrt (+ (/ (* p (- 1d0 p)) n)
                                    (/ (* z z) (* 4.0d0 n n)))))
                      denominator)))
        (%interval :point p :low (max 0d0 (- center half))
                   :high (min 1d0 (+ center half)) :level level :n n))))

(defun required-n (rate target &optional (level 0.95d0) (max-n 100000))
  "The smallest N at which RATE would clear TARGET as a lower bound, or NIL
when no sample size would (RATE at or below TARGET)."
  (when (<= rate target)
    (return-from required-n nil))
  (let ((lo 1) (hi 1))
    (loop
      (when (> hi max-n) (return-from required-n nil))
      (when (>= (interval-low (wilson (round (* rate hi)) hi level)) target)
        (return))
      (setf lo hi hi (* 2 hi)))
    ;; the predicate jitters by a case between adjacent n, so this lands
    ;; within a few of the true minimum: "roughly how many", not a guarantee
    (loop while (< lo hi)
          do (let ((mid (floor (+ lo hi) 2)))
               (if (>= (interval-low (wilson (round (* rate mid)) mid level)) target)
                   (setf hi mid)
                   (setf lo (1+ mid)))))
    lo))

;;; --- printing a cut point and a share -----------------------------------------

(defun fmt-cut (x)
  "X printed so it reproduces itself: 2 places when they are enough, else 3, else 4."
  (let ((four (format nil "~,4f" x)))
    (or (loop for places from 2 to 3
              for text = (format nil (format nil "~~,~df" places) x)
              when (string= four (format nil "~,4f" (read-from-string text)))
                return text)
        four)))

(defun fmt-share (fraction)
  "FRACTION as a percent: whole when it is whole, else one place (62.5%)."
  (let ((tenths (round (* fraction 1000d0))))
    (if (zerop (mod tenths 10))
        (format nil "~d%" (round tenths 10))
        (format nil "~,1f%" (/ tenths 10.0d0)))))

;;; --- the risk-coverage curve ---------------------------------------------------

(defstruct (operating-point (:constructor %operating-point))
  "What you get if the agent abstains below THRESHOLD."
  (threshold 0d0) (coverage 0d0) (n-covered 0) (n-deferred 0) (precision nil))

(defun risk-coverage (trials &optional (level 0.95d0))
  "One OPERATING-POINT per distinct confidence in TRIALS, least selective first.
A trial is (CONFIDENCE . PASSED)."
  (let ((n (length trials)))
    (loop for threshold in (sort (remove-duplicates (mapcar #'car trials)) #'<)
          for covered = (remove-if-not (lambda (trial) (>= (car trial) threshold)) trials)
          when covered
            collect (%operating-point
                     :threshold threshold
                     :coverage (/ (length covered) (float n))
                     :n-covered (length covered)
                     :n-deferred (- n (length covered))
                     :precision (wilson (count-if #'cdr covered) (length covered) level)))))

(defun best-operating-point (trials target &optional (min-coverage 0d0) (level 0.95d0))
  "The highest-coverage threshold whose precision LOWER bound clears TARGET."
  (let ((candidates (remove-if-not
                      (lambda (point)
                        (and (>= (interval-low (operating-point-precision point)) target)
                             (>= (operating-point-coverage point) min-coverage)))
                      (risk-coverage trials level))))
    (first (sort candidates #'> :key #'operating-point-coverage))))

(defun expected-calibration-error (trials &optional (bins 10))
  "Expected calibration error: does stated confidence match observed accuracy?
NIL when there are no trials."
  (let ((n (length trials)))
    (if (zerop n)
        nil
        (loop for b from 0 below bins
              for lo = (/ (coerce b 'double-float) bins)
              for hi = (/ (coerce (1+ b) 'double-float) bins)
              for bucket = (remove-if-not
                            (lambda (trial)
                              (let ((c (car trial)))
                                (or (and (<= lo c) (< c hi))
                                    (and (= b (1- bins)) (= c hi)))))
                            trials)
              when bucket
                sum (* (/ (length bucket) (float n))
                       (abs (- (/ (count-if #'cdr bucket) (float (length bucket)))
                               (/ (reduce #'+ bucket :key #'car)
                                  (float (length bucket))))))))))

;;; --- two agents on the same cases ----------------------------------------------

(defparameter +exact-limit+ 1000
  "Above this many disagreements the exact sum is slow and the chi-square
approximation is indistinguishable from it anyway.")

(defun binomial (n k)
  "The binomial coefficient of N and K."
  (let ((k (min k (- n k)))
        (result 1))
    (loop for i from 1 to k
          do (setf result (floor (* result (+ 1 (- n i))) i)))
    result))

(defun erfc (x)
  "The complementary error function (Abramowitz & Stegun 7.1.26, to ~1.5e-7)."
  (let* ((absolute (abs (coerce x 'double-float)))
         (factor (/ 1d0 (+ 1d0 (* 0.3275911d0 absolute))))
         (poly (* factor (+ 0.254829592d0
                             (* factor (+ -0.284496736d0
                                          (* factor (+ 1.421413741d0
                                                       (* factor (+ -1.453152027d0
                                                                    (* factor 1.061405429d0))))))))))
         (erf (- 1d0 (* poly (exp (- (* absolute absolute)))))))
    (if (minusp x) (+ 1d0 erf) (- 1d0 erf))))

(defun mcnemar-p (only-a only-b)
  "Two-sided McNemar p for the two disagreement counts => (values P EXACT)."
  (let ((n (+ only-a only-b)))
    (cond ((zerop n) (values 1d0 t))
          ((<= n +exact-limit+)
           (let* ((larger (max only-a only-b))
                  (tail (loop for i from larger to n sum (binomial n i))))
             (values (min 1d0 (* 2d0 (/ (coerce tail 'double-float)
                                         (coerce (ash 1 n) 'double-float))))
                     t)))
          (t (let ((chi (/ (expt (- (abs (- only-a only-b)) 1) 2) (float n))))
               (values (erfc (sqrt (/ chi 2.0d0))) nil))))))

(defun paired-difference-interval (only-a only-b n level)
  "A conservative interval on P(only B) - P(only A) => (values LOW HIGH).
Simultaneous Wilson bounds on the two discordant shares, which do not
collapse to zero width at a boundary outcome."
  (let* ((component (- 1d0 (/ (- 1d0 level) 2d0)))
         (a (wilson only-a n component))
         (b (wilson only-b n component)))
    (values (max -1d0 (- (interval-low b) (interval-high a)))
            (min 1d0 (- (interval-high b) (interval-low a))))))
