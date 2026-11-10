;;;; stats.lisp --- the two exact tests this cell decides with.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Both are exact: they sum probabilities rather than approximate them, so
;;;; a run of fourteen questions against a baseline of five such runs needs no
;;;; asymptotic hand-waving. Nothing here knows what a question is.

(in-package #:nodecode-model-drift-watch)

(defun choose (n k)
  "The binomial coefficient of N over K, as an integer."
  (let ((k (min k (- n k))))
    (loop with value = 1
          for i from 0 below k
          do (setf value (/ (* value (- n i)) (1+ i)))
          finally (return value))))

(defun binomial-two-sided (hits trials)
  "The two-sided exact p of HITS out of TRIALS at a half, as a double."
  (if (or (zerop trials) (minusp hits) (> hits trials))
      1d0
      (let* ((total (expt 2 trials))
             (at-most (loop for i from 0 to hits sum (choose trials i)))
             (at-least (loop for i from hits to trials sum (choose trials i))))
        (min 1d0 (* 2d0 (float (/ (min at-most at-least) total) 1d0))))))

(defun fisher-lower (passed-a failed-a passed-b failed-b)
  "The exact one-sided p of PASSED-B out of the second row's draws being at
most what it is, given the two rows pooled: the chance that the second
measurement is this much worse than the first by luck alone. Row A is the
baseline, row B the run being tested."
  (let* ((trials (+ passed-a failed-a passed-b failed-b))
         (successes (+ passed-a passed-b))
         (draws (+ passed-b failed-b)))
    (if (zerop draws)
        1d0
        (let ((total (choose trials draws))
              (low (max 0 (- draws (- trials successes))))
              (high (min draws successes)))
          (if (> low high)
              1d0
              (let ((tail (loop for x from low to (min passed-b high)
                                sum (/ (* (choose successes x)
                                          (choose (- trials successes) (- draws x)))
                                       total))))
                (min 1d0 (float tail 1d0))))))))

(defun rate (passed failed)
  "PASSED over the two, as a double; 0.0 when nothing was measured."
  (let ((total (+ passed failed)))
    (if (zerop total) 0d0 (float (/ passed total) 1d0))))
