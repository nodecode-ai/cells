;;;; detect.lisp --- the flyover model and the fit that finds one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A straight pass at constant speed V, closest approach slant distance D at
;;;; time TC, emitting a rest tone F0, is heard as
;;;;
;;;;   f(t) = F0 * c / (c + vr(t)),   vr(t) = V * x / sqrt(D^2 + x^2),
;;;;   x = V * (t - TC)
;;;;
;;;; so the tone is highest at closest approach and falls away either side,
;;;; the drop and the steepness of the drop being V and D. The fit is the
;;;; original's, done by search instead of scipy: a coarse grid over (V, D,
;;;; TC) with F0 solved in closed form for each cell, then a refinement
;;;; around the winner.

(in-package #:nodecode-doppler-geo)

(defparameter *sound-speed* 343d0
  "Metres per second at 20 C. The original corrected it with Open-Meteo.")

(defun fit-curve (points c v d tc)
  "Least-squares F0 for one (V, D, TC) over POINTS ((T F) ...).
=> residual rms in Hz, and F0."
  (let ((sgg 0d0) (sfg 0d0) (n 0))
    (dolist (p points)
      (let* ((x (* v (- (first p) tc)))
             (r (sqrt (+ (* d d) (* x x))))
             (g (/ c (+ c (* v (/ x r))))))
        (incf sgg (* g g))
        (incf sfg (* (second p) g))
        (incf n)))
    (if (or (zerop n) (not (plusp sgg)))
        (values most-positive-double-float 0d0)
        (let ((f0 (/ sfg sgg)) (ss 0d0))
          (dolist (p points)
            (let* ((x (* v (- (first p) tc)))
                   (r (sqrt (+ (* d d) (* x x))))
                   (g (/ c (+ c (* v (/ x r)))))
                   (e (- (second p) (* f0 g))))
              (incf ss (* e e))))
          (values (sqrt (/ ss n)) f0)))))

(defun curve-fit (points c vs ds tcs)
  "The best (F0 V D TC) over the grid of VS x DS x TCS. => list, rms."
  (let ((best nil) (best-rms most-positive-double-float))
    (dolist (v vs)
      (dolist (d ds)
        (dolist (tc tcs)
          (multiple-value-bind (rms f0) (fit-curve points c v d tc)
            (when (< rms best-rms)
              (setf best-rms rms best (list f0 (as-double v) (as-double d) (as-double tc))))))))
    (values best best-rms)))

(defun spread-hz (points)
  "How far apart the lowest and highest tone of POINTS are."
  (let ((lo most-positive-double-float) (hi most-negative-double-float))
    (dolist (p points) (setf lo (min lo (second p)) hi (max hi (second p))))
    (if (zerop (length points)) 0d0 (- hi lo))))

(defun detect-events (samples rate &key (fmin 60d0) (fmax 400d0) (max-events 3)
                                        (z-min 4d0) (sound-speed *sound-speed*)
                                        (window 8d0) (min-frames 10) (rms-max 2.5d0))
  "Flyover events in SAMPLES at RATE, newest fit per event, in time order.
Each event is a plist (:cpa-time :rest-hz :speed-mps :slant-m :rms-hz :frames
:z :points), POINTS its ((T F) ...) curve for the scorer."
  (let* ((peaks (spectrogram-peaks samples rate :fmin fmin :fmax fmax :z-min z-min))
         (events '()))
    (dolist (cand (if (> (length peaks) 8)
                      (subseq (sort (copy-list peaks) #'> :key #'third) 0 8)
                      peaks))
      (let* ((tc (first cand))
             (near (remove-if-not (lambda (p) (<= (abs (- (first p) tc)) window)) peaks)))
        (when (>= (length near) min-frames)
          (let* ((points (mapcar (lambda (p) (list (first p) (second p))) near))
                 (vs (loop for v from 30d0 to 300d0 by 20d0 collect v))
                 (ds (loop for d in '(150d0 300d0 500d0 800d0 1200d0 1800d0 2600d0 3600d0 5000d0)
                           collect d))
                 (tcs (loop for s from -1.5d0 to 1.5d0 by 0.5d0 collect (+ tc s))))
            (multiple-value-bind (coarse coarse-rms)
                (curve-fit points sound-speed vs ds tcs)
              (when coarse
                (destructuring-bind (f0 v d t-best) coarse
                  (declare (ignore f0))
                  (let* ((v2 (loop for s from -20d0 to 20d0 by 4d0 collect (max 10d0 (+ v s))))
                         (d2 (loop for k in '(0.7d0 0.85d0 1d0 1.18d0 1.4d0) collect (* d k)))
                         (tc2 (loop for s from -0.25d0 to 0.25d0 by 0.05d0 collect (+ t-best s))))
                    (multiple-value-bind (best rms) (curve-fit points sound-speed v2 d2 tc2)
                      (when (and best (< rms rms-max) (< coarse-rms rms-max))
                        (destructuring-bind (f0 v d tc) best
                          (when (and (> f0 0d0)
                                     (> (spread-hz points) (* 0.004d0 f0))
                                     (notany (lambda (e) (< (abs (- (getf e :cpa-time) tc)) 1.5d0))
                                             events))
                            (push (list :cpa-time tc :rest-hz f0 :speed-mps v :slant-m d
                                        :rms-hz rms :frames (length points)
                                        :z (/ (reduce #'+ near :key #'third) (length near))
                                        :points points)
                                  events)))))))))))))
    (let ((ordered (sort events #'< :key (lambda (e) (getf e :cpa-time)))))
      (if (> (length ordered) max-events) (subseq ordered 0 max-events) ordered))))
