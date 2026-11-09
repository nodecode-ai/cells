;;;; dsp.lisp --- one FFT and the peaks of a spectrogram.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original used numpy's rfft over a Hann-windowed spectrogram and
;;;; read one ridge per frame. This is the same thing in plain Lisp: an
;;;; in-place radix-2 FFT, a Hann window, and for each frame the strongest
;;;; bin in the band, refined by a parabola through its neighbours, with its
;;;; significance as a robust z score against the frame's own noise floor.

(in-package #:nodecode-doppler-geo)

(defun hann (n)
  "A Hann window of N points, as a double-float vector."
  (let ((w (make-array n :element-type 'double-float)))
    (dotimes (i n)
      (setf (aref w i)
            (* 0.5d0 (- 1d0 (cos (/ (* 2d0 pi i) (max 1 (1- n))))))))
    w))

(defun fft! (re im)
  "In-place radix-2 FFT of the complex vector RE + i IM; length a power of two."
  (let* ((n (length re))
         (j 0))
    (dotimes (i (1- n))
      (when (< i j)
        (rotatef (aref re i) (aref re j))
        (rotatef (aref im i) (aref im j)))
      (let ((m (ash n -1)))
        (loop while (and (>= m 1) (>= j m))
              do (decf j m) (setf m (ash m -1)))
        (incf j m)))
    (loop for len = 2 then (* 2 len) while (<= len n)
          do (let* ((half (ash len -1))
                    (ang (/ (* -2d0 pi) len))
                    (wr (cos ang))
                    (wi (sin ang)))
               (loop for start from 0 below n by len
                     do (let ((cr 1d0) (ci 0d0))
                          (dotimes (k half)
                            (let* ((a (+ start k))
                                   (b (+ a half))
                                   (xr (- (* (aref re b) cr) (* (aref im b) ci)))
                                   (xi (+ (* (aref re b) ci) (* (aref im b) cr)))
                                   (tr (+ (aref re a) xr))
                                   (ti (+ (aref im a) xi)))
                              (setf (aref re b) (- (aref re a) xr)
                                    (aref im b) (- (aref im a) xi)
                                    (aref re a) tr
                                    (aref im a) ti)
                              (let ((nr (- (* cr wr) (* ci wi))))
                                (setf ci (+ (* cr wi) (* ci wr))
                                      cr nr)))))))))
  (values re im))

(defun median (vector)
  "The median of a vector of doubles, copying it."
  (let* ((n (length vector))
         (copy (sort (copy-seq vector) #'<)))
    (if (zerop n) 0d0
        (if (oddp n) (aref copy (ash n -1))
            (/ (+ (aref copy (1- (ash n -1))) (aref copy (ash n -1))) 2d0)))))

(defparameter *frame-size* 4096
  "Samples per analysis frame: 2.7 Hz per bin at 11 kHz, 1.9 Hz at 8 kHz.")

(defparameter *hop* 1024
  "Samples between frames: 128 ms at 8 kHz.")

(defun spectrogram-peaks (samples rate &key (fmin 60d0) (fmax 400d0)
                                       (size *frame-size*) (hop *hop*) (z-min 4d0))
  "One row per frame: (TIME-S F-HZ Z), the strongest tone of the band, when its
z score against the frame's noise floor clears Z-MIN. => list, in time order."
  (let* ((n (length samples))
         (win (hann size))
         (re (make-array size :element-type 'double-float :initial-element 0d0))
         (im (make-array size :element-type 'double-float :initial-element 0d0))
         (mags (make-array (ash size -1) :element-type 'double-float :initial-element 0d0))
         (band (make-array 2 :element-type 'double-float :fill-pointer 0 :adjustable t))
         (bin (/ (as-double rate) size))
         (lo (max 1 (floor fmin bin)))
         (hi (min (1- (ash size -1)) (ceiling fmax bin)))
         (frames (1+ (floor (- n size) hop)))
         (out '()))
    (when (or (< n size) (>= lo hi))
      (return-from spectrogram-peaks '()))
    (loop for k below frames
          for start = (* k hop)
          for time = (/ (+ start (/ size 2d0)) (as-double rate))
          do (fill re 0d0)
             (fill im 0d0)
             (dotimes (i size)
               (setf (aref re i) (* (aref samples (+ start i)) (aref win i))))
             (fft! re im)
             (loop for i from 1 below (length mags)
                   do (setf (aref mags i)
                            (+ 1d-12 (sqrt (+ (* (aref re i) (aref re i))
                                              (* (aref im i) (aref im i)))))))
             (let ((best lo) (bestv 0d0))
               (loop for i from lo to hi
                     do (when (> (aref mags i) bestv)
                          (setf bestv (aref mags i) best i)))
               (setf (fill-pointer band) 0)
               (loop for i from lo to hi do (vector-push (aref mags i) band))
               (let* ((med (median band))
                      (dev (let ((d (make-array (length band) :element-type 'double-float)))
                             (loop for i below (length band)
                                   do (setf (aref d i) (abs (- (aref band i) med))))
                             d))
                      (mad (median dev))
                      (scale (* 1.4826d0 mad))
                      (z (if (plusp scale) (/ (- bestv med) scale) 0d0)))
                 (when (and (> z z-min) (> best lo) (< best hi))
                   (let* ((a (log (aref mags (1- best))))
                          (b (log (aref mags best)))
                          (c (log (aref mags (1+ best))))
                          (den (+ a c (* -2d0 b)))
                          (delta (if (minusp den) (/ (- a c) (* 2d0 den)) 0d0))
                          (freq (* bin (+ best delta))))
                     (push (list time freq z) out))))))
    (nreverse out)))
