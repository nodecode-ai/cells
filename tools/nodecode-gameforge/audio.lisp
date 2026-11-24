;;;; audio.lisp --- sound effects and music, synthesized offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The sound-effect half is the sfxr algorithm the original plugin carries:
;;;; the same parameter names and ranges (wave_type, env_attack, base_freq,
;;;; freq_ramp, arp_mod, lpf_freq...), the same 8x supersampled render loop,
;;;; the same generators, and the same promise that a seed makes a variation.
;;;; The music half composes a loop from a mood's mode, tempo and chord
;;;; progression: bass, an arpeggio, a seeded melody and drums, mixed into
;;;; one buffer whose length is exactly the loop.

(in-package #:nodecode-gameforge)

(defparameter +wave-square+ 0)
(defparameter +wave-saw+ 1)
(defparameter +wave-sine+ 2)
(defparameter +wave-noise+ 3)

(defparameter +wave-names+ '(("square" . 0) ("saw" . 1) ("sawtooth" . 1)
                             ("sine" . 2) ("noise" . 3))
  "The wave_type spellings a call may use, and the waveform each names.")

(defun wave-number (name)
  "NAME as a wave_type; 0 (square) for a spelling this cell does not carry."
  (let ((found (assoc (string-downcase (string name)) +wave-names+ :test #'string=)))
    (if found (cdr found) +wave-square+)))

;;; --- one sound effect's parameters ------------------------------------------

(defstruct sfx-params
  (wave-type +wave-square+ :type fixnum)
  (env-attack 0.0d0 :type double-float)
  (env-sustain 0.3d0 :type double-float)
  (env-punch 0.0d0 :type double-float)
  (env-decay 0.4d0 :type double-float)
  (base-freq 0.3d0 :type double-float)
  (freq-limit 0.0d0 :type double-float)
  (freq-ramp 0.0d0 :type double-float)
  (freq-dramp 0.0d0 :type double-float)
  (vib-strength 0.0d0 :type double-float)
  (vib-speed 0.0d0 :type double-float)
  (arp-mod 0.0d0 :type double-float)
  (arp-speed 0.0d0 :type double-float)
  (duty 0.0d0 :type double-float)
  (duty-ramp 0.0d0 :type double-float)
  (repeat-speed 0.0d0 :type double-float)
  (pha-offset 0.0d0 :type double-float)
  (pha-ramp 0.0d0 :type double-float)
  (lpf-freq 1.0d0 :type double-float)
  (lpf-ramp 0.0d0 :type double-float)
  (lpf-resonance 0.0d0 :type double-float)
  (hpf-freq 0.0d0 :type double-float)
  (hpf-ramp 0.0d0 :type double-float)
  (sound-vol 0.5d0 :type double-float))

(defparameter +sfx-parameter-names+
  '("wave-type" "env-attack" "env-sustain" "env-punch" "env-decay" "base-freq"
    "freq-limit" "freq-ramp" "freq-dramp" "vib-strength" "vib-speed" "arp-mod"
    "arp-speed" "duty" "duty-ramp" "repeat-speed" "pha-offset" "pha-ramp" "lpf-freq"
    "lpf-ramp" "lpf-resonance" "hpf-freq" "hpf-ramp" "sound-vol")
  "Every parameter a call may override, by its sfxr/jsfxr name.")

(defun apply-overrides (params overrides)
  "PARAMS with each (NAME . VALUE) of OVERRIDES set. A name this cell does
not carry is ignored, as the original's own reader ignores it."
  (dolist (pair overrides params)
    (let ((name (string-downcase (string (car pair)))))
      (when (member name +sfx-parameter-names+ :test #'string=)
        (if (string= name "wave-type")
            (let ((value (cdr pair)))
              (setf (sfx-params-wave-type params)
                    (if (stringp value) (wave-number value) (truncate value))))
            (setf (slot-value params (intern (string-upcase name) '#:nodecode-gameforge))
                  (coerce (cdr pair) 'double-float)))))))

;;; --- the frequency registers ------------------------------------------------

(defstruct freq-registers
  (fperiod 0.0d0 :type double-float)
  (fmaxperiod 0.0d0 :type double-float)
  (fslide 1.0d0 :type double-float)
  (fdslide 0.0d0 :type double-float)
  (arp-mod 1.0d0 :type double-float)
  (arp-time 0 :type fixnum)
  (arp-limit 0 :type fixnum))

(defun reset-registers (registers p)
  "The frequency and arpeggio state sfxr re-initialises on every repeat."
  (setf (freq-registers-fperiod registers) (/ 100.0d0 (+ (expt (sfx-params-base-freq p) 2) 0.001d0))
        (freq-registers-fmaxperiod registers) (/ 100.0d0 (+ (expt (sfx-params-freq-limit p) 2) 0.001d0))
        (freq-registers-fslide registers) (- 1.0d0 (* (expt (sfx-params-freq-ramp p) 3) 0.01d0))
        (freq-registers-fdslide registers) (* (- (expt (sfx-params-freq-dramp p) 3)) 0.000001d0)
        (freq-registers-arp-mod registers)
        (if (minusp (sfx-params-arp-mod p))
            (+ 1.0d0 (* (expt (sfx-params-arp-mod p) 2) 10.0d0))
            (- 1.0d0 (* (expt (sfx-params-arp-mod p) 2) 0.9d0)))
        (freq-registers-arp-time registers) 0
        (freq-registers-arp-limit registers)
        (if (= (sfx-params-arp-speed p) 1.0d0)
            0
            (truncate (+ (* (expt (- 1.0d0 (sfx-params-arp-speed p)) 2) 20000.0d0) 32.0d0))))
  registers)

;;; --- the render loop ---------------------------------------------------------

(defun synth-sfx (p rng &key (max-seconds 3.0d0))
  "P rendered at +RATE+ into a vector of samples in -1..1."
  (let* ((registers (reset-registers (make-freq-registers) p))
         (square-duty (- 0.5d0 (* (sfx-params-duty p) 0.5d0)))
         (square-slide (* (- (sfx-params-duty-ramp p)) 0.00005d0))
         (rep-limit (if (plusp (sfx-params-repeat-speed p))
                        (truncate (+ (* (expt (- 1.0d0 (sfx-params-repeat-speed p)) 2) 20000.0d0) 32.0d0))
                        0))
         (rep-time 0)
         (fltp 0.0d0) (fltdp 0.0d0) (fltphp 0.0d0)
         (fltw (* (expt (sfx-params-lpf-freq p) 3) 0.1d0))
         (fltw-d (+ 1.0d0 (* (sfx-params-lpf-ramp p) 0.0001d0)))
         (fltdmp (min 0.8d0 (* (/ 5.0d0 (+ 1.0d0 (* (expt (sfx-params-lpf-resonance p) 2) 20.0d0)))
                              (+ 0.01d0 fltw))))
         (flthp (* (expt (sfx-params-hpf-freq p) 2) 0.1d0))
         (flthp-d (+ 1.0d0 (* (sfx-params-hpf-ramp p) 0.0003d0)))
         (vib-phase 0.0d0)
         (vib-speed (* (expt (sfx-params-vib-speed p) 2) 0.01d0))
         (vib-amp (* (sfx-params-vib-strength p) 0.5d0))
         (env-length (vector (max 1 (truncate (* (expt (sfx-params-env-attack p) 2) 100000.0d0)))
                             (max 1 (truncate (* (expt (sfx-params-env-sustain p) 2) 100000.0d0)))
                             (max 1 (truncate (* (expt (sfx-params-env-decay p) 2) 100000.0d0)))))
         (env-stage 0) (env-time 0)
         (fphase (* (expt (sfx-params-pha-offset p) 2) 1020.0d0
                    (if (minusp (sfx-params-pha-offset p)) -1.0d0 1.0d0)))
         (fdphase (* (expt (sfx-params-pha-ramp p) 2)
                     (if (minusp (sfx-params-pha-ramp p)) -1.0d0 1.0d0)))
         (phaser (make-array 1024 :initial-element 0.0d0))
         (ipp 0)
         (noise (make-array 32 :element-type 'double-float))
         (phase 0)
         (limit (truncate (* (coerce max-seconds 'double-float) +rate+)))
         (out (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0)))
    (loop for index below 32 do (setf (aref noise index) (rrange rng -1.0d0 1.0d0)))
    (loop while (< (fill-pointer out) limit)
          do (incf rep-time)
             (when (and (plusp rep-limit) (>= rep-time rep-limit))
               (setf rep-time 0)
               (reset-registers registers p))
             (incf (freq-registers-arp-time registers))
             (when (and (plusp (freq-registers-arp-limit registers))
                        (>= (freq-registers-arp-time registers) (freq-registers-arp-limit registers)))
               (setf (freq-registers-arp-limit registers) 0)
               (setf (freq-registers-fperiod registers)
                     (* (freq-registers-fperiod registers) (freq-registers-arp-mod registers))))
             (incf (freq-registers-fslide registers) (freq-registers-fdslide registers))
             (setf (freq-registers-fperiod registers)
                   (* (freq-registers-fperiod registers) (freq-registers-fslide registers)))
             (when (> (freq-registers-fperiod registers) (freq-registers-fmaxperiod registers))
               (setf (freq-registers-fperiod registers) (freq-registers-fmaxperiod registers))
               (when (plusp (sfx-params-freq-limit p)) (return)))
             (let* ((rfperiod (if (plusp vib-amp)
                                  (progn (incf vib-phase vib-speed)
                                         (* (freq-registers-fperiod registers)
                                            (+ 1.0d0 (* (sin vib-phase) vib-amp))))
                                  (freq-registers-fperiod registers)))
                    (period (max 8 (truncate rfperiod))))
               (setf square-duty (min 0.5d0 (max 0.0d0 (+ square-duty square-slide))))
               (incf env-time)
               (when (> env-time (aref env-length env-stage))
                 (setf env-time 0)
                 (incf env-stage)
                 (when (= env-stage 3) (return)))
               (let ((env-vol (cond ((zerop env-stage)
                                     (/ (coerce env-time 'double-float) (aref env-length 0)))
                                    ((= env-stage 1)
                                     (+ 1.0d0 (* (- 1.0d0 (/ (coerce env-time 'double-float)
                                                             (aref env-length 1)))
                                                 2.0d0 (sfx-params-env-punch p))))
                                    (t (- 1.0d0 (/ (coerce env-time 'double-float)
                                                   (aref env-length 2)))))))
                 (incf fphase fdphase)
                 (let ((iphase (min 1023 (truncate (abs fphase)))))
                   (when (/= flthp-d 1.0d0)
                     (setf flthp (min 0.1d0 (max 0.00001d0 (* flthp flthp-d)))))
                   (let ((ssample 0.0d0))
                     (dotimes (sub 8)
                       (incf phase)
                       (when (>= phase period)
                         (setf phase (mod phase period))
                         (when (= (sfx-params-wave-type p) +wave-noise+)
                           (loop for index below 32
                                 do (setf (aref noise index) (rrange rng -1.0d0 1.0d0)))))
                       (let* ((fp (/ (coerce phase 'double-float) period))
                              (sample (case (sfx-params-wave-type p)
                                        (0 (if (< fp square-duty) 0.5d0 -0.5d0))
                                        (1 (- 1.0d0 (* fp 2.0d0)))
                                        (2 (sin (* fp 2.0d0 pi)))
                                        (t (aref noise (mod (truncate (/ (* phase 32.0d0) period)) 32))))))
                         (let ((pp fltp))
                           (setf fltw (min 0.1d0 (max 0.0d0 (* fltw fltw-d))))
                           (if (= (sfx-params-lpf-freq p) 1.0d0)
                               (setf fltp sample fltdp 0.0d0)
                               (progn (incf fltdp (* (- sample fltp) fltw))
                                      (decf fltdp (* fltdp fltdmp))))
                           (incf fltp fltdp)
                           (incf fltphp (- fltp pp))
                           (decf fltphp (* fltphp flthp))
                           (setf sample fltphp)
                           (setf (aref phaser (logand ipp 1023)) sample)
                           (incf sample (aref phaser (logand (+ (- ipp iphase) 1024) 1023)))
                           (setf ipp (logand (1+ ipp) 1023))
                           (incf ssample (* sample env-vol))))
                     (vector-push-extend
                      (max -1.0d0 (min 1.0d0 (* (/ ssample 8.0d0) 2.0d0 (sfx-params-sound-vol p))))
                      out)))))))
    out))

;;; --- the generators ----------------------------------------------------------

(defun gen-coin (rng)
  (let ((p (make-sfx-params :base-freq (rrange rng 0.4d0 0.9d0)
                             :env-sustain (rrange rng 0.0d0 0.1d0)
                             :env-decay (rrange rng 0.1d0 0.5d0)
                             :env-punch (rrange rng 0.3d0 0.6d0))))
    (when (rchance rng 0.5d0)
      (setf (sfx-params-arp-speed p) (rrange rng 0.5d0 0.7d0)
            (sfx-params-arp-mod p) (rrange rng 0.2d0 0.6d0)))
    p))

(defun gen-laser (rng)
  (let* ((wave (rchoice rng (vector +wave-square+ +wave-saw+ +wave-sine+)))
         (p (make-sfx-params :wave-type wave)))
    (when (and (= wave +wave-sine+) (rchance rng 0.5d0))
      (setf (sfx-params-wave-type p) (rchoice rng (vector +wave-square+ +wave-saw+))))
    (setf (sfx-params-base-freq p) (rrange rng 0.5d0 1.0d0))
    (setf (sfx-params-freq-limit p) (max 0.2d0 (- (sfx-params-base-freq p) 0.2d0 (rrange rng 0.0d0 0.6d0))))
    (setf (sfx-params-freq-ramp p) (rrange rng -0.35d0 -0.15d0))
    (when (zerop (rint rng 3))
      (setf (sfx-params-base-freq p) (rrange rng 0.3d0 0.9d0)
            (sfx-params-freq-limit p) (rrange rng 0.0d0 0.1d0)
            (sfx-params-freq-ramp p) (rrange rng -0.65d0 -0.35d0)))
    (if (rchance rng 0.5d0)
        (setf (sfx-params-duty p) (rrange rng 0.0d0 0.5d0)
              (sfx-params-duty-ramp p) (rrange rng 0.0d0 0.2d0))
        (setf (sfx-params-duty p) (rrange rng 0.4d0 0.9d0)
              (sfx-params-duty-ramp p) (rrange rng -0.7d0 0.0d0)))
    (setf (sfx-params-env-sustain p) (rrange rng 0.1d0 0.3d0)
          (sfx-params-env-decay p) (rrange rng 0.0d0 0.4d0))
    (when (rchance rng 0.5d0) (setf (sfx-params-env-punch p) (rrange rng 0.0d0 0.3d0)))
    (when (zerop (rint rng 3))
      (setf (sfx-params-pha-offset p) (rrange rng 0.0d0 0.2d0)
            (sfx-params-pha-ramp p) (rrange rng -0.2d0 0.0d0)))
    (when (rchance rng 0.5d0) (setf (sfx-params-hpf-freq p) (rrange rng 0.0d0 0.3d0)))
    p))

(defun gen-explosion (rng)
  (let ((p (make-sfx-params :wave-type +wave-noise+)))
    (if (rchance rng 0.5d0)
        (setf (sfx-params-base-freq p) (rrange rng 0.1d0 0.5d0)
              (sfx-params-freq-ramp p) (rrange rng -0.1d0 0.3d0))
        (setf (sfx-params-base-freq p) (rrange rng 0.2d0 0.9d0)
              (sfx-params-freq-ramp p) (rrange rng -0.4d0 -0.2d0)))
    (setf (sfx-params-base-freq p) (expt (sfx-params-base-freq p) 2))
    (when (zerop (rint rng 5)) (setf (sfx-params-freq-ramp p) 0.0d0))
    (when (zerop (rint rng 3)) (setf (sfx-params-repeat-speed p) (rrange rng 0.3d0 0.8d0)))
    (setf (sfx-params-env-sustain p) (rrange rng 0.1d0 0.4d0)
          (sfx-params-env-decay p) (rrange rng 0.0d0 0.5d0))
    (when (rchance rng 0.5d0)
      (setf (sfx-params-pha-offset p) (rrange rng -0.3d0 0.6d0)
            (sfx-params-pha-ramp p) (rrange rng -0.3d0 0.0d0)))
    (setf (sfx-params-env-punch p) (rrange rng 0.2d0 0.8d0))
    (when (rchance rng 0.5d0)
      (setf (sfx-params-vib-strength p) (rrange rng 0.0d0 0.7d0)
            (sfx-params-vib-speed p) (rrange rng 0.0d0 0.6d0)))
    (when (zerop (rint rng 3))
      (setf (sfx-params-arp-speed p) (rrange rng 0.6d0 0.9d0)
            (sfx-params-arp-mod p) (rrange rng -0.8d0 0.8d0)))
    p))

(defun gen-powerup (rng)
  (let ((p (make-sfx-params)))
    (if (rchance rng 0.5d0)
        (setf (sfx-params-wave-type p) +wave-saw+)
        (setf (sfx-params-duty p) (rrange rng 0.0d0 0.6d0)))
    (setf (sfx-params-base-freq p) (rrange rng 0.2d0 0.5d0))
    (if (rchance rng 0.5d0)
        (setf (sfx-params-freq-ramp p) (rrange rng 0.1d0 0.5d0)
              (sfx-params-repeat-speed p) (rrange rng 0.4d0 0.8d0))
        (progn
          (setf (sfx-params-freq-ramp p) (rrange rng 0.05d0 0.25d0))
          (when (rchance rng 0.5d0)
            (setf (sfx-params-vib-strength p) (rrange rng 0.0d0 0.7d0)
                  (sfx-params-vib-speed p) (rrange rng 0.0d0 0.6d0)))))
    (setf (sfx-params-env-sustain p) (rrange rng 0.0d0 0.4d0)
          (sfx-params-env-decay p) (rrange rng 0.1d0 0.5d0))
    p))

(defun gen-hit (rng)
  (let* ((wave (rchoice rng (vector +wave-square+ +wave-saw+ +wave-sine+)))
         (p (make-sfx-params :wave-type (if (= wave +wave-sine+) +wave-noise+ wave))))
    (when (= (sfx-params-wave-type p) +wave-square+)
      (setf (sfx-params-duty p) (rrange rng 0.0d0 0.6d0)))
    (setf (sfx-params-base-freq p) (rrange rng 0.2d0 0.8d0)
          (sfx-params-freq-ramp p) (rrange rng -0.7d0 -0.3d0)
          (sfx-params-env-sustain p) (rrange rng 0.0d0 0.1d0)
          (sfx-params-env-decay p) (rrange rng 0.1d0 0.3d0))
    (when (rchance rng 0.5d0) (setf (sfx-params-hpf-freq p) (rrange rng 0.0d0 0.3d0)))
    p))

(defun gen-jump (rng)
  (let ((p (make-sfx-params :duty (rrange rng 0.0d0 0.6d0)
                             :base-freq (rrange rng 0.3d0 0.6d0)
                             :freq-ramp (rrange rng 0.1d0 0.3d0)
                             :env-sustain (rrange rng 0.1d0 0.4d0)
                             :env-decay (rrange rng 0.1d0 0.3d0))))
    (when (rchance rng 0.5d0) (setf (sfx-params-hpf-freq p) (rrange rng 0.0d0 0.3d0)))
    (when (rchance rng 0.5d0) (setf (sfx-params-lpf-freq p) (rrange rng 0.4d0 1.0d0)))
    p))

(defun gen-blip (rng)
  (let ((p (make-sfx-params :wave-type (rchoice rng (vector +wave-square+ +wave-saw+))
                             :base-freq (rrange rng 0.2d0 0.6d0)
                             :env-sustain (rrange rng 0.1d0 0.2d0)
                             :env-decay (rrange rng 0.0d0 0.2d0)
                             :hpf-freq 0.1d0)))
    (when (= (sfx-params-wave-type p) +wave-square+)
      (setf (sfx-params-duty p) (rrange rng 0.0d0 0.6d0)))
    p))

(defparameter +sfx-presets+
  '(;; the sfxr generators, under every name the plugin answers to
    ("coin" . gen-coin) ("pickup" . gen-coin)
    ("laser" . gen-laser) ("shoot" . gen-laser)
    ("explosion" . gen-explosion)
    ("powerup" . gen-powerup)
    ("hit" . gen-hit) ("hurt" . gen-hit)
    ("jump" . gen-jump)
    ("blip" . gen-blip) ("select" . gen-blip))
  "The preset names a call may ask for, and the generator each runs.")

(defun preset-names ()
  (mapcar #'car +sfx-presets+))

(defun preset-params (preset rng)
  "The parameters PRESET generates from RNG, or a refusal naming the list."
  (let ((found (assoc (string-downcase (string preset)) +sfx-presets+ :test #'string=)))
    (unless found
      (fail "~a is not a preset this cell synthesizes; choose from ~{~a~^, ~}"
            preset (preset-names)))
    (funcall (cdr found) rng)))

;;; --- post-processing, and the sfxr payload ------------------------------------

(defun normalize-samples (samples &key (peak 0.9d0))
  "SAMPLES scaled so the loudest is PEAK, or as they are when they are silent."
  (let ((loudest (loop for sample across samples maximize (abs sample))))
    (if (plusp loudest)
        (let ((gain (/ peak loudest)))
          (map 'vector (lambda (sample) (* sample gain)) samples))
        samples)))

(defun lowpass-samples (samples cutoff)
  "One pole at CUTOFF Hz: what `distant' does to a sound."
  (let* ((alpha (/ (* 2.0d0 pi cutoff) (+ (* 2.0d0 pi cutoff) +rate+)))
         (out (copy-seq samples)) (state 0.0d0))
    (loop for index below (length out)
          do (incf state (* alpha (- (aref out index) state)))
             (setf (aref out index) state))
    out))

(defun bitcrush-samples (samples bits)
  "SAMPLES to BITS, with a two-sample hold: what `crunchy' does."
  (let* ((levels (expt 2.0d0 (max 1 (- bits 1))))
         (out (copy-seq samples)))
    (loop for index below (length out)
          do (setf (aref out index) (/ (round (* (aref out index) levels)) levels)))
    out))

(defun echo-samples (samples delay-seconds feedback wet)
  "SAMPLES with one echo, DELAY-SECONDS later: distance without a reverb unit."
  (let* ((delay (max 1 (truncate (* delay-seconds +rate+))))
         (out (copy-seq samples)))
    (loop for index from delay below (length out)
          do (incf (aref out index) (* wet feedback (aref samples (- index delay)))))
    out))

(defun polish-samples (samples style)
  "SAMPLES as STYLE asks: retro (as generated), modern (airy, quiet highs),
crunchy (5-bit and lo-fi), distant (muffled, echoing)."
  (let ((polished (cond ((string-equal style "crunchy") (bitcrush-samples samples 5))
                        ((string-equal style "distant") (echo-samples (lowpass-samples samples 1800.0d0)
                                                                        0.09d0 0.35d0 0.45d0))
                        ((string-equal style "modern") (lowpass-samples samples 9000.0d0))
                        (t samples))))
    (normalize-samples polished)))

(defun render-sfx (preset seed &key overrides (style "retro"))
  "A rendered sound: (values SAMPLES PARAMS). The seed fixes both the
generator's choices and the noise, so two calls with one seed are one sound."
  (let* ((rng (make-rng seed))
         (params (preset-params preset rng)))
    (when overrides (apply-overrides params overrides))
    (let ((samples (synth-sfx params rng :max-seconds 2.0d0)))
      (values (polish-samples samples style) params))))

;;; --- music -----------------------------------------------------------------------

(defparameter +modes+
  '(("major" 0 2 4 5 7 9 11)
    ("minor" 0 2 3 5 7 8 10)
    ("dorian" 0 2 3 5 7 9 10)
    ("phrygian" 0 1 3 5 7 8 10)
    ("mixolydian" 0 2 4 5 7 9 10)
    ("lydian" 0 2 4 6 7 9 11)
    ("harmonic_minor" 0 2 3 5 7 8 11))
  "The scales a mood may compose in, as semitones from the tonic.")

(defun mode-steps (mode)
  (let ((found (assoc (string-downcase (string mode)) +modes+ :test #'string=)))
    (unless found
      (fail "~a is not a mode this cell composes in; choose from ~{~a~^, ~}"
            mode (mapcar #'car +modes+)))
    (mapcar #'identity (cdr found))))

(defparameter +notes+
  '(("c" . 0) ("c#" . 1) ("db" . 1) ("d" . 2) ("d#" . 3) ("eb" . 3) ("e" . 4) ("f" . 5)
    ("f#" . 6) ("gb" . 6) ("g" . 7) ("g#" . 8) ("ab" . 8) ("a" . 9) ("a#" . 10) ("bb" . 10)
    ("b" . 11))
  "The note spellings a key may use, and the semitone each names.")

(defun note-offset (key)
  "KEY as semitones above C; a refusal for a spelling this cell does not know."
  (let* ((name (string-downcase (string key)))
         (found (assoc name +notes+ :test #'string=)))
    (unless found
      (fail "~a is not a key this cell knows; name a root note such as C, F#, Bb or A" key))
    (cdr found)))

(defstruct mood
  (name "happy")
  (mode "major")
  (bpm 120)
  (progressions '((0 5 3 4)))
  (drums "drive")
  (density 0.7d0)
  (arpeggio t))

(defparameter +moods+
  (list (make-mood :name "heroic" :mode "major" :bpm 132 :drums "march" :density 0.7d0 :arpeggio t
                   :progressions '((0 4 5 3) (0 3 4 4) (5 3 0 4)))
        (make-mood :name "happy" :mode "major" :bpm 120 :drums "bounce" :density 0.75d0 :arpeggio t
                   :progressions '((0 5 3 4) (0 3 0 4) (3 4 2 5)))
        (make-mood :name "adventure" :mode "mixolydian" :bpm 126 :drums "drive" :density 0.7d0 :arpeggio t
                   :progressions '((0 6 3 0) (0 3 6 4)))
        (make-mood :name "sad" :mode "minor" :bpm 78 :drums "sparse" :density 0.45d0 :arpeggio nil
                   :progressions '((0 5 2 6) (0 3 5 4)))
        (make-mood :name "mysterious" :mode "dorian" :bpm 92 :drums "sparse" :density 0.5d0 :arpeggio t
                   :progressions '((0 1 0 6) (0 3 1 0)))
        (make-mood :name "tense" :mode "harmonic_minor" :bpm 140 :drums "drive" :density 0.6d0 :arpeggio t
                   :progressions '((0 0 5 4) (0 5 3 4)))
        (make-mood :name "calm" :mode "lydian" :bpm 84 :drums "sparse" :density 0.4d0 :arpeggio t
                   :progressions '((0 3 0 4) (0 4 5 3))))
  "The moods the original composes for, with the same progressions as scale
degrees: one chord a bar.")

(defun mood-named (name)
  (or (find (string-downcase (string name)) +moods+ :key #'mood-name :test #'string=)
      (fail "~a is not a mood this cell composes; choose from ~{~a~^, ~}"
            name (mapcar #'mood-name +moods+))))

(defun degree->midi (root steps degree)
  "DEGREE (0 = tonic, 7 = the octave above) above ROOT in STEPS."
  (let ((octaves (floor degree 7)) (within (mod degree 7)))
    (+ root (nth within steps) (* 12 octaves))))

(defun add-tone! (buffer rng start seconds frequency wave gain
                  &key (duty 0.5d0) (attack 0.004d0) (decay 2.0d0))
  "One note summed into BUFFER at START; FREQUENCY in Hz."
  (let* ((length (max 1 (truncate (* seconds +rate+))))
         (start (max 0 (truncate start)))
         (attack-samples (max 1 (truncate (* attack +rate+))))
         (phase 0.0d0)
         (step (/ frequency +rate+)))
    (dotimes (index length)
      (let* ((progress (/ (coerce index 'double-float) length))
             (env (* (min 1.0d0 (/ (coerce index 'double-float) attack-samples))
                     (expt (- 1.0d0 progress) decay)))
             (sample (case wave
                       (:pulse (if (< phase duty) 1.0d0 -1.0d0))
                       (:triangle (+ -1.0d0 (* 4.0d0 (abs (- phase 0.5d0)))))
                       (:saw (- 1.0d0 (* 2.0d0 phase)))
                       (:sine (sin (* 2.0d0 pi phase)))
                       (t (rrange rng -1.0d0 1.0d0))))
             (at (+ start index)))
        (when (< at (length buffer))
          (incf (aref buffer at) (* sample env gain)))
        (incf phase step)
        (when (>= phase 1.0d0) (decf phase 1.0d0))))
    buffer))

(defun add-drum! (buffer rng start kind gain)
  "A kick, a snare or a hat at START, from synthesis alone."
  (case kind
    (:kick (let* ((length (truncate (* 0.16d0 +rate+))) (phase 0.0d0))
             (dotimes (index length)
               (let* ((progress (/ (coerce index 'double-float) length))
                      (frequency (+ 45.0d0 (* 120.0d0 (expt (- 1.0d0 progress) 2)))))
                 (incf (aref buffer (+ start index))
                       (* (sin (* 2.0d0 pi phase)) (expt (- 1.0d0 progress) 2.0d0) gain))
                 (incf phase (/ frequency +rate+))))))
    (:snare (add-tone! buffer rng start 0.13d0 0.0d0 :noise gain :decay 3.0d0))
    (:hat (add-tone! buffer rng start 0.05d0 0.0d0 :noise (* gain 0.5d0) :decay 4.0d0))))

(defun compose-music (&key mood seed (bpm nil) (bars 8) (key "C") (mode nil) (style "chiptune"))
  "A seamless loop: (values SAMPLES MOOD-USED BPM-USED BARS-USED)."
  (let* ((found (mood-named mood))
         (rng (make-rng seed))
         (bpm (or bpm (mood-bpm found)))
         (bars (or bars 8))
         (steps (mode-steps (or mode (mood-mode found))))
         (root (+ 36 (note-offset key)))
         (progression (rchoice rng (mood-progressions found)))
         (beats (* 4 bars))
         (seconds (/ (* 60.0d0 beats) bpm))
         (buffer (make-array (truncate (* seconds +rate+)) :element-type 'double-float
                                                            :initial-element 0.0d0))
         (beat (/ 60.0d0 bpm))
         (density (if (string-equal style "ambient") (* 0.6d0 (mood-density found)) (mood-density found)))
         (lead-wave (if (string-equal style "synthwave") :saw (if (string-equal style "orchestral") :triangle :pulse)))
         (bass-wave (if (string-equal style "ambient") :sine :triangle)))
    (dotimes (bar bars)
      (let* ((start (* bar 4 beat))
             (degree (nth (mod bar (length progression)) progression))
             (chord (list degree (+ degree 2) (+ degree 4)))
             (chord-notes (mapcar (lambda (d) (+ 12 (degree->midi root steps d))) chord)))
        ;; bass: the root, twice a bar, or four times when the drums drive
        (let ((bass (degree->midi root steps degree)))
          (loop for at in (if (string= (mood-drums found) "march") '(0 2) '(0 2 3))
                do (add-tone! buffer rng (+ start (* at beat)) (* beat 0.9d0) bass bass-wave 0.34d0
                              :decay 1.2d0)))
        ;; arpeggio: the chord, one note every half beat
        (when (mood-arpeggio found)
          (loop for step below 8
                do (add-tone! buffer rng (+ start (* step 0.5d0 beat)) (* 0.45d0 beat)
                              (nth (mod step 3) chord-notes) :pulse 0.09d0 :duty 0.25d0 :decay 2.0d0)))
        ;; melody: eighth notes, density against the mood, chord tones and their neighbours
        (loop for step below 8
              do (when (rchance rng density)
                   (let* ((pick (if (rchance rng 0.6d0)
                                    (nth (rint rng 3) chord)
                                    (+ degree (rchoice rng '(-2 -1 1 2 3)))))
                          (pitch (+ 24 (degree->midi root steps pick))))
                     (add-tone! buffer rng (+ start (* step 0.5d0 beat))
                                (* (rchoice rng '(0.5d0 0.5d0 1.0d0)) beat)
                                pitch lead-wave 0.16d0 :duty 0.5d0 :decay 1.6d0))))
        ;; drums
        (let ((drums (mood-drums found)))
          (add-drum! buffer rng (truncate (* start +rate+)) :kick 0.55d0)
          (when (member drums '("bounce" "drive" "march") :test #'string=)
            (add-drum! buffer rng (truncate (* (+ start (* 2 beat)) +rate+)) :kick 0.4d0))
          (when (member drums '("bounce" "drive") :test #'string=)
            (add-drum! buffer rng (truncate (* (+ start (* 1 beat)) +rate+)) :snare 0.3d0)
            (add-drum! buffer rng (truncate (* (+ start (* 3 beat)) +rate+)) :snare 0.3d0))
          (when (string= drums "march")
            (add-drum! buffer rng (truncate (* (+ start (* 3 beat)) +rate+)) :snare 0.22d0))
          (when (member drums '("drive" "bounce") :test #'string=)
            (loop for step below 8
                  do (add-drum! buffer rng (truncate (* (+ start (* step 0.5d0 beat)) +rate+)) :hat 0.12d0))))))
    (values (normalize-samples buffer :peak 0.86d0) (mood-name found) bpm bars)))

(defparameter +music-styles+ '("chiptune" "synthwave" "ambient" "orchestral")
  "The styles a call may compose in.")
(defparameter +mood-name-list+ (mapcar #'mood-name +moods+))
(defparameter +mode-name-list+ (mapcar #'car +modes+))
(defparameter +sfx-preset-names+ (mapcar #'car +sfx-presets+))

(defparameter +drums+ '("march" "bounce" "drive" "sparse"))

(defparameter +max-bars+ 64)
