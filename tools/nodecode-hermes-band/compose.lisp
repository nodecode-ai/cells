;;;; compose.lisp --- the composer: a progression and parts become a performance.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A role knows how a real instrument plays its part: where the thumb goes,
;;;; how long a fingerpicked note rings, how far behind the beat a snare sits.
;;;; That, not the notes, is what separates this from a typed-in piano roll.

(in-package #:nodecode-hermes-band)

(defparameter +fingerings+
  '(("folk" . ((0 . 0) (2 . 1) (4 . 2) (6 . 1) (8 . 0) (10 . 2) (12 . 3) (14 . 1)))
    ("travis" . ((0 . 0) (2 . 2) (4 . 1) (6 . 3) (8 . 0) (10 . 3) (12 . 1) (14 . 2)))
    ("sparse" . ((0 . 0) (4 . 2) (8 . 0) (12 . 3)))
    ("arp_up" . ((0 . 1) (2 . 2) (4 . 3) (6 . 4) (8 . 0) (10 . 4) (12 . 3) (14 . 2)))
    ("rolling" . ((0 . 0) (2 . 1) (3 . 2) (4 . 1) (6 . 3) (8 . 0) (10 . 2) (11 . 3) (12 . 2) (14 . 1)))
    ("oompah" . ((0 . 0) (2 . 5) (4 . 0) (6 . 5) (8 . 0) (10 . 5) (12 . 0) (14 . 5))))
  "Fingering patterns: (eighth-slot . palette index), the palette being the
bass note then the upper voicing bottom to top.")

(defparameter +drum-grids+
  '(("folk_soft" . (("kick" . "x.......o.......") ("stick" . "....>.......>...")
                    ("shaker" . "o.o.x.o.o.o.x.o.")))
    ("folk_full" . (("kick" . "x.....o.x.......") ("stick" . "....>.......>...")
                    ("snare" . "............-...") ("shaker" . "o.o.x.o.o.o.x.o.")
                    ("hh_pedal" . "..o...o...o...o.")))
    ("pulse" . (("kick" . "x...x...x...x...") ("clap" . "....x.......x...")
                ("hh" . "--o---o---o---o-") ("hh_open" . "......o.......o.")))
    ("heartbeat" . (("kick" . "x.......x.......") ("shaker" . "..o...o...o...o.")))
    ("boom_bap" . (("kick" . "x.....x...x.....") ("snare" . "....>.......>...")
                   ("hh" . "x-o-x-o-x-o-x-o-")))
    ("funk" . (("kick" . "x.....x..x......") ("snare" . "..-o>..-o.-o>-o")
               ("hh" . "x-o-x-o-x-o-x-o-") ("hh_open" . "..............o."))))
  "Drum patterns on a sixteen-step bar. x is a hit, o softer, > accented,
- a ghost; a drummer's feel lives in those numbers.")

(defparameter +feel-offsets+
  '(("kick" . 0) ("stick" . 4) ("snare" . 6) ("clap" . 5)
    ("shaker" . -3) ("hh" . -2) ("hh_pedal" . 3) ("hh_open" . 2))
  "Per-voice microtiming in ticks: no drummer is ever on the grid.")

(defun hit-velocity (char base)
  "The share of BASE a grid character names."
  (let ((scale (cond ((char= char #\x) 1.0) ((char= char #\o) 0.6)
                     ((char= char #\>) 1.18) ((char= char #\-) 0.42) (t 0.0))))
    (* base scale)))

(defun section-level (score bar)
  "The dynamic arc: parts enter, the middle is the loudest, the last bar falls
back. One velocity for a whole piece is the clearest machine tell."
  (let* ((bars (band-score-bars score))
         (position (if (plusp bars) (/ (float bar 1.0) bars) 0.0)))
    (cond ((< position 0.17) 0.78)
          ((< position 0.34) 0.88)
          ((< position 0.9) 1.0)
          (t 0.9))))

(defun slot-tick (slot swing)
  "SLOT eighths as ticks, odd slots delayed by SWING of an eighth."
  (let ((tick (* slot +e+)))
    (if (and (plusp swing) (oddp slot)) (+ tick (round (* +e+ swing))) tick)))

(defun part-bars (score part)
  "The bars PART plays: from its start_bar to its end_bar or the last bar."
  (let ((from (max 0 (band-part-start-bar part)))
        (to (min (band-score-bars score) (or (band-part-end-bar part)
                                             (band-score-bars score)))))
    (loop for bar from from below to collect bar)))

(defun bar-context (score bar rng previous-visiting)
  "What one bar plays over: the chord, its voicing, and the bass note."
  (let* ((progression (band-score-progression score))
         (symbol (nth (mod bar (length progression)) progression))
         (voicing (voice-lead (band-score-key score) (band-score-mode score) symbol
                               previous-visiting
                               :low 55 :high 79 :voices 4))
         (root (chord-root (band-score-key score) (band-score-mode score) symbol)))
    (values symbol voicing (bass-note root) voicing)))

(defun bass-note (root &key (low 40) (high 52))
  "ROOT moved into the bass register."
  (loop while (< root low) do (incf root 12))
  (loop while (> root high) do (decf root 12))
  root)

;;; --- the roles ------------------------------------------------------------------

(defun role-fingerpick (score part bar ctx)
  (declare (ignore score bar))
  (destructuring-bind (symbol voicing bass) ctx
    (declare (ignore symbol))
    (let* ((pattern (or (cdr (assoc (band-part-pattern part) +fingerings+ :test #'string=))
                        (cdr (assoc "folk" +fingerings+ :test #'string=))))
           (palette (cons bass (sort (copy-list voicing) #'<)))
           (rng (make-rng (+ (band-score-seed score) (length palette))))
           (out '()))
      (loop for (slot . index) in pattern
            for k from 0
            do (let* ((idx (min index (1- (length palette))))
                      (pitch (+ (nth idx palette) (* 12 (band-part-octave part))))
                      (jitter (- (funcall rng (1+ (* 2 (max 0 (band-part-jitter part)))))
                                 (max 0 (band-part-jitter part))))
                      (tick (max 0 (+ (slot-tick slot (band-part-swing part)) jitter)))
                      (thumb (zerop idx))
                      (velocity (if thumb (+ 92 (- (funcall rng 9) 4))
                                    (+ (if (evenp k) 78 68) (- (funcall rng 11) 5))))
                      (duration (if thumb (* +e+ 2.4) (* +e+ 1.8))))
                 (push (list tick pitch velocity duration) out)))
      (nreverse out))))

(defun chord-hits (voicing rng count low high)
  "COUNT pitches of VOICING inside [LOW, HIGH], bottom to top."
  (let ((pool (sort (loop for pitch in (sort (copy-list voicing) #'<)
                           for candidate = pitch
                           do (loop while (< candidate low) do (incf candidate 12))
                           do (loop while (> candidate high) do (decf candidate 12))
                           when (<= low candidate high) collect candidate)
                     #'<)))
    (when (zerop (length pool)) (setf pool (list low)))
    (loop for index below (max 1 (min count (length pool)))
          collect (nth (mod (+ index (funcall rng 2)) (length pool)) pool))))

(defun role-strum (score part bar ctx)
  (destructuring-bind (symbol voicing bass) ctx
    (declare (ignore symbol bass))
    (let* ((rng (make-rng (+ (band-score-seed score) bar (length (band-part-inst part)))))
           (beats (or (getf (band-part-extra part) :beats) '(0 4)))
           (spread (or (getf (band-part-extra part) :spread) 7))
           (out '()))
      (dolist (beat beats)
        (let ((base (+ (* beat +e+) (- (funcall rng 9) 4))))
          (loop for pitch in (chord-hits voicing rng 4 48 88)
                for i from 0
                do (push (list (max 0 (+ base (* i spread))) pitch (- 86 (* i 3)) (* +e+ 3.2))
                         out))))
      (nreverse out))))

(defun role-block (score part bar ctx)
  (destructuring-bind (symbol voicing bass) ctx
    (declare (ignore symbol bass))
    (let ((rng (make-rng (+ (band-score-seed score) (* 7 bar)))))
      (loop for pitch in (chord-hits voicing rng (band-part-voices part) 40 84)
            collect (list 0 pitch 64 (* +beats+ +div+))))))

(defun role-bass-root5 (score part bar ctx)
  (destructuring-bind (symbol voicing bass) ctx
    (declare (ignore symbol voicing))
    (let* ((rng (make-rng (+ (band-score-seed score) bar 5)))
           (quarters (getf (band-part-extra part) :quarters))
           (fifth (bass-note (+ bass 7)))
           (slots (if quarters '(0 2 4 6 8 10 12 14) '(0 4 8 12)))
           (degrees (if quarters
                        (loop repeat (length slots) collect bass)
                        (list bass bass fifth bass))))
      (loop for slot in slots
            for pitch in degrees
            for i from 0
            collect (list (max 0 (+ (slot-tick slot 0.0) (- (funcall rng 5) 2)))
                          pitch
                          (- (if (evenp i) 96 88) (funcall rng 7))
                          (* +e+ 1.6))))))

(defun role-bass-walk (score part bar ctx)
  (destructuring-bind (symbol voicing bass) ctx
    (declare (ignore symbol voicing))
    (let* ((rng (make-rng (+ (band-score-seed score) bar 11)))
           (progression (band-score-progression score))
           (next-symbol (nth (mod (1+ bar) (length progression)) progression))
           (next-root (chord-root (band-score-key score) (band-score-mode score) next-symbol))
           (approach (if (> next-root bass) (1- next-root) (1+ next-root)))
           (pitches (list bass (+ bass 7) (bass-note approach) (bass-note (+ bass 12)))))
      (loop for pitch in pitches
            for beat from 0
            collect (list (max 0 (+ (* beat +div+) (- (funcall rng 5) 2)))
                          pitch
                          (- (if (zerop beat) 98 90) (funcall rng 6))
                          +div+)))))

(defun role-arp (score part bar ctx)
  (destructuring-bind (symbol voicing bass) ctx
    (declare (ignore symbol))
    (let* ((pattern (or (cdr (assoc (band-part-pattern part) +fingerings+ :test #'string=))
                        (cdr (assoc "arp_up" +fingerings+ :test #'string=))))
           (palette (cons bass (sort (copy-list voicing) #'<)))
           (rng (make-rng (+ (band-score-seed score) bar 3)))
           (low (first (band-part-register part)))
           (high (second (band-part-register part))))
      (loop for (slot . index) in pattern
            collect (let* ((pitch (+ (nth (min index (1- (length palette))) palette)
                                     (* 12 (band-part-octave part))))
                           (pitch (loop while (< pitch low) do (incf pitch 12)
                                        while (> pitch high) do (decf pitch 12)))
                           (tick (+ (slot-tick slot (band-part-swing part))
                                    (- (funcall rng 7) 3))))
                      (list (max 0 tick) pitch (- 72 (funcall rng 9)) (* +e+ 1.4)))))))

(defun role-melody (score part bar ctx)
  (let* ((rng (make-rng (+ (band-score-seed score) bar 13)))
         (low (first (band-part-register part)))
         (high (second (band-part-register part)))
         (out '()))
    (destructuring-bind (symbol voicing bass) ctx
      (declare (ignore symbol voicing bass))
      (loop for beat from 0 below +beats+
            when (< (funcall rng 10) 7)
              do (let* ((degree (+ 1 (funcall rng 7)))
                        (pitch (diatonic-note (band-score-key score) (band-score-mode score) degree))
                        (octave (loop with p = pitch
                                       while (< p low) do (incf p 12)
                                       while (> p high) do (decf p 12)
                                       finally (return p))))
                   (push (list (+ (* beat +div+) (- (funcall rng 13) 6))
                               octave
                               (- 84 (funcall rng 12))
                               (+ (round (* +div+ 0.9)) (funcall rng 120)))
                         out))))
    (nreverse out)))

(defun role-sparkle (score part bar ctx)
  (destructuring-bind (symbol voicing bass) ctx
    (declare (ignore symbol bass))
    (let* ((rng (make-rng (+ (band-score-seed score) bar 17)))
           (slots (or (getf (band-part-extra part) :slots) '(3 7 11 14)))
           (high (second (band-part-register part))))
      (loop for slot in slots
            for pitch in (chord-hits voicing rng (length slots) 60 high)
            collect (list (max 0 (slot-tick slot 0.0)) pitch (- 80 (funcall rng 10)) (* +e+ 2))))))

(defun role-drums (score part bar ctx)
  (declare (ignore ctx))
  (let* ((grid (or (cdr (assoc (band-part-pattern part) +drum-grids+ :test #'string=))
                   (cdr (assoc "folk_soft" +drum-grids+ :test #'string=))))
         (rng (make-rng (+ (band-score-seed score) (* 3 bar))))
         (out '()))
    (loop for (voice . steps) in grid
          do (loop for char across steps
                   for step from 0
                   do (let ((velocity (hit-velocity char (cdr (assoc voice '(("kick" . 0.72) ("stick" . 0.6)
                                                                              ("snare" . 0.8) ("clap" . 0.44)
                                                                              ("shaker" . 0.3) ("hh" . 0.3)
                                                                              ("hh_pedal" . 0.26) ("hh_open" . 0.2))
                                                                 :test #'string=)))))
                        (when (plusp velocity)
                          (let ((pitch (cdr (assoc voice +drum-notes+ :test #'string=)))
                                (offset (or (cdr (assoc voice +feel-offsets+ :test #'string=)) 0)))
                            (push (list (max 0 (+ (* step 120) offset (- (funcall rng 3) 1)))
                                        pitch
                                        (round (* 127 (min 1.0 velocity)))
                                        (* +e+ 0.5))
                                  out))))))
    (nreverse out)))

(defun role-funk-guitar (score part bar ctx)
  (destructuring-bind (symbol voicing bass) ctx
    (declare (ignore symbol bass))
    (let* ((rng (make-rng (+ (band-score-seed score) bar 19)))
           (out '()))
      (loop for step from 0 below 16
            when (not (member step '(4 12)))
              do (let ((pitch (nth (mod step 3) (sort (copy-list voicing) #'<)))
                       (accent (if (member step '(0 10)) 96 62)))
                   (push (list (* step 120) pitch (- accent (funcall rng 8)) 90) out)))
      (nreverse out))))

(defun role-slap-bass (score part bar ctx)
  (destructuring-bind (symbol voicing bass) ctx
    (declare (ignore symbol voicing))
    (let* ((rng (make-rng (+ (band-score-seed score) bar 23)))
           (fifth (bass-note (+ bass 7)))
           (pop (bass-note (+ bass 12))))
      (loop for (step . pitch) in (list (cons 0 bass) (cons 3 bass) (cons 6 fifth)
                                        (cons 10 pop) (cons 12 bass))
            collect (list (* step 120)
                          pitch
                          (- (if (member step '(0 10)) 104 82) (funcall rng 8))
                          180)))))

(defun role-stab (score part bar ctx)
  (destructuring-bind (symbol voicing bass) ctx
    (declare (ignore symbol bass))
    (let* ((rng (make-rng (+ (band-score-seed score) bar 29)))
           (steps (or (getf (band-part-extra part) :steps) '(2 6 10 14)))
           (length (or (getf (band-part-extra part) :len16) 1.2)))
      (loop for step in steps
            append (loop for pitch in (chord-hits voicing rng 3 48 84)
                         collect (list (* step 120) pitch (- 92 (funcall rng 8))
                                       (round (* 120 length))))))))

;;; --- one part, one bar ----------------------------------------------------------

(defun role-events (score part bar ctx)
  (let ((role (band-part-role part)))
    (cond ((string= role "fingerpick") (role-fingerpick score part bar ctx))
          ((string= role "strum") (role-strum score part bar ctx))
          ((string= role "block") (role-block score part bar ctx))
          ((string= role "bass_root5") (role-bass-root5 score part bar ctx))
          ((string= role "bass_walk") (role-bass-walk score part bar ctx))
          ((string= role "arp") (role-arp score part bar ctx))
          ((string= role "melody") (role-melody score part bar ctx))
          ((string= role "sparkle") (role-sparkle score part bar ctx))
          ((string= role "drums") (role-drums score part bar ctx))
          ((string= role "funk_guitar") (role-funk-guitar score part bar ctx))
          ((string= role "slap_bass") (role-slap-bass score part bar ctx))
          ((string= role "stab") (role-stab score part bar ctx))
          (t '()))))

(defun part-performance (score part)
  "Every note PART plays, as (TICK PITCH VELOCITY DURATION), ticks from bar 0."
  (let ((events '()) (previous nil))
    (dolist (bar (part-bars score part))
      (multiple-value-bind (symbol voicing bass)
          (bar-context score bar nil previous)
        (declare (ignore symbol))
        (setf previous voicing)
        (let ((level (* (band-part-gain part) (section-level score bar))))
          (dolist (event (role-events score part bar (list symbol voicing bass)))
            (destructuring-bind (tick pitch velocity duration) event
              (push (list (+ tick (* bar +beats+ +div+))
                          (max 0 (min 127 (round pitch)))
                          (max 1 (min 127 (round (* velocity level))))
                          (max 1 (round duration)))
                    events))))))
    (sort (nreverse events) #'< :key #'first)))

(defun performance-parts (score)
  "One (PART . EVENTS) pair per part, in the score's order."
  (loop for part in (band-score-parts score)
        collect (cons part (part-performance score part))))
