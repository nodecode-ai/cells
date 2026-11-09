;;;; theory.lisp --- notes, scales, roman-numeral chords, voice leading.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; All pitch math is integer MIDI note numbers, as in the plugin this
;;;; rewrites. The one thing worth keeping from it: a progression is voiced by
;;;; moving each chord as little as possible from the last one, which is what
;;;; separates composed harmony from a piano roll.

(in-package #:nodecode-hermes-band)

(defparameter +pc+ '(("C" . 0) ("D" . 2) ("E" . 4) ("F" . 5) ("G" . 7) ("A" . 9) ("B" . 11))
  "Pitch class of a natural note name.")

(defparameter +pc-names+ '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

(defparameter +scales+
  '(("major" . (0 2 4 5 7 9 11))
    ("ionian" . (0 2 4 5 7 9 11))
    ("minor" . (0 2 3 5 7 8 10))
    ("aeolian" . (0 2 3 5 7 8 10))
    ("harmonic_minor" . (0 2 3 5 7 8 11))
    ("melodic_minor" . (0 2 3 5 7 9 11))
    ("dorian" . (0 2 3 5 7 9 10))
    ("phrygian" . (0 1 3 5 7 8 10))
    ("lydian" . (0 2 4 6 7 9 11))
    ("mixolydian" . (0 2 4 5 7 9 10))
    ("locrian" . (0 1 3 5 6 8 10))
    ("major_pentatonic" . (0 2 4 7 9))
    ("minor_pentatonic" . (0 3 5 7 10))
    ("blues" . (0 3 5 6 7 10)))
  "Scale intervals from the tonic, by mode name.")

(defparameter +qualities+
  '(("" . (0 4 7)) ("maj" . (0 4 7)) ("min" . (0 3 7)) ("m" . (0 3 7))
    ("dim" . (0 3 6)) ("aug" . (0 4 8)) ("sus2" . (0 2 7)) ("sus4" . (0 5 7))
    ("5" . (0 7)) ("6" . (0 4 7 9)) ("m6" . (0 3 7 9)) ("7" . (0 4 7 10))
    ("maj7" . (0 4 7 11)) ("m7" . (0 3 7 10)) ("mmaj7" . (0 3 7 11))
    ("dim7" . (0 3 6 9)) ("m7b5" . (0 3 6 10)) ("add9" . (0 4 7 14))
    ("madd9" . (0 3 7 14)) ("9" . (0 4 7 10 14)) ("maj9" . (0 4 7 11 14))
    ("m9" . (0 3 7 10 14)))
  "Chord quality to the semitone stack above its root.")

(defparameter +roman-base+
  '(("I" . 1) ("II" . 2) ("III" . 3) ("IV" . 4) ("V" . 5) ("VI" . 6) ("VII" . 7))
  "Roman numerals to scale degrees.")

(defun note-to-midi (name)
  "A note name and octave, such as D4 or F#3, as a MIDI number. C4 is 60."
  (let* ((text (string-upcase (nlk:trimmed name)))
         (letter (and (plusp (length text)) (subseq text 0 1)))
         (base (cdr (assoc letter +pc+ :test #'string=))))
    (unless base (fail "~s is not a note name; use one such as D4 or F#3" name))
    (let ((index 1) (pc base))
      (when (and (> (length text) 1) (find (char text 1) "#b"))
        (setf pc (+ pc (if (char= (char text 1) #\#) 1 -1)) index 2))
      (let ((octave (parse-integer text :start index :junk-allowed t)))
        (unless octave (fail "~s names no octave; use one such as D4" name))
        (+ pc (* 12 (1+ octave)))))))

(defun key-pc (tonic)
  "The pitch class TONIC names, with its octave or without."
  (mod (note-to-midi (if (find (char tonic (1- (length tonic))) "0123456789")
                         tonic
                         (concatenate 'string tonic "4")))
       12))

(defun scale-intervals (mode)
  (or (cdr (assoc (string-downcase mode) +scales+ :test #'string=))
      (fail "~s is not a mode; try one of ~{~a~^, ~}" mode (mapcar #'car +scales+))))

(defun degree-pitch (tonic mode degree &optional (accidental 0))
  "Scale DEGREE, 1-based, as a pitch class, bent by ACCIDENTAL semitones."
  (let* ((intervals (scale-intervals mode))
         (n (length intervals))
         (d (mod (1- degree) n))
         (lift (* 12 (floor (1- degree) n))))
    (mod (+ (key-pc tonic) (nth d intervals) accidental lift) 12)))

(defun parse-roman (symbol)
  "A roman numeral such as bVII7 or im7 as (values DEGREE ACCIDENTAL QUALITY)."
  (let* ((text (nlk:trimmed symbol))
         (accidental 0)
         (at 0))
    (loop while (and (< at (length text)) (find (char text at) "#b"))
          do (setf accidental (+ accidental (if (char= (char text at) #\#) 1 -1)))
             (incf at))
    (let* ((rest (subseq text at))
           (letters (subseq rest 0 (or (position-if-not #'alpha-char-p rest) (length rest))))
           (suffix (subseq rest (length letters)))
           (degree (cdr (assoc (string-upcase letters) +roman-base+ :test #'string=))))
      (unless degree (fail "~s is not a roman numeral such as I, vi or bVII7" symbol))
      (let ((quality (cond ((search "°" rest) (if (search "7" rest) "dim7" "dim"))
                            ((search "ø" rest) "m7b5")
                            ((search "+" rest) "aug")
                            ((plusp (length suffix)) suffix)
                            ((every #'upper-case-p letters) "")
                            (t "m"))))
        (unless (assoc quality +qualities+ :test #'string=)
          (fail "~s names the chord quality ~s, which is not one of ~{~a~^, ~}"
                symbol quality (remove "" (mapcar #'car +qualities+) :test #'string=)))
        (values degree accidental quality)))))

(defun chord-pcs (tonic mode symbol)
  "The pitch classes of SYMBOL in the key, as a sorted list."
  (multiple-value-bind (degree accidental quality) (parse-roman symbol)
    (let* ((root (degree-pitch tonic mode degree accidental))
           (stack (cdr (assoc quality +qualities+ :test #'string=))))
      (sort (remove-duplicates (mapcar (lambda (iv) (mod (+ root iv) 12)) stack))
            #'<))))

(defun chord-root (tonic mode symbol)
  (multiple-value-bind (degree accidental) (parse-roman symbol)
    (degree-pitch tonic mode degree accidental)))

(defun diatonic-note (tonic mode degree)
  "A scale pitch in a comfortable register, for a walking line or a melody."
  (degree-pitch tonic mode degree))

(defun voice-lead (tonic mode symbol previous &key (low 55) (high 79) (voices 4) (rootless nil))
  "The voicing of SYMBOL nearest PREVIOUS inside [LOW, HIGH]."
  (let* ((pcs (chord-pcs tonic mode symbol))
         (root (chord-root tonic mode symbol))
         (pool (if (and rootless (> (length pcs) 3)) (remove root pcs) pcs))
         (best nil) (best-score nil))
    (when (> (length pool) voices) (setf pool (subseq pool 0 voices)))
    (loop for inversion below (length pool)
          do (loop for base from 2 below 7
                   do (let ((voicing '()))
                        (dolist (pc (append (subseq pool inversion) (subseq pool 0 inversion)))
                          (let ((target (+ (* 12 base) pc)))
                            (loop while (and voicing (<= target (car voicing)))
                                  do (incf target 12))
                            (push target voicing)))
                        (setf voicing (sort voicing #'<))
                        (let ((score 0.0))
                          (when (member root (mapcar (lambda (p) (mod p 12)) voicing))
                            (decf score 3.0))
                          (unless (member root (mapcar (lambda (p) (mod p 12)) voicing))
                            (incf score 7.0))
                          (incf score (* 0.02 (- (car (last voicing)) (first voicing))))
                          (if previous
                              (incf score (* 0.35 (loop for a in voicing
                                                        for b in (sort (copy-list previous) #'<)
                                                        sum (abs (- a b)))))
                              (progn (incf score (* 0.5 (abs (- (first voicing) low))))
                                     (incf score (* 4.0 inversion))))
                          (when (and (>= (first voicing) low) (<= (car (last voicing)) high)
                                     (or (null best-score) (< score best-score)))
                            (setf best voicing best-score score))))))
    (or best (list (+ (* 12 4) root)))))
