;;;; score.lisp --- the score: an arrangement as data, its checks and the presets.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The score file is the contract the whole band depends on: it names the
;;;; tempo, the key, the bars and one part per player. Everything downstream
;;;; is deterministic, so a score that renders well today renders identically
;;;; tomorrow.

(in-package #:nodecode-hermes-band)

(defstruct (band-part (:constructor make-band-part))
  (inst "steel") (role "fingerpick") (start-bar 0) (end-bar nil)
  (gain 1.0) (octave 0) (pattern "folk") (swing 0.0) (jitter 3)
  (register '(55 79)) (voices 4) (rootless nil) (howl 0.0)
  (pan 0.0) (level 0.0) (send 1.0) (extra nil))

(defstruct (band-score (:constructor make-band-score))
  (name "track") (bpm 90) (key "D") (mode "major") (bars 12) (seed 11)
  (progression '("I" "V" "vi" "IV")) (parts '()))

;;; --- what a score may say -------------------------------------------------------

(defun json-member (table name)
  "NAME's value in TABLE, a decoded JSON object, or NIL."
  (when (hash-table-p table)
    (gethash name table)))

(defun number-member (table name default)
  (let ((value (json-member table name)))
    (if (numberp value) value default)))

(defun string-member (table name default)
  (let ((value (json-member table name)))
    (if (stringp value) value default)))

(defun list-of-strings (value)
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun register-of (value)
  "A [low high] pair, or the default when the score names none."
  (let ((pair (list-of-strings value)))
    (if (and (= 2 (length pair)) (every #'integerp pair))
        pair
        '(55 79))))

(defun extra-of (value)
  "A part's extra object, read through the same lenient reader JSON uses."
  (when (hash-table-p value)
    (list :beats (list-of-strings (json-member value "beats"))
          :slots (list-of-strings (json-member value "slots"))
          :steps (list-of-strings (json-member value "steps"))
          :quarters (json-member value "quarters")
          :spread (number-member value "spread" 7)
          :ring (number-member value "ring" 3.2)
          :fill (json-member value "fill"))))

(defun mix-of (value)
  "The mix members a part may carry: pan, level and send."
  (list :pan (number-member value "pan" 0.0)
        :level (number-member value "level" 0.0)
        :send (number-member value "send" 1.0)))

(defun part-from-json (table)
  (let ((mix (json-member table "mix")))
    (make-band-part
     :inst (string-member table "inst" "steel")
     :role (string-member table "role" "fingerpick")
     :start-bar (number-member table "start_bar" 0)
     :end-bar (json-member table "end_bar")
     :gain (number-member table "gain" 1.0)
     :octave (number-member table "octave" 0)
     :pattern (string-member table "pattern" "folk")
     :swing (number-member table "swing" 0.0)
     :jitter (number-member table "jitter" 3)
     :register (register-of (json-member table "register"))
     :voices (number-member table "voices" 4)
     :rootless (json-member table "rootless")
     :howl (number-member table "howl" 0.0)
     :pan (number-member mix "pan" 0.0)
     :level (number-member mix "level" 0.0)
     :send (number-member mix "send" 1.0)
     :extra (extra-of (json-member table "extra")))))

(defun score-from-json (table)
  "A BAND-SCORE from a decoded score object. Missing members take the
plugin's defaults; VALIDATE-SCORE is what refuses a wrong one."
  (let ((parts (json-member table "parts")))
    (make-band-score
     :name (string-member table "name" "track")
     :bpm (number-member table "bpm" 90)
     :key (string-member table "key" "D")
     :mode (string-member table "mode" "major")
     :bars (number-member table "bars" 12)
     :seed (number-member table "seed" 11)
     :progression (or (list-of-strings (json-member table "progression"))
                      '("I" "V" "vi" "IV"))
     :parts (if (vectorp parts)
                (loop for part across parts collect (part-from-json part))
                (when (listp parts)
                  (loop for part in parts collect (part-from-json part)))))))

;;; --- checking it ---------------------------------------------------------------

(defun inst-known-p (inst)
  (or (assoc inst +programs+ :test #'string=) (string= inst "drums")))

(defun validate-part (part index)
  "Every reason PART cannot play, each prefixed with its index."
  (let ((problems '()))
    (flet ((say (format-control &rest args)
             (push (format nil "parts[~d]: ~a" index (apply #'format nil format-control args))
                   problems)))
    (let ((role (band-part-role part))
          (register (band-part-register part)))
      (unless (inst-known-p (band-part-inst part))
        (say "~s is not an instrument; use one of ~{~a~^, ~}"
             (band-part-inst part) (mapcar #'car +programs+)))
      (unless (member role +roles+ :test #'string=)
        (say "~s is not a role; use one of ~{~a~^, ~}" role +roles+))
      (when (string= role "fingerpick")
        (unless (member (band-part-pattern part) +fingering-patterns+ :test #'string=)
          (say "~s is not a fingering pattern; use one of ~{~a~^, ~}"
               (band-part-pattern part) +fingering-patterns+)))
      (when (string= role "arp")
        (unless (member (band-part-pattern part) +fingering-patterns+ :test #'string=)
          (say "~s is not an arpeggio pattern; use one of ~{~a~^, ~}"
               (band-part-pattern part) +fingering-patterns+)))
      (unless (and (= 2 (length register)) (every #'integerp register)
                   (<= 0 (first register)) (< (first register) (second register))
                   (<= (second register) 127))
        (say "register is ~s; it is [low high], two notes, low under high, inside 0-127"
             register))
      (when (minusp (band-part-start-bar part))
        (say "start_bar is ~d; bars count from 0" (band-part-start-bar part)))
      (let ((end (band-part-end-bar part)))
        (when (and end (numberp end) (<= end (band-part-start-bar part)))
          (say "end_bar is ~d; it must be past start_bar ~d" end (band-part-start-bar part))))
      (unless (and (numberp (band-part-gain part)) (<= 0 (band-part-gain part) 2))
        (say "gain is ~s; it runs from 0 to 2" (band-part-gain part)))
      (unless (and (integerp (band-part-voices part)) (<= 1 (band-part-voices part) 6))
        (say "voices is ~s; it runs from 1 to 6" (band-part-voices part))))
      (nreverse problems))))


(defun validate-score (score)
  "Every reason SCORE cannot be rendered, oldest first. Empty means it can."
  (let ((problems '()))
    (flet ((say (format-control &rest args)
             (push (apply #'format nil format-control args) problems)))
      (unless (and (numberp (band-score-bpm score)) (<= 30 (band-score-bpm score) 300))
        (say "bpm is ~s; it runs from 30 to 300" (band-score-bpm score)))
      (unless (and (integerp (band-score-bars score)) (<= 1 (band-score-bars score) 512))
        (say "bars is ~s; it runs from 1 to 512" (band-score-bars score)))
      (unless (assoc (string-downcase (band-score-mode score)) +scales+ :test #'string=)
        (say "~s is not a mode; try one of ~{~a~^, ~}"
             (band-score-mode score) (mapcar #'car +scales+)))
      (handler-case (key-pc (band-score-key score))
        (hermes-band-error (condition) (say "~a" (hermes-band-error-detail condition))))
      (unless (plusp (length (band-score-progression score)))
        (say "progression is empty; name at least one chord such as I or vi"))
      (loop for symbol in (band-score-progression score)
            do (handler-case
                   (multiple-value-bind (degree accidental quality)
                       (parse-roman (string symbol))
                     (declare (ignore degree accidental))
                     (unless (assoc quality +qualities+ :test #'string=)
                       (say "~s names a chord quality this engine does not know" symbol)))
                 (hermes-band-error (condition) (say "~a" (hermes-band-error-detail condition)))))
      (unless (band-score-parts score)
        (say "parts is empty; a score with no parts renders silence"))
      (loop for part in (band-score-parts score)
            for index from 0
            do (loop for problem in (validate-part part index)
                     do (push problem problems))))
    (nreverse problems)))

(defun describe-score (score)
  "One line about what a score will render, for the model's answer."
  (format nil "~a: ~d bar~:p at ~a BPM in ~a ~a, ~d part~:p (~{~a~^, ~}), ~{~a~^ / ~}"
          (band-score-name score) (band-score-bars score) (band-score-bpm score)
          (band-score-key score) (band-score-mode score)
          (length (band-score-parts score))
          (mapcar #'band-part-inst (band-score-parts score))
          (band-score-progression score)))

(defun music-duration (score)
  "The musical length in seconds: bars of four beats at the score's tempo."
  (* (band-score-bars score) +beats+ (/ 60.0 (float (band-score-bpm score) 1.0))))

;;; --- the four presets -----------------------------------------------------------

(defun preset-bed ()
  (make-band-score
   :name "bed" :bpm 90 :key "D" :mode "major" :bars 12 :seed 11
   :progression '("I" "V" "vi" "IV")
   :parts (list (make-band-part :inst "steel" :role "fingerpick" :pattern "folk"
                                :register '(55 79) :voices 4 :jitter 4 :howl 0.4 :pan -0.20)
                (make-band-part :inst "fingerbass" :role "bass_root5" :start-bar 2
                                :register '(40 52) :gain 0.9 :jitter 2)
                (make-band-part :inst "drums" :role "drums" :start-bar 4 :gain 0.82
                                :pattern "folk_soft" :extra (list :fill nil))
                (make-band-part :inst "steel2" :role "arp" :pattern "sparse" :start-bar 6
                                :register '(71 86) :gain 0.62 :jitter 5 :pan 0.42))))

(defun preset-band ()
  (make-band-score
   :name "band" :bpm 84 :key "D" :mode "major" :bars 12 :seed 23
   :progression '("I" "V" "vi" "IV" "I" "V" "IV" "V" "vi" "IV" "V" "I")
   :parts (list (make-band-part :inst "steel" :role "fingerpick" :pattern "travis"
                                :register '(55 79) :jitter 4 :howl 0.45)
                (make-band-part :inst "fingerbass" :role "bass_walk"
                                :register '(40 52) :jitter 2)
                (make-band-part :inst "drums" :role "drums" :start-bar 2 :gain 0.9
                                :pattern "folk_full" :extra (list :fill t))
                (make-band-part :inst "strings" :role "block" :start-bar 4
                                :register '(57 81) :voices 4 :gain 0.52 :jitter 6)
                (make-band-part :inst "kalimba" :role "sparkle" :start-bar 8
                                :register '(72 91) :gain 0.5 :jitter 6
                                :extra (list :slots '(3 7 11 14)))
                (make-band-part :inst "nylon" :role "melody" :register '(64 81)
                                :gain 0.95 :jitter 6 :voices 3 :pan 0.16
                                :level 5.5 :send 1.2))))

(defun preset-pulse ()
  (make-band-score
   :name "pulse" :bpm 112 :key "A" :mode "minor" :bars 8 :seed 31
   :progression '("i" "VI" "III" "VII")
   :parts (list (make-band-part :inst "warm_pad" :role "block" :register '(52 76)
                                :voices 4 :gain 0.9 :jitter 3 :level -3.0 :send 1.4)
                (make-band-part :inst "fingerbass" :role "bass_root5"
                                :register '(33 45) :gain 0.95 :jitter 1
                                :level -1.0 :send 0.15
                                :extra (list :quarters t))
                (make-band-part :inst "drums" :role "drums" :start-bar 1 :gain 0.95
                                :pattern "pulse" :extra (list :fill t))
                (make-band-part :inst "glass_pad" :role "arp" :pattern "arp_up"
                                :start-bar 3 :register '(64 88) :gain 0.7
                                :jitter 3 :swing 0.06 :level -1.0 :send 1.6)
                (make-band-part :inst "rhodes" :role "melody" :start-bar 2
                                :register '(60 79) :gain 0.85 :jitter 4
                                :pan 0.10 :level 2.0 :send 1.4))))

(defun preset-funk ()
  (make-band-score
   :name "funk" :bpm 104 :key "E" :mode "dorian" :bars 16 :seed 41
   :progression '("im7" "im7" "im7" "im7" "IV7" "IV7" "im7" "im7"
                  "iim7" "iim7" "im7" "im7" "IV7" "IV7" "IV7" "im7")
   :parts (list (make-band-part :inst "drums" :role "drums" :pattern "funk"
                                :level 1.0)
                (make-band-part :inst "fingerbass" :role "slap_bass"
                                :register '(33 45) :jitter 2 :level -4.0)
                (make-band-part :inst "muted_gtr" :role "funk_guitar"
                                :register '(52 76) :gain 0.95 :jitter 3 :pan -0.28)
                (make-band-part :inst "clav" :role "stab" :start-bar 2
                                :register '(55 79) :gain 0.9 :jitter 2
                                :extra (list :steps '(2 6 10 14)))
                (make-band-part :inst "brass" :role "stab" :start-bar 8
                                :register '(52 74) :gain 0.62 :jitter 2
                                :extra (list :steps '(3 11)) :send 1.4))))

(defparameter +presets+
  `(("bed" . (,#'preset-bed . "room"))
    ("band" . (,#'preset-band . "hall"))
    ("pulse" . (,#'preset-pulse . "close"))
    ("funk" . (,#'preset-funk . "close")))
  "The bundled arrangements, each with the room it was written for.")

(defun preset-names () (mapcar #'car +presets+))

(defun preset (name)
  "The arrangement NAME names, or NIL."
  (let ((entry (assoc (string-downcase (or name "")) +presets+ :test #'string=)))
    (and entry (funcall (car (cdr entry))))))

(defun preset-room (name)
  (let ((entry (assoc (string-downcase (or name "")) +presets+ :test #'string=)))
    (and entry (cdr (cdr entry)))))
