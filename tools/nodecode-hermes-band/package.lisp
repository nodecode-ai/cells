;;;; package.lisp --- the cell's package and the musical vocabulary.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of RegeneratusLabs/hermes-band: an arrangement is data (tempo,
;;;; key, chords, parts), and it is rendered to an exact-length wav and mp3 on
;;;; this machine with fluidsynth and ffmpeg -- no music API, no model, no key.

(defpackage #:nodecode-hermes-band
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*band*
           #:band-error #:band-error-detail))

(in-package #:nodecode-hermes-band)

(nlk:define-peripheral hermes-band :not-running t)

;;; --- the grid -------------------------------------------------------------------

(defparameter +div+ 480 "Ticks per quarter note in every file this cell writes.")
(defparameter +e+ 240 "One eighth note in ticks.")
(defparameter +beats+ 4 "Beats in a bar.")

;;; --- what a score may name ------------------------------------------------------

(defparameter +roles+
  '("fingerpick" "strum" "block" "bass_root5" "bass_walk" "arp" "melody"
    "sparkle" "drums" "funk_guitar" "slap_bass" "stab")
  "How a part plays. The vocabulary is the plugin's.")

(defparameter +fingering-patterns+ '("folk" "travis" "sparse" "arp_up" "rolling" "oompah")
  "Fingering patterns a fingerpicked or arpeggiated part may name.")

(defparameter +drum-patterns+ '("folk_soft" "folk_full" "pulse" "heartbeat" "boom_bap" "funk")
  "Drum patterns a drums part may name.")

(defparameter +rooms+
  '(("dry" . (:size 0.55 :rt60 0.85 :wet 0.14))
    ("close" . (:size 0.8 :rt60 1.4 :wet 0.24))
    ("room" . (:size 1.0 :rt60 1.9 :wet 0.30))
    ("hall" . (:size 1.5 :rt60 2.9 :wet 0.38))
    ("cathedral" . (:size 2.2 :rt60 4.2 :wet 0.46)))
  "Reverb rooms, as the plugin names them: wet is the send's share.")

(defparameter +room-names+ (mapcar #'car +rooms+))

(defparameter +programs+
  '(("piano" . 0) ("rhodes" . 4) ("vibes" . 11) ("kalimba" . 108)
    ("nylon" . 24) ("steel" . 25) ("steel2" . 25) ("clean_elec" . 27)
    ("muted_gtr" . 28) ("clav" . 7) ("organ" . 16) ("brass" . 61)
    ("upright" . 32) ("fingerbass" . 33) ("cello" . 42) ("strings" . 48)
    ("strings_hi" . 49) ("choir" . 52) ("flute" . 73) ("warm_pad" . 89)
    ("glass_pad" . 92)
    ("drums" . 0))
  "General MIDI programs by the plugin's instrument names.")

(defparameter +drum-notes+
  '(("kick" . 36) ("stick" . 37) ("snare" . 38) ("clap" . 39)
    ("hh" . 42) ("hh_pedal" . 44) ("hh_open" . 46) ("shaker" . 70))
  "The kit the drum patterns address, on channel 10.")

(defparameter +soundfont-candidates+
  '("/usr/share/sounds/sf2/FluidR3_GM.sf2"
    "/usr/share/sounds/sf2/FluidR3_GS.sf2"
    "/usr/share/sounds/sf2/default-GM.sf2"
    "/usr/share/soundfonts/default.sf2")
  "Where a General MIDI soundfont usually lives when the section names none.")

;;; --- deterministic noise ---------------------------------------------------------

(defun make-rng (seed)
  "A closure over a small linear congruential generator: (RNG N) is an integer
in [0, N). The same seed gives the same performance, in every process, so a
render is reproducible."
  (let ((state (logand (+ (* 2654435761 (abs seed)) 12345) #xFFFFFFFF)))
    (lambda (n)
      (setf state (logand (+ (* 1103515245 state) 12345) #xFFFFFFFF))
      (if (plusp n) (mod (ash state -8) n) 0))))

(defun between (rng low high)
  "A number in [LOW, HIGH] from RNG, as a float."
  (+ low (* (float (funcall rng 10001) 1.0) (/ (- high low) 10000.0))))
