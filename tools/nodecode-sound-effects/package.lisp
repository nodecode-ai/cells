;;;; package.lisp --- the cell's own package, and the vocabulary every file shares.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-sound-effects", which is this folder's name.

(defpackage #:nodecode-sound-effects
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*sound-effects*
           #:sound-effects-error #:sound-effects-error-detail))

(in-package #:nodecode-sound-effects)

(nlk:define-peripheral sound-effects :not-running t)

;;; --- what ElevenLabs offers, as data -------------------------------------------

(defparameter +models+ '("eleven_text_to_sound_v2" "eleven_text_to_sound_v3")
  "ElevenLabs' sound-effect models, the ones its API documents.")

(defparameter +formats+
  '(("mp3_44100_128" . "mp3") ("mp3_44100_192" . "mp3")
    ("opus_48000_128" . "opus") ("opus_48000_192" . "opus")
    ("wav_44100" . "wav") ("wav_48000" . "wav"))
  "The output_format choices, and the container each one is saved with. Every
extension here is one a chat surface or a game engine reads.")

(defun format-names ()
  "The output_format choices, for the section's own options."
  (mapcar #'car +formats+))

(defun extension-for (output-format)
  "The file extension OUTPUT-FORMAT is saved with, or NIL for a format this
cell does not offer."
  (cdr (assoc output-format +formats+ :test #'string=)))

(defparameter +min-seconds+ 0.5
  "The shortest clip ElevenLabs generates.")

(defparameter +max-seconds+ 30.0
  "The longest clip ElevenLabs generates.")

(defparameter +max-prompt-chars+ 450
  "What the API accepts: a longer description is refused with HTTP 400. Refused
here first, so a billed call is never spent on it.")

(defparameter +max-audio-bytes+ (* 50 1024 1024)
  "A cap on one response, so a misbehaving endpoint cannot fill memory: 30 s of
48 kHz WAV is about 6 MB.")

(defparameter +default-influence+ 0.3
  "What ElevenLabs uses when prompt_influence is not sent.")

(defparameter *getenv* #'uiop:getenv
  "How an environment variable is read. A test binds this; nothing else does.")
