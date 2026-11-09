;;;; package.lisp --- the cell's own package, and the vocabulary every file
;;;; shares.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-live-voice", which is this folder's name.

(defpackage #:nodecode-live-voice
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*live-voice*
           #:live-voice-error #:live-voice-error-detail))

(in-package #:nodecode-live-voice)

(nlk:define-peripheral live-voice :not-running t)

;;; --- what OpenAI's audio API offers, as data --------------------------------

(defparameter +transcribe-models+ '("gpt-4o-mini-transcribe" "gpt-4o-transcribe" "whisper-1")
  "The transcription models the section offers. The mini one is the cheap
default; whisper-1 is the oldest and takes a language hint most literally.")

(defparameter +speech-models+ '("gpt-4o-mini-tts" "tts-1" "tts-1-hd")
  "The speech models the section offers: gpt-4o-mini-tts follows
instructions, the tts-1 pair do not.")

(defparameter +formats+
  '(("mp3" . "mp3") ("wav" . "wav") ("opus" . "opus") ("flac" . "flac") ("aac" . "aac"))
  "The audio_format choices, and the container each one is saved with.")

(defun format-names ()
  "The audio_format choices, for the section's own options."
  (mapcar #'car +formats+))

(defun extension-for (format)
  "The file extension FORMAT is saved with, or NIL for a format this cell
does not offer."
  (cdr (assoc format +formats+ :test #'string=)))

(defparameter +default-listen-seconds+ 8
  "What voice_listen records when the call names no length.")

(defparameter +max-listen-seconds+ 300
  "The ceiling on one recording, whatever the section's max_seconds says: the
transcription API's own file limit is the reason, not taste.")

(defparameter +max-audio-bytes+ (* 25 1024 1024)
  "A cap on one speech response, so a misbehaving endpoint cannot fill memory:
the 25 MB the transcription API shares.")

(defvar *getenv* #'uiop:getenv
  "How an environment variable is read. A test binds this; nothing else does.")
