;;;; package.lisp --- the cell's own package, and the vocabulary every file shares.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-fish-audio", which is this folder's name.

(defpackage #:nodecode-fish-audio
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*fish-audio*
           #:fish-audio-error #:fish-audio-error-detail))

(in-package #:nodecode-fish-audio)

(nlk:define-peripheral fish-audio :not-running t)

;;; --- what Fish Audio offers, as data ------------------------------------------

(defparameter +tts-models+ '("s2.1-pro" "s2.1-pro-free" "s2-pro" "drama-3")
  "Fish Audio's speech models, in the order a picker shows them. s1 is
deprecated and left out; drama-3 is the one for multi-speaker scenes.")

(defparameter +stt-models+ '("transcribe-1-pro" "transcribe-1")
  "Fish Audio's transcription models. transcribe-1-pro is the one that takes
WebM and speaker hints; transcribe-1 is the older, cheaper one.")

(defparameter +formats+ '("mp3" "ogg" "wav")
  "The audio containers a call may ask for. `ogg' is Ogg Opus, which Fish
Audio names `opus': small enough for a voice note, and playable in browsers.")

(defparameter +audio-kinds+
  '(("mp3" . "audio/mpeg") ("wav" . "audio/wav") ("ogg" . "audio/ogg")
    ("opus" . "audio/ogg") ("webm" . "audio/webm") ("flac" . "audio/flac")
    ("mp4" . "audio/mp4") ("m4a" . "audio/mp4") ("aac" . "audio/aac"))
  "The input formats Fish Audio transcribes, and the MIME each is sent as.")

(defparameter +max-audio-bytes+ (* 32 1024 1024)
  "A cap on one response, so a misbehaving endpoint cannot fill memory.")

(defparameter +max-upload-bytes+ (* 50 1024 1024)
  "What Fish Audio accepts for one transcription, and the cap read here first.")

(defparameter +max-sample-bytes+ (* 20 1024 1024)
  "What Fish Audio accepts for one cloning sample.")

(defparameter +max-text-chars+ 3000
  "A ceiling on one synthesis request, refused here rather than billed for and
rejected by the API: a longer text is best spoken in paragraphs.")

(defparameter *getenv* #'uiop:getenv
  "How an environment variable is read. A test binds this; nothing else does.")

(defun model-name (value)
  "VALUE when it is one of the models a call may name, else NIL."
  (and (stringp value) (member value +tts-models+ :test #'string=) value))

(defun stt-model-name (value)
  (and (stringp value) (member value +stt-models+ :test #'string=) value))

(defun extension-for (format)
  "The file extension FORMAT is saved with, or NIL for a format this cell does
not offer."
  (and (member format +formats+ :test #'string=) format))

(defun api-format (format)
  "The `format' field Fish Audio reads: it calls Ogg Opus `opus'."
  (if (string= format "ogg") "opus" format))
