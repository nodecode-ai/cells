;;;; package.lisp --- the cell's own package, and what every file shares.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-speech", which is this folder's name.

(defpackage #:nodecode-speech
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*speech*
           #:speech-error #:speech-error-detail))

(in-package #:nodecode-speech)

(nlk:define-peripheral speech :not-running t)

;;; --- what AllModels offers, as data --------------------------------------------

(defparameter +root-default+ "https://api.allmodels.io"
  "AllModels' API root. The OpenAI-compatible audio endpoints sit under /oai.")

(defparameter +openai-path+ "/oai"
  "Where the OpenAI-compatible surface (audio/speech, audio/transcriptions)
lives under the root: the plugin's OPENAI_BASE_URL is the root plus this.")

(defparameter +formats+
  '(("mp3" . "mp3") ("opus" . "opus") ("aac" . "aac")
    ("flac" . "flac") ("wav" . "wav") ("pcm" . "pcm"))
  "The response_format choices, and the extension each is saved with. The
plugin maps Hermes' Ogg request to opus and keeps mp3 for local playback.")

(defun format-names ()
  "The format choices, for the section's own options."
  (mapcar #'car +formats+))

(defparameter +min-speed+ 0.25
  "The slowest speech the API accepts, as the plugin clamps it.")

(defparameter +max-speed+ 4.0
  "The fastest speech the API accepts.")

(defparameter +max-audio-bytes+ (* 25 1024 1024)
  "A cap on one response, so a misbehaving endpoint cannot fill memory: 25 MB
is far past ten minutes of 128 kbps MP3.")

(defparameter +max-upload-bytes+ (* 25 1024 1024)
  "The most audio one transcription may send: the ceiling the hosted
transcription APIs share.")

(defparameter +max-voice-rows+ 50
  "The most voices one search lists; the catalogue's own page is smaller.")

(defparameter +default-voice-limit+ 10
  "How many voices a search lists when the call names no limit.")

(defparameter *getenv* #'uiop:getenv
  "How an environment variable is read. A test binds this; nothing else does.")

(defparameter +uneven-voice-note+
  "A voice id and the provider that serves it travel together: pass both, or
keep both in the section.")

(defun voice-pair-text (voice provider)
  "The voice as a line names it, with the provider when one is known."
  (if (and provider (plusp (length provider)))
      (format nil "~a (~a)" voice provider)
      voice))
