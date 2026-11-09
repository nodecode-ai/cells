;;;; package.lisp --- the cell's own package, and the vocabulary every file shares.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-omnivoice", which is this folder's name.

(defpackage #:nodecode-omnivoice
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*omnivoice*
           #:omnivoice-error #:omnivoice-error-detail))

(in-package #:nodecode-omnivoice)

(nlk:define-peripheral omnivoice :not-running t)

;;; --- what the runtime writes, and what a call may ask for ---------------------------

(defparameter +formats+ '("wav" "mp3" "flac" "ogg")
  "The containers the OmniVoice runtime writes: WAV, FLAC and Ogg Vorbis
straight from the model's samples, MP3 through ffmpeg -- and WAV in place of
MP3 when ffmpeg is not installed, which a call finds after the fact.")

(defparameter +max-text-chars+ 5000
  "What the runtime's model advertises as one text's length; longer is refused
here, before a model load is spent on it.")

(defparameter +output-tail+ 800
  "How much of a failed runtime's own output is quoted back to the model.")

(defparameter *getenv* #'uiop:getenv
  "How an environment variable is read. A test binds this; nothing else does.")
