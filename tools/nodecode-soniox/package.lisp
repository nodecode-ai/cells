;;;; package.lisp --- the cell's own package, the Soniox vocabulary, and the
;;;; two boundaries its tools read.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-soniox", which is this folder's name.

(defpackage #:nodecode-soniox
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*soniox*
           #:soniox-error #:soniox-error-detail))

(in-package #:nodecode-soniox)

(nlk:define-peripheral soniox :not-running t)

;;; --- what Soniox offers, as data ---------------------------------------------

(defparameter +voices+
  '(("Adrian" . "Adrian - English, neutral")
    ("Daniel" . "Daniel - English")
    ("Meilin" . "Meilin - Mandarin")
    ("Wei" . "Wei - Mandarin")
    ("Aoi" . "Aoi - Japanese")
    ("Jiwoo" . "Jiwoo - Korean")
    ("Lucia" . "Lucia - Spanish")
    ("Priya" . "Priya - Indian English")
    ("Camille" . "Camille - French")
    ("Annika" . "Annika - German"))
  "A short list of Soniox voices, as (ID . ONE-LINE CHARACTER), the ones the
plugin this cell rewrites shipped. Any voice id the account's catalogue serves
may be named instead: this is a starting point, not the whole set.")

(defun voice-line (voice)
  "VOICE with the one-line character Soniox gives it, or VOICE alone."
  (let ((entry (assoc voice +voices+ :test #'string=)))
    (if entry (format nil "~a: ~a" (car entry) (cdr entry)) voice)))

(defparameter +formats+
  '(("mp3" "mp3") ("wav" "wav") ("flac" "flac") ("opus" "opus") ("ogg" "opus"))
  "The containers a speak call may ask for, as (NAME SONIOX AUDIO_FORMAT).
Both ogg and opus ask Soniox for opus; the file is saved as .ogg when the
bytes are an Ogg container and .opus when they are raw packets.")

(defparameter +format-names+ '("mp3" "wav" "flac" "opus" "ogg")
  "The container choices, for the section's and the tool's own options.")

(defun format-entry (name)
  "The (NAME AUDIO-FORMAT) row NAME names, or NIL."
  (assoc (string-downcase (or name "")) +formats+ :test #'string=))

(defparameter +tts-model+ "tts-rt-v2"
  "The Soniox text-to-speech model the plugin defaulted to, and this cell does.")

(defparameter +stt-model+ "stt-async-v6"
  "The Soniox asynchronous speech-to-text model the plugin defaulted to, and
this cell does.")

(defparameter +tts-language+ "en"
  "The language a speak call asks for when neither the call nor the section
names one.")

(defparameter +tts-voice+ "Adrian"
  "The voice a speak call uses when neither the call nor the section names one.")

(defparameter +max-speak-chars+ 5000
  "What one text-to-speech call is sized for: Soniox's own limit. A longer
text is refused here, before a billed call, with the sentence that says how to
split it.")

(defparameter +min-speed+ 0.7
  "The slowest rate Soniox speaks at. A call below it is clamped, not refused:
the plugin clamped, and a pace is a preference, not a mistake.")

(defparameter +max-speed+ 1.3
  "The fastest rate Soniox speaks at.")

(defparameter +max-context-chars+ 10000
  "The most prompt text a transcription sends as context, about 8000 tokens.")

(defparameter +max-audio-bytes+ (* 16 1024 1024)
  "A cap on one spoken response, so a misbehaving endpoint cannot fill memory:
5000 characters of speech is well under this.")

(defparameter +max-upload-bytes+ (* 100 1024 1024)
  "A cap on the recording one transcribe call uploads. Soniox accepts more,
but a cell that reads a whole file says where its ceiling is.")

(defparameter +content-types+
  '(("mp3" . "audio/mpeg") ("mp2" . "audio/mpeg") ("m4a" . "audio/mp4")
    ("mp4" . "audio/mp4") ("wav" . "audio/wav") ("flac" . "audio/flac")
    ("ogg" . "audio/ogg") ("opus" . "audio/ogg") ("webm" . "audio/webm")
    ("aac" . "audio/aac") ("aiff" . "audio/aiff") ("amr" . "audio/amr"))
  "The content type a recording is uploaded as, by its extension. Soniox
sniffs the file itself; this is what the part says it is.")

(defparameter *getenv* #'uiop:getenv
  "How an environment variable is read. A test binds this; nothing else does.")

(defparameter *sleep* #'sleep
  "How a poll waits between two status reads. A test binds this, so a
transcription that takes 300 seconds of Soniox's time takes none of the run's.")

(defparameter *monotonic*
  (lambda () (/ (get-internal-real-time) 1.0 internal-time-units-per-second))
  "Seconds since an arbitrary start, the clock a poll's timeout is measured
with. A test binds this to a counter its *SLEEP* advances.")

;;; --- the two boundaries --------------------------------------------------------

(nlk:define-contract speak-request
  (:doc "Speak text out loud with Soniox and save it as an audio file: narration, a spoken summary, a line for a video or a game. The answer names the saved file - give that path to the operator. Billed to the operator's Soniox account.")
  ("text" :text :required t
          :doc "what to say, at most 5000 characters; split longer text into several calls")
  ("voice" :text
           :doc "the voice id to read it, such as Adrian, Meilin or Aoi (any voice the account's catalogue serves); omit for the section's voice")
  ("language" :text
              :doc "the spoken language as a code such as en or zh; a BCP-47 value like zh-CN reads as zh; omit for the section's language")
  ("format" :text :options +format-names+ :slot audio-format
           :doc "the audio container to save; omit for the section's audio_format")
  ("speed" :number :slot rate
           :doc "the pace, 0.7 to 1.3; outside that it is clamped, and omitted the voice's own"))

(nlk:define-contract transcribe-request
  (:doc "Transcribe a recording - a voice note, a meeting, an interview, a video's audio - with Soniox and answer the words as text. Billed to the operator's Soniox account.")
  ("file" :text :required t
          :doc "the recording: an absolute path, a ~/ path, or a path under the Nodecode home")
  ("model" :text
           :doc "the asynchronous model that reads it, such as stt-async-v6; omit for the section's stt_model")
  ("language" :text
              :doc "the spoken language as one code, or several comma separated (zh,en); omitted, Soniox detects it")
  ("prompt" :text
            :doc "names and words the recording is likely to contain, sent as context; at most 10000 characters"))
