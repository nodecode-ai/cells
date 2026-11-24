;;;; package.lisp --- the cell's own package, the Deepgram vocabulary, and the
;;;; two boundaries its tools read.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-deepgram-voice", which is this folder's name.

(defpackage #:nodecode-deepgram-voice
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*deepgram-voice*
           #:deepgram-voice-error #:deepgram-voice-error-detail))

(in-package #:nodecode-deepgram-voice)

(nlk:define-peripheral deepgram-voice :not-running t)

;;; --- what Deepgram offers, as data --------------------------------------------

(defparameter +voices+
  '(("aura-2-thalia-en" . "Thalia - clear, confident, energetic")
    ("aura-2-asteria-en" . "Asteria - warm, expressive")
    ("aura-2-luna-en" . "Luna - friendly, natural")
    ("aura-2-orion-en" . "Orion - approachable, professional")
    ("aura-2-arcas-en" . "Arcas - deep, calm")
    ("aura-2-zeus-en" . "Zeus - authoritative, steady"))
  "Deepgram Aura 2 voices, the ones the documented API lists. A voice's id is
sent as the `model` parameter of /v1/speak.")

(defparameter +voice-names+
  '("aura-2-thalia-en" "aura-2-asteria-en" "aura-2-luna-en"
    "aura-2-orion-en" "aura-2-arcas-en" "aura-2-zeus-en")
  "The voice ids, for the section's and the tool's own options.")

(defun voice-names ()
  "The voice ids, for the section's and the tool's own options."
  +voice-names+)

(defun voice-line (voice)
  "VOICE with the one-line character Deepgram gives it, or VOICE alone."
  (let ((entry (assoc voice +voices+ :test #'string=)))
    (if entry (format nil "~a: ~a" (car entry) (cdr entry)) voice)))

(defparameter +formats+
  '(("mp3" "mp3" "mp3" nil)
    ("wav" "wav" "linear16" "wav")
    ("ogg" "ogg" "opus" "ogg")
    ("opus" "ogg" "opus" "ogg")
    ("flac" "flac" "flac" nil))
  "The audio containers a speak call may ask for, as
(NAME EXTENSION ENCODING CONTAINER): what the file is saved as, and the two
query parameters /v1/speak is asked with.")

(defparameter +format-names+ '("mp3" "wav" "ogg" "opus" "flac")
  "The container choices, for the section's and the tool's own options.")

(defun format-entry (name)
  "The (NAME EXTENSION ENCODING CONTAINER) row NAME names, or NIL."
  (assoc (string-downcase (or name "")) +formats+ :test #'string=))

(defparameter +stt-models+
  '("nova-3" "nova-2" "enhanced" "base")
  "The pre-recorded Listen models Deepgram documents, newest first.")

(defparameter +max-speak-chars+ 2000
  "What one /v1/speak call is sized for: about two minutes of speech. A longer
text is refused here, before a billed call, with the sentence that says how to
split it.")

(defparameter +min-speed+ 0.7
  "The slowest rate /v1/speak takes.")

(defparameter +max-speed+ 1.5
  "The fastest rate /v1/speak takes.")

(defparameter +max-audio-bytes+ (* 30 1024 1024)
  "A cap on one spoken response, so a misbehaving endpoint cannot fill memory:
two minutes of 128 kbps MP3 is about 2 MB.")

(defparameter +max-upload-bytes+ (* 100 1024 1024)
  "A cap on the recording one transcribe call uploads. Deepgram accepts more,
but a cell that reads a whole file into memory says where its ceiling is.")

(defparameter +content-types+
  '(("mp3" . "audio/mpeg") ("mp2" . "audio/mpeg") ("m4a" . "audio/mp4")
    ("mp4" . "audio/mp4") ("wav" . "audio/wav") ("flac" . "audio/flac")
    ("ogg" . "audio/ogg") ("opus" . "audio/ogg") ("webm" . "audio/webm")
    ("aac" . "audio/aac") ("aiff" . "audio/aiff") ("amr" . "audio/amr"))
  "The content type a recording is uploaded as, by its extension. An unknown
extension is sent as application/octet-stream, which Deepgram sniffs.")

(defparameter +endpoint+ "https://api.deepgram.com/v1"
  "Deepgram's API root. The section's api_base_url replaces it, for a proxy or
a localhost stand-in.")

(defparameter *getenv* #'uiop:getenv
  "How an environment variable is read. A test binds this; nothing else does.")

;;; --- the two boundaries --------------------------------------------------------

(nlk:define-contract speak-request
  (:doc "Speak text out loud with Deepgram Aura 2 and save it as an audio file: narration, a spoken summary, a line for a video or a game. The answer names the saved file - give that path to the operator. Billed to the operator's Deepgram account.")
  ("text" :text :required t
          :doc "what to say, at most 2000 characters; split longer text into several calls")
  ("voice" :text :options +voice-names+
           :doc "which Aura 2 voice reads it; omit for the section's voice")
  ("format" :text :options +format-names+ :slot audio-format
           :doc "the audio container to save; omit for the section's audio_format")
  ("speed" :number :slot rate
           :doc "the speaking rate, 0.7 to 1.5; omitted, the voice's own"))

(nlk:define-contract transcribe-request
  (:doc "Transcribe a recording - a voice note, a meeting, an interview, a video's audio - with Deepgram Nova, and answer the words as text. Billed to the operator's Deepgram account.")
  ("file" :text :required t
          :doc "the recording: an absolute path, a ~/ path, or a path under the Nodecode home")
  ("model" :text :options +stt-models+
           :doc "which Listen model reads it; omit for the section's stt_model")
  ("language" :text
              :doc "the spoken language as a BCP-47 code such as en or pt-BR; omitted, Deepgram detects it")
  ("diarize" :boolean
             :doc "label who spoke each stretch of the recording")
  ("smart_format" :boolean :default t
                  :doc "punctuate, capitalise and write numbers and dates as a reader expects; on unless turned off"))
