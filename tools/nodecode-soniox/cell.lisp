;;;; cell.lisp --- Soniox speech and transcription as two tools, the section
;;;; they run on, and /soniox.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites registered a text-to-speech and a
;;;; speech-to-text provider with Hermes, which asked it for a voice and a
;;;; transcript as the harness needed them. Nodecode has no provider slot for a
;;;; voice, so the same two calls became what a model can reach directly: one
;;;; tool that speaks text into a file, one that reads a recording back as
;;;; text, and /soniox for a line said out loud from the prompt line.

(in-package #:nodecode-soniox)

(defparameter +tool-names+ '("soniox_speak" "soniox_transcribe")
  "The tools' names, and what the tests and the lifecycle check look for.")

(defparameter +primer+
  "Soniox voice is on through the nodecode-soniox cell: two tools, soniox_speak and
soniox_transcribe, and the /soniox command.
  soniox_speak(text, voice?, language?, format?, speed?)
      Speaks the text with Soniox and saves it as an audio file; the answer's first
      line is that path, so give it to the operator -- it plays in any audio player.
      At most 5000 characters per call: split longer text at sentence ends. voice is a
      voice id such as Adrian, Meilin, Aoi or Camille (any id the account's catalogue
      serves) and defaults to the section's; language is a code such as en or zh, and
      zh-CN reads as zh; format is mp3, wav, flac, opus or ogg; speed runs 0.7 to 1.3
      and a value outside it is clamped.
  soniox_transcribe(file, model?, language?, prompt?)
      Reads a recording -- a voice note, a meeting, a video's audio -- and answers the
      words as text. file is an absolute path, a ~/ path, or a path under the Nodecode
      home. language is one code or several comma separated (zh,en); omitted, Soniox
      detects the spoken language. prompt carries names and jargon the recording is
      likely to contain. The upload and the transcription are deleted from Soniox once
      the words are read.
Every call is billed to the operator's Soniox account, so speak once and reuse the
file, and transcribe a recording once. What leaves the machine is the text to speak or
the recording's audio, sent to Soniox with the key -- nothing else from the
conversation. /soniox <text> says one line out loud from the prompt and answers the
file's path; /soniox alone says what the cell is set to."
  "What (help :soniox) answers while the cell runs.")

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY, with every refusal answered as the model's own failure sentence: a tool
call is repaired from what it reads, never from a condition it cannot see."
  `(handler-case (progn ,@body)
     (soniox-error (condition)
       (nle:failure (format nil "ERROR: ~a" (soniox-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: ~a" condition)))))

(defun speak-tool (request)
  "The soniox_speak tool, from a materialized SPEAK-REQUEST."
  (answering
    (speak-now (speak-request-text request)
                :voice (speak-request-voice request)
                :language (speak-request-language request)
                :format (speak-request-audio-format request)
                :speed (speak-request-rate request))))

(defun transcribe-tool (request)
  "The soniox_transcribe tool, from a materialized TRANSCRIBE-REQUEST."
  (answering
    (transcribe-now (transcribe-request-file request)
                    :model (transcribe-request-model request)
                    :language (transcribe-request-language request)
                    :prompt (transcribe-request-prompt request))))

;;; --- /soniox ----------------------------------------------------------------------

(defun settings-text ()
  "What this cell is set to, and the voices and containers it offers."
  (with-output-to-string (out)
    (format out "Say it out loud with /soniox <text>, or let the model call soniox_speak.~%")
    (format out "speaking with ~a, language ~a, voice ~a, ~a by default; files are saved ~
                 in ~a~%"
            (setting :model) (setting :language) (setting :voice)
            (setting :audio-format) (namestring (setting :directory)))
    (format out "reading a recording back as text with ~a~@[, language ~a~]; ~
                 soniox_transcribe does it for a file~%"
            (setting :stt-model) (filled (setting :stt-language)))
    (format out "voices, a starting point rather than the whole catalogue:~%")
    (dolist (voice +voices+)
      (format out "  ~a~%" (voice-line (car voice))))
    (format out "containers: ~{~a~^, ~}~%" +format-names+)))

(defun run-command (args session-id)
  "One /soniox invocation: ARGS spoken by Soniox and saved, or, with no
argument, what this cell is set to."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (settings-text)
        (answering (speak-now text)))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell soniox
  (:section ("soniox")
    (:guide "create a key at https://console.soniox.com/ and paste it into api_key, or export SONIOX_API_KEY and leave api_key empty; model, language, voice and audio_format are what speaking uses, stt_model and stt_language what transcription uses, and directory is where the spoken files are kept; every call is billed to that account")
    ("api_key" :secret
               :doc "the Soniox API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "SONIOX_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("model" :string :default "tts-rt-v2"
             :doc "the Soniox text-to-speech model a speak call uses")
    ("voice" :string :default "Adrian"
             :doc "the voice a speak call uses when it names none: Adrian, Daniel, Meilin, Wei, Aoi, Jiwoo, Lucia, Priya, Camille, Annika, or any id the account's catalogue serves")
    ("language" :string :default "en"
                :doc "the language a speak call speaks in when it names none, as an ISO code")
    ("audio_format" :choice :options +format-names+ :default "mp3"
                    :doc "the container a spoken file is saved as: mp3 plays inline in most chat apps")
    ("stt_model" :string :default "stt-async-v6"
                 :doc "the asynchronous speech-to-text model a transcribe call uses when it names none")
    ("stt_language" :string
                    :doc "the language recordings are mostly in, or several comma separated (zh,en); unset lets Soniox detect it")
    ("poll_timeout_seconds" :integer :default 300 :min 5
                            :doc "how long a transcription may take before the call gives up, in seconds")
    ("api_base_url" :string :default "https://api.soniox.com/v1"
                    :doc "Soniox's API root for files and transcriptions; change it only for a proxy, and https is required")
    ("tts_url" :string :default "https://tts-rt.soniox.com/tts"
               :doc "Soniox's text-to-speech endpoint; change it only for a proxy, and https is required")
    ("directory" :directory :default "soniox/"
                 :doc "where spoken files are kept, under the home"))
  ;; The one thing the declaration cannot say: both roots are web addresses,
  ;; and plain http would send the key in clear.
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :api-base-url) (checked-url (getf values :api-base-url) "api_base_url"))
               (setf (getf values :tts-url) (checked-url (getf values :tts-url) "tts_url"))
               values))
  (:tool "soniox_speak" (nlk:contract 'speak-request) 'speak-tool)
  (:tool "soniox_transcribe" (nlk:contract 'transcribe-request) 'transcribe-tool)
  (:help :soniox "soniox: soniox_speak saves spoken text as an audio file; soniox_transcribe reads a recording back as text" +primer+)
  (:command "soniox" 'run-command
            :description "Say a line out loud: Soniox speaks it into an audio file"
            :argument-hint "TEXT")
  (:start (lambda ()
            ;; The folder the operator is pointed at exists from the first start.
            (ensure-directories-exist (setting :directory)))))
