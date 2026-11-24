;;;; cell.lisp --- Deepgram speech and transcription as two tools, the section
;;;; they run on, and /say.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites registered a text-to-speech and a
;;;; speech-to-text provider with Hermes. Nodecode has no provider slot for a
;;;; voice, so the same two calls became what a model can reach directly: one
;;;; tool that speaks text into a file, one that reads a recording back as
;;;; text, and /say for a line said out loud from the prompt line.

(in-package #:nodecode-deepgram-voice)

(defparameter +tool-names+ '("deepgram_speak" "deepgram_transcribe")
  "The tools' names, and what the tests and the lifecycle check look for.")

(defparameter +primer+
  "Deepgram voice is on through the nodecode-deepgram-voice cell: two tools,
deepgram_speak and deepgram_transcribe, and the /say command.
  deepgram_speak(text, voice?, format?, speed?)
      Speaks the text and saves it as an audio file; the answer's first line is that
      path, so give it to the operator -- it plays in any audio player. At most 2000
      characters per call: split longer text at sentence ends. voice is one of the Aura 2
      voices (aura-2-thalia-en, aura-2-asteria-en, aura-2-luna-en, aura-2-orion-en,
      aura-2-arcas-en, aura-2-zeus-en) and defaults to the section's; format is mp3,
      wav, ogg, opus or flac; speed runs 0.7 to 1.5.
  deepgram_transcribe(file, model?, language?, diarize?, smart_format?)
      Reads a recording -- a voice note, a meeting, a video's audio -- and answers the
      words as text. file is an absolute path, a ~/ path, or a path under the Nodecode
      home. language is a code such as en or pt-BR; omitted, Deepgram detects the spoken
      language and says which. diarize labels who spoke each stretch.
Every call is billed to the operator's Deepgram account (Aura and Nova bill per second
of audio), so speak once and reuse the file, and transcribe a recording once. What
leaves the machine is the text to speak, or the recording's audio, sent to Deepgram
with the key -- nothing else from the conversation. /say <text> says one line out
loud from the prompt and answers the file's path."
  "What (help :voice) answers while the cell runs.")

;;; --- the tools -------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY, with every refusal answered as the model's own failure sentence: a tool
call is repaired from what it reads, never from a condition it cannot see."
  `(handler-case (progn ,@body)
     (deepgram-voice-error (condition)
       (nle:failure (format nil "ERROR: ~a" (deepgram-voice-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: ~a" condition)))))

(defun speak-tool (request)
  "The deepgram_speak tool, from a materialized SPEAK-REQUEST."
  (answering
    (speak-now (speak-request-text request)
                :voice (speak-request-voice request)
                :format (speak-request-audio-format request)
                :speed (speak-request-rate request))))

(defun transcribe-tool (request)
  "The deepgram_transcribe tool, from a materialized TRANSCRIBE-REQUEST."
  (answering
    (transcribe-now (transcribe-request-file request)
                    :model (transcribe-request-model request)
                    :language (transcribe-request-language request)
                    :diarize (transcribe-request-diarize request)
                    :smart-format (if (null (transcribe-request-smart-format request)) nil t))))

;;; --- /say -------------------------------------------------------------------------

(defun voices-text ()
  "The voices this cell offers, and the one the section picks."
  (with-output-to-string (out)
    (format out "Say it out loud with /say <text>, or let the model call deepgram_speak.~%")
    (format out "voices, the section's ~a by default:~%" (setting :voice))
    (dolist (voice +voices+)
      (format out "  ~a~%" (voice-line (car voice))))
    (format out "formats: ~{~a~^, ~}; files are saved in ~a~%"
            +format-names+ (namestring (setting :directory)))
    (format out "a recording is read back as text by deepgram_transcribe~%")))

(defun run-say (args session-id)
  "One /say invocation: ARGS spoken by Deepgram and saved, or, with no
argument, the voices and formats this cell offers."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (voices-text)
        (answering (speak-now text)))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell deepgram-voice
  (:section ("deepgram-voice")
    (:guide "create a key at https://console.deepgram.com/ and paste it into api_key, or export DEEPGRAM_API_KEY and leave api_key empty; voice and audio_format are what speaking uses, stt_model what transcription uses, and directory is where the spoken files are kept; every call is billed to that account")
    ("api_key" :secret
               :doc "the Deepgram API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "DEEPGRAM_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("voice" :choice :options +voice-names+
             :default "aura-2-thalia-en"
             :doc "the Aura 2 voice a speak call uses when it names none")
    ("audio_format" :choice :options +format-names+
                    :default "mp3"
                    :doc "the container a spoken file is saved as: mp3 plays inline in most chat apps")
    ("stt_model" :choice :options +stt-models+
                 :default "nova-3"
                 :doc "the Listen model a transcription uses when a call names none")
    ("api_base_url" :string :default "https://api.deepgram.com/v1"
                    :doc "Deepgram's API root; change it only for a proxy, and https is required")
    ("directory" :directory :default "voice/"
                 :doc "where spoken files are kept, under the home"))
  ;; The one thing the declaration cannot say: the API root is a web address,
  ;; and plain http would send the key in clear.
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :api-base-url) (checked-base-url (getf values :api-base-url)))
               values))
  (:tool "deepgram_speak" (nlk:contract 'speak-request) 'speak-tool)
  (:tool "deepgram_transcribe" (nlk:contract 'transcribe-request) 'transcribe-tool)
  (:help :voice "voice: deepgram_speak saves spoken text as an audio file; deepgram_transcribe reads a recording back as text" +primer+)
  (:command "say" 'run-say
            :description "Say a line out loud: Deepgram speaks it into an audio file"
            :argument-hint "TEXT")
  (:start (lambda ()
            ;; The folder the operator is pointed at exists from the first start.
            (ensure-directories-exist (setting :directory)))))
