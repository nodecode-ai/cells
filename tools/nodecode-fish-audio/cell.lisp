;;;; cell.lisp --- Fish Audio's three verbs, the /fish command, and the section
;;;; they read.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The plugin this cell rewrites gave Hermes a speech provider and three
;;;; tools; a cell is reached from eval, so the three tools are kept under
;;;; their own names -- a prompt written for fish_speak still reads right --
;;;; and /fish stands in for the plugin's /fish command. Every call is a
;;;; billed Fish Audio request on the account the key belongs to, which is why
;;;; each tool description says so and every argument is checked first.

(in-package #:nodecode-fish-audio)

;;; --- the primer ----------------------------------------------------------------

(defparameter +primer+
  "Fish Audio is reachable through the nodecode-fish-audio cell: three tools and the
/fish command. Every call is a paid Fish Audio API request on the account the key
belongs to, so make one call per thing the user asked for.
  fish_speak(text, voice?, model?, format?, speed?, language?, speakers?)
      ONE piece of text becomes one audio file in this profile's fish-audio folder and
      the answer names its path: give that path to the operator. Emotion cues ride in
      square brackets -- [excited], [whispering], [laughing], [sighing] -- and
      drama-3 is the model for multi-speaker scenes, where speakers takes 2 to 4 voice
      ids. speed is 0.5 to 2.0. format is mp3 (playable anywhere), ogg (Ogg Opus,
      small enough for a voice note) or wav.
  fish_voices(action, ...)
      search: the public library, by query (words in the title), language and sort
      (score, task_count, created_at), 1 to 20 a page. mine: the voices on this
      account. get: one voice in full. clone: 1 to 20 local audio samples become a
      voice -- ask the speaker for permission first and pass consent true; a clone is
      billed and lands private or unlisted.
      A voice id is 8 to 128 letters, digits, - and _: search first, then pass the id
      to fish_speak's voice.
  fish_transcribe(file_path, model?, language?, timestamps?, diarize?, srt?)
      A local audio or video file becomes text, with speaker turns and language when
      transcribe-1-pro reads them, and with srt true a .srt file beside the audio.
      file_path is absolute, or relative to the folder Nodecode runs in.
/fish alone prints the settings and whether a key is set; /fish voices <words> searches
the library; /fish voice <id> shows one."
  "What (help :fish) answers while the cell runs.")

;;; --- the contracts ----------------------------------------------------------------

;;; --- /fish ------------------------------------------------------------------------

(defun key-present-p ()
  (let ((key (trimmed-text (setting :api-key))))
    (or (and key (plusp (length key)))
        (let ((from-env (trimmed-text (funcall *getenv* (setting :api-key-env)))))
          (and from-env (plusp (length from-env)))))))

(defun status-text ()
  "The settings a call runs on, and whether a key is there: two facts that
cost nothing to read, and the one thing to check first when a call refuses."
  (with-output-to-string (out)
    (format out "fish-audio (nodecode-fish-audio)~%")
    (format out "  key: ~a~%"
            (if (key-present-p)
                "set"
                (format nil "not set -- paste one into the fish-audio section's api_key, or set ~a (create one at https://fish.audio/app/api-keys)"
                        (setting :api-key-env))))
    (format out "  endpoint: ~a~%" (setting :api-base-url))
    (format out "  model: ~a   transcription: ~a~%" (setting :tts-model) (setting :stt-model))
    (format out "  voice: ~a   format: ~a~%"
            (let ((voice (trimmed-text (setting :voice))))
              (if (and voice (plusp (length voice))) voice "Fish Audio's default"))
            (setting :output-format))
    (format out "  files: ~a~%" (namestring (setting :directory)))
    (format out "  tools: fish_speak, fish_voices, fish_transcribe -- (help :fish) explains them~%")
    (format out "  /fish voices <words> searches the library; /fish voice <id> shows one~%")))

(defun slash-voices (words)
  "The library from the command line: a search of WORDS, or the first page of
it when WORDS is empty. Refusals come back as one sentence, as a command
answers, not as a signalled condition."
  (handler-case
      (voices-now (nlk:contract-decode (nlk:contract 'fish-voices)
                                        (nlk:make-json-object "action" "search"
                                                               "query" words
                                                               "page_size" 10)))
    (fish-audio-error (condition) (fish-audio-error-detail condition))
    (error (condition) (format nil "ERROR: ~a" condition))))

(defun slash-voice (id)
  (handler-case
      (voices-now (nlk:contract-decode (nlk:contract 'fish-voices)
                                        (nlk:make-json-object "action" "get" "voice_id" id)))
    (fish-audio-error (condition) (fish-audio-error-detail condition))
    (error (condition) (format nil "ERROR: ~a" condition))))

(defun run-slash (args session-id)
  "One /fish invocation: no argument or `status' prints the settings,
`voices [words]' searches the library, `voice <id>' shows one."
  (declare (ignore session-id))
  (let* ((line (nlk:trimmed (or args "")))
         (space (position #\Space line))
         (verb (if space (subseq line 0 space) line))
         (rest (if space (nlk:trimmed (subseq line (1+ space))) "")))
    (cond ((or (zerop (length line)) (string-equal verb "status")) (status-text))
          ((string-equal verb "voices") (slash-voices (if (zerop (length rest)) "" rest)))
          ((string-equal verb "voice")
           (if (zerop (length rest))
               "which voice? pass its id: /fish voice <id>"
               (slash-voice rest)))
          (t (format nil "~a~%unknown: ~a -- try /fish, /fish voices <words> or /fish voice <id>"
                      (status-text) line)))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell fish-audio
  (:section ("fish-audio")
    (:guide "create a key at https://fish.audio/app/api-keys; paste it in api_key, or name the variable that holds it in api_key_env (FISH_API_KEY by default). tts_model, stt_model, voice and output_format are what every call uses unless it names its own; directory is where the audio and subtitle files are kept; every call is billed to that Fish Audio account")
    ("api_key" :secret
               :doc "the Fish Audio API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "FISH_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("api_base_url" :string :default "https://api.fish.audio"
                    :doc "change only for an enterprise Fish Audio deployment; https, or http on a loopback stand-in")
    ("tts_model" :choice :options +tts-models+ :default "s2.1-pro"
                 :doc "the speech model a call uses when it names none; s2.1-pro-free is free for now and Fish may train on its requests, drama-3 is the expressive one")
    ("stt_model" :choice :options +stt-models+ :default "transcribe-1-pro"
                 :doc "the transcription model a call uses when it names none")
    ("voice" :string :default ""
             :doc "the voice id every fish_speak call uses unless it names one; empty is Fish Audio's default voice")
    ("output_format" :choice :options +formats+ :default "mp3"
                     :doc "the container a file is saved in: mp3 plays anywhere, ogg is Ogg Opus, wav is for editing")
    ("directory" :directory :default "fish-audio/"
                 :doc "where the audio, the subtitles and nothing else are kept, under the home"))
  ;; The one thing the declaration cannot say: a base URL is a web address,
  ;; and plain http is a cleartext key.
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :api-base-url) (checked-base-url (getf values :api-base-url)))
               (setf (getf values :voice) (or (getf values :voice) ""))
               values))
  (:tool "fish_speak" (nlk:contract 'fish-speech) 'speak)
  (:tool "fish_voices" (nlk:contract 'fish-voices) 'voices)
  (:tool "fish_transcribe" (nlk:contract 'fish-transcript) 'transcribe)
  (:help :fish "fish: Fish Audio speech, dictation and voices - fish_speak, fish_voices, fish_transcribe, and /fish" +primer+)
  (:command "fish" 'run-slash
            :description "Fish Audio: the settings, the voice library, one voice"
            :argument-hint "[status|voices [words]|voice <id>]")
  (:start (lambda ()
            ;; The folder the operator is pointed at exists from the first start,
            ;; even before the first file.
            (ensure-directories-exist (setting :directory)))))
