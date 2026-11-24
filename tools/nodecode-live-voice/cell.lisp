;;;; cell.lisp --- two tools and a command: say this, hear that.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this rewrites kept a full-duplex WebRTC call open between a
;;;; desktop app and OpenAI's live model, with a composer button, an
;;;; iMessage-style transcript and a delegation policy in the middle. An image
;;;; has no WebRTC and no composer, so the one conversation becomes two turns
;;;; the model drives: VOICE_LISTEN records from the microphone and answers
;;;; the words, VOICE_SPEAK writes an answer and plays it, and /voice shows
;;;; the transcript of both. Both POST to the same OpenAI audio API the
;;;; plugin's optional gpt-realtime lane spoke to, with the operator's key.
;;;;
;;;; The delegation policy the plugin kept -- filler stays conversational,
;;;; a real task becomes a tool call -- is the tool description's job here:
;;;; the model decides when to listen and what to say.

(in-package #:nodecode-live-voice)

;;; --- the key and the endpoint -----------------------------------------------

(defun key-text ()
  "The key a call is billed to: the section's, else the variable api_key_env
names; \"\" when neither holds one."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (when (zerop (length key))
      (setf key (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
    key))

(defun api-key ()
  "KEY-TEXT, or a refusal naming both places to put one. A refusal here costs
nothing, so it is raised before any request."
  (let ((key (key-text)))
    (when (zerop (length key))
      (fail "no OpenAI key: paste one into the live-voice section's api_key, or set the variable ~a. ~
             Nothing was sent."
            (setting :api-key-env)))
    (unless (every (lambda (char) (<= 33 (char-code char) 126)) key)
      (fail "the OpenAI key holds characters a key cannot have; enter it again. Nothing was sent."))
    key))

(defun checked-base-url (text)
  "TEXT as the endpoint root, without its trailing slash, or a
CONFIG-REFUSAL. Plain http is allowed only for a loopback host: anywhere
else it would send the bearer key in clear."
  (let ((url (string-right-trim "/" (nlk:trimmed (or text "")))))
    (unless (plusp (length url))
      (nlk:config-error "live-voice.base_url is empty; it is https://api.openai.com/v1 unless a compatible stand-in is wanted"))
    (let* ((secure (uiop:string-prefix-p "https://" url))
           (plain (uiop:string-prefix-p "http://" url))
           (rest (and (or secure plain) (subseq url (if secure 8 7))))
           (host (and rest (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                               (length rest))))))
      (unless (and rest (plusp (length host)))
        (nlk:config-error "live-voice.base_url is ~s; it must be an https URL, such as https://api.openai.com/v1" url))
      (when (and plain (not (member (string-downcase host)
                                     '("localhost" "127.0.0.1" "::1" "[::1]") :test #'string=)))
        (nlk:config-error "live-voice.base_url is ~s; plain http would send the key in clear, so it is allowed only for a localhost stand-in" url))
      url)))

(defun api-error-text (body)
  "OpenAI's own sentence for a failed call, out of the JSON error object, or
the first characters of whatever came back."
  (let* ((text (nlk:trimmed (octets-text (octets-of body))))
         (payload (and (plusp (length text)) (ignore-errors (nlk:decode-json text))))
         (error (and (hash-table-p payload) (nlk:json-value payload :any "error"))))
    (cond ((hash-table-p error) (or (nlk:json-value error :string "message") "no detail"))
          ((and (stringp error) (plusp (length error))) error)
          ((plusp (length text)) (subseq text 0 (min 300 (length text))))
          (t "no body"))))

;;; --- what a call is made of -------------------------------------------------

(nlk:define-contract voice-listen-request
  (:doc "Listen to this machine's microphone for a few seconds and answer what was said, as text. Use it when the operator speaks instead of typing, or says 'listen', 'record this', 'dictate'. Nothing else is captured: the recording is one utterance, saved beside the transcript, and the audio is sent to OpenAI to be transcribed. One call per utterance; a long answer is several calls.")
  ("seconds" :integer :default 8
             :doc "how long to listen, in seconds (1 to the section's max_seconds, 8 unless set)")
  ("language" :text
              :doc "the language spoken, as an ISO-639-1 code (en, fr, de), when it is known: it improves the transcript"))

(nlk:define-contract voice-say-request
  (:doc "Speak a text out loud on this machine: OpenAI writes it as audio, it is saved in the voice folder, and it is played here at once. Use it for an answer the operator asked to hear, or a hands-free reply; the text is sent to OpenAI and nothing else. The call waits while the answer plays, so keep the spoken form short -- a sentence or two -- and leave detail in the page.")
  ("text" :text :required t
          :doc "what to say, in the words to be spoken: no markup, no file paths, no code")
  ("voice" :text :doc "the voice to say it in (alloy, ash, coral, echo, fable, nova, onyx, sage, shimmer, verse); the section's voice unless set")
  ("instructions" :text :doc "how to say it, for gpt-4o-mini-tts: tone, pace, accent, emotion")
  ("play" :boolean :default t :doc "play it here as soon as it is written; false saves the file only"))

;;; --- hearing ------------------------------------------------------------------

(defun transcribe (path &key language)
  "PATH, a recording, as the words it says: one multipart POST to the
transcription endpoint with the section's model and the key as a bearer
token."
  (let ((key (api-key)))
    (multiple-value-bind (body status)
        (nlk:http :post (format nil "~a/audio/transcriptions" (setting :base-url))
                  :headers (list (cons "Authorization" (format nil "Bearer ~a" key)))
                  :content (append (list (cons "model" (setting :transcribe-model))
                                         (cons "response_format" "json")
                                         (cons "file" path))
                                   (and language (list (cons "language" language))))
                  :timeout 180)
      (when (>= status 400)
        (fail "OpenAI returned HTTP ~d from the transcription: ~a" status (api-error-text body)))
      (let* ((text (octets-text (octets-of body)))
             (payload (ignore-errors (nlk:decode-json text)))
             (heard (nlk:json-value payload :string "text")))
        (unless (stringp heard)
          (fail "OpenAI answered without a transcript: ~a" (nlk:clip (nlk:one-line text) 200)))
        (nlk:trimmed heard)))))

(defun listen-audio (&key seconds language)
  "Record and transcribe one utterance => the words, \"\" when the recording
held none. Every recording leaves a line of the transcript."
  (let* ((seconds (or seconds +default-listen-seconds+))
         (ceiling (min +max-listen-seconds+ (or (setting :max-seconds) +max-listen-seconds+))))
    (unless (and (integerp seconds) (<= 1 seconds ceiling))
      (fail "seconds is ~a: this machine listens for 1 to ~a seconds (live-voice.max_seconds)"
            seconds ceiling))
    ;; The key is asked for before the microphone opens: a missing key costs no
    ;; recording.
    (api-key)
    (let* ((path (record seconds))
           (text (transcribe path :language language)))
      (write-transcript (nlk:json-object "at" (nlk:iso-time (nlk:unix-now))
                                          "kind" "heard"
                                          "seconds" seconds
                                          "text" text
                                          "file" (uiop:native-namestring path)))
      text)))

;;; --- speaking ------------------------------------------------------------------

(defun synthesize (text &key voice instructions)
  "TEXT as audio by the section's speech model => (values OCTETS EXTENSION)."
  (let* ((key (api-key))
         (format (setting :audio-format))
         (extension (or (extension-for format)
                        (fail "the live-voice section's audio_format is ~s; use one of ~{~a~^, ~}"
                              format (format-names)))))
    (multiple-value-bind (answer status headers truncated)
        (nlk:http :post (format nil "~a/audio/speech" (setting :base-url))
                  :headers (list (cons "Authorization" (format nil "Bearer ~a" key))
                                 (cons "accept" "audio/*"))
                  :content (nlk:encode-json-object
                            (nlk:json-object "model" (setting :speech-model)
                                             "voice" (or voice (setting :voice))
                                             "input" text
                                             "response_format" format
                                             :opt "instructions" instructions))
                  :binary t
                  :max-bytes +max-audio-bytes+
                  :timeout 180)
      (declare (ignore headers))
      (when truncated
        (fail "OpenAI sent more than ~a, which is no single spoken answer"
              (nlk:size-text +max-audio-bytes+)))
      (let ((octets (octets-of answer)))
        (unless (= status 200)
          (fail "OpenAI returned HTTP ~d from the speech endpoint: ~a" status (api-error-text octets)))
        (when (zerop (length octets))
          (fail "OpenAI answered the speech request with no audio"))
        (unless (audio-p octets extension)
          (fail "OpenAI answered HTTP 200, but the bytes are not ~a audio" extension))
        (values octets extension)))))

(defun say (text &key voice instructions (play-p t))
  "Write TEXT as speech, keep it in the folder, and play it here unless
PLAY-P is false => the report the model reads."
  (multiple-value-bind (octets extension) (synthesize text :voice voice :instructions instructions)
    (let ((path (merge-pathnames (format nil "said_~a_~a.~a" (stamp) (fresh-id) extension)
                                  (setting :directory))))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :error
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
        (write-sequence octets out))
      (write-transcript (nlk:json-object "at" (nlk:iso-time (nlk:unix-now))
                                          "kind" "said"
                                          "chars" (length text)
                                          "text" text
                                          "file" (uiop:native-namestring path)))
      (multiple-value-bind (played note)
          (if play-p (play path) (values nil "play was false: the file is there to play"))
        (with-output-to-string (out)
          (format out "saved ~a~%" (uiop:native-namestring path))
          (format out "~a, ~a, ~a (~a), ~:d characters~%"
                  (nlk:size-text (length octets)) extension (setting :speech-model)
                  (or voice (setting :voice)) (length text))
          (if played
              (format out "played here: the operator hears it now~%")
              (format out "not played: ~a~%" note))
          (format out "Give the operator that path: the file plays in any audio player.~%"))))))

;;; --- the two tools ----------------------------------------------------------------

(defun listen-tool (request)
  "voice_listen: a refusal is answered, never signalled, so the model reads
why and repairs the call."
  (handler-case
      (let ((text (listen-audio :seconds (voice-listen-request-seconds request)
                                 :language (voice-listen-request-language request))))
        (if (zerop (length text))
            "the recording held no words: ask the operator to say it again, or listen longer"
            text))
    (live-voice-error (condition)
      (nle:failure (format nil "ERROR: ~a" (live-voice-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

(defun speak-tool (request)
  "voice_speak: the report, or the reason it could not be spoken."
  (handler-case
      (say (voice-say-request-text request)
           :voice (voice-say-request-voice request)
           :instructions (voice-say-request-instructions request)
           :play-p (voice-say-request-play request))
    (live-voice-error (condition)
      (nle:failure (format nil "ERROR: ~a" (live-voice-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- /voice -------------------------------------------------------------------

(defun recorder-name ()
  "The recorder a recording runs, or the reason there is none."
  (handler-case (format nil "~a" (first (recorder-template)))
    (live-voice-error (condition) (live-voice-error-detail condition))))

(defun player-name ()
  "The player a spoken answer runs, or the words for a machine without one."
  (let ((template (player-template)))
    (if template
        (format nil "~a" (first template))
        "none found: a spoken file is saved but not played (set live-voice.player)")))

(define-verb status-text ()
  "What /voice answers with no argument: the key, the microphone, the speaker,
the folder, and the last lines of the transcript."
  (with-output-to-string (out)
    (format out "voice: OpenAI audio -- ~a, then ~a (~a)~%"
            (setting :transcribe-model) (setting :speech-model) (setting :voice))
    (format out "key: ~a~%"
            (if (plusp (length (key-text)))
                "set"
                (format nil "missing -- paste one into the live-voice section's api_key, or export ~a"
                        (setting :api-key-env))))
    (format out "microphone: ~a~%" (recorder-name))
    (format out "player: ~a~%" (player-name))
    (format out "folder: ~a~%" (uiop:native-namestring (setting :directory)))
    (let ((entries (transcript-entries 3)))
      (if entries
          (format out "transcript (newest last):~%~{  ~a~%~}" (mapcar #'entry-line entries))
          (format out "nothing heard or said yet: voice_listen records, voice_speak speaks~%")))))

(define-verb transcript-text ()
  "The last lines of the transcript, oldest first, for /voice transcript."
  (let ((entries (transcript-entries 20)))
    (if entries
        (format nil "~{~a~%~}" (mapcar #'entry-line entries))
        "the transcript is empty: nothing has been heard or said here yet")))

(defun run-slash (args session-id)
  "The /voice command: no argument reports; 'say TEXT' speaks; 'hear
[SECONDS]' records and transcribes; 'transcript' lists what was heard and
said."
  (declare (ignore session-id))
  (let* ((argument (nlk:trimmed (or args "")))
         (lower (string-downcase argument)))
    (handler-case
        (cond ((zerop (length argument)) (status-text))
              ((uiop:string-prefix-p "say " lower)
               (say (nlk:trimmed (subseq argument 4))))
              ((uiop:string-prefix-p "hear" lower)
               (let* ((rest (nlk:trimmed (subseq argument (min 4 (length argument)))))
                      (seconds (and (plusp (length rest))
                                    (parse-integer rest :junk-allowed t)))
                      (text (listen-audio :seconds seconds)))
                 (if (zerop (length text))
                     "the recording held no words: say it again, or /voice hear 15"
                     text)))
              ((string= lower "transcript") (transcript-text))
              (t (status-text)))
      (live-voice-error (condition)
        (format nil "ERROR: ~a" (live-voice-error-detail condition))))))

;;; --- what the model reads once the cell runs -------------------------------------

(defparameter +primer+
  "Live voice is on: the nodecode-live-voice cell gives the conversation a microphone and a
voice. Two tools and one command.
  voice_listen(seconds?, language?) -- record this machine's microphone for a few seconds and
      answer the words. Use it when the operator says they will speak, says 'listen' or
      'dictate', or a spoken answer is easier than typing. One call per utterance, 8 seconds
      unless set; a long answer is several calls. The recording is sent to OpenAI to be
      transcribed and kept beside the transcript.
  voice_speak(text, voice?, instructions?, play?) -- have OpenAI say TEXT and play it on this
      machine. Use it for a hands-free answer, or when the operator asks to hear it. The call
      waits while it plays: keep the spoken form to a sentence or two, leave detail in the
      page, and never read out code, paths or tables.
  /voice reports the key, the microphone, the speaker and the folder, with the last lines of
      the transcript; /voice say TEXT speaks; /voice hear [SECONDS] records; /voice transcript
      lists what was heard and said.
Both calls are billed to the operator's OpenAI account: speak when asked, or when the operator
is clearly hands-free, not after every turn. Audio and the spoken text go to the API base URL;
nothing else of the conversation does."
  "What (help :voice) answers while the cell runs.")

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell live-voice
  (:section ("live-voice")
    (:guide "export OPENAI_API_KEY or paste a key in api_key; a recorder (ffmpeg, sox's rec, arecord) makes voice_listen work and a player (afplay, mpv, ffplay, sox's play, paplay, aplay) speaks voice_speak; base_url, the models, voice and audio_format are what every call uses; directory is where the recordings, the spoken files and transcript.jsonl are kept")
    ("api_key" :secret
               :doc "the OpenAI API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "OPENAI_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("base_url" :string :default "https://api.openai.com/v1"
                :doc "the OpenAI-compatible API root; https unless a loopback stand-in")
    ("transcribe_model" :choice :options '("gpt-4o-mini-transcribe" "gpt-4o-transcribe" "whisper-1")
                        :default "gpt-4o-mini-transcribe"
                        :doc "what voice_listen transcribes with; the mini model is the cheapest")
    ("speech_model" :choice :options '("gpt-4o-mini-tts" "tts-1" "tts-1-hd")
                     :default "gpt-4o-mini-tts"
                     :doc "what voice_speak speaks with; only gpt-4o-mini-tts follows instructions")
    ("voice" :string :default "alloy"
             :doc "the OpenAI voice a call uses when it names none: alloy, ash, coral, echo, fable, nova, onyx, sage, shimmer, verse")
    ("audio_format" :choice :options '("mp3" "wav" "opus" "flac" "aac")
                     :default "mp3"
                     :doc "the file a spoken answer is saved as; mp3 plays everywhere, wav is what aplay and paplay read")
    ("max_seconds" :integer :default 60 :min 1
                   :doc "the longest one recording may be, in seconds")
    ("recorder" :string
                :doc "a recorder command of your own, with %f the file and %s the seconds; empty, the first of ffmpeg, rec and arecord on PATH")
    ("player" :string
              :doc "a player command of your own, with %f the file; empty, the first of afplay, mpv, ffplay, play, paplay and aplay")
    ("directory" :directory :default "voice/"
                 :doc "where recordings, spoken files and the transcript are kept, under the home"))
  ;; The one thing the declaration cannot say: a base URL is a web address,
  ;; and plain http is a cleartext key.
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :base-url) (checked-base-url (getf values :base-url)))
               (let ((voice (nlk:trimmed (or (getf values :voice) ""))))
                 (setf (getf values :voice) (if (plusp (length voice)) voice "alloy")))
               values))
  (:tool "voice_listen" (nlk:contract 'voice-listen-request) 'listen-tool)
  (:tool "voice_speak" (nlk:contract 'voice-say-request) 'speak-tool)
  (:help :voice "voice: voice_listen takes dictation from the microphone and voice_speak speaks an answer out loud, through OpenAI's audio API" +primer+)
  (:command "voice" 'run-slash
            :description "Live voice: the microphone, the spoken answers, and the transcript"
            :argument-hint "[say TEXT | hear [SECONDS] | transcript]")
  (:start (lambda ()
            ;; The folder the operator is pointed at exists from the first
            ;; start, before the first recording.
            (ensure-directories-exist (transcript-path)))))
