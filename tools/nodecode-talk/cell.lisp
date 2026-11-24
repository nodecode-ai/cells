;;;; cell.lisp --- the three voice verbs, the /talk command, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :talk) answers the primer below, and every request's help section
;;;; carries one line naming it. Nothing hooks the provider path: the audio
;;;; API speaks HTTP, and NLK:HTTP is the one seam.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "talk": {"api_key_env": "OPENAI_API_KEY", "voice": "cedar",
;;;;            "player": "", "record_command": ""}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from TheSmokeDev's hermes-talk (see NOTICE): its OpenAI TTS and
;;;; transcription providers -- /v1/audio/speech and /v1/audio/transcriptions,
;;;; their default models, its voice list and default voice, and the envelope
;;;; each one answers.

(in-package #:nodecode-talk)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Voice is available through the nodecode-talk cell: three tools, which a person can also call
from eval in the talk package.
  talk_speak      {text, voice?}
      Say TEXT aloud: it is synthesized and played on this machine. Use it when the operator
      asked to hear something, or when a spoken answer is easier than reading one. Keep TEXT to
      a sentence or two; a long answer is read aloud badly. voice picks one the section allows,
      otherwise the configured one answers.
  talk_transcribe {path, language?, model?}
      Turn an audio file already on this machine into text, in the language you name or the
      model detects.
  talk_listen     {seconds?, language?, model?}
      Record the microphone for a few seconds and turn what was said into text: the operator
      speaks their question instead of typing it. The recording then transcribes exactly as
      talk_transcribe does.
  talk_status     {part?}
      This cell's own state, read-only and without a request to the API: the speech and
      transcription models, the voice, the variable the key comes from, and which player
      and recorder were found. Use it when the operator asks what you are running on, or
      when a verb fails and you need to say why.
A voice outside the list the section carries is refused, with the list, before any request
is made; speaking goes faster or slower with speed (0.25 to 4.0).
Every answer is a JSON object: {\"spoken\": true, \"path\": ..., \"bytes\": N, \"voice\": ...,
\"player\": ...} for talk_speak, and {\"text\": ..., \"model\": ..., \"source\": ...} for the
two transcribing verbs. A call that cannot go on answers ERROR: with the reason -- a missing
key, a missing file, no recorder, no player, or the API's own refusal. The key is read from the
environment variable the section's api_key_env names; audio and text go to OpenAI and nowhere
else."
  "What (help :talk) answers while the cell runs.")

;;; --- small things the bodies share -------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun octets-of (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun as-text (body)
  "BODY as text: the wire answers text, and with :binary it answers octets."
  (etypecase body
    (string body)
    ((vector (unsigned-byte 8))
     (let ((end (or (position 0 body) (length body))))
       (sb-ext:octets-to-string (subseq body 0 end) :external-format :utf-8)))))

(defun answer-detail (body)
  "The first words of a refused answer, for the sentence the model reads."
  (let ((text (ignore-errors (as-text body))))
    (if (and text (plusp (length text)))
        (subseq text 0 (min 300 (length text)))
        "no body")))

(defun fail (format-control &rest args)
  "A verb that cannot go on: one ERROR sentence as the tool's answer."
  (error 'talk-error :detail (apply #'format nil format-control args)))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (talk-error (condition)
       (nle:failure (format nil "ERROR: ~a" (talk-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun find-program (name)
  "NAME on PATH, or NIL. No shell, no glob: one directory at a time."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
        for candidate = (and (plusp (length directory))
                             (merge-pathnames name (uiop:ensure-directory-pathname directory)))
        when (and candidate (probe-file candidate))
          return (namestring candidate)))

;;; --- the wire -----------------------------------------------------------------

(defun endpoint (path)
  (concatenate 'string (string-right-trim "/" (or (setting :base-url) +default-base-url+)) path))

(defun auth-headers (content-type)
  (list (cons "authorization" (format nil "Bearer ~a" (setting :api-key)))
        (cons "content-type" content-type)))

(defun exchange (path &key content content-type binary)
  "One POST => (values BODY STATUS). A dial that fails is a refusal."
  (let ((url (endpoint path)))
    (handler-case
        (multiple-value-bind (body status)
            (funcall *http* :post url
                     :headers (auth-headers content-type)
                     :content content
                     :binary binary
                     :timeout (setting :timeout)
                     :connect-timeout (setting :timeout))
          (values body status))
      (error (condition)
        (fail "~a did not answer: ~a" url (nlk:one-line (princ-to-string condition)))))))

;;; --- saying it out loud ---------------------------------------------------------

(defun fresh-path (extension)
  "A new file under the home's talk/ folder, its directories made."
  (let ((path (nlk:home (format nil "talk/voice-~36r-~36r.~a"
                                 (get-universal-time) (random 1000000) extension))))
    (ensure-directories-exist path)
    path))

(defun write-octets (path octets)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (write-sequence octets stream))
  path)

(defun play (path)
  "Play PATH with the configured player, else the first of +PLAYERS+ found.
=> the player's name, or NIL when there is none (the file stays written)."
  (let* ((configured (nonblank (setting :player)))
         (argv (if configured
                   (append (uiop:split-string configured :separator " ") (list (namestring path)))
                   (loop for (name . prefix) in +players+
                         for found = (find-program name)
                         when found
                           return (append (list found) (cdr prefix) (list (namestring path)))))))
    (when argv
      (multiple-value-bind (out ok) (funcall *run* argv)
        (unless ok
          (fail "~a could not play the file: ~a" (file-namestring (first argv)) (nlk:one-line out))))
      (file-namestring (first argv)))))

(defun wanted-voice (voice)
  "The voice this call speaks in: the one asked for, or the section's. Fail
closed on anything the API does not speak, as the original plugin did."
  (let ((wanted (or (nonblank voice) (setting :voice))))
    (unless (member wanted +voices+ :test #'string=)
      (fail "voice ~s is not one the audio API speaks: ~{~a~^, ~}" wanted +voices+))
    wanted))

(defun speak-text (text &key voice speed)
  "Synthesize TEXT, write it under the home, play it. => the answer object."
  (let* ((voice (wanted-voice voice))
         (path (fresh-path (setting :format)))
         (payload (if (and (realp speed) (plusp speed))
                      (nlk:json-object "model" (setting :tts-model)
                                        "input" text
                                        "voice" voice
                                        "speed" speed
                                        "response_format" (setting :format))
                      (nlk:json-object "model" (setting :tts-model)
                                        "input" text
                                        "voice" voice
                                        "response_format" (setting :format)))))
    (multiple-value-bind (body status)
        (exchange "/audio/speech" :content (nlk:encode-json-object payload)
                                    :content-type "application/json" :binary t)
      (unless (and (integerp status) (= status 200))
        (fail "speech answered HTTP ~a~@[: ~a~]" status (answer-detail body)))
      (let ((octets (if (stringp body) (octets-of body) body)))
        (when (zerop (length octets))
          (fail "speech answered no audio"))
        (write-octets path octets)
        (let ((player (handler-case (play path)
                        (talk-error (condition)
                          (nle:notice (format nil "talk: ~a" (talk-error-detail condition))
                                       :level :warning :key "nodecode-talk-player")
                          nil))))
          (nlk:json-object "spoken" t
                           "path" (namestring path)
                           "bytes" (length octets)
                           "voice" voice
                           "player" player))))))

;;; --- hearing it ------------------------------------------------------------------

(defun text-part (boundary name value)
  (octets-of (format nil "--~a~cContent-Disposition: form-data; name=\"~a\"~c~c~a~c"
                      boundary #\Return name #\Return #\Return value #\Return)))

(defun multipart-body (boundary filename content-type octets &optional fields)
  "One multipart/form-data body: FIELDS as text parts, then the file itself."
  (let ((head (format nil "--~a~cContent-Disposition: form-data; name=\"file\"; filename=\"~a\"~cContent-Type: ~a~c~c"
                       boundary #\Return filename #\Return content-type #\Return #\Return))
        (tail (format nil "~c--~a--~c~c" #\Return boundary #\Return #\Return)))
    (concatenate '(vector (unsigned-byte 8))
                 (apply #'concatenate '(vector (unsigned-byte 8))
                        (mapcar (lambda (field)
                                  (text-part boundary (car field) (cdr field)))
                                fields))
                 (octets-of head) octets (octets-of tail))))

(defun audio-content-type (path)
  (let ((type (string-downcase (or (pathname-type path) ""))))
    (cond ((string= type "wav") "audio/wav")
          ((string= type "m4a") "audio/mp4")
          ((string= type "ogg") "audio/ogg")
          ((string= type "webm") "audio/webm")
          ((string= type "flac") "audio/flac")
          ((string= type "mp3") "audio/mpeg")
          (t "application/octet-stream"))))

(defun read-audio (path)
  "Every octet of PATH, as the body part."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun transcribe-file (path &key language model source)
  "Send PATH to /v1/audio/transcriptions => the answer object."
  (unless (probe-file path)
    (fail "no audio file at ~a" path))
  (let* ((boundary (format nil "----nodecode-talk-~36r" (random (expt 16 10))))
         (wanted (or (nonblank model) (setting :stt-model)))
         (spoken (nonblank language))
         (fields (append (list (cons "model" wanted)
                               (cons "response_format" "json"))
                         (and spoken (list (cons "language" spoken)))))
         (body (multipart-body boundary (file-namestring path)
                               (audio-content-type path) (read-audio path) fields)))
    (multiple-value-bind (answer status)
        (exchange "/audio/transcriptions" :content body
                                           :content-type (format nil "multipart/form-data; boundary=~a" boundary))
      (unless (and (integerp status) (= status 200))
        (fail "transcription answered HTTP ~a~@[: ~a~]" status (answer-detail answer)))
      (let* ((text (as-text answer))
             (decoded (ignore-errors (nlk:decode-json text)))
             (transcript (and (hash-table-p decoded)
                              (nlk:json-value decoded :string "text"))))
        (unless transcript
          (fail "transcription answered no text: ~a" (subseq text 0 (min 200 (length text)))))
        (nlk:json-object "text" transcript
                         "model" wanted
                         "source" (or source (namestring path)))))))

(defun record-argv (path seconds)
  "The argv that records SECONDS of the microphone into PATH, or NIL.
record_command is a command line whose %f is the file and %s the seconds;
left empty, the first of arecord, sox and ffmpeg found answers."
  (let ((configured (nonblank (setting :record-command))))
    (flet ((fill-in (word)
             (cond ((string= word "%f") (namestring path))
                   ((string= word "%s") (princ-to-string seconds))
                   (t word))))
      (cond (configured
             (mapcar #'fill-in (uiop:split-string configured :separator " ")))
            (t
             (let ((program (or (find-program "arecord") (find-program "sox") (find-program "ffmpeg"))))
               (cond ((null program) nil)
                     ((search "arecord" program)
                      (list program "-q" "-f" "S16_LE" "-r" "16000" "-c" "1"
                              "-d" (princ-to-string seconds) (namestring path)))
                     ((search "sox" program)
                      (list program "-q" "-d" (namestring path) "trim" "0" (princ-to-string seconds)))
                     (t (list program "-y" "-loglevel" "quiet" "-f" "pulse" "-i" "default"
                              "-ar" "16000" "-ac" "1" "-t" (princ-to-string seconds)
                              (namestring path))))))))))

(defun listen-answer (seconds language model)
  (let* ((path (fresh-path "wav"))
         (argv (record-argv path seconds)))
    (unless argv
      (fail "no recorder found: install arecord, sox or ffmpeg, or set record_command (with %f for the file and %s for the seconds)"))
    (multiple-value-bind (out ok) (funcall *run* argv)
      (unless ok
        (fail "the recorder ~a failed: ~a" (file-namestring (first argv)) (nlk:one-line out))))
    (unless (probe-file path)
      (fail "the recorder wrote no file at ~a" path))
    (transcribe-file path :language language :model model :source "microphone")))

;;; --- the tools --------------------------------------------------------------------

;;; --- reporting the lane, as the original plugin's talk_status did --------------------

(defun player-name ()
  "The player that would be asked to play a file: the configured one, else the
first of +PLAYERS+ found. Never runs it."
  (or (nonblank (setting :player))
      (loop for (name . prefix) in +players+
            when (find-program name)
              return name)))

(defun recorder-name ()
  "The recorder that would be asked to record: the configured command line, else
the first of arecord, sox and ffmpeg found. Never runs it."
  (or (nonblank (setting :record-command))
      (find-program "arecord")
      (find-program "sox")
      (find-program "ffmpeg")))

(defun status-object ()
  "This cell's own state, read-only and without a dial: what speaks, what
transcribes, where the key comes from, and which CLIs answer."
  (nlk:json-object "version" +version+
                    "speech_model" (setting :tts-model)
                    "voice" (setting :voice)
                    "format" (setting :format)
                    "transcription_model" (setting :stt-model)
                    "key_env" (setting :api-key-env)
                    "base_url" (setting :base-url)
                    "player" (or (player-name) "")
                    "recorder" (or (recorder-name) "")
                    "audio" (and (player-name) t)))

(defun status-text ()
  "The same state as one line, for a person typing /talk status."
  (format nil "talk ~a: ~a speaking as ~a in ~a, transcribing with ~a; key from ~a; player ~a; recorder ~a"
          +version+ (setting :tts-model) (setting :voice) (setting :format)
          (setting :stt-model) (setting :api-key-env)
          (or (player-name) "none found") (or (recorder-name) "none found")))

(nlk:define-contract status-request
  (:doc "Report this cell's own state: the models, the voice and the two audio CLIs.")
  ("part" :string :default "" :doc "which part to report; empty reports everything"))

(defun status-tool (call)
  "The talk_status tool: what speaks, what transcribes, and which CLIs answer."
  (declare (ignore call))
  (answering (nlk:encode-json-object (status-object))))

(nlk:define-contract speak-request
  (:doc "Say a text aloud on this machine.")
  ("text" :text :required t :doc "what to say, one or two spoken sentences")
  ("voice" :string :default "" :doc "a voice the section allows, e.g. cedar; empty, the configured one")
  ("speed" :number :default 0 :slot speak-speed
   :doc "how fast to speak, 0.25 to 4.0; 0 leaves the model's own pace"))

(nlk:define-contract transcribe-request
  (:doc "Turn an audio file on this machine into text.")
  ("path" :text :required t :doc "the audio file to transcribe, wav or mp3")
  ("language" :string :default "" :doc "the spoken language as a two-letter code, e.g. en; empty, detected")
  ("model" :string :default "" :doc "the transcription model; empty, the configured one"))

(nlk:define-contract listen-request
  (:doc "Record the microphone and turn what was said into text.")
  ("seconds" :integer :default 0 :doc "how long to record; 0, the configured record_seconds")
  ("language" :string :default "" :doc "the spoken language as a two-letter code; empty, detected")
  ("model" :string :default "" :doc "the transcription model; empty, the configured one"))

(defun speak-tool (call)
  "The talk_speak tool: say it, and play it."
  (answering
    (nlk:encode-json-object (speak-text (speak-request-text call)
                                         :voice (speak-request-voice call)
                                         :speed (speak-request-speak-speed call)))))

(defun transcribe-tool (call)
  "The talk_transcribe tool: an audio file in, text out."
  (answering
    (nlk:encode-json-object
     (transcribe-file (nlk:expand-home (transcribe-request-path call))
                      :language (nonblank (transcribe-request-language call))
                      :model (transcribe-request-model call)))))

(defun listen-tool (call)
  "The talk_listen tool: the microphone for a few seconds, then the text."
  (answering
    (let ((seconds (listen-request-seconds call)))
      (nlk:encode-json-object
       (listen-answer (if (and (integerp seconds) (plusp seconds)) seconds (setting :record-seconds))
                      (nonblank (listen-request-language call))
                      (listen-request-model call))))))

;;; --- /talk ------------------------------------------------------------------------

(defun run-talk (args session-id)
  "One /talk invocation: the words given are said aloud; with none, the
microphone is recorded and the transcription is the answer, so the operator
can speak their question instead of typing it."
  (declare (ignore session-id))
  (let ((words (nlk:trimmed (or args ""))))
    (values (cond ((string-equal words "status") (status-text))
                  ((and words (plusp (length words)))
                   (answering (nlk:encode-json-object (speak-text words))))
                  (t (answering (nlk:encode-json-object
                                 (listen-answer (setting :record-seconds) nil nil)))))
            nil)))

;;; --- the cell ---------------------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the key the section names: read once, here."
  (declare (ignore table))
  (let ((env (nonblank (getf values :api-key-env)))
        (plist (copy-list values)))
    (unless env
      (nlk:config-error "no key: set api_key_env to the environment variable holding an OpenAI API key, e.g. OPENAI_API_KEY"))
    (setf (getf plist :api-key)
          (or (nonblank (nle::credential-env env))
              (nlk:config-error "api_key_env names ~a, which is unset or empty" env)))
    plist))

(nle:define-cell talk
  (:section ("talk")
    (:guide "an OpenAI API key in the environment variable api_key_env names (platform.openai.com/api-keys); voice is what speaks, tts_model and stt_model what synthesize and transcribe; player and record_command name the CLIs, left empty the cell finds one; next step: type /talk hello")
    ("api_key_env" :env :default "OPENAI_API_KEY"
     :doc "the environment variable holding the OpenAI API key both the speaking and the transcribing verbs use")
    ("base_url" :string :default "https://api.openai.com/v1"
     :doc "the API root; OpenAI's unless a compatible one is named here")
    ("voice" :choice :options '("alloy" "ash" "ballad" "coral" "echo" "sage" "shimmer" "verse" "marin" "cedar")
     :default "cedar"
     :doc "the voice that speaks: the original plugin's list, cedar its default")
    ("tts_model" :string :default "gpt-4o-mini-tts"
     :doc "the speech model; tts-1 and tts-1-hd also answer")
    ("stt_model" :string :default "gpt-4o-mini-transcribe"
     :doc "the transcription model; whisper-1 also answers")
    ("format" :choice :options '("mp3" "wav" "opus" "flac" "aac")
     :default "mp3"
     :doc "the audio container speech is written in")
    ("player" :string :default ""
     :doc "the CLI that plays a file, given its path; empty, the first of afplay, paplay, aplay, ffplay and mpv found")
    ("record_command" :string :default ""
     :doc "the CLI that records the microphone, a command line with %f for the file and %s for the seconds; empty, the first of arecord, sox and ffmpeg found")
    ("record_seconds" :integer :default 6 :min 1
     :doc "how long talk_listen and a bare /talk record for")
    ("timeout" :integer :default 120 :min 1
     :doc "how long one audio request may take, in seconds"))
  (:settings #'read-settings)
  (:help :talk
         "talk: talk_speak says a text aloud, talk_transcribe and talk_listen turn speech into text, talk_status reports the lane"
         *manual*)
  (:tool "talk_speak" (nlk:contract 'speak-request) #'speak-tool)
  (:tool "talk_status" (nlk:contract 'status-request) #'status-tool)
  (:tool "talk_transcribe" (nlk:contract 'transcribe-request) #'transcribe-tool)
  (:tool "talk_listen" (nlk:contract 'listen-request) #'listen-tool)
  (:command "talk" 'run-talk
            :description "Say the words given aloud, record and transcribe a question, or report status"
            :argument-hint "[WORDS|status]"))
