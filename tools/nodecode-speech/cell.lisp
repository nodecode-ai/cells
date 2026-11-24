;;;; cell.lisp --- one AllModels key that speaks and listens, and the tools,
;;;; the /speech command and the manual topic that make it reachable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites registered AllModels as Hermes' own TTS and
;;;; transcription provider, so Hermes' voice pipeline spoke its sentences and
;;;; transcribed its voice notes. A cell has no voice pipeline to join; it
;;;; carries what the provider did, as two tools the model calls:
;;;;
;;;;   allmodels_speak     POST {root}/oai/v1/audio/speech  -> an audio file
;;;;   allmodels_transcribe POST {root}/oai/v1/audio/transcriptions -> text
;;;;   allmodels_speech_voices  GET {root}/v1/voices (public) -> matching voices
;;;;
;;;; The first two are the plugin's providers.synthesize and
;;;; providers.transcribe, on the plugin's own OpenAI-compatible base URL
;;;; (client.py's OPENAI_BASE_URL = https://api.allmodels.io/oai); the third is
;;;; the plugin's catalog client (client.py's list_voices). Which model, voice
;;;; and language each call uses is the section's, exactly the values the
;;;; plugin wrote into Hermes' config.yaml under tts/stt.allmodels.
;;;;
;;;; Everything that could waste a billed call -- a missing key, a missing
;;;; voice, a speed outside the API's range, a file that is not there, or one
;;;; over the upload ceiling -- is refused before any request goes out.

(in-package #:nodecode-speech)

;;; --- text, bytes and names ----------------------------------------------------

(defun ascii (text)
  "TEXT's characters as the octets a container header stores them as."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun octets-of (data)
  "DATA as octets: NLK:HTTP answers a string when the peer sent text."
  (etypecase data
    (string (map '(vector (unsigned-byte 8)) #'char-code data))
    ((array (unsigned-byte 8) (*)) data)
    ((array t (*)) data)
    (null (make-array 0 :element-type '(unsigned-byte 8)))))

(defun octets-text (data)
  "DATA, text or octets, as a string."
  (if (stringp data) data (map 'string #'code-char data)))

(defun slug (text)
  "TEXT as a file name's stem: lowercase letters, digits and single dashes, at
most 40 of them, never empty."
  (let ((chars '()) (dash t) (n 0))
    (loop for char across (string-downcase (or text ""))
          while (< n 40)
          do (cond ((alphanumericp char) (push char chars) (setf dash nil) (incf n))
                   ((not dash) (push #\- chars) (setf dash t) (incf n))))
    (let ((out (string-right-trim "-" (coerce (nreverse chars) 'string))))
      (if (zerop (length out)) "speech" out))))

(defun datestamp ()
  "The local time as 20261108_101500, the way a file name carries it."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4,'0d~2,'0d~2,'0d_~2,'0d~2,'0d~2,'0d"
            year month date hour minute second)))

(defun speech-id ()
  "A fresh id for one saved file: 12 lowercase hex digits."
  (format nil "~12,'0x" (random (expt 16 12))))

(defun idempotency-key ()
  "A v4-shaped id for the request header the plugin sent, so a retried call
is not billed twice: 32 hex digits, dashed 8-4-4-4-12."
  (let ((hex (format nil "~32,'0x" (random (expt 16 32)))))
    (format nil "~a-~a-4~a-~a-~a"
            (subseq hex 0 8) (subseq hex 8 12) (subseq hex 13 16)
            (subseq hex 16 20) (subseq hex 20 32))))

(defun url-encode (text)
  "TEXT as a query-string value: everything but the unreserved characters
percent-encoded as UTF-8."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (or text "") :external-format :utf-8)
          do (let ((char (code-char octet)))
               (if (or (alphanumericp char)
                       (find char "-_.~" :test #'char=))
                   (write-char char out)
                   (format out "%~2,'0X" octet))))))

(defun one-line (text &optional (cap 300))
  "TEXT on one line, at most CAP characters."
  (let ((line (substitute #\Space #\Newline (nlk:trimmed (or text "")))))
    (if (<= (length line) cap) line (concatenate 'string (subseq line 0 (- cap 3)) "..."))))

;;; --- which container are these bytes ---------------------------------------------

(defun audio-p (octets format)
  "Whether OCTETS begin like the container FORMAT names. A 200 carrying a
JSON error is what this catches; pcm has no header to read."
  (let ((n (length octets)))
    (cond ((string= format "pcm") (plusp n))
          ((string= format "wav")
           (and (>= n 12) (equalp (subseq octets 0 4) (ascii "RIFF"))
                (equalp (subseq octets 8 12) (ascii "WAVE"))))
          ((string= format "opus")
           (and (>= n 4) (equalp (subseq octets 0 4) (ascii "OggS"))
                (and (search (ascii "OpusHead") octets :end2 (min n 512)) t)))
          ((string= format "flac")
           (and (>= n 4) (equalp (subseq octets 0 4) (ascii "fLaC"))))
          ;; mp3 and aac share a frame sync; an ID3 tag is mp3's own.
          (t (or (and (>= n 3) (equalp (subseq octets 0 3) (ascii "ID3")))
                 (and (>= n 2) (= (aref octets 0) #xFF)
                      (= (logand #xE0 (aref octets 1)) #xE0)))))))

(defun extension-for (format)
  "The file extension FORMAT is saved with, or NIL for a format this cell
does not offer."
  (cdr (assoc format +formats+ :test #'string=)))

;;; --- the key and the endpoint ----------------------------------------------------

(defun api-key ()
  "The AllModels key a call is billed to: the section's, else the variable
api_key_env names. A refusal here costs nothing, so it is raised before any
request."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (when (zerop (length key))
      (setf key (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
    (when (zerop (length key))
      (fail "no AllModels key: paste one into the allmodels-speech section's api_key, ~
             or set the variable ~a (keys are created at https://allmodels.io). Nothing ~
             was sent and nothing was billed." (setting :api-key-env)))
    (unless (every (lambda (char) (<= 33 (char-code char) 126)) key)
      (fail "the AllModels key holds characters an API key cannot have; enter it again. ~
             Nothing was sent and nothing was billed."))
    key))

(defun checked-base-url (text)
  "TEXT as the endpoint root, without its trailing slash, or a CONFIG-REFUSAL.
Plain http is allowed only for a loopback host: anywhere else it would send
the key in clear."
  (let ((url (string-right-trim "/" (nlk:trimmed (or text "")))))
    (unless (plusp (length url))
      (nlk:config-error "allmodels-speech.api_base_url is empty; it is https://api.allmodels.io unless a stand-in is wanted"))
    (let* ((secure (uiop:string-prefix-p "https://" url))
           (plain (uiop:string-prefix-p "http://" url))
           (rest (and (or secure plain) (subseq url (if secure 8 7))))
           (host (and rest (subseq rest 0 (or (position-if (lambda (char) (find char "/:?#")) rest)
                                               (length rest))))))
      (unless (and rest host (plusp (length host)))
        (nlk:config-error "allmodels-speech.api_base_url is ~s; it must be an https URL, such as ~a" url +root-default+))
      (when (and plain (not (member (string-downcase host) '("localhost" "127.0.0.1" "::1" "[::1]")
                                     :test #'string=)))
        (nlk:config-error "allmodels-speech.api_base_url is ~s; plain http would send the key in clear, so it is allowed only for a localhost stand-in" url))
      url)))

(defun parse-speed (text)
  "TEXT as a pace, or NIL when empty; a CONFIG-REFUSAL when it is not a
number. The plugin kept a float in a YAML file; a section field is a string."
  (let ((text (nlk:trimmed (or text ""))))
    (if (zerop (length text))
        nil
        (let ((value (ignore-errors
                       (multiple-value-bind (number end) (read-from-string text nil nil)
                         (and (realp number) (= end (length text))
                              (coerce number 'double-float))))))
          (unless value
            (nlk:config-error "allmodels-speech.tts_speed is ~s; it must be a number from ~a to ~a, or empty"
                              text +min-speed+ +max-speed+))
          value))))

(defun openai-base ()
  "The OpenAI-compatible base the audio endpoints hang off: the root plus
/oai, as the plugin computed it."
  (format nil "~a~a" (setting :api-base-url) +openai-path+))

(defun error-sentence (status body)
  "AllModels' own words for a failed STATUS, or the body's first line."
  (let* ((text (octets-text body))
         (payload (ignore-errors (nlk:decode-json text)))
         (raw (and (hash-table-p payload) (nlk:json-value payload :any "error")))
         (detail (cond ((hash-table-p raw)
                        (or (nlk:json-value raw :string "message")
                            (nlk:json-value raw :string "code")))
                       ((stringp raw) raw)
                       ((hash-table-p payload) (nlk:json-value payload :string "message"))))
         (hint (cdr (assoc status '((401 . " (check the key)")
                                    (403 . " (the key is inactive, or the account lacks credit)")
                                    (429 . " (rate limited, or out of balance; try again shortly)"))
                          :test #'=))))
    (format nil "AllModels returned HTTP ~d~@[~a~]: ~a" status hint
            (or detail (one-line text) "no detail"))))

;;; --- the shape of one call -----------------------------------------------------------

(nlk:define-contract speak-request
  (:doc "Say text out loud with AllModels and save it as an audio file: one call, one file. The answer names the saved path -- give that path to the operator. Billed to the operator's AllModels account. Pick a voice with allmodels_speech_voices first when none is selected in the section.")
  ("text" :text :required t
          :doc "what to say, in the words to be spoken; numbers and names are read as written")
  ("voice" :text
           :doc "the AllModels voice id; omit for the allmodels-speech section's tts_voice")
  ("voice_provider" :text
                    :doc "the provider that serves that voice (fish, elevenlabs, cartesia...); it travels with a voice id the call names, and is otherwise the section's")
  ("model" :text
           :doc "the AllModels TTS model id, such as fish/s2-1-pro; omit for the section's tts_model")
  ("speed" :number :slot pace
           :doc "0.25 to 4.0; omit for the section's tts_speed, or the API's own pace")
  ("format" :text :slot container :options '("mp3" "opus" "aac" "flac" "wav" "pcm")
            :doc "the container to save; mp3 plays in any player, opus suits voice bubbles, wav and flac suit editing. Omit for the section's tts_format"))

(nlk:define-contract transcribe-request
  (:doc "Turn one audio file on this machine into the words it says, through AllModels' speech-to-text models. Billed to the operator's AllModels account.")
  ("path" :text :required t
          :doc "the audio file to transcribe: a path as the operator gave it, absolute or under the home; mp3, wav, ogg, flac, m4a and most other containers")
  ("model" :text
           :doc "the AllModels STT model id, such as soniox/stt-async-v5; omit for the section's stt_model")
  ("language" :text
              :doc "the language spoken, as a code such as en or ja; omit for the section's stt_language, or to let the model detect it")
  ("prompt" :text
            :doc "names and words the recording is likely to contain, to steer spelling; omit for the section's stt_prompt"))

(nlk:define-contract voice-search-request
  (:doc "Search the AllModels voice catalogue: natural language across voice names, descriptions, languages, genders, providers and qualities. Public -- no key, nothing billed. Each result carries the voice id and the provider allmodels_speak takes.")
  ("query" :text :required t
           :doc "what the voice should be, in words: \"British female narrator\", \"warm low male, French\", \"cartesia, whispery\"")
  ("model" :text
           :doc "only voices a given TTS model can speak with, such as fish/s2-1-pro")
  ("provider" :text
              :doc "only one provider's voices: fish, elevenlabs, cartesia, openai...")
  ("language" :text
              :doc "only voices for one language code, such as en-GB or ja")
  ("limit" :integer
           :doc "how many voices to list, 1 to 50; 10 unless asked"))

;;; --- what a call runs on -------------------------------------------------------------

(defun check-speed (speed)
  "Refuse a speed the API would clamp or reject."
  (when (and speed (or (< speed +min-speed+) (> speed +max-speed+)))
    (fail "speed is ~a; AllModels speaks from ~a to ~a. Nothing was sent and nothing was billed."
          speed +min-speed+ +max-speed+)))

(defun checked-format (format)
  "FORMAT's extension, or a refusal naming the choices."
  (or (extension-for format)
      (fail "format is ~s; use one of ~{~a~^, ~}" format (format-names))))

(defun request-speech (key body)
  "One POST to the OpenAI-compatible speech endpoint, under the audio size
cap. A stub in a test stands here."
  (nlk:http :post (format nil "~a/v1/audio/speech" (openai-base))
            :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                           (cons "content-type" "application/json")
                           (cons "accept" "audio/*")
                           (cons "x-idempotency-key" (idempotency-key)))
            :content (nlk:encode-json-object body)
            :binary t
            :max-bytes +max-audio-bytes+
            :timeout 120))

(defun header (headers name)
  "HEADERS' value for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun save-speech (octets text extension format model voice provider speed)
  "Write the audio under the section's folder and answer the model: where the
file is, what it is, and who it is billed to."
  (let* ((id (speech-id))
         (directory (setting :directory))
         (name (format nil "speech_~a_~a_~a.~a" (slug text) (datestamp) id extension))
         (path (merge-pathnames name directory)))
    (ensure-directories-exist path)
    ;; IF-EXISTS :ERROR: a stamp and an id make a collision absurd, and one
    ;; utterance never overwrites the one before it.
    (with-open-file (out path :direction :output :if-exists :error
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    (with-output-to-string (out)
      (format out "saved ~a~%" (namestring path))
      (format out "~a in voice ~a, ~a, ~a~@[, speed ~a~]~%"
              model (voice-pair-text voice provider) format
              (nlk:size-text (length octets)) speed)
      (format out "Give the operator that path: any audio player opens it, and it can be copied into a project.~%")
      (format out "Billed to the AllModels account the key belongs to.~%"))))

(defun speak-now (request)
  "The allmodels_speak call, from a materialized SPEAK-REQUEST."
  (let* ((text (speak-request-text request))
         (model (nlk:trimmed (or (speak-request-model request) (setting :tts-model) "")))
         (voice (nlk:trimmed (or (speak-request-voice request) (setting :tts-voice) "")))
         (provider (nlk:trimmed (or (speak-request-voice-provider request)
                                     (setting :tts-voice-provider) "")))
         (speed (or (speak-request-pace request) (setting :tts-speed)))
         (format (or (speak-request-container request) (setting :tts-format)))
         (extension (checked-format format))
         (key (api-key)))
    (when (zerop (length model))
      (fail "no AllModels TTS model: set allmodels-speech.tts_model, or pass model ~
             (fish/s2-1-pro is the plugin's own starting default). Nothing was sent and nothing was billed."))
    (when (zerop (length voice))
      (fail "no voice is selected: search one with allmodels_speech_voices, then set ~
             allmodels-speech.tts_voice and tts_voice_provider, or pass voice and voice_provider ~
             to this call. ~a Nothing was sent and nothing was billed." +uneven-voice-note+))
    (check-speed speed)
    (progn
      (multiple-value-bind (answer status headers truncated)
          (request-speech key
                          (nlk:json-object "model" model
                                           "voice" voice
                                           "input" text
                                           "response_format" format
                                           :opt "speed" speed
                                           :opt "provider" (and (plusp (length provider)) provider)))
        (when truncated
          (fail "AllModels sent more than ~a of audio, which is no single answer"
                (nlk:size-text +max-audio-bytes+)))
        (let ((octets (octets-of answer)))
          (unless (= status 200)
            (fail "~a" (error-sentence status octets)))
          (when (zerop (length octets))
            (fail "AllModels returned an empty audio response"))
          (unless (audio-p octets extension)
            (fail "AllModels answered HTTP 200 with ~a, but the bytes are not ~a audio"
                  (or (header headers "content-type") "no content type") extension))
          (save-speech octets text extension format model voice provider speed))))))

;;; --- speech to text ---------------------------------------------------------------

(defun audio-file (path)
  "PATH as a file that is there, or a refusal: an absolute path as given, a
relative one under the home."
  (let ((file (or (probe-file path)
                  (and (plusp (length (nlk:trimmed path)))
                       (probe-file (nlk:home (nlk:trimmed path)))))))
    (unless file
      (fail "no file at ~a: give the audio file's path as the operator gave it" path))
    (when (uiop:directory-pathname-p file)
      (fail "~a is a directory; give the audio file's path" path))
    (let ((bytes (ignore-errors (nlk:file-bytes file))))
      (when (and bytes (> bytes +max-upload-bytes+))
        (fail "~a is ~a, over the ~a ceiling one transcription may send"
              path (nlk:size-text bytes) (nlk:size-text +max-upload-bytes+))))
    file))

(defun request-transcription (key content)
  "One multipart POST to the OpenAI-compatible transcription endpoint. A stub
in a test stands here."
  (nlk:http :post (format nil "~a/v1/audio/transcriptions" (openai-base))
            :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                           (cons "accept" "application/json"))
            :content content
            :timeout 180))

(defun transcribe-now (request)
  "The allmodels_transcribe call, from a materialized TRANSCRIBE-REQUEST."
  (let* ((path (transcribe-request-path request))
         (file (audio-file path))
         (model (nlk:trimmed (or (transcribe-request-model request) (setting :stt-model) "")))
         (language (nlk:trimmed (or (transcribe-request-language request)
                                     (setting :stt-language) "")))
         (prompt (nlk:trimmed (or (transcribe-request-prompt request)
                                   (setting :stt-prompt) ""))))
    (when (zerop (length model))
      (fail "no AllModels STT model: set allmodels-speech.stt_model, or pass model ~
             (soniox/stt-async-v5 is the plugin's own starting default). Nothing was sent and nothing was billed."))
    (let ((key (api-key))
          (content (remove nil
                           (list (cons "model" model)
                                 (cons "response_format" "json")
                                 (cons "file" file)
                                 (and (plusp (length language)) (cons "language" language))
                                 (and (plusp (length prompt)) (cons "prompt" prompt))))))
      (multiple-value-bind (answer status headers)
          (request-transcription key content)
        (declare (ignore headers))
        (let* ((body (octets-text answer))
               (payload (ignore-errors (nlk:decode-json body)))
               (transcript (or (and (hash-table-p payload) (nlk:json-value payload :string "text"))
                               (and (not (hash-table-p payload)) (plusp (length (nlk:trimmed body)))
                                    (nlk:trimmed body)))))
          (unless (= status 200)
            (fail "~a" (error-sentence status answer)))
          (unless transcript
            (fail "AllModels answered without a transcript: ~a" (one-line body)))
          (with-output-to-string (out)
            (format out "~a~%" transcript)
            (format out "~%(~d characters, ~a, from ~a)~%"
                    (length transcript) model (file-namestring file))))))))

;;; --- voice search ------------------------------------------------------------------

(defun record-value (row key)
  (getf row key))

(defun first-of (vector key)
  "KEY of the first object in VECTOR, a decoded JSON array, or NIL."
  (let ((first (and vector (plusp (length vector)) (aref vector 0))))
    (and (hash-table-p first) (nlk:json-value first :string key))))

(defun voice-gender (voice)
  "The voice's gender label, as the catalogue carries it."
  (let ((labels (nlk:json-value voice :array "labels")))
    (and labels
         (loop for label across labels
               when (equal "gender"
                           (ignore-errors (nlk:json-value (nlk:json-value label :object "facet")
                                                           :string "id")))
                 return (nlk:json-value (nlk:json-value label :object "value") :string "id")))))

(defun voice-rows (payload)
  "One row per (voice, provider) the catalogue page carries, in its order:
(:id :name :provider :model :language :gender)."
  (let ((rows (nlk:json-value payload :array "voices"))
        (out '()))
    (loop for row across (or rows #()) do
      (let* ((voice (nlk:json-value row :object "voice"))
             (model (nlk:json-value row :object "model"))
             (id (and voice (nlk:json-value voice :string "id")))
             (providers (nlk:json-value row :array "providers")))
        (when (and id (plusp (length id)))
          (let ((name (or (and voice (nlk:json-value voice :string "name")) id))
                (model-id (and model (nlk:json-value model :string "id")))
                (language (and voice (first-of (nlk:json-value voice :array "languages") "id")))
                (gender (and voice (voice-gender voice))))
            (if (and providers (plusp (length providers)))
                (loop for provider across providers
                      for pid = (nlk:json-value provider :string "id")
                      when (and pid (plusp (length pid)))
                        do (push (list :id id :name name :provider pid :model model-id
                                       :language language :gender gender)
                                 out))
                (push (list :id id :name name :provider "" :model model-id
                            :language language :gender gender)
                      out))))))
    (nreverse out)))

(defun row-text (index row)
  "One numbered line of a search result: the id and provider the next call
needs, and what the voice is."
  (format nil "  ~d. ~a -- ~a (voice id ~a)~@[ - ~a~@[ ~a~]~]~%"
          index
          (record-value row :name)
          (let ((provider (record-value row :provider)))
            (if (plusp (length provider)) provider "provider not named"))
          (record-value row :id)
          (record-value row :model)
          (let ((language (record-value row :language))
                (gender (record-value row :gender)))
            (format nil "~@[~a~]~@[ ~a~]" language gender))))

(defun voice-list-text (payload limit)
  "The catalogue page as the tool answers it."
  (let ((rows (voice-rows payload))
        (total (nlk:json-value payload :integer "total_count")))
    (if (null rows)
        (format nil "No voice matches that search: try fewer words, another language, or no model.~%")
        (let ((shown (subseq rows 0 (min (length rows) limit))))
          (with-output-to-string (out)
            (format out "Voices: ~@[~d match~:p; ~]showing ~d~%" total (length shown))
            (loop for row in shown for index from 1 do (format out "~a" (row-text index row)))
            (when (> (length rows) (length shown))
              (format out "  ... and ~d more; raise limit to see them~%" (- (length rows) (length shown))))
            (format out "~a Pass the voice id as voice and the provider as voice_provider to ~
                         allmodels_speak, or keep both in the allmodels-speech section ~
                         (tts_voice and tts_voice_provider).~%"
                    +uneven-voice-note+))))))

(defun voice-search* (query model provider language limit)
  "One voice-catalogue search, by its words: what the tool and /speech both run."
  (let* ((query (nlk:trimmed (or query "")))
         (limit (min +max-voice-rows+ (max 1 (or limit +default-voice-limit+))))
         (model (nlk:trimmed (or model "")))
         (provider (nlk:trimmed (or provider "")))
         (language (nlk:trimmed (or language "")))
         (params (remove nil
                         (list (cons "q" query)
                               (cons "page_size" (princ-to-string limit))
                               (and (plusp (length model)) (cons "model" model))
                               (and (plusp (length provider)) (cons "provider" provider))
                               (and (plusp (length language)) (cons "language" language)))))
         (url (format nil "~a/v1/voices?~{~a~^&~}"
                      (setting :api-base-url)
                      (mapcar (lambda (pair)
                                (format nil "~a=~a" (car pair) (url-encode (cdr pair))))
                              params))))
    (when (zerop (length query))
      (fail "say what kind of voice to look for: \"British female narrator\", \"warm low male, French\""))
    (multiple-value-bind (answer status headers)
        (nlk:http :get url :headers (list (cons "accept" "application/json")) :timeout 30)
      (declare (ignore headers))
      (unless (= status 200)
        (fail "~a" (error-sentence status answer)))
      (let ((payload (ignore-errors (nlk:decode-json (octets-text answer)))))
        (unless (hash-table-p payload)
          (fail "the voice catalogue answered something that is not a JSON object"))
        (voice-list-text payload limit)))))

(defun search-now (request)
  "The allmodels_speech_voices call, from a materialized VOICE-SEARCH-REQUEST."
  (voice-search* (voice-search-request-query request)
                 (voice-search-request-model request)
                 (voice-search-request-provider request)
                 (voice-search-request-language request)
                 (voice-search-request-limit request)))

;;; --- the tools -------------------------------------------------------------------

(defun speak (request)
  "The allmodels_speak tool. Every refusal is answered, never signalled: the
model reads why in the result and repairs the call."
  (handler-case (speak-now request)
    (speech-error (condition)
      (nle:failure (format nil "ERROR: ~a" (speech-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

(defun transcribe (request)
  "The allmodels_transcribe tool, refusing the same way."
  (handler-case (transcribe-now request)
    (speech-error (condition)
      (nle:failure (format nil "ERROR: ~a" (speech-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

(defun search-voices (request)
  "The allmodels_speech_voices tool, refusing the same way."
  (handler-case (search-now request)
    (speech-error (condition)
      (nle:failure (format nil "ERROR: ~a" (speech-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- /speech ---------------------------------------------------------------------

(defun search-voices-text (query model provider language limit)
  "One voice search from the command's own words, through the call the tool
makes."
  (voice-search* query model provider language limit))

(defun key-state ()
  "Whether a key is ready, and where it comes from -- never the key itself."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (cond ((plusp (length key)) "set in allmodels-speech.api_key")
          ((plusp (length (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) ""))))
           (format nil "read from the variable ~a" (setting :api-key-env)))
          (t (format nil "not set: fill api_key in /settings, or export ~a (keys at ~a)"
                     (setting :api-key-env) "https://allmodels.io")))))

(defun status-text ()
  "What /speech says with no argument: the settings every call runs on."
  (let ((voice (nlk:trimmed (or (setting :tts-voice) "")))
        (provider (nlk:trimmed (or (setting :tts-voice-provider) ""))))
    (with-output-to-string (out)
      (format out "AllModels speech~%")
      (format out "  key: ~a~%" (key-state))
      (format out "  TTS: ~a, voice ~a, ~a~@[, speed ~a~]~%"
              (setting :tts-model)
              (if (plusp (length voice)) (voice-pair-text voice provider) "(none selected)")
              (setting :tts-format) (setting :tts-speed))
      (format out "  STT: ~a~@[, language ~a~]~@[, prompt set~]~%"
              (setting :stt-model) (setting :stt-language)
              (and (nlk:trimmed (or (setting :stt-prompt) "")) t))
      (format out "  files: ~a~%" (namestring (setting :directory)))
      (format out "  /speech balance -- the account's credit; /speech voices <words> -- the voice catalogue~%"))))

(defun micro-dollars (value)
  "VALUE, in millionths of a dollar, as dollars."
  (if (realp value) (/ (coerce value 'double-float) 1000000) 0.0d0))

(defun balance-text ()
  "The account's credit, as the plugin's /speech balance answered it."
  (let ((key (api-key)))
    (multiple-value-bind (answer status headers)
        (nlk:http :get (format nil "~a/account/balance" (setting :api-base-url))
                  :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                                 (cons "accept" "application/json"))
                  :timeout 30)
      (declare (ignore headers))
      (unless (= status 200)
        (fail "~a" (error-sentence status answer)))
      (let* ((payload (ignore-errors (nlk:decode-json (octets-text answer))))
             (paid (nlk:json-value payload :number "paid_balance_usd"))
             (promotional (nlk:json-value payload :number "promotional_credits"))
             (state (nlk:json-value payload :string "state")))
        (with-output-to-string (out)
          (format out "AllModels balance~%")
          (format out "  status: ~a~%" (or state "unknown"))
          (format out "  spendable paid balance: $~,2f~%" (coerce (or paid 0) 'double-float))
          (format out "  promotional balance: $~,2f~%" (micro-dollars promotional)))))))

(defun command-text (argument)
  "ARGUMENT, the words after /speech, as the text the shell shows."
  (let* ((argument (nlk:trimmed (or argument "")))
         (space (position #\Space argument))
         (word (string-downcase (if space (subseq argument 0 space) argument)))
         (rest (if space (nlk:trimmed (subseq argument (1+ space))) "")))
    (cond ((zerop (length argument)) (status-text))
          ((string= word "balance") (balance-text))
          ((string= word "voices")
           (if (zerop (length rest))
               "Say what to look for: /speech voices British female narrator"
               (search-voices-text rest nil nil nil +default-voice-limit+)))
          ((string= word "status") (status-text))
          (t (format nil "Unknown: /speech ~a. Try /speech, /speech balance, or /speech voices <words>.~%"
                     argument)))))

(defun run-command (args session-id)
  "The /speech command: the settings, the balance, or the voice catalogue."
  (declare (ignore session-id))
  (handler-case (command-text args)
    (speech-error (condition) (speech-error-detail condition))
    (error (condition) (format nil "speech: ~a" condition))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "AllModels speech is available through the nodecode-speech cell: three tools and the
/speech command, on one AllModels key billed to the operator's account.
  allmodels_speak(text, voice?, voice_provider?, model?, speed?, format?)
      one utterance, saved as an audio file in this profile's speech folder; the answer
      names the path -- give it to the operator. A voice is an id plus the provider that
      serves it; both come from allmodels_speech_voices, and both travel together.
      At most a paragraph or two per call: the file is played, not read.
  allmodels_transcribe(path, model?, language?, prompt?)
      one audio file on this machine as the words it says. Pass the path the operator
      gave; a relative one is read under the home. Name names and jargon in prompt to
      steer spelling.
  allmodels_speech_voices(query, model?, provider?, language?, limit?)
      the public AllModels catalogue, searched in words: \"British female narrator\".
      Nothing is billed and no key is needed. Pick a voice here, then pass its id as
      voice and its provider as voice_provider to allmodels_speak, or keep them in the
      allmodels-speech section (tts_voice, tts_voice_provider) for every call.
Every call that reaches AllModels is billed to that account; /speech balance says what
is left, and /speech voices <words> is the same search from the operator's side.
The section holds the defaults: tts_model, tts_voice, tts_voice_provider, tts_speed,
tts_format, stt_model, stt_language, stt_prompt, api_base_url, directory, and the key
(api_key, or the variable api_key_env names)."
  "What (help :speech) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell speech
  (:section ("allmodels-speech")
    (:guide "create a key at https://allmodels.io and paste it in api_key, or name the variable that holds it in api_key_env (ALLMODELS_API_KEY by default); pick a voice with allmodels_speech_voices or /speech voices <words> and keep its id in tts_voice with the provider that serves it in tts_voice_provider; tts_model and stt_model say which speech models every call uses; directory is where the audio files are kept; every call is billed to that AllModels account")
    ("api_key" :secret
               :doc "the AllModels API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "ALLMODELS_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("tts_model" :string :default "fish/s2-1-pro"
                 :doc "the AllModels text-to-speech model every call uses unless it names one; the plugin's own starting default")
    ("tts_voice" :string
                 :doc "the voice id every call speaks in unless it names one; find one with allmodels_speech_voices. A voice id and the provider that serves it are chosen together")
    ("tts_voice_provider" :string
                          :doc "the provider that serves tts_voice: fish, elevenlabs, cartesia, openai...; it travels with the voice id")
    ("tts_speed" :string
                 :doc "the pace every call speaks at, 0.25 to 4.0, written as a number; empty, the provider's own")
    ("tts_format" :choice :options '("mp3" "opus" "aac" "flac" "wav" "pcm")
                  :default "mp3"
                  :doc "the container a synthesized file is saved in; mp3 plays in any player, opus suits voice bubbles, wav and flac suit editing")
    ("stt_model" :string :default "soniox/stt-async-v5"
                 :doc "the AllModels speech-to-text model every transcription uses unless it names one; the plugin's own starting default")
    ("stt_language" :string
                    :doc "the language code recordings are mostly in, such as en or ja; absent, the model detects it")
    ("stt_prompt" :string
                  :doc "names, products and jargon the recordings are likely to contain, to steer spelling")
    ("api_base_url" :string :default "https://api.allmodels.io"
                    :doc "AllModels' API root; the audio endpoints are its /oai path. Change it only for a stand-in")
    ("directory" :directory :default "speech/"
                 :doc "where the synthesized audio files are kept, under the home"))
  ;; The one thing the declaration cannot say: a base URL is a web address,
  ;; and plain http is a cleartext key.
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :api-base-url) (checked-base-url (getf values :api-base-url)))
               (setf (getf values :tts-speed) (parse-speed (getf values :tts-speed)))
               values))
  (:tool "allmodels_speak" (nlk:contract 'speak-request) 'speak)
  (:tool "allmodels_transcribe" (nlk:contract 'transcribe-request) 'transcribe)
  (:tool "allmodels_speech_voices" (nlk:contract 'voice-search-request) 'search-voices)
  (:help :speech "speech: AllModels text-to-speech, speech-to-text and voice search on one key" +primer+)
  (:command "speech" 'run-command
            :description "AllModels speech: the settings, the balance, the voice catalogue"
            :argument-hint "[status|balance|voices <words>]"
            :session nil)
  (:start (lambda ()
            ;; The folder the operator is pointed at exists from the first
            ;; start, even before the first utterance.
            (ensure-directories-exist (merge-pathnames "speech.keep" (setting :directory))))))
