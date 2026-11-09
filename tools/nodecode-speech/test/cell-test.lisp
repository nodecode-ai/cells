;;;; cell-test.lisp --- one utterance saved, one file read back, the voice
;;;; catalogue asked, the account's balance shown, and every refusal that
;;;; costs nothing.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials: NLK:HTTP is a stub that records the call and answers
;;;; handcrafted bytes, the environment is a stub, and every file is written
;;;; into a folder of this test's own under the scratch home. What is pinned is
;;;; what a user gets -- words become a real file, an audio file becomes its
;;;; words, a search lists the id and provider the next call needs -- and what a
;;;; refused call never costs: a missing key, a missing voice, a missing file
;;;; and a 401 all answer before, or instead of, a second request.

(in-package #:nodecode.test)

(define-test-slice "speech" "SPEECH-")

;;; The section is ALLMODELS-SPEECH, not the folder's short name, so the
;;; config and the start are written here rather than derived from the slice.
(defun speech-config (&rest pairs)
  "A whole shared config with one allmodels-speech section built from PAIRS."
  (nlk:json-object "allmodels-speech" (apply #'nlk:make-json-object pairs)))

(defun speech-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-speech:start-cell (apply #'speech-config pairs)))

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "speech"
  (:config "api_key" "test-key"
           "tts_voice" "voice-1"
           "tts_voice_provider" "fish"
           "directory" (namestring (merge-pathnames "speech-cell-test/" (nlk:home))))
  (:help :speech)
  (:command "speech")
  (:running (is (equal "test-key" (nodecode-speech::setting :api-key)) "the section's own key")
            (is (equal "fish/s2-1-pro" (nodecode-speech::setting :tts-model)) "the plugin's starting TTS model")
            (is (equal "https://api.allmodels.io" (nodecode-speech::setting :api-base-url)) "the base URL")
            (is-present (entry (nle::find-tool "allmodels_speak")) "the speaking tool"
              (is (equal "allmodels_speak" (first entry))))
            (is-present (entry (nle::find-tool "allmodels_transcribe")) "the transcription tool"
              (is (equal "allmodels_transcribe" (first entry))))
            (is-present (entry (nle::find-tool "allmodels_speech_voices")) "the voice search tool"
              (is (equal "allmodels_speech_voices" (first entry)))))
  (:stopped (is (null (nle::find-tool "allmodels_speak")) "stop takes the speaking tool back out")
            (is (null (nle::find-tool "allmodels_transcribe")) "and the transcription tool")
            (is (null (nle::find-tool "allmodels_speech_voices")) "and the voice search"))
  (:refused ("api_base_url" "ftp://api.allmodels.io")
            ("api_base_url" "http://api.allmodels.io"))
  (:idle nodecode-speech:speech-error (nodecode-speech::status-text)))

;;; --- fixtures --------------------------------------------------------------------

(defun speech-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "speech-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defun speech-start* (&rest pairs)
  "The cell started on PAIRS, in a fresh folder unless PAIRS names one."
  (apply #'speech-start
         (append (unless (member "directory" pairs :test #'string=)
                   (list "directory" (namestring (speech-directory))))
                 (copy-list pairs))))

(defmacro with-speech ((&body pairs) &body body)
  "BODY with the cell started on PAIRS, the section's members as a flat
key/value list; its folder is deleted afterwards."
  (let ((flat (if (every #'consp pairs)
                  (loop for pair in pairs append pair)
                  pairs)))
    `(let* ((started (speech-start* ,@flat))
            (directory (nodecode-speech::setting :directory)))
     (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t
                                                            :if-does-not-exist :ignore)))
       ,@body))))

(defun speech-tool (name &rest pairs)
  "The tool NAME as the engine calls it: the declared contract decoded, then
the body."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-endpoint ((asked &key (answer '(lambda (url headers content)
                                                (declare (ignore url headers content))
                                                (values (speech-mp3) 200 nil))))
                         &body body)
  "BODY with the endpoint stubbed: ASKED collects (METHOD URL HEADERS CONTENT)
oldest first, and ANSWER answers (values BODY STATUS HEADERS)."
  `(let ((,asked '()))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content &allow-other-keys)
                                (push (list method url headers content) ,asked)
                                (funcall ,answer url headers content))
       ,@body)))

(defun speech-call (asked &optional (index 0))
  "The INDEXth call ASKED recorded, as (values METHOD URL HEADERS CONTENT)."
  (let ((call (nth index (reverse asked))))
    (values (first call) (second call) (third call) (fourth call))))

(defun speech-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

;;; --- handcrafted audio -------------------------------------------------------------

(defun speech-ascii (text)
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun speech-mp3 (&optional (frames 10))
  "FRAMES MPEG-1 Layer III frames at 128 kbps and 44.1 kHz, 417 bytes each."
  (let ((frame (make-array 417 :element-type '(unsigned-byte 8) :initial-element 0))
        (out (make-array (* frames 417) :element-type '(unsigned-byte 8))))
    (setf (aref frame 0) #xFF (aref frame 1) #xFB (aref frame 2) #x90 (aref frame 3) #x00)
    (dotimes (index frames) (replace out frame :start1 (* index 417)))
    out))

(defun speech-ogg ()
  "Two Ogg Opus pages: an OpusHead and a granule."
  (let ((out (make-array 60 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace out (speech-ascii "OggS") :start1 0)
    (setf (aref out 26) 1 (aref out 27) 19)
    (replace out (speech-ascii "OpusHead") :start1 28)
    out))

(defun speech-json (object)
  "OBJECT as the text body a stub answers."
  (nlk:encode-json-object object))

(defun speech-voice-page ()
  "One page of the voice catalogue, shaped as its own client reads it."
  (nlk:json-object
   "voices" (vector
             (nlk:json-object
              "model" (nlk:json-object "id" "fish/s2-1-pro")
              "voice" (nlk:json-object
                       "id" "voice-1" "name" "Narrator One"
                       "languages" (vector (nlk:json-object "id" "en-GB"))
                       "labels" (vector (nlk:json-object
                                         "facet" (nlk:json-object "id" "gender")
                                         "value" (nlk:json-object "id" "female"))))
              "providers" (vector (nlk:json-object "id" "fish")))
             (nlk:json-object
              "model" (nlk:json-object "id" "elevenlabs/tts-x")
              "voice" (nlk:json-object
                       "id" "voice-2" "name" "Narrator Two"
                       "languages" (vector (nlk:json-object "id" "ja")))
              "providers" (vector (nlk:json-object "id" "elevenlabs"))))
   "total_count" 2))

(defun speech-first-line (answer)
  (subseq answer 0 (or (position #\Newline answer) (length answer))))

(defun speech-file-length (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

;;; --- speaking ---------------------------------------------------------------------

(deftest speech-words-become-an-audio-file ()
  (with-speech (("api_key" "test-key") ("tts_voice" "voice-1") ("tts_voice_provider" "fish"))
    (let ((asked '()) (text "Hello from the cell, this is the test speaking."))
      (with-endpoint (asked
                      :answer (lambda (url headers content)
                                (declare (ignore url headers content))
                                (values (speech-mp3 10) 200
                                        (list (cons "content-type" "audio/mpeg")))))
        (let ((answer (speech-tool "allmodels_speak" "text" text)))
          (multiple-value-bind (method url headers content) (speech-call asked)
            (is (eq :post method) "one POST")
            (is (search "/oai/v1/audio/speech" url) "to the OpenAI-compatible speech endpoint")
            (is (equal "Bearer test-key" (speech-header headers "authorization")) "with the section's key")
            (is-present (id (speech-header headers "x-idempotency-key")) "and the idempotency header the plugin sent"
              (is (>= (length id) 32)))
            (let ((sent (nlk:decode-json content)))
              (is (equal text (nlk:json-value sent :text "input")) "the words ride as input")
              (is (equal "fish/s2-1-pro" (nlk:json-value sent :text "model")) "the section's model")
              (is (equal "voice-1" (nlk:json-value sent :text "voice")) "the section's voice")
              (is (equal "fish" (nlk:json-value sent :text "provider")) "with the provider that serves it")
              (is (equal "mp3" (nlk:json-value sent :text "response_format")) "and the section's format")))
          (let ((path (subseq (speech-first-line answer) 6)))
            (is (search "saved " answer))
            (is (probe-file path) "the answer names a file that is there")
            (is (= (* 10 417) (speech-file-length path)) "holding exactly the bytes that came back")
            (is (search "voice-1 (fish)" answer) "and says which voice said it")
            (is (search "Billed to the AllModels account" answer) "and who it is billed to")))))))

(deftest speech-a-call-overrides-the-section-and-saves-what-it-asked-for ()
  (with-speech (("api_key" "test-key"))
    (let ((asked '()))
      (with-endpoint (asked
                      :answer (lambda (url headers content)
                                (declare (ignore url headers content))
                                (values (speech-ogg) 200 (list (cons "content-type" "audio/ogg")))))
        (let ((answer (speech-tool "allmodels_speak"
                                    "text" "a voice bubble"
                                    "voice" "voice-2"
                                    "voice_provider" "elevenlabs"
                                    "model" "elevenlabs/tts-x"
                                    "speed" 1.25
                                    "format" "opus")))
          (multiple-value-bind (method url headers content) (speech-call asked)
            (declare (ignore url headers))
            (is (eq :post method))
            (let ((sent (nlk:decode-json content)))
              (is (equal "voice-2" (nlk:json-value sent :text "voice")) "the named voice")
              (is (equal "elevenlabs" (nlk:json-value sent :text "provider")) "and its provider")
              (is (equal "elevenlabs/tts-x" (nlk:json-value sent :text "model")) "the named model")
              (is (= 1.25 (nlk:json-value sent :number "speed")) "the named pace")
              (is (equal "opus" (nlk:json-value sent :text "response_format")) "the named container")))
          (is (search ".opus" (speech-first-line answer)) "the file carries that container's extension"))))))

(deftest speech-without-a-key-refuses-and-sends-nothing ()
  (with-speech (("api_key" "") ("api_key_env" "SPEECH_TEST_KEY_UNSET"))
    (let ((asked '()))
      (with-endpoint (asked :answer (lambda (&rest call) (declare (ignore call))
                                         (error "no request should have gone out")))
        (let ((nodecode-speech::*getenv* (lambda (name) (declare (ignore name)) nil)))
          (let ((answer (speech-tool "allmodels_speak" "text" "anything at all")))
            (is (search "ERROR:" answer) "the call answers a failure")
            (is (search "SPEECH_TEST_KEY_UNSET" answer) "naming the variable to set")
            (is (search "Nothing was sent" answer)))
          (let ((answer (speech-tool "allmodels_transcribe" "path" "nothing-here.wav")))
            (is (search "no file at" answer) "a transcription needs a file that is there"))
          (is (null asked) "and nothing was dialled"))))))

(deftest speech-without-a-voice-refuses-before-sending ()
  (with-speech (("api_key" "test-key") ("tts_voice" "") ("tts_voice_provider" ""))
    (let ((asked '()))
      (with-endpoint (asked :answer (lambda (&rest call) (declare (ignore call))
                                         (error "no request should have gone out")))
        (let ((answer (speech-tool "allmodels_speak" "text" "hello")))
          (is (search "ERROR:" answer))
          (is (search "allmodels_speech_voices" answer) "it says where a voice comes from")
          (is (search "tts_voice" answer) "and which setting keeps it")
          (is (null asked) "and nothing was dialled"))))))

(deftest speech-reports-what-allmodels-refused-and-keeps-nothing ()
  (with-speech (("api_key" "test-key") ("tts_voice" "voice-1"))
    (let ((asked '()))
      (with-endpoint (asked
                      :answer (lambda (url headers content)
                                (declare (ignore url headers content))
                                (values (speech-json (nlk:json-object
                                                       "error" (nlk:json-object
                                                                "message" "Invalid API key")))
                                        401 nil)))
        (let ((answer (speech-tool "allmodels_speak" "text" "a sentence")))
          (is (= 1 (length asked)) "one call was made")
          (is (search "HTTP 401" answer))
          (is (search "Invalid API key" answer) "AllModels' own sentence")
          (is (search "check the key" answer) "and the fix that status names")))
      (with-endpoint (asked
                      :answer (lambda (url headers content)
                                (declare (ignore url headers content))
                                (values (speech-ascii "{\"detail\":\"no\"}") 200 nil)))
        (let ((answer (speech-tool "allmodels_speak" "text" "a sentence")))
          (is (= 1 (length asked)) "one call was made")
          (is (search "not mp3 audio" answer) "a 200 that is not audio is refused"))))))

;;; --- transcribing -------------------------------------------------------------------

(defun speech-wav-file (&optional (data 1600))
  "A small RIFF/WAVE file in this test's own folder."
  (let ((path (merge-pathnames (format nil "note-~6,'0x.wav" (random (expt 16 6)))
                                (speech-directory))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence (concatenate '(vector (unsigned-byte 8))
                                   (speech-ascii "RIFF")
                                   (make-array (+ 4 data) :element-type '(unsigned-byte 8)
                                                          :initial-element 0))
                      out))
    path))

(deftest speech-an-audio-file-becomes-its-words ()
  (with-speech (("api_key" "test-key"))
    (let ((asked '())
          (file (speech-wav-file)))
      (nlk:with-cleanup ((uiop:delete-file-if-exists file))
        (with-endpoint (asked
                        :answer (lambda (url headers content)
                                  (declare (ignore url headers content))
                                  (values (speech-json (nlk:json-object "text" "hello from the recording"))
                                          200 nil)))
          (let ((answer (speech-tool "allmodels_transcribe" "path" (namestring file))))
            (multiple-value-bind (method url headers content) (speech-call asked)
              (is (eq :post method) "one POST")
              (is (search "/oai/v1/audio/transcriptions" url) "to the transcription endpoint")
              (is (equal "Bearer test-key" (speech-header headers "authorization")) "with the section's key")
              (is (equal "soniox/stt-async-v5" (cdr (assoc "model" content :test #'string=))) "the STT model")
              (is (equal "json" (cdr (assoc "response_format" content :test #'string=))))
              (is (equal (namestring file) (namestring (cdr (assoc "file" content :test #'string=)))) "the file")
              (is (null (assoc "language" content :test #'string=))) "no language when none is set")
            (is (search "hello from the recording" answer) "the transcript is the answer")))
        (with-endpoint (asked
                        :answer (lambda (url headers content)
                                  (declare (ignore url headers content))
                                  (values (speech-json (nlk:json-object "text" "bonjour"))
                                          200 nil)))
          (let ((answer (speech-tool "allmodels_transcribe" "path" (namestring file)
                                      "model" "openai/whisper-1"
                                      "language" "fr"
                                      "prompt" "AllModels, Nodecode")))
            (multiple-value-bind (method url headers content) (speech-call asked)
              (declare (ignore method url headers))
              (is (equal "openai/whisper-1" (cdr (assoc "model" content :test #'string=))) "the named model")
              (is (equal "fr" (cdr (assoc "language" content :test #'string=))) "the language rides")
              (is (equal "AllModels, Nodecode" (cdr (assoc "prompt" content :test #'string=))) "the prompt"))
            (is (search "bonjour" answer))))))))

;;; --- the voice catalogue ------------------------------------------------------------

(deftest speech-voice-search-lists-the-id-and-provider-to-use ()
  (with-speech ()
    (let ((asked '()))
      (with-endpoint (asked
                      :answer (lambda (url headers content)
                                (declare (ignore url headers content))
                                (values (speech-json (speech-voice-page)) 200 nil)))
        (let ((answer (speech-tool "allmodels_speech_voices" "query" "British female narrator")))
          (multiple-value-bind (method url headers content) (speech-call asked)
            (declare (ignore content))
            (is (eq :get method) "one GET")
            (is (search "/v1/voices?q=British%20female%20narrator&page_size=10" url) "the words search it")
            (is (null (speech-header headers "authorization")) "with no key at all"))
          (is (search "Narrator One" answer))
          (is (search "voice id voice-1" answer) "the id the next call takes")
          (is (search "fish" answer) "and the provider that serves it")
          (is (search "en-GB female" answer) "with what the voice is")
          (is (search "Narrator Two" answer) "and the second row")
          (is (search "voice_provider" answer) "the answer says how to pass both"))))))

;;; --- /speech ----------------------------------------------------------------

(deftest speech-command-answers-the-status-the-balance-and-the-catalogue ()
  (with-speech (("api_key" "test-key") ("tts_voice" "voice-1") ("tts_voice_provider" "fish"))
    (let ((status (nodecode-speech::run-command "" "s1")))
      (is (search "AllModels speech" status))
      (is (search "fish/s2-1-pro" status) "the model in use")
      (is (search "voice-1 (fish)" status) "the voice in use")
      (is (search "set in allmodels-speech.api_key" status) "and where the key comes from")
      (is (not (search "test-key" status)) "but never the key itself"))
    (let ((asked '()))
      (with-endpoint (asked
                      :answer (lambda (url headers content)
                                (declare (ignore url headers content))
                                (values (speech-json
                                         (nlk:json-object "state" "active"
                                                          "paid_balance_usd" 12.5
                                                          "promotional_credits" 1000000))
                                        200 nil)))
        (let ((balance (nodecode-speech::run-command "balance" "s1")))
          (multiple-value-bind (method url headers content) (speech-call asked)
            (declare (ignore content))
            (is (eq :get method))
            (is (search "/account/balance" url))
            (is (equal "Bearer test-key" (speech-header headers "authorization"))))
          (is (search "$12.50" balance) "the spendable balance reads as dollars")
          (is (search "$1.00" balance) "and the promotional credit as its own dollars")))
      (with-endpoint (asked
                      :answer (lambda (url headers content)
                                (declare (ignore url headers content))
                                (values (speech-json (speech-voice-page)) 200 nil)))
        (let ((voices (nodecode-speech::run-command "voices British female" "s1")))
          (is (search "Narrator One" voices) "/speech voices searches the same catalogue")))
      (is (search "Unknown" (nodecode-speech::run-command "nonsense" "s1")) "and the rest says what it takes"))))
