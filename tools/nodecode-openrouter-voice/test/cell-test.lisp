;;;; cell-test.lisp --- lifecycle, both tools, /voice, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an integer under its floor or an unreadable speed refused, a start
;;;; with no key refused), the two tools through the seam the model calls them
;;;; by -- their URL, their headers, their body and the answer they build --
;;;; the WAV normaliser the transcription endpoint needs, the rate split
;;;; between the API and ffmpeg, and /voice with and without an argument.
;;;; Every request is a stubbed NLK:HTTP and ffmpeg is stubbed absent, so
;;;; nothing reaches OpenRouter, the network or a child process.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values BODY STATUS).")

(defmacro with-voice-wire (&body body)
  "Run BODY with NLK:HTTP stubbed -- a request is logged in *REQUESTS* and
answered by *ANSWER-FN* -- and ffmpeg stubbed absent, so no test dials
OpenRouter and none spawns a child."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected OpenRouter request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       (with-stubbed-fdefinition (nodecode-openrouter-voice::ffmpeg-path () nil)
         ,@body))))

(defun voice-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun voice-request (index)
  "The INDEXth stubbed request, oldest first."
  (nth index (voice-asked)))

(defun voice-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun voice-content (request)
  "REQUEST's body as it was sent: a string, or the octets of a multipart one."
  (getf (cddr request) :content))

(defun voice-json (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (voice-content request)))

(defun voice-head (request &optional (limit 600))
  "The head of REQUEST's body as characters: enough to read the multipart part
headers of an audio upload without decoding its audio."
  (let ((body (voice-content request)))
    (if (stringp body)
        (subseq body 0 (min limit (length body)))
        (map 'string #'code-char (subseq body 0 (min limit (length body)))))))

(defun voice-body-wav (request)
  "REQUEST's body from its first RIFF: the uploaded WAV itself."
  (let* ((body (voice-content request))
         (at (and (vectorp body) (search (map 'vector #'char-code "RIFF") body :test #'=))))
    (and at (subseq body at))))

(defun voice-answer (text &optional (status 200))
  "What the stubbed NLK:HTTP answers: TEXT as the body, STATUS as the code."
  (setf *answer-fn* (lambda (method url args)
                       (declare (ignore method url args))
                       (values text status))))

(defun voice-answer-audio (octets &optional (status 200))
  "What the stubbed NLK:HTTP answers for a speech call: OCTETS, a status."
  (setf *answer-fn* (lambda (method url args)
                       (declare (ignore method url args))
                       (values octets status))))

(defun voice-setting (key)
  "The running cell's setting KEY, read the way the cell reads it."
  (funcall (intern "SETTING" :nodecode-openrouter-voice) key))

(defun voice-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defun voice-wav (rate channels frames)
  "A 16-bit PCM WAVE of FRAMES frames, CHANNELS channels at RATE, built here
so the normaliser has something real to rewrite."
  (let* ((data (* 2 channels frames))
         (out (make-array (+ 44 data) :element-type '(unsigned-byte 8))))
    (flet ((text (at string)
             (loop for character across string for index from at
                   do (setf (aref out index) (char-code character))))
           (u16 (at value)
             (setf (aref out at) (logand value #xff)
                   (aref out (1+ at)) (logand (ash value -8) #xff)))
           (u32 (at value)
             (loop for index from 0 below 4
                   do (setf (aref out (+ at index)) (logand (ash value (* -8 index)) #xff)))))
      (text 0 "RIFF") (u32 4 (+ 36 data)) (text 8 "WAVE") (text 12 "fmt ")
      (u32 16 16) (u16 20 1) (u16 22 channels) (u32 24 rate) (u32 28 (* rate 2 channels))
      (u16 32 (* 2 channels)) (u16 34 16) (text 36 "data") (u32 40 data)
      (loop for index from 0 below (* frames channels)
            do (u16 (+ 44 (* 2 index)) (logand (+ 400 index) #xffff))))
    out))

(defun voice-file (octets &key (type "wav"))
  "OCTETS written to a scratch file of TYPE => its namestring."
  (let ((path (merge-pathnames (format nil "voice-fixture-~36r.~a" (random (expt 36 8)) type)
                                (uiop:temporary-directory))))
    (with-open-file (out path :element-type '(unsigned-byte 8) :direction :output
                              :if-exists :supersede :if-does-not-exist :create)
      (write-sequence octets out))
    path))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "openrouter-voice" "OPENROUTER-VOICE-CELL-"
  :start nodecode-openrouter-voice:start-cell)

(define-cell-lifecycle-tests "openrouter-voice"
  (:fixture with-voice-wire)
  (:config "api_key" "k-test")
  (:help :openrouter-voice)
  (:command "voice")
  (:running (is (nle::find-tool "transcribe_audio") "transcribe_audio is registered")
            (is (nle::find-tool "speak_text") "speak_text is registered")
            (is (equal "k-test" (voice-setting :key)) "the key the section wrote")
            (is (equal "https://openrouter.ai/api/v1" (voice-setting :base-url)) "the endpoint root")
            (is (equal "openai/whisper-large-v3" (voice-setting :stt-model)) "the transcription default")
            (is (equal "deepgram/aura-2" (voice-setting :tts-model)) "the speech default")
            (is (= 120 (voice-setting :timeout-seconds)) "one request's ceiling")
            (is (< (abs (- 1.0 (voice-setting :speed))) 0.001) "speed reads as the number it was written as"))
  (:stopped (is (null (nle::find-tool "transcribe_audio")) "stop unregisters the tools")
            (is (null (nle::find-tool "speak_text")) "both of them"))
  (:refused ("timeout_seconds" 0) ("speed" "fast") ("base_url" "openrouter.ai"))
  (:idle nodecode-openrouter-voice:openrouter-voice-error
         (nodecode-openrouter-voice::run-command "" nil)))

(deftest openrouter-voice-cell-a-missing-key-refuses-the-start
  (with-voice-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (and (string= name "OPENROUTER_UNSET") "no"))
      (is-carrying (text (refusal-text nlk:config-refusal (openrouter-voice-start)))
                   ("no key" "the refusal says what to set")
                   ("OPENROUTER_API_KEY" "and names the variable"))
      (is (null (nle::find-tool "transcribe_audio")) "a refused start installed nothing"))))

(deftest openrouter-voice-cell-the-key-may-come-from-the-environment
  (with-voice-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                               (cond ((string= name "OPENROUTER_HOME_KEY") "k-env")
                                     ((string= name "OPENROUTER_API_KEY") "k-fallback")
                                     (t nil)))
      (with-cell-stop ((openrouter-voice-start "api_key_env" "OPENROUTER_HOME_KEY"))
        (is (equal "k-env" (voice-setting :key)) "api_key_env names the variable"))
      (with-cell-stop ((openrouter-voice-start))
        (is (equal "k-fallback" (voice-setting :key)) "OPENROUTER_API_KEY is read either way")))))

;;; --- transcribe_audio ------------------------------------------------------------

(deftest openrouter-voice-cell-transcribe-uploads-the-file ()
  (with-voice-wire
    (with-cell-stop ((openrouter-voice-start "api_key" "k-test"))
      (voice-answer "{\"text\":\"hello from the recording\"}")
      (let* ((path (voice-file (nlk:utf-8-octets "not really a wav") :type "mp3"))
             (answer nil))
        (unwind-protect
             (progn
               (setf answer (voice-tool "transcribe_audio"
                                         (nlk:json-object "path" path "language" "no")))
               (let ((request (voice-request 0)))
                 (is (= 1 (length (voice-asked))) "one request")
                 (is (eq :post (first request)))
                 (is (equal "https://openrouter.ai/api/v1/audio/transcriptions" (second request)))
                 (is (equal "Bearer k-test" (voice-header request "authorization")))
                 (is (search "multipart/form-data; boundary=" (voice-header request "content-type")))
                 (let ((head (voice-head request)))
                   (is (search "name=\"model\"" head) "the model goes as a field")
                   (is (search "openai/whisper-large-v3" head) "the section's slug")
                   (is (search "name=\"response_format\"" head))
                   (is (search "name=\"language\"" head) "the language the call named")))
               (is (equal "hello from the recording" answer) "the transcript is the answer"))
          (ignore-errors (delete-file path)))))))

(deftest openrouter-voice-cell-a-bare-model-name-falls-back-to-the-slug ()
  (with-voice-wire
    (with-cell-stop ((openrouter-voice-start "api_key" "k-test"))
      (voice-answer "{\"text\":\"ok\"}")
      (let ((path (voice-file (nlk:utf-8-octets "bytes") :type "mp3")))
        (unwind-protect
             (progn
               (voice-tool "transcribe_audio" (nlk:json-object "path" path "model" "whisper-1"))
               (is (search "openai/whisper-large-v3" (voice-head (voice-request 0))) "a bare model name becomes the shipped slug"))
          (ignore-errors (delete-file path)))))))

(deftest openrouter-voice-cell-a-wav-is-brought-to-16k-mono ()
  (with-voice-wire
    (with-cell-stop ((openrouter-voice-start "api_key" "k-test"))
      (voice-answer "{\"text\":\"ok\"}")
      (dolist (shape '((8000 1 800 1 16000 1600) (16000 2 400 1 16000 400)))
        (destructuring-bind (rate channels frames want-channels want-rate want-frames) shape
          (let ((path (voice-file (voice-wav rate channels frames))))
            (unwind-protect
                 (progn
                   (setf *requests* '())
                   (voice-tool "transcribe_audio" (nlk:json-object "path" path))
                   (multiple-value-bind (got-channels got-rate bits start got-frames)
                       (nodecode-openrouter-voice::wav-info (voice-body-wav (voice-request 0)))
                     (declare (ignore start))
                     (is (= want-channels got-channels)
                         (format nil "~d Hz x~d arrives mono" rate channels))
                     (is (= want-rate got-rate) "at 16 kHz")
                     (is (= 16 bits) "16-bit")
                     (is (= want-frames got-frames) "with the frames the resample gives")))
              (ignore-errors (delete-file path)))))))))

;;; --- speak_text -------------------------------------------------------------------

(deftest openrouter-voice-cell-speak-writes-the-mp3 ()
  (with-voice-wire
    (with-cell-stop ((openrouter-voice-start "api_key" "k-test"))
      (voice-answer-audio (make-array 5 :element-type '(unsigned-byte 8)
                                         :initial-contents '(73 68 51 4 0)))
      (with-temp-file (target :type "mp3")
        (let ((answer (voice-tool "speak_text"
                                   (nlk:json-object "text" "hello there"
                                                    "path" target
                                                    "voice" "aura-2-zeus-en"))))
          (let ((request (voice-request 0)))
            (is (equal "https://openrouter.ai/api/v1/audio/speech" (second request)))
            (is (equal "Bearer k-test" (voice-header request "authorization")))
            (is (equal "application/json" (voice-header request "content-type")))
            (let ((body (voice-json request)))
              (is (equal "deepgram/aura-2" (nlk:json-value body :string "model")))
              (is (equal "hello there" (nlk:json-value body :string "input")))
              (is (equal "mp3" (nlk:json-value body :string "response_format")) "mp3 is what the catalog's models agree on")
              (is (equal "aura-2-zeus-en" (nlk:json-value body :string "voice")) "the call's own voice")
              (is (null (nlk:json-value body :number "speed")) "1.0 is not sent")))
          (is (search "wrote " answer) "the answer says where the file went")
          (is (equal '(73 68 51 4 0) (coerce (nodecode-openrouter-voice::read-octets target) 'list)) "the mp3 landed there"))))))

(deftest openrouter-voice-cell-speed-is-clamped-and-a-voiceless-model-omits-it ()
  (with-voice-wire
    (with-cell-stop ((openrouter-voice-start "api_key" "k-test"))
      (voice-answer-audio (make-array 3 :element-type '(unsigned-byte 8) :initial-element 1))
      (with-temp-file (target :type "mp3")
        (voice-tool "speak_text" (nlk:json-object "text" "hi" "path" target "speed" 3.0))
        (let ((body (voice-json (voice-request 0))))
          (is (= 1.5 (nlk:json-value body :number "speed")) "aura-2 takes 0.7-1.5 natively, so 3.0 is clamped")
          (is (equal "aura-2-thalia-en" (nlk:json-value body :string "voice")) "the section's voice")))
      (voice-answer-audio (make-array 3 :element-type '(unsigned-byte 8) :initial-element 2))
      (with-temp-file (target :type "mp3")
        (voice-tool "speak_text" (nlk:json-object "text" "hi" "path" target
                                                    "model" "fish-audio/s2-pro"))
        (let ((body (voice-json (voice-request 1))))
          (is (equal "fish-audio/s2-pro" (nlk:json-value body :string "model")))
          (is (null (gethash "voice" body)) "a model with no voices is sent none"))))))

(deftest openrouter-voice-cell-a-refused-call-is-a-repair-not-a-crash
  (with-voice-wire
    (with-cell-stop ((openrouter-voice-start "api_key" "k-test"))
      (voice-answer "{\"error\":{\"message\":\"Invalid API key\"}}" 401)
      (with-temp-file (audio :type "mp3" :contents "bytes")
        (is-carrying (text (voice-tool "transcribe_audio" (nlk:json-object "path" audio)))
                     ("ERROR: OpenRouter answered HTTP 401" "the status is repeated")
                     ("Invalid API key" "and OpenRouter's own words")))
      (is-carrying (text (voice-tool "transcribe_audio" (nlk:json-object "path" "/nonexistent/rec.wav")))
                   ("cannot read" "a missing file is a refusal too")))))

;;; --- /voice ---------------------------------------------------------------------

(deftest openrouter-voice-cell-the-command-shows-the-endpoint-or-transcribes ()
  (with-voice-wire
    (with-cell-stop ((openrouter-voice-start "api_key" "k-test"))
      (let ((answer (cell-entry "nodecode-openrouter-voice" "voice" "")))
        (is (search "OpenRouter voice: https://openrouter.ai/api/v1" answer))
        (is (search "openai/whisper-large-v3" answer) "the transcription default")
        (is (search "deepgram/aura-2" answer) "the speech default")
        (is (search "ffmpeg: not installed" answer) "and that ffmpeg is missing")
        (is (null (voice-asked)) "a status read asks OpenRouter nothing"))
      (voice-answer "{\"text\":\"take one\"}")
      (with-temp-file (audio :type "mp3" :contents "bytes")
        (is (equal "take one" (cell-entry "nodecode-openrouter-voice" "voice" audio)) "an argument is a file to transcribe"))
      (is (search "deepgram/aura-2 ships" (cell-entry "nodecode-openrouter-voice" "voice" "voices")) "/voice voices lists the default model's voice ids"))))
