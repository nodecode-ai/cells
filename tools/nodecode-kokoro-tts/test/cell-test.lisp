;;;; cell-test.lisp --- lifecycle, the two tools, /kokoro, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a voice that is not a voice id, a speed the server would refuse, a
;;;; wrong-typed member refused, a start with a url that is no root), the two
;;;; tools through the seam the model calls them by -- their URL, their JSON,
;;;; the WAV they build, and the language each sentence is read in -- and
;;;; /kokoro with and without text. Every request is a stubbed NLK:HTTP, so
;;;; nothing here dials a Kokoro server or the network.

(in-package #:nodecode.test)

;;; --- fixtures ----------------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values BODY STATUS).")

(defmacro with-kokoro-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered by
*ANSWER-FN*, so no test dials a server."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected Kokoro request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun kokoro-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun kokoro-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun kokoro-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun kokoro-wav (&optional (samples 240))
  "A WAV as the server would answer one: SAMPLES octets of 24 kHz mono 16-bit PCM."
  (nodecode-kokoro-tts::wav-octets
   (make-array samples :element-type '(unsigned-byte 8) :initial-element 7)
   24000 1 16))

(defun kokoro-answer-speech (&optional (samples 240))
  "Make the stub answer SAMPLES of speech, whatever is asked of it."
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values (kokoro-wav samples) 200))))

(defun kokoro-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defun kokoro-file-octets (file)
  "FILE's bytes."
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun kokoro-header-text (file start end)
  "FILE's bytes START..END as text -- a WAV chunk id."
  (map 'string #'code-char (subseq (kokoro-file-octets file) start end)))

(defun kokoro-header-number (file at bytes)
  "FILE's little-endian number of BYTES at AT."
  (let ((octets (kokoro-file-octets file))
        (value 0))
    (loop for index from 0 below bytes
          do (setf value (logior value (ash (aref octets (+ at index)) (* 8 index)))))
    value))

(defun kokoro-recording (&optional (name "kokoro-test.wav"))
  "A fresh path in the test's temporary folder."
  (merge-pathnames name (uiop:ensure-directory-pathname (uiop:temporary-directory))))

;;; --- lifecycle -----------------------------------------------------------------------

(defun run-kokoro-tts-tests ()
  "Run every KOKORO-TTS-CELL- test. This is the slice runner DEFINE-TEST-SLICE
would give, minus the slice name on the count line, so a run ends
`N tests, 0 failed' exactly as the port brief puts it; the entries, the
live-image refusal and the zero-count error are the slice's own."
  (let ((entries (remove "KOKORO-TTS-CELL-" *tests*
                         :test-not #'uiop:string-prefix-p
                         :key (lambda (entry) (symbol-name (car entry))))))
    (prog1 (run-suite entries
                       "run-kokoro-tts-tests refuses to run in a live image: store ~a is open")
      (unless entries
        (error "run-kokoro-tts-tests matched no tests - registration is broken")))))

(defun kokoro-tts-config (&rest pairs)
  "A whole shared config with one `kokoro' section built from PAIRS."
  (nlk:json-object "kokoro" (apply #'nlk:make-json-object pairs)))

(defun kokoro-tts-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-kokoro-tts:start-cell (apply #'kokoro-tts-config pairs)))

(define-cell-lifecycle-tests "kokoro-tts"
  (:fixture with-kokoro-wire)
  (:config "voice" "bf_emma")
  (:help :kokoro)
  (:command "kokoro")
  (:running (is (nle::find-tool "kokoro_speak") "kokoro_speak is registered")
            (is (nle::find-tool "kokoro_voices") "kokoro_voices is registered")
            (is (equal "http://127.0.0.1:8770" (nodecode-kokoro-tts::setting :url)) "the local server")
            (is (equal "bf_emma" (nodecode-kokoro-tts::setting :voice)) "the voice the section wrote")
            (is (= 120 (nodecode-kokoro-tts::setting :timeout-seconds)) "the default ceiling"))
  (:stopped (is (null (nle::find-tool "kokoro_speak")) "stop unregisters the tools")
            (is (null (nle::find-tool "kokoro_voices")) "both of them"))
  (:refused ("voice" "af heart") ("voice" "../etc/passwd") ("speed" "9") ("speed" 1)
            ("url" "127.0.0.1:8770") ("english_accent" "c"))
  (:idle nodecode-kokoro-tts:kokoro-tts-error (nodecode-kokoro-tts::run-command "" nil)))

(deftest kokoro-tts-cell-a-voice-that-is-no-voice-id-refuses-at-start ()
  (with-kokoro-wire
    (is-carrying (answer (refusal-text nlk:config-refusal (kokoro-tts-start "voice" "af/../../etc")))
                 ("not a Kokoro voice id" "the refusal says what a voice id is")
                 ("voice" "and names the setting"))
    (is (null (nle::find-tool "kokoro_speak")) "a refused start installed nothing")))

(deftest kokoro-tts-cell-a-voice-map-is-read-once-with-its-problems ()
  (with-kokoro-wire
    (with-cell-stop ((kokoro-tts-start "voices" (vector "pt=pf_dora" "fr=ff_siwis" "it=nope" "xx=af_heart")))
      (let ((map (nodecode-kokoro-tts::setting :voice-map))
            (problems (nodecode-kokoro-tts::setting :voice-problems)))
        (is (equal '(("pt" . "pf_dora")) map) "only a detected language with a real voice is kept")
        (is (= 3 (length problems)) "each entry that is ignored says so")
        (let ((named (some (lambda (problem)
                              (search "it: \"nope\" is not a Kokoro voice id" problem))
                            problems)))
          (is named "the bad voice is named"))))))

;;; --- kokoro_speak ---------------------------------------------------------------------

(deftest kokoro-tts-cell-speak-writes-the-recording-where-it-was-asked ()
  (with-kokoro-wire
    (with-cell-stop ((kokoro-tts-start "voice" "af_heart"))
      (kokoro-answer-speech 240)
      (let* ((file (kokoro-recording))
             (answer (kokoro-tool "kokoro_speak"
                                   (nlk:json-object "text" "Hello there, this is the local voice."
                                                    "path" (uiop:native-namestring file)))))
        (let ((said (format nil "Spoke ~d characters" (length "Hello there, this is the local voice."))))
          (is (search said answer) "the answer says what was said"))
        (is (search "voices: af_heart" answer) "and which voice said it")
        (is (search (uiop:native-namestring file) answer) "and where the recording is")
        (is (probe-file file) "the file is there")
        (is (string= "RIFF" (kokoro-header-text file 0 4)) "and it is a WAV")
        (is (= 240 (kokoro-header-number file 40 4)) "whose data chunk is the samples the server gave")
        (is (= 24000 (kokoro-header-number file 24 4)) "at the server's own rate")
        (let ((request (first (kokoro-asked))))
          (is (= 1 (length (kokoro-asked))) "one request, one sentence")
          (is (eq :post (first request)))
          (is (equal "http://127.0.0.1:8770/v1/audio/speech" (second request)))
          (is (equal "application/json" (kokoro-request-header request "content-type")))
          (let ((body (kokoro-request-body request)))
            (is (equal "Hello there, this is the local voice." (nlk:json-value body :string "input")))
            (is (equal "af_heart" (nlk:json-value body :string "voice")))
            (is (equal "wav" (nlk:json-value body :string "response_format")))
            (is (equal "a" (nlk:json-value body :string "lang_code")) "the voice's own language")
            (is (= 1 (nlk:json-value body :number "speed")) "the default pace")))))))

(deftest kokoro-tts-cell-each-sentence-is-read-in-its-own-language ()
  (with-kokoro-wire
    (with-cell-stop ((kokoro-tts-start "voice" "af_heart" "voices" (vector "pt=pf_dora")))
      (kokoro-answer-speech 100)
      (let* ((file (kokoro-recording "kokoro-mixed.wav"))
             (answer (kokoro-tool "kokoro_speak"
                                   (nlk:json-object
                                    "text" "Hello there, this is the local voice. O tempo está bom hoje?"
                                    "path" (uiop:native-namestring file)))))
        (is (= 2 (length (kokoro-asked))) "one request a language")
        (let ((first-body (kokoro-request-body (first (kokoro-asked))))
              (second-body (kokoro-request-body (second (kokoro-asked)))))
          (is (equal "af_heart" (nlk:json-value first-body :string "voice")) "English in the default voice")
          (is (equal "pf_dora" (nlk:json-value second-body :string "voice")) "Portuguese in its own")
          (is (equal "p" (nlk:json-value second-body :string "lang_code")) "with the voice's own language code")
          (is (search "está bom hoje" (nlk:json-value second-body :string "input")) "the sentence itself"))
        (is (search "voices: af_heart, pf_dora" answer) "the answer names both voices")
        (is (= 200 (kokoro-header-number file 40 4)) "and the file holds both recordings, joined")
        (is (string= "RIFF" (kokoro-header-text file 0 4)) "as one WAV")))))

(deftest kokoro-tts-cell-a-call-may-name-its-voice-and-the-server-may-refuse ()
  (with-kokoro-wire
    (with-cell-stop ((kokoro-tts-start "voice" "af_heart"))
      (kokoro-answer-speech 48)
      (let ((answer (kokoro-tool "kokoro_speak"
                                  (nlk:json-object "text" "Bom dia."
                                                    "voice" "pf_dora"
                                                    "path" (uiop:native-namestring (kokoro-recording "kokoro-dora.wav"))))))
        (is (search "voices: pf_dora" answer) "the call's voice wins over the section's")
        (let ((body (kokoro-request-body (first (kokoro-asked)))))
          (is (equal "pf_dora" (nlk:json-value body :string "voice")))
          (is (equal "p" (nlk:json-value body :string "lang_code")))))
      (setf *answer-fn* (lambda (method url args)
                          (declare (ignore method url args))
                          (values "{\"error\":{\"message\":\"voice not in the installed model\"}}" 400)))
      (is-carrying (answer (kokoro-tool "kokoro_speak" (nlk:json-object "text" "Ola.")))
                   ("ERROR: the server answered HTTP 400")
                   ("voice not in the installed model" "the server's own words"))
      (is-carrying (answer (kokoro-tool "kokoro_speak" (nlk:json-object "text" "   ")))
                   ("ERROR: there is nothing to say" "an empty text is refused before any request")))))

(deftest kokoro-tts-cell-english-words-ride-only-a-non-english-voice ()
  (with-kokoro-wire
    (with-cell-stop ((kokoro-tts-start "voice" "af_heart"
                                       "english_words" (vector "Kubernetes" "GitHub")
                                       "english_accent" "b"))
      (kokoro-answer-speech 24)
      (kokoro-tool "kokoro_speak" (nlk:json-object "text" "Hello." "voice" "af_heart"
                                                    "path" (uiop:native-namestring (kokoro-recording "kokoro-en.wav"))))
      (kokoro-tool "kokoro_speak" (nlk:json-object "text" "Bom dia." "voice" "pf_dora"
                                                    "path" (uiop:native-namestring (kokoro-recording "kokoro-pt.wav"))))
      (let ((english (kokoro-request-body (first (kokoro-asked))))
            (portuguese (kokoro-request-body (second (kokoro-asked)))))
        (is (null (nlk:json-value english :any "english_words")) "an English voice needs no loanword pass")
        (is (equal "b" (nlk:json-value portuguese :string "english_accent")) "the accent the section names")
        (let ((words (coerce (nlk:json-array portuguese "english_words") 'list)))
          (is (equal '("Kubernetes" "GitHub") words) "the words the section names"))))))

;;; --- kokoro_voices ---------------------------------------------------------------------

(deftest kokoro-tts-cell-voices-are-listed-with-their-language ()
  (with-kokoro-wire
    (with-cell-stop ((kokoro-tts-start "voice" "af_heart"))
      (setf *answer-fn* (lambda (method url args)
                          (declare (ignore method url args))
                          (values "{\"voices\":[\"af_heart\",\"pf_dora\",\"if_sara\",\"../etc\"]}" 200)))
      (let ((answer (kokoro-tool "kokoro_voices" (nlk:json-object))))
        (is (search "3 voices at http://127.0.0.1:8770" answer) "only the voice ids count")
        (is (search "af_heart  <- the section's voice" answer) "the default is marked")
        (is (search "Dora (Brazilian Portuguese)" answer) "each voice says its language")
        (is (search "Sara (Italian)" answer))
        (is (eq :get (first (first (kokoro-asked)))))
        (is (equal "http://127.0.0.1:8770/v1/voices" (second (first (kokoro-asked))))))
      (let ((answer (kokoro-tool "kokoro_voices" (nlk:json-object "language" "pt"))))
        (is (search "1 voice at http://127.0.0.1:8770, pt only" answer) "a language narrows the list")
        (is (null (search "if_sara" answer)) "and leaves the others out")))))

;;; --- /kokoro ---------------------------------------------------------------------------

(deftest kokoro-tts-cell-the-command-says-where-the-voice-is-or-speaks ()
  (with-kokoro-wire
    (with-cell-stop ((kokoro-tts-start "voices" (vector "pt=pf_dora")))
      (let ((answer (cell-entry "nodecode-kokoro-tts" "kokoro" "")))
        (is (search "Kokoro voice: http://127.0.0.1:8770, speaking as af_heart" answer)
            (format nil "answered: ~s" answer))
        (is (search "pt=pf_dora" answer) "the language map it will use")
        (is (search "speech.base_url" answer) "and how a room's spoken replies use it")
        (is (null (kokoro-asked)) "a status read asks the server nothing"))
      (kokoro-answer-speech 120)
      (let ((answer (cell-entry "nodecode-kokoro-tts" "kokoro" "Good morning, this is the local voice.")))
        (let ((said (format nil "Spoke ~d characters" (length "Good morning, this is the local voice."))))
          (is (search said answer) "with text, the command says it"))
        (is (search "voices: af_heart" answer) "in the default voice")
        (is (= 1 (length (kokoro-asked))) "one request")))))
