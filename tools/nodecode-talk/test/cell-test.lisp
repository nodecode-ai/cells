;;;; cell-test.lisp --- the talk cell against a stubbed wire and a stubbed CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing leaves the machine: NLK:HTTP is a stub, the player and the
;;;; recorder are a stub, and the key is one stubbed NLE::CREDENTIAL-ENV
;;;; answers. Each tool is called the way a model's call reaches it --
;;;; through the real dispatch, contract and all -- so the schema the model
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "talk" "TALK-CELL-")

(defun talk-config (&rest pairs)
  "A whole shared config holding the `talk' section PAIRS make, with the key
variable and the player defaulted to something a test can answer."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "api_key_env" section "TALK_TEST_KEY")
    (alexandria:ensure-gethash "player" section "afplay")
    (nlk:json-object "talk" section)))

(defun talk-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-talk:start-cell (apply #'talk-config pairs)))

(defmacro with-talk-key (&body body)
  "BODY with the key environment answering one fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "TALK_TEST_KEY") "sk-test-not-a-real-key"))
     ,@body))

(defmacro with-talk-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values BODY
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT HEADERS)."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)
                                               (getf options :headers))
                                         ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected talk request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defmacro with-talk-cli ((asked &key (ok t)) &body body)
  "BODY with the CLI seam answering OK and recording every argv it was given."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (let ((nodecode-talk::*run* (lambda (argv)
                                     (push argv ,log)
                                     ;; a recorder is asked to leave a file behind
                                     (dolist (word argv)
                                       (when (and (stringp word)
                                                  (uiop:string-suffix-p word ".wav"))
                                         (ensure-directories-exist word)
                                         (with-open-file (stream word :direction :output
                                                                      :if-exists :supersede)
                                           (write-string "RIFF" stream))))
                                     (values "" ,ok))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +audio+
  (make-array 6 :element-type '(unsigned-byte 8) :initial-contents '(255 251 144 100 1 2))
  "Six bytes standing in for one synthesized mp3.")

(defparameter +transcript+
  "{\"text\":\"put the kettle on when the build lands\"}"
  "What POST /v1/audio/transcriptions answers.")

(defun talk-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun talk-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded; a refusal is raised with its own words."
  (let ((answer (talk-answer id tool arguments)))
    (handler-case (nlk:decode-json answer)
      (error (condition)
        (error "~a answered ~a (~a)" tool answer condition)))))

(defun talk-write-audio ()
  "A small wav under the home, the file a transcribing call reads."
  (let ((path (nlk:home "talk/test-audio.wav")))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
      (write-sequence +audio+ stream))
    path))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "talk"
  (:config "api_key_env" "TALK_TEST_KEY")
  (:help :talk)
  (:fixture with-talk-key)
  (:command "talk"))

(deftest talk-cell-without-a-key-refuses ()
  ;; The one setting no verb can work without: the section names a variable
  ;; holding a key, and a variable that answers nothing is refused at start,
  ;; before anything installs.
  (with-stubbed-fdefinition (nle::credential-env (name)
                              (declare (ignorable name))
                              nil)
    (let ((condition (signals-error nlk:config-refusal
                                     (talk-start "api_key_env" "TALK_MISSING_KEY"))))
      (is (search "TALK_MISSING_KEY" (princ-to-string condition))
          "the refusal names the variable that answered nothing")
      (is (null (nle::find-tool "talk_speak")) "and installs nothing"))))

;;; --- talk_speak ---------------------------------------------------------------------

(deftest talk-cell-speaks-text-it-is-given (with-talk-key)
  (with-cell-stop ((talk-start))
    (with-talk-cli (ran)
      (with-talk-wire (asked ("/audio/speech" (values +audio+ 200)))
        (let* ((object (talk-object "talk-1" "talk_speak" "{\"text\":\"the build is green\"}"))
               (path (nlk:json-value object :string "path")))
          (is (eq t (nlk:json-value object :boolean "spoken")) "the call says it was spoken")
          (is (= (length +audio+) (nlk:json-value object :integer "bytes"))
              "and how many bytes it wrote")
          (is (equal "cedar" (nlk:json-value object :string "voice"))
              "the section's voice is the one sent")
          (is (equal "afplay" (nlk:json-value object :string "player"))
              "the configured player is the one that played it")
          (is (= 1 (length asked)) "one request speaks one text")
          (is (equal :post (first (first asked))))
          (is (search "/audio/speech" (second (first asked))))
          (let* ((headers (fourth (first asked)))
                 (auth (cdr (assoc "authorization" headers :test #'string=))))
            (is (equal "Bearer sk-test-not-a-real-key" auth) "the key rides the request")
            (is (search "gpt-4o-mini-tts" (third (first asked)))
                "and the original plugin's default speech model is asked for"))
          (is (equal 1 (length ran)) "the file is handed to the player once")
          (is (equal "afplay" (first (first ran))))
          (is (equal (namestring path) (second (first ran))))
          (is (probe-file path) "the audio is written where the answer says")
          (with-open-file (stream path :element-type '(unsigned-byte 8))
            (let ((written (make-array (length +audio+) :element-type '(unsigned-byte 8))))
              (read-sequence written stream)
              (is (equalp +audio+ written) "and holds the bytes the wire answered"))))))))

(deftest talk-cell-refuses-a-speech-the-wire-refused (with-talk-key)
  (with-cell-stop ((talk-start))
    (with-talk-cli (ran)
      (with-talk-wire (asked ("/audio/speech" (values "{\"error\":\"invalid voice\"}" 400)))
        (declare (ignore asked))
        (let ((answer (talk-answer "talk-2" "talk_speak" "{\"text\":\"hello\"}")))
          (is (search "ERROR:" answer) "a refused answer is an error the model can read")
          (is (search "400" answer) "and it names the status")
          (is (null ran) "nothing is played when there is no audio"))))))

;;; --- talk_transcribe ------------------------------------------------------------------

(deftest talk-cell-transcribes-an-audio-file (with-talk-key)
  (with-cell-stop ((talk-start))
    (let ((audio (talk-write-audio)))
      (with-talk-wire (asked ("/audio/transcriptions" (values +transcript+ 200)))
        (let ((object (talk-object "talk-3" "talk_transcribe"
                                   (format nil "{\"path\":~s,\"language\":\"en\"}"
                                           (namestring audio)))))
          (is (equal "put the kettle on when the build lands"
                     (nlk:json-value object :string "text"))
              "the transcript is the answer")
          (is (equal "gpt-4o-mini-transcribe" (nlk:json-value object :string "model"))
              "the original plugin's default transcription model is the one asked for")
          (is (= 1 (length asked)))
          (is (search "/audio/transcriptions" (second (first asked))))
          (let* ((headers (fourth (first asked)))
                 (type (cdr (assoc "content-type" headers :test #'string=)))
                 (body (third (first asked))))
            (is (search "multipart/form-data; boundary=" type) "the file rides multipart")
            (is (vectorp body) "and the body is octets, not a string")
            (is (search "test-audio.wav" (map 'string #'code-char body))
                "the filename the API is given is the file's own")))))))

(deftest talk-cell-records-then-transcribes (with-talk-key)
  (with-cell-stop ((talk-start "record_command" "recorder %f %s" "record_seconds" 3))
    (with-talk-cli (ran)
      (with-talk-wire (asked ("/audio/transcriptions" (values +transcript+ 200)))
        (let ((object (talk-object "talk-4" "talk_listen" "{\"seconds\":2}")))
          (is (equal "put the kettle on when the build lands"
                     (nlk:json-value object :string "text"))
              "what was recorded is transcribed")
          (is (equal "microphone" (nlk:json-value object :string "source"))
              "and the answer says it came from the microphone")
          (is (= 1 (length ran)) "the recorder ran once")
          (is (equal "recorder" (first (first ran))))
          (is (equal "2" (third (first ran))) "the call's seconds reach %s")
          (is (search "voice-" (second (first ran))) "and the file %f")
          (is (= 1 (length asked)))        (is (search "/audio/transcriptions" (second (first asked)))))))))

(deftest talk-cell-refuses-a-file-that-is-not-there (with-talk-key)
  (with-cell-stop ((talk-start))
    (with-talk-wire (asked ("/audio/transcriptions" (values +transcript+ 200)))
      (declare (ignore asked))
      (let ((answer (talk-answer "talk-5" "talk_transcribe"
                                  "{\"path\":\"/nowhere/at/all/missing.wav\"}")))
        (is (search "ERROR:" answer) "a missing file is a refusal, not a crash")
        (is (search "missing.wav" answer) "and the refusal names the file")))))

(deftest talk-cell-refuses-with-no-recorder (with-talk-key)
  (with-cell-stop ((talk-start "record_command" "" "player" "afplay"))
    (with-talk-cli (ran)
      (let ((nodecode-talk::*run* (lambda (argv) (declare (ignore argv)) (values "" t))))
        (with-stubbed-fdefinition (nodecode-talk::find-program (name)
                                    (declare (ignorable name))
                                    nil)
          (let ((answer (talk-answer "talk-6" "talk_listen" "{}")))
            (is (search "ERROR:" answer) "no recorder is a refusal")
            (is (search "record_command" answer) "and it names the setting that fixes it")))))))
;;; --- speed, a voice outside the list, and the state report -----------------------------

(deftest talk-cell-speaks-at-the-speed-it-is-given (with-talk-key)
  (with-cell-stop ((talk-start))
    (with-talk-cli (ran)
      (declare (ignore ran))
      (with-talk-wire (asked ("/audio/speech" (values +audio+ 200)))
        (let ((object (talk-object "talk-7" "talk_speak"
                                    "{\"text\":\"slow down\",\"speed\":0.7}")))
          (is (eq t (nlk:json-value object :boolean "spoken")))
          (let ((payload (nlk:decode-json (third (first asked)))))
            (is (< 0.69 (nlk:json-value payload :number "speed") 0.71)
                "the pace asked for rides the speech request, as the original provider sent it")))))))

(deftest talk-cell-refuses-a-voice-it-does-not-speak (with-talk-key)
  (with-cell-stop ((talk-start))
    (with-talk-cli (ran)
      (declare (ignore ran))
      (with-talk-wire (asked ("/audio/speech" (values +audio+ 200)))
        (let ((answer (talk-answer "talk-8" "talk_speak"
                                    "{\"text\":\"hello\",\"voice\":\"baritone\"}")))
          (is (search "ERROR:" answer) "an unknown voice is a refusal")
          (is (search "cedar" answer) "and the refusal lists the voices that do answer")
          (is (null asked) "nothing is dialled: the voice is checked first"))))))

(deftest talk-cell-reports-its-state (with-talk-key)
  (with-cell-stop ((talk-start "voice" "marin" "tts_model" "tts-1"))
    (with-talk-cli (ran)
      (declare (ignore ran))
      (with-talk-wire (asked ("/nowhere" (values "{}" 200)))
        (let ((object (talk-object "talk-9" "talk_status" "{}")))
          (is (equal "tts-1" (nlk:json-value object :string "speech_model"))
              "the report names the model that would speak")
          (is (equal "marin" (nlk:json-value object :string "voice")))
          (is (equal "gpt-4o-mini-transcribe"
                     (nlk:json-value object :string "transcription_model")))
          (is (equal "TALK_TEST_KEY" (nlk:json-value object :string "key_env"))
              "and the variable the key comes from, never the key")
          (is (null asked) "reporting the state dials nothing"))
        (let ((line (nodecode-talk::run-talk "status" nil)))
          (is (search "marin" line) "/talk status says the same in one line")
          (is (search "gpt-4o-mini-transcribe" line) "and names what transcribes"))))))
