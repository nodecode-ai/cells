;;;; cell-test.lisp --- one utterance schemad, submitted, polled, saved; the
;;;; voices a model declares; the ids the queue lists; and every refusal that
;;;; costs nothing.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here dials: NLK:HTTP is a stub that records the call and answers
;;;; handcrafted JSON or bytes, the clock, the environment and the schema cache
;;;; are stubs, and every recording is written into a folder of this test's own
;;;; under the scratch home. What is pinned is what a user gets -- words become
;;;; a real audio file whose extension is the container GMI declared, a voice
;;;; list costs one schema read -- and what a refused call never costs: a
;;;; missing key, a voice the model does not declare, a model with no container
;;;; parameter, and a 401 all answer before, or instead of, a second request.

(in-package #:nodecode.test)

(define-test-slice "gmi-speech" "GMI-SPEECH-CELL-" :start nodecode-gmi-speech:start-cell)

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "gmi-speech"
  (:config "api_key" "test-key"
           "voice" "English_expressive_narrator"
           "directory" (namestring (merge-pathnames "gmi-speech-cell-test/" (nlk:home))))
  (:help :gmi-speech)
  (:command "gmi-speech")
  (:running (is (equal "test-key" (nodecode-gmi-speech::setting :api-key)) "the section's own key")
            (is (equal "English_expressive_narrator" (nodecode-gmi-speech::setting :voice)) "the section's voice")
            (is (null (nodecode-gmi-speech::setting :speed)) "no pace of its own")
            (is (equal "mp3" (nodecode-gmi-speech::setting :format)) "mp3 unless asked otherwise")
            (is (= 300 (nodecode-gmi-speech::setting :poll-timeout-seconds)) "the plugin's own job ceiling")
            (is (search "console.gmicloud.ai" (nodecode-gmi-speech::setting :api-base-url)) "the queue the plugin dialled")
            (is-present (entry (nle::find-tool "gmi_speech_say")) "the speaking tool"
              (is (equal "gmi_speech_say" (first entry))))
            (is-present (entry (nle::find-tool "gmi_speech_voices")) "the voice list"
              (is (equal "gmi_speech_voices" (first entry))))
            (is-present (entry (nle::find-tool "gmi_speech_models")) "the model list"
              (is (equal "gmi_speech_models" (first entry)))))
  (:stopped (is (null (nle::find-tool "gmi_speech_say")) "stop takes the speaking tool back out")
            (is (null (nle::find-tool "gmi_speech_voices")) "and the voice list")
            (is (null (nle::find-tool "gmi_speech_models")) "and the model list"))
  (:refused ("api_base_url" "ftp://console.gmicloud.ai/api/v1/ie/requestqueue/apikey")
            ("api_base_url" "http://console.gmicloud.ai/api/v1/ie/requestqueue/apikey")
            ("speed" "fast")
            ("poll_timeout_seconds" 5))
  (:idle nodecode-gmi-speech:gmi-speech-error (nodecode-gmi-speech::status-text)))

;;; --- fixtures -------------------------------------------------------------------

(defun gmi-speech-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "gmi-voice-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defun gmi-speech-start* (&rest pairs)
  "The cell started on PAIRS, in a fresh folder unless PAIRS names one."
  (apply #'gmi-speech-start
         (append (unless (member "directory" pairs :test #'string=)
                   (list "directory" (namestring (gmi-speech-directory))))
                 pairs)))

(defmacro with-gmi-speech (pairs &body body)
  "BODY with the cell started on PAIRS, a list of (KEY VALUE) pairs, in a fresh
folder unless PAIRS names one; the folder is deleted afterwards."
  `(let* ((started (gmi-speech-start* ,@(loop for pair in pairs append pair)))
          (directory (nodecode-gmi-speech::setting :directory)))
     (nlk:with-cleanup ((progn (funcall started)
                                (uiop:delete-directory-tree directory :validate t
                                                             :if-does-not-exist :ignore)))
       ,@body)))

(defun gmi-speech-tool (name &rest pairs)
  "The tool NAME as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun gmi-speech-object (&rest specs)
  "One JSON object from SPECS, as NLK:JSON-OBJECT builds it -- callable, because
a fixture builds objects whose keys it already has in hand."
  (apply #'nlk:make-json-object specs))

(defun gmi-speech-json (object)
  "OBJECT as the text body a stub answers."
  (nlk:encode-json-object object))

(defun gmi-speech-bytes (&optional (n 8))
  "N octets, enough for a recording the cell only copies."
  (let ((out (make-array n :element-type '(unsigned-byte 8) :initial-element #x41)))
    (setf (aref out 0) #xFF)
    out))

(defun gmi-speech-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun gmi-speech-first-line (answer)
  (subseq answer 0 (or (position #\Newline answer) (length answer))))

(defun gmi-speech-saved-path (answer)
  "The path the first line of ANSWER names."
  (let ((line (gmi-speech-first-line answer)))
    (and (uiop:string-prefix-p "saved " line) (subseq line 6))))

(defun gmi-speech-file-length (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

(defmacro with-queue ((asked &key answer) &body body)
  "BODY with NLK:HTTP stubbed: ASKED collects (METHOD URL HEADERS CONTENT) oldest
first, and ANSWER answers (values BODY STATUS HEADERS)."
  `(let ((,asked '()))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content &allow-other-keys)
                                (push (list method url headers content) ,asked)
                                (funcall ,answer method url :headers headers :content content))
       (let ((nodecode-gmi-speech::*sleep* (lambda (seconds) (declare (ignore seconds)) nil))
             (nodecode-gmi-speech::*now* (let ((tick 0)) (lambda () (incf tick))))
             (nodecode-gmi-speech::*getenv* (lambda (name) (declare (ignore name)) nil))
             (nodecode-gmi-speech::*schemas* (make-hash-table :test #'equal)))
         ,@body))))

(defun gmi-speech-call (asked &optional (index 0))
  "The INDEXth call ASKED recorded, as (values METHOD URL HEADERS CONTENT)."
  (let ((call (nth index (reverse asked))))
    (values (first call) (second call) (third call) (fourth call))))

(defun gmi-speech-sent (content)
  "The object a POST carried."
  (nlk:decode-json content))

;;; --- schema and job bodies ----------------------------------------------------------

(defun gmi-speech-parameters (&key (formats '("mp3" "wav")) voice-options
                                    (speed-min 0.5) (speed-max 2.0))
  "A GMI speech schema's parameters, in the shape the queue declares them."
  (let ((out (list (gmi-speech-object "name" "text" "type" "string" "required" t)
                    (gmi-speech-object "name" "speed" "type" "float"
                                       "min_value" speed-min "max_value" speed-max))))
    (when voice-options
      (push (gmi-speech-object "name" "voice_id" "type" "string"
                               "options" (map 'vector (lambda (voice)
                                                        (gmi-speech-object "value" voice))
                                              voice-options))
            out))
    (when formats
      (push (gmi-speech-object "name" "format" "type" "string" "default_value" "mp3"
                               "options" (map 'vector (lambda (format)
                                                        (gmi-speech-object "value" format))
                                              formats))
            out))
    (coerce (nreverse out) 'vector)))

(defun gmi-speech-schema-body (&rest keys &key (model "minimax-tts-speech-2.6-turbo")
                                             (status "active")
                                             ((:formats formats) '("mp3" "wav"))
                                             ((:voice-options voices) '("English_expressive_narrator"
                                                                       "English_radiant_girl")))
  "The body a model-schema read answers, wrapped in the `data' member GMI uses."
  (gmi-speech-json
   (gmi-speech-object
    "data" (gmi-speech-object
             "model" model
             "status" status
             "model_type" "audio"
             "qworkers" 4
             "brief_description" "a speech model"
             "modalities" (gmi-speech-object "output"
                                              (vector (gmi-speech-object "type" "audio")))
             "parameters" (gmi-speech-parameters
                           :formats formats :voice-options voices)))))

(defun gmi-speech-accepted-body (&optional (request-id "req-1"))
  (gmi-speech-json (gmi-speech-object "request_id" request-id "status" "queued")))

(defun gmi-speech-running-body (&optional (request-id "req-1"))
  (gmi-speech-json (gmi-speech-object "request_id" request-id "status" "running")))

(defun gmi-speech-finished-body (audio-url &optional (request-id "req-1"))
  (gmi-speech-json
   (gmi-speech-object "request_id" request-id
                       "status" "success"
                       "outcome" (gmi-speech-object
                                   "audio_url" (vector (gmi-speech-object "url" audio-url))))))

(defun gmi-speech-failed-body (detail &optional (request-id "req-1"))
  (gmi-speech-json (gmi-speech-object "request_id" request-id "status" "failed"
                                       "error" detail)))

(defun gmi-speech-schema-answer (asked)
  "What a well-behaved queue answers for one speech call: a schema, an accepted
job, one running poll, a finished poll, and the recording. ASKED is not read;
it keeps the call sites alike."
  (declare (ignore asked))
  (let ((polls 0))
    (lambda (method url &rest keys)
      (declare (ignore keys))
      (cond ((search "/models/" url) (values (gmi-speech-schema-body) 200 nil))
            ((and (eq method :post) (search "/requests" url)) (values (gmi-speech-accepted-body) 202 nil))
            ((search "/requests/req-1" url)
             (incf polls)
             (if (= polls 1)
                 (values (gmi-speech-running-body) 200 nil)
                 (values (gmi-speech-finished-body "https://cdn.gmicloud.ai/audio/req-1.mp3")
                         200 nil)))
            ((search "cdn.gmicloud.ai" url) (values (gmi-speech-bytes 9) 200 nil))
            (t (values "{}" 404 nil))))))

(defun gmi-speech-simple-answer (asked)
  "The same queue without the running poll: the job is finished when it is accepted."
  (lambda (method url &rest keys)
    (declare (ignore keys))
    (declare (ignore asked))
    (cond ((search "/models/" url) (values (gmi-speech-schema-body) 200 nil))
          ((and (eq method :post) (search "/requests" url))
           (values (gmi-speech-finished-body "https://cdn.gmicloud.ai/audio/req-1.mp3") 200 nil))
          ((search "cdn.gmicloud.ai" url) (values (gmi-speech-bytes 5) 200 nil))
          (t (values "{}" 404 nil)))))

;;; --- speaking --------------------------------------------------------------------

(deftest gmi-speech-cell-words-become-an-audio-file ()
  (with-gmi-speech (("api_key" "test-key")) 
    (let ((asked '()) (text "Hello from the lake, this is the cell speaking."))
      (with-queue (asked :answer (gmi-speech-simple-answer asked))
        (let ((answer (gmi-speech-tool "gmi_speech_say" "text" text
                                                         "voice" "English_radiant_girl"
                                                         "speed" 1.5)))
          (multiple-value-bind (method url headers content) (gmi-speech-call asked)
            (is (eq :get method) "the first call reads the model's schema")
            (is (search "models/minimax-tts-speech-2.6-turbo" url) "the first of the plugin's speech models")
            (is (equal "Bearer test-key" (gmi-speech-header headers "authorization")) "with the section's key"))
          (multiple-value-bind (method url headers content) (gmi-speech-call asked 1)
            (declare (ignore url headers))
            (is (eq :post method) "then one POST to the request queue")
            (let* ((sent (gmi-speech-sent content))
                   (payload (nlk:json-value sent :object "payload")))
              (is (equal "minimax-tts-speech-2.6-turbo" (nlk:json-value sent :string "model")) "naming the model")
              (is (equal text (nlk:json-value payload :string "text")) "the words ride as text")
              (is (equal "English_radiant_girl" (nlk:json-value payload :string "voice_id")) "the call's own voice")
              (is (= 1.5 (nlk:json-value payload :number "speed")) "and its pace")
              (is (equal "mp3" (nlk:json-value payload :string "format")) "the section's container")))
          (let ((path (gmi-speech-saved-path answer)))
            (is (search "saved " answer))
            (is-present (file (probe-file path)) "the answer names a file that is there"
              (is (uiop:string-suffix-p (namestring file) ".mp3") "carrying the container's extension"))
            (is (= 5 (gmi-speech-file-length path)) "holding exactly the bytes that came back")
            (is (search "billed to the gmi cloud account" (string-downcase answer)) "and who it is billed to"))
          (is (= 3 (length asked)) "schema, submit, download -- and a finished submit needs no poll"))))))

(deftest gmi-speech-cell-a-container-falls-back-to-one-the-model-writes ()
  (with-gmi-speech (("api_key" "test-key"))
    (let ((asked '()))
      (with-queue (asked :answer (lambda (method url &rest keys)
                                    (declare (ignore method keys))
                                    (cond ((search "/models/" url)
                                           (values (gmi-speech-schema-body :formats '("mp3" "LINEAR16"))
                                                   200 nil))
                                          ((search "/requests/req-1" url)
                                           (values (gmi-speech-accepted-body) 202 nil))
                                          ((search "/requests" url)
                                           (values (gmi-speech-finished-body
                                                    "https://cdn.gmicloud.ai/audio/req-1.mp3")
                                                   200 nil))
                                          ((search "cdn.gmicloud.ai" url) (values (gmi-speech-bytes 4) 200 nil))
                                          (t (values "{}" 404 nil)))))
        (let ((answer (gmi-speech-tool "gmi_speech_say" "text" "a wave" "format" "wav")))
          (multiple-value-bind (method url headers content) (gmi-speech-call asked 1)
            (declare (ignore url headers))
            (is (eq :post method))
            (let ((payload (nlk:json-value (gmi-speech-sent content) :object "payload")))
              (is (equal "LINEAR16" (nlk:json-value payload :string "format")) "wav is the LINEAR16 the model offers")))
          (is (uiop:string-suffix-p (gmi-speech-saved-path answer) ".wav") "and the file is a wav"))))))

(deftest gmi-speech-cell-an-unknown-voice-is-refused-before-the-job ()
  (with-gmi-speech (("api_key" "test-key"))
    (let ((asked '()))
      (with-queue (asked :answer (gmi-speech-simple-answer asked))
        (let ((answer (gmi-speech-tool "gmi_speech_say" "text" "hello" "voice" "Nobody")))
          (is (search "ERROR:" answer) "the call answers a failure")
          (is (search "English_expressive_narrator" answer) "naming a voice the model does declare")
          (is (= 1 (length asked)) "and nothing was submitted"))))))

(deftest gmi-speech-cell-a-model-with-no-container-parameter-is-refused ()
  (with-gmi-speech (("api_key" "test-key"))
    (let ((asked '()))
      (with-queue (asked :answer (lambda (method url &rest keys)
                                    (declare (ignore method keys))
                                    (cond ((search "/models/" url)
                                           (values (gmi-speech-schema-body :formats nil) 200 nil))
                                          (t (values "{}" 404 nil)))))
        (let ((answer (gmi-speech-tool "gmi_speech_say" "text" "hello" "model" "a-model-with-no-format")))
          (is (search "ERROR:" answer))
          (is (search "names no audio file" answer) "the refusal names the missing container")
          (is (= 1 (length asked)) "nothing was submitted"))))))

(deftest gmi-speech-cell-a-failed-job-reports-gmi-s-own-words ()
  (with-gmi-speech (("api_key" "test-key"))
    (let ((asked '()))
      (with-queue (asked :answer (lambda (method url &rest keys)
                                    (declare (ignore method keys))
                                    (cond ((search "/models/" url) (values (gmi-speech-schema-body) 200 nil))
                                          ((search "/requests/req-1" url)
                                           (values (gmi-speech-failed-body "the voice engine gave up")
                                                   200 nil))
                                          ((search "/requests" url) (values (gmi-speech-accepted-body) 202 nil))
                                          (t (values "{}" 404 nil)))))
        (let ((answer (gmi-speech-tool "gmi_speech_say" "text" "something")))
          (is (search "ERROR:" answer))
          (is (search "the voice engine gave up" answer) "GMI's own sentence")
          (is (= 3 (length asked)) "and no download was attempted"))))))

(deftest gmi-speech-cell-without-a-key-refuses-and-sends-nothing ()
  (with-gmi-speech (("api_key" "") ("api_key_env" "GMI_SPEECH_TEST_KEY_UNSET"))
    (let ((asked '()))
      (with-queue (asked)
        (let ((answer (gmi-speech-tool "gmi_speech_say" "text" "anything at all")))
          (is (search "ERROR:" answer))
          (is (search "GMI_SPEECH_TEST_KEY_UNSET" answer) "naming the variable to set")
          (is (search "Nothing was sent" answer)))
        (is (null asked) "and nothing was dialled")))))

(deftest gmi-speech-cell-reports-a-rejected-credential ()
  (with-gmi-speech (("api_key" "test-key"))
    (let ((asked '()))
      (with-queue (asked :answer (lambda (method url &rest keys)
                                    (declare (ignore method url keys))
                                    (values (gmi-speech-json (gmi-speech-object
                                                               "error" "Invalid API key"))
                                            401 nil)))
        (let ((answer (gmi-speech-tool "gmi_speech_say" "text" "a sentence")))
          (is (search "rejected the credential" answer))
          (is (search "Invalid API key" answer) "GMI's own words")
          (is (search "Check the key" answer) "and the fix it names")
          (is (= 1 (length asked)) "one call was made"))))))

;;; --- what a model declares ------------------------------------------------------------

(deftest gmi-speech-cell-voices-come-from-the-model-s-schema ()
  (with-gmi-speech (("api_key" "test-key") ("voice" "English_radiant_girl"))
    (let ((asked '()))
      (with-queue (asked :answer (gmi-speech-schema-answer asked))
        (let ((answer (gmi-speech-tool "gmi_speech_voices")))
          (is (search "English_expressive_narrator" answer) "a voice the schema declares")
          (is (search "* English_radiant_girl" answer) "the section's own, marked")
          (is (search "the model's own schema" answer) "and where the list came from"))
        (is (= 1 (length asked)) "one schema read, and nothing generated")))))

(deftest gmi-speech-cell-a-voice-query-narrows-the-list ()
  (with-gmi-speech (("api_key" "test-key"))
    (let ((asked '()))
      (with-queue (asked :answer (gmi-speech-schema-answer asked))
        (let ((answer (gmi-speech-tool "gmi_speech_voices" "query" "radiant")))
          (is (search "English_radiant_girl" answer))
          (is (not (search "English_expressive_narrator" answer)) "the other voice is filtered out"))
        (let ((answer (gmi-speech-tool "gmi_speech_voices" "query" "klingon")))
          (is (search "ERROR:" answer) "nothing matching is a refusal, not an empty list"))))))

(deftest gmi-speech-cell-models-list-the-queues-ids ()
  (with-gmi-speech (("api_key" "test-key"))
    (let ((asked '()))
      (with-queue (asked :answer (lambda (method url &rest keys)
                                    (declare (ignore method keys))
                                    (declare (ignore url))
                                    (values (gmi-speech-json
                                             (gmi-speech-object
                                              "model_ids"
                                              (vector "minimax-tts-speech-2.6-turbo"
                                                      "inworld-tts-1.5-mini"
                                                      "b0f0f0f0-1111-2222-3333-444455556666"
                                                      "internal-probe-1")))
                                            200 nil)))
        (let ((answer (gmi-speech-tool "gmi_speech_models")))
          (is (search "minimax-tts-speech-2.6-turbo" answer) "a speech model")
          (is (search "inworld-tts-1.5-mini" answer) "the other the plugin preferred")
          (is (not (search "b0f0f0f0" answer)) "the queue's uuid is dropped")
          (is (not (search "internal-probe" answer)) "and its probe model")
          (is (search "nothing spent" answer)))
        (is (= 1 (length asked)) "one request for the whole list")))))

;;; --- the command ----------------------------------------------------------------------

(deftest gmi-speech-cell-command-answers-the-settings ()
  (with-gmi-speech (("api_key" "test-key"))
    (let ((asked '())
          (command (lambda (args)
                     (funcall (nle::slash-command-headless
                               (nle::find-registered-command "gmi-speech"))
                              args nil))))
      (with-queue (asked :answer (gmi-speech-schema-answer asked))
        (let ((answer (funcall command "")))
          (is (search "GMI Cloud speech" answer) "the command answers")
          (is (search "set in gmi-speech.api_key" answer) "and where the key comes from")
          (is (not (search "test-key" answer)) "never the key itself")
          (is (search "minimax-tts-speech-2.6-turbo" answer) "and the model it would ask for"))
        (let ((answer (funcall command "unknown-thing")))
          (is (search "Unknown" answer) "an unknown verb says what there is")))
      (is (null asked) "and the settings dialled nothing"))))
