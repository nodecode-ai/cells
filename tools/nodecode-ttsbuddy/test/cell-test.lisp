;;;; cell-test.lisp --- one utterance submitted, polled and saved; one known
;;;; job recovered; the offline catalogue listed; and every refusal that
;;;; costs nothing.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials: NLK:HTTP is a stub that records the call and answers
;;;; handcrafted JSON or bytes, the clock and the environment are stubs, and
;;;; every file is written into a folder of this test's own under the scratch
;;;; home. What is pinned is what a user gets -- text becomes a real MP3, a
;;;; voice list costs no request, a known job downloads once -- and what a
;;;; refused call never costs: a missing key, a missing voice, a language on
;;;; a voice that takes none, and a 401 all answer before, or instead of, a
;;;; second request.

(in-package #:nodecode.test)

(define-test-slice "ttsbuddy" "TTSBUDDY-" :start nodecode-ttsbuddy:start-cell)

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "ttsbuddy"
  (:config "api_key" "test-key"
           "voice" "af_heart"
           "directory" (namestring (merge-pathnames "ttsbuddy-cell-test/" (nlk:home))))
  (:help :ttsbuddy)
  (:command "ttsbuddy")
  (:running (is (equal "test-key" (nodecode-ttsbuddy::setting :api-key)) "the section's own key")
            (is (equal "af_heart" (nodecode-ttsbuddy::setting :voice)) "the section's voice")
            (is (null (nodecode-ttsbuddy::setting :speed)) "no pace of its own")
            (is (= 600 (nodecode-ttsbuddy::setting :poll-timeout-seconds)) "the plugin's own job ceiling")
            (is (search "www.ttsbuddy.com" (nodecode-ttsbuddy::setting :api-base-url)) "the CLI's endpoint")
            (is-present (entry (nle::find-tool "ttsbuddy_speak")) "the speaking tool"
              (is (equal "ttsbuddy_speak" (first entry))))
            (is-present (entry (nle::find-tool "ttsbuddy_voices")) "the voice list"
              (is (equal "ttsbuddy_voices" (first entry))))
            (is-present (entry (nle::find-tool "ttsbuddy_recover")) "the recovery tool"
              (is (equal "ttsbuddy_recover" (first entry)))))
  (:stopped (is (null (nle::find-tool "ttsbuddy_speak")) "stop takes the speaking tool back out")
            (is (null (nle::find-tool "ttsbuddy_voices")) "and the voice list")
            (is (null (nle::find-tool "ttsbuddy_recover")) "and the recovery tool"))
  (:refused ("api_base_url" "ftp://www.ttsbuddy.com/v1/agent-tts")
            ("api_base_url" "http://www.ttsbuddy.com/v1/agent-tts")
            ("speed" "2")
            ("speed" "fast")
            ("poll_timeout_seconds" 5)
            ("poll_timeout_seconds" 4000))
  (:idle nodecode-ttsbuddy:ttsbuddy-error (nodecode-ttsbuddy::status-text)))

;;; --- fixtures -------------------------------------------------------------------

(defun ttsbuddy-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "voice-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defun ttsbuddy-start* (&rest pairs)
  "The cell started on PAIRS, in a fresh folder unless PAIRS names one."
  (apply #'ttsbuddy-start
         (append (unless (member "directory" pairs :test #'string=)
                   (list "directory" (namestring (ttsbuddy-directory))))
                 pairs)))

(defmacro with-ttsbuddy ((&body pairs) &body body)
  "BODY with the cell started on PAIRS, the section's members as a flat
key/value list or as pairs, in a fresh folder unless PAIRS names one; the
folder is deleted afterwards."
  (let ((flat (if (every #'consp pairs)
                  (loop for pair in pairs append pair)
                  pairs)))
    `(let* ((started (ttsbuddy-start* ,@flat))
            (directory (nodecode-ttsbuddy::setting :directory)))
       (nlk:with-cleanup ((progn (funcall started)
                                  (uiop:delete-directory-tree directory :validate t
                                                               :if-does-not-exist :ignore)))
         ,@body))))

(defun ttsbuddy-tool (name &rest pairs)
  "The tool NAME as the engine calls it: the declared contract decoded, then
the body."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun ttsbuddy-json (object)
  "OBJECT as the text body a stub answers."
  (nlk:encode-json-object object))

(defun ttsbuddy-ascii (text)
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun ttsbuddy-mp3 (&optional (frames 6))
  "FRAMES MPEG-1 Layer III frames, so the bytes begin like real audio."
  (let ((out (make-array (* frames 417) :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref out 0) #xFF (aref out 1) #xFB (aref out 2) #x90 (aref out 3) #x00)
    out))

(defun ttsbuddy-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun ttsbuddy-file-length (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

(defun ttsbuddy-first-line (answer)
  (subseq answer 0 (or (position #\Newline answer) (length answer))))

(defmacro with-endpoint ((asked &key (answer '(lambda (method url &rest keys)
                                                (declare (ignore method url keys))
                                                (error "no request should have gone out"))))
                         &body body)
  "BODY with NLK:HTTP stubbed: ASKED collects (METHOD URL HEADERS CONTENT)
oldest first, and ANSWER answers (values BODY STATUS HEADERS)."
  `(let ((,asked '()))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content &allow-other-keys)
                                (push (list method url headers content) ,asked)
                                (funcall ,answer method url :headers headers :content content))
       (let ((nodecode-ttsbuddy::*sleep* (lambda (seconds) (declare (ignore seconds)) nil))
             (nodecode-ttsbuddy::*now* (let ((tick 0)) (lambda () (incf tick)))))
         ,@body))))

(defun ttsbuddy-call (asked &optional (index 0))
  "The INDEXth call ASKED recorded, as (values METHOD URL HEADERS CONTENT)."
  (let ((call (nth index (reverse asked))))
    (values (first call) (second call) (third call) (fourth call))))

(defun ttsbuddy-completed (audio-url)
  "The body the API answers for a finished job."
  (ttsbuddy-json (nlk:json-object
                   "success" t
                   "status" "completed"
                   "job_id" "job-1"
                   "audio_url" audio-url
                   "audio" (nlk:json-object "format" "mp3" "voice" "af_heart" "speed" 1))))

(defun ttsbuddy-processing ()
  "The body the API answers for a job that was just accepted."
  (ttsbuddy-json (nlk:json-object
                   "success" t
                   "status" "processing"
                   "job_id" "job-1"
                   "retry_after_seconds" 1)))

;;; --- speaking --------------------------------------------------------------------

(deftest ttsbuddy-words-become-an-mp3-file ()
  (with-ttsbuddy (("api_key" "test-key") ("voice" "af_heart"))
    (let ((asked '()) (text "Hello from the cell, this is the test speaking."))
      (with-endpoint (asked
                      :answer (lambda (method url &rest keys)
                                (declare (ignore url keys))
                                (if (eq method :post)
                                    (values (ttsbuddy-completed "https://www.ttsbuddy.com/audio/job-1.mp3")
                                            200 nil)
                                    (values (ttsbuddy-mp3 6) 200
                                            (list (cons "content-type" "audio/mpeg"))))))
        (let ((answer (ttsbuddy-tool "ttsbuddy_speak" "text" text)))
          (multiple-value-bind (method url headers content) (ttsbuddy-call asked)
            (is (eq :post method) "one POST")
            (is (equal "https://www.ttsbuddy.com/v1/agent-tts" url) "to the agent endpoint the CLI used")
            (is (equal "Bearer test-key" (ttsbuddy-header headers "authorization")) "with the section's key")
            (is-present (key (ttsbuddy-header headers "idempotency-key")) "and an idempotency header"
              (is (>= (length key) 32)))
            (let ((sent (nlk:decode-json content)))
              (is (equal text (nlk:json-value sent :text "text")) "the words ride as text")
              (is (equal "af_heart" (nlk:json-value sent :text "voice")) "the section's voice")
              (is (equal "nodecode-ttsbuddy" (nlk:json-value sent :text "client_context" "client")) "who asks")))
          (multiple-value-bind (method url) (ttsbuddy-call asked 1)
            (is (eq :get method) "one GET for the finished audio")
            (is (equal "https://www.ttsbuddy.com/audio/job-1.mp3" url) "at the URL the job named"))
          (let ((path (subseq (ttsbuddy-first-line answer) 6)))
            (is (search "saved " answer))
            (is (probe-file path) "the answer names a file that is there")
            (is (= (* 6 417) (ttsbuddy-file-length path)) "holding exactly the bytes that came back")
            (is (search "Billed to the TTS Buddy account" answer) "and who it is billed to")))))))

(deftest ttsbuddy-a-running-job-is-polled-until-it-finishes ()
  (with-ttsbuddy (("api_key" "test-key"))
    (let ((asked '()))
      (with-endpoint (asked
                      :answer (lambda (method url &rest keys)
                                (declare (ignore url keys))
                                (cond ((eq method :post)
                                       (values (ttsbuddy-processing) 202 nil))
                                      ((search "?id=job-1" url)
                                       (values (ttsbuddy-completed
                                                "https://www.ttsbuddy.com/audio/job-1.mp3")
                                               200 nil))
                                      (t (values (ttsbuddy-mp3 3) 200 nil)))))
        (let ((answer (ttsbuddy-tool "ttsbuddy_speak" "text" "a longer thought")))
          (is (= 3 (length asked)) "the submit, one status poll and the download")
          (multiple-value-bind (method url) (ttsbuddy-call asked 1)
            (is (eq :get method) "the poll is a GET")
            (is (search "?id=job-1" url) "asking about the job id the submit named"))
          (is (probe-file (subseq (ttsbuddy-first-line answer) 6)) "and the file is there"))))))

(deftest ttsbuddy-a-failed-job-is-refused-with-its-own-reason ()
  (with-ttsbuddy (("api_key" "test-key"))
    (let ((asked '()))
      (with-endpoint (asked
                      :answer (lambda (method url &rest keys)
                                (declare (ignore method url keys))
                                (values (ttsbuddy-json
                                         (nlk:json-object
                                          "success" nil
                                          "status" "failed"
                                          "error" (nlk:json-object
                                                   "code" "TTS_PROVIDER_ERROR"
                                                   "message" "the voice engine gave up")))
                                        200 nil)))
        (let ((answer (ttsbuddy-tool "ttsbuddy_speak" "text" "something")))
          (is (search "ERROR:" answer))
          (is (search "the voice engine gave up" answer) "the API's own sentence")
          (is (= 1 (length asked)) "and no download was attempted"))))))

(deftest ttsbuddy-without-a-key-refuses-and-sends-nothing ()
  (with-ttsbuddy (("api_key" "") ("api_key_env" "TTSBUDDY_TEST_KEY_UNSET"))
    (let ((asked '()))
      (with-endpoint (asked)
        (let ((nodecode-ttsbuddy::*getenv* (lambda (name) (declare (ignore name)) nil)))
          (let ((answer (ttsbuddy-tool "ttsbuddy_speak" "text" "anything at all")))
            (is (search "ERROR:" answer) "the call answers a failure")
            (is (search "TTSBUDDY_TEST_KEY_UNSET" answer) "naming the variable to set")
            (is (search "Nothing was sent" answer)))
          (is (null asked) "and nothing was dialled"))))))

(deftest ttsbuddy-refuses-a-pace-and-a-language-no-voice-takes ()
  (with-ttsbuddy (("api_key" "test-key") ("voice" "af_heart"))
    (let ((asked '()))
      (with-endpoint (asked)
        (let ((answer (ttsbuddy-tool "ttsbuddy_speak" "text" "hello" "speed" 3.0)))
          (is (search "ERROR:" answer))
          (is (search "0.5 to 1.5" answer) "the pace the API accepts"))
        (let ((answer (ttsbuddy-tool "ttsbuddy_speak" "text" "hello" "language" "fr")))
          (is (search "only to the Supertonic" answer) "why the language fits no Kokoro voice"))
        (is (null asked) "and neither refusal cost a request")))))

(deftest ttsbuddy-reports-what-the-api-refused ()
  (with-ttsbuddy (("api_key" "test-key"))
    (let ((asked '()))
      (with-endpoint (asked
                      :answer (lambda (method url &rest keys)
                                (declare (ignore method url keys))
                                (values (ttsbuddy-json
                                         (nlk:json-object
                                          "success" nil
                                          "error" (nlk:json-object
                                                   "code" "INVALID_KEY"
                                                   "message" "Invalid API key")))
                                        401 nil)))
        (let ((answer (ttsbuddy-tool "ttsbuddy_speak" "text" "a sentence")))
          (is (search "HTTP 401" answer))
          (is (search "Invalid API key" answer) "TTS Buddy's own words")
          (is (search "check the key" answer) "and the fix that code names")
          (is (= 1 (length asked)) "one call was made"))))))

;;; --- recovery ----------------------------------------------------------------------

(deftest ttsbuddy-recovers-a-known-job-without-submitting-anything ()
  (with-ttsbuddy (("api_key" "test-key"))
    (let ((asked '()) (path (merge-pathnames "recovered.mp3" (ttsbuddy-directory))))
      (with-endpoint (asked
                      :answer (lambda (method url &rest keys)
                                (declare (ignore method url keys))
                                (if (search "?id=" url)
                                    (values (ttsbuddy-completed
                                             "https://www.ttsbuddy.com/audio/job-1.mp3")
                                            200 nil)
                                    (values (ttsbuddy-mp3 4) 200 nil))))
        (let ((answer (ttsbuddy-tool "ttsbuddy_recover" "job_id" "job-1"
                                                                "path" (namestring path))))
          (is (search "saved " answer))
          (is (probe-file path) "the file is there")
          (is (search "Nothing was submitted again" answer))
          (multiple-value-bind (method url) (ttsbuddy-call asked)
            (is (eq :get method) "no POST: recovery never submits speech")
            (is (search "?id=job-1" url) "it asks about the exact job id")))))))

(deftest ttsbuddy-recovery-never-overwrites-a-file ()
  (with-ttsbuddy (("api_key" "test-key"))
    (let ((asked '()) (path (merge-pathnames "keep.mp3" (ttsbuddy-directory))))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
        (write-sequence (ttsbuddy-ascii "mine") out))
      (nlk:with-cleanup ((uiop:delete-file-if-exists path))
        (with-endpoint (asked)
          (let ((answer (ttsbuddy-tool "ttsbuddy_recover" "job_id" "job-1"
                                                                  "path" (namestring path))))
            (is (search "already a file" answer) "the refusal names the file")
            (is (null asked) "and nothing was dialled"))
          (let ((answer (ttsbuddy-tool "ttsbuddy_recover" "job_id" "not a job id")))
            (is (search "exact id" answer) "a malformed id is refused too")))))))

;;; --- the catalogue, offline -----------------------------------------------------------

(deftest ttsbuddy-voices-are-listed-offline ()
  (with-ttsbuddy (("api_key" "test-key"))
    (let ((asked '()))
      (with-endpoint (asked)
        (let ((answer (ttsbuddy-tool "ttsbuddy_voices" "query" "french")))
          (is (search "ff_siwis" answer) "the French voice is listed")
          (is (null asked) "with no request at all"))
        (let ((answer (ttsbuddy-tool "ttsbuddy_voices" "language" "Supertonic" "limit" 3)))
          (is (search "st_m1" answer) "Supertonic voices are listed")
          (is (search "and" answer)))
        (let ((answer (ttsbuddy-tool "ttsbuddy_voices" "query" "klingon")))
          (is (search "ERROR:" answer) "nothing matching is a refusal, not an empty list"))
        (is (null asked) "the catalogue never dials")))))

;;; --- the command ----------------------------------------------------------------------

(deftest ttsbuddy-command-answers-the-settings-and-the-catalogue ()
  (with-ttsbuddy (("api_key" "test-key") ("voice" "af_heart"))
    (let ((asked '())
          (command (lambda (args)
                     (funcall (nle::slash-command-headless
                               (nle::find-registered-command "ttsbuddy"))
                              args nil))))
      (with-endpoint (asked)
        (let ((answer (funcall command "")))
          (is (search "TTS Buddy" answer) "the command answers")
          (is (search "af_heart" answer) "the voice it speaks in")
          (is (search "set in ttsbuddy.api_key" answer) "and where the key comes from")
          (is (not (search "test-key" answer)) "never the key itself"))
        (let ((answer (funcall command "voices british")))
          (is (search "bf_emma" answer) "a voice search from the operator's side")))
      (is (null asked) "and the command dialled nothing"))))
