;;;; cell-test.lisp --- the two calls, the folder they leave, and every
;;;; refusal, with the API, the recorder and the player stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials and nothing here touches a device: NLE::HTTP answers
;;;; handcrafted bytes, NLK:RUN-BOUNDED writes the WAV a recorder would
;;;; write, and *PATH-LOOKUP* is what a machine without a player looks like.
;;;; What is pinned is what a user gets -- a spoken file that is there and
;;;; played, a recording whose words come back, a transcript both leave --
;;;; and what a refused call never costs: no key, no recorder and too long a
;;;; listening all answer before anything is sent.

(in-package #:nodecode.test)

(define-test-slice "live-voice" "LIVE-VOICE-" :start nodecode-live-voice:start-cell)

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "live-voice"
  (:config "api_key" "test-key"
           "directory" (namestring (nlk:home "voice-cell-test/")))
  (:help :voice)
  (:command "voice")
  (:running (is (equal "test-key" (nodecode-live-voice::setting :api-key)) "the section's own key")
            (is (equal "https://api.openai.com/v1" (nodecode-live-voice::setting :base-url)) "the address the section defaults to")
            (is-present (entry (nle::find-tool "voice_listen")) "voice_listen is registered"
                        (is (equal "voice_listen" (first entry))))
            (is-present (entry (nle::find-tool "voice_speak")) "voice_speak is registered"
                        (is (equal "voice_speak" (first entry)))))
  (:stopped (is (null (nle::find-tool "voice_listen")) "stop takes voice_listen back out")
            (is (null (nle::find-tool "voice_speak")) "and voice_speak with it"))
  (:refused ("base_url" "ftp://api.openai.com/v1")
            ("base_url" "http://api.openai.com/v1"))
  (:idle nodecode-live-voice:live-voice-error (nodecode-live-voice::status-text)))

;;; --- fixtures ---------------------------------------------------------------------

(defun voice-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "voice-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defun voice-start (&rest pairs)
  "The cell started on PAIRS: a recorder and a player of this test's own, so
no machine's device decides what a test runs."
  (apply #'live-voice-start
         (append (unless (member "directory" pairs :test #'equal)
                   (list "directory" (namestring (voice-directory))))
                 (unless (member "recorder" pairs :test #'equal)
                   (list "recorder" "test-recorder %f %s"))
                 (unless (member "player" pairs :test #'equal)
                   (list "player" "test-player %f"))
                 (copy-list pairs))))

(defmacro with-voice ((&body pairs) &body body)
  "BODY with the cell started on PAIRS, the section's members as a flat
key/value list (or one list of them); its folder and its settings go after."
  (let ((flat (if (every #'consp pairs)
                  (loop for pair in pairs append pair)
                  pairs)))
    `(let* ((started (voice-start ,@flat))
            (directory (nodecode-live-voice::setting :directory)))
       (nlk:with-cleanup ((progn (funcall started)
                                  (uiop:delete-directory-tree directory :validate t
                                                               :if-does-not-exist :ignore)))
         ,@body))))

(defun voice-tool (name &rest pairs)
  "The tool as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-voice-endpoint ((asked &key (answer '(lambda (&rest call)
                                                      (declare (ignore call))
                                                      (values (voice-mp3) 200 nil))))
                               &body body)
  "BODY with the API stubbed: ASKED collects (URL HEADERS CONTENT) oldest
first, and ANSWER answers (values BODY STATUS HEADERS)."
  `(let ((,asked '()))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content &allow-other-keys)
                                (push (list method url headers content) ,asked)
                                (funcall ,answer url headers content))
       ,@body)))

(defun voice-call (asked &optional (index 0))
  "The INDEXth call ASKED recorded, as (values METHOD URL HEADERS CONTENT)."
  (let ((call (nth index (reverse asked))))
    (values (first call) (second call) (third call) (fourth call))))

(defun voice-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-voice-children ((commands &key (answer '(lambda (argv) (declare (ignore argv))
                                                        (values "" "" 0))))
                               &body body)
  "BODY with every child process stubbed: COMMANDS collects each argv, and
ANSWER answers (values OUTPUT ERRORS STATUS)."
  `(let ((,commands '()))
     (with-stubbed-fdefinition (nlk:run-bounded (argv &key &allow-other-keys)
                                (push argv ,commands)
                                (funcall ,answer argv))
       ,@body)))

;;; --- handcrafted audio -------------------------------------------------------------

(defun voice-wav (&optional (data-bytes 1600) (rate 16000))
  "A canonical RIFF/WAVE header over DATA-BYTES of silence."
  (let ((out (make-array (+ 44 data-bytes) :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((text (at string) (replace out (nodecode-live-voice::ascii string) :start1 at))
           (put (at n bytes)
             (loop for i below bytes do (setf (aref out (+ at i)) (ldb (byte 8 (* 8 i)) n)))))
      (text 0 "RIFF") (put 4 (- (+ 44 data-bytes) 8) 4) (text 8 "WAVE")
      (text 12 "fmt ") (put 16 16 4) (put 20 1 2) (put 22 1 2) (put 24 rate 4)
      (put 28 (* rate 2) 4) (put 32 2 2) (put 34 16 2)
      (text 36 "data") (put 40 data-bytes 4))
    out))

(defun voice-mp3 (&optional (bytes 512))
  "BYTES of an MPEG frame sync: what the speech endpoint answers."
  (let ((out (make-array bytes :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref out 0) #xFF (aref out 1) #xFB (aref out 2) #x90)
    out))

(defun voice-fake-recorder (argv)
  "What a recorder does: write the WAV the argv names, and answer success."
  (let ((file (find-if (lambda (word) (search ".wav" word)) argv)))
    (when file
      (with-open-file (out (pathname file) :direction :output :if-exists :supersede
                                         :if-does-not-exist :create
                                         :element-type '(unsigned-byte 8))
        (write-sequence (voice-wav 3200) out)))
    (values "" "" 0)))

(defun voice-json (content name)
  (cdr (assoc name content :test #'string=)))

(defun voice-file-length (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

;;; --- speaking --------------------------------------------------------------------

(deftest live-voice-speaks-a-text-into-a-file-and-plays-it ()
  (with-voice (("api_key" "test-key"))
    (let ((asked '()) (commands '()) (text "the tests are green"))
      (with-voice-endpoint (asked)
        (with-voice-children (commands)
          (let ((answer (voice-tool "voice_speak" "text" text "instructions" "quietly")))
            (multiple-value-bind (method url headers content) (voice-call asked)
              (is (eq :post method) "one POST")
              (is (search "/audio/speech" url) "to the speech endpoint")
              (is (equal "Bearer test-key" (voice-header headers "Authorization")) "the section's key, as a bearer")
              (let ((sent (nlk:decode-json content)))
                (is (equal text (nlk:json-value sent :text "input")) "the text rides as input")
                (is (equal "gpt-4o-mini-tts" (nlk:json-value sent :text "model")) "the section's speech model")
                (is (equal "alloy" (nlk:json-value sent :text "voice")) "and the section's voice")
                (is (equal "mp3" (nlk:json-value sent :text "response_format")))
                (is (equal "quietly" (nlk:json-value sent :text "instructions")) "the instruction rides too")))
            (let ((path (subseq answer 6 (position #\Newline answer))))
              (is (probe-file path) "the answer names a file that is there")
              (is (= 512 (voice-file-length path)) "holding exactly the bytes that came back")
              (is (equal "test-player" (first (first commands))) "the player ran on it")
              (is (equal path (second (first commands))) "with the file it wrote"))
            (is (search "played here" answer) "the answer says it was played")
            (is (search (format nil "~d characters" (length text)) answer) "and what it spoke")))))))

(deftest live-voice-without-a-player-saves-the-file-and-says-so ()
  (with-voice (("api_key" "test-key") ("player" ""))
    (let ((asked '()) (commands '()))
      (with-voice-endpoint (asked)
        (with-stubbed-fdefinition (nlk:run-bounded (argv &key &allow-other-keys)
                                   (push argv commands) (values "" "" 0))
          (let ((nodecode-live-voice::*path-lookup*
                  (lambda (name) (declare (ignore name)) nil)))
            (let ((answer (voice-tool "voice_speak" "text" "a short answer")))
              (is (search "saved " answer) "the file is saved anyway")
              (is (search "not played" answer) "and the answer says why")
              (is (search "live-voice.player" answer) "naming the setting that fixes it")
              (is (null commands) "no player ran"))))))))

;;; --- hearing ---------------------------------------------------------------------

(deftest live-voice-hears-a-recording-as-text ()
  (with-voice (("api_key" "test-key"))
    (let ((asked '()) (commands '()))
      (with-voice-endpoint (asked
                            :answer (lambda (&rest call)
                                      (declare (ignore call))
                                      (values "{\"text\":\"  make the tests green \"}" 200 nil)))
        (with-voice-children (commands :answer #'voice-fake-recorder)
          (let ((answer (voice-tool "voice_listen" "seconds" 2 "language" "en")))
            (is (equal "make the tests green" answer) "the transcript is the tool's answer")
            (let ((recorder (first (reverse commands))))
              (is (equal "test-recorder" (first recorder)) "the section's recorder ran")
              (is (equal "2" (third recorder)) "for the seconds asked for"))
            (multiple-value-bind (method url headers content) (voice-call asked)
              (is (eq :post method) "one POST")
              (is (search "/audio/transcriptions" url) "to the transcription endpoint")
              (is (equal "Bearer test-key" (voice-header headers "Authorization")))
              (is (equal "gpt-4o-mini-transcribe" (voice-json content "model")))
              (is (equal "json" (voice-json content "response_format")))
              (is (equal "en" (voice-json content "language")) "the language hint rides")
              (is (probe-file (voice-json content "file")) "and the recording that was just made"))
            (let ((entries (nodecode-live-voice::transcript-entries)))
              (is (= 1 (length entries)) "the recording leaves one transcript line")
              (is (equal "heard" (nlk:json-value (first entries) :string "kind")))
              (is (equal "make the tests green" (nlk:json-value (first entries) :string "text")))
              (is (= 2 (nlk:json-value (first entries) :integer "seconds"))))
            (let ((listing (nodecode-live-voice::run-slash "transcript" "s1")))
              (is (search "heard:" listing) "/voice transcript shows it")
              (is (search "make the tests green" listing)))))))))

(deftest live-voice-refuses-a-length-the-section-does-not-allow ()
  (with-voice (("api_key" "test-key") ("max_seconds" 10))
    (let ((asked '()) (commands '()))
      (with-voice-endpoint (asked)
        (with-voice-children (commands :answer #'voice-fake-recorder)
          (let ((answer (voice-tool "voice_listen" "seconds" 40)))
            (is (search "1 to 10" answer) "the ceiling the section set is named")
            (is (null commands) "nothing recorded")
            (is (null asked) "and nothing was sent")))))))

(deftest live-voice-without-a-recorder-refuses-and-sends-nothing ()
  (with-voice (("api_key" "test-key") ("recorder" ""))
    (let ((asked '()))
      (with-voice-endpoint (asked)
        (let ((nodecode-live-voice::*path-lookup*
                (lambda (name) (declare (ignore name)) nil)))
          (let ((answer (voice-tool "voice_listen")))
            (is (search "no recorder" answer) "the answer is the reason")
            (is (search "live-voice.recorder" answer) "and the setting that fixes it")
            (is (null asked) "nothing was sent")))))))

;;; --- no key ---------------------------------------------------------------------

(deftest live-voice-without-a-key-refuses-and-sends-nothing ()
  (with-voice (("api_key" "") ("api_key_env" "VOICE_TEST_KEY_UNSET"))
    (let ((asked '()))
      (with-voice-endpoint (asked :answer (lambda (&rest call)
                                            (declare (ignore call))
                                            (error "no request should have gone out")))
        (let ((nodecode-live-voice::*getenv*
                (lambda (name) (declare (ignore name)) nil)))
          (let ((answer (voice-tool "voice_speak" "text" "hello")))
            (is (search "ERROR:" answer) "the call answers a failure")
            (is (search "VOICE_TEST_KEY_UNSET" answer) "naming the variable to set")
            (is (search "Nothing was sent" answer)))
          (let ((answer (voice-tool "voice_listen")))
            (is (search "VOICE_TEST_KEY_UNSET" answer) "listening refuses before it records"))
          (is (null asked) "and nothing was dialled"))))))

;;; --- the command -------------------------------------------------------------------

(deftest live-voice-the-command-reports-speaks-and-shows-the-transcript ()
  (with-voice (("api_key" "test-key"))
    (let ((asked '()) (commands '()))
      (with-voice-endpoint (asked)
        (with-voice-children (commands)
          (let ((report (nodecode-live-voice::run-slash "" "s1")))
            (is (search "key: set" report) "the report says the key is there")
            (is (search "microphone: test-recorder" report) "and which recorder hears")
            (is (search "player: test-player" report) "and which player speaks")
            (is (search "nothing heard or said yet" report)))
          (let ((answer (nodecode-live-voice::run-slash "say good evening" "s1")))
            (is (search "saved " answer) "/voice say writes a file")
            (let ((said (nlk:decode-json (fourth (first (reverse asked))))))
              (is (equal "good evening" (nlk:json-value said :text "input")) "it speaks what was typed after say")))
          (let ((listing (nodecode-live-voice::run-slash "transcript" "s1")))
            (is (search "said: good evening" listing) "/voice transcript shows what was said")
            (is (not (search "heard:" listing)) "and nothing has been heard")))))))
