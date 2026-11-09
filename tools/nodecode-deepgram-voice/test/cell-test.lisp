;;;; cell-test.lisp --- the two billed calls, the files they touch, and every
;;;; refusal, with the endpoint stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials: NLK:HTTP is a stub that records the call and answers
;;;; handcrafted bytes, the environment is a stub, and every spoken file is
;;;; written into a folder of this test's own under the scratch home. What is
;;;; pinned is what a user gets -- text becomes a real file the answer names, a
;;;; recording becomes its words -- and what a refused call never costs: a
;;;; missing key, an oversize text, a speed outside the range, a 200 that is
;;;; not audio and a missing file all answer before, or instead of, a second
;;;; request.

(in-package #:nodecode.test)

(define-test-slice "deepgram-voice" "DEEPGRAM-VOICE-" :start nodecode-deepgram-voice:start-cell)

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "deepgram-voice"
  (:config "api_key" "test-key"
           "directory" (namestring (nlk:home "deepgram-voice-cell-test/")))
  (:help :voice)
  (:command "say")
  (:running (is (equal "test-key" (nodecode-deepgram-voice::setting :api-key)) "the section's own key")
            (is (equal "aura-2-thalia-en" (nodecode-deepgram-voice::setting :voice)) "the documented default voice")
            (is (equal "nova-3" (nodecode-deepgram-voice::setting :stt-model)) "and the default Listen model")
            (is-present (entry (nle::find-tool "deepgram_speak"))
                        "the speak tool is registered under its own name"
                        (is (equal "deepgram_speak" (first entry))))
            (is-present (entry (nle::find-tool "deepgram_transcribe"))
                        "and the transcribe tool"
                        (is (equal "deepgram_transcribe" (first entry)))))
  (:stopped (is (null (nle::find-tool "deepgram_speak")) "stop takes the speak tool back out")
            (is (null (nle::find-tool "deepgram_transcribe")) "and the transcribe tool"))
  (:refused ("api_base_url" "ftp://api.deepgram.com/v1")
            ("api_base_url" "http://api.deepgram.com/v1"))
  (:idle nodecode-deepgram-voice:deepgram-voice-error (nodecode-deepgram-voice::voices-text)))

;;; --- fixtures ---------------------------------------------------------------------

(defun dg-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "dg-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defun dg-start (&rest pairs)
  "The cell started on PAIRS, in a fresh folder unless PAIRS names one."
  (apply #'deepgram-voice-start
         (append (unless (member "directory" pairs :test #'string=)
                   (list "directory" (namestring (dg-directory))))
                 (copy-list pairs))))

(defmacro with-voice ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, a flat key/value list; its folder is
removed afterwards."
  `(let* ((started (dg-start ,@pairs))
          (directory (nodecode-deepgram-voice::setting :directory)))
     (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t
                                                            :if-does-not-exist :ignore)))
       ,@body)))

(defun dg-tool (name &rest pairs)
  "The tool NAME as the engine calls it: the declared contract decoded, then
the body."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-dg-endpoint ((asked &key (answer '(lambda (url headers content)
                                                    (declare (ignore url headers content))
                                                    (values (dg-mp3) 200 nil))))
                             &body body)
  "BODY with the endpoint stubbed: ASKED collects (METHOD URL HEADERS CONTENT)
oldest first, and ANSWER answers (values BODY STATUS HEADERS)."
  `(let ((,asked '()))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content &allow-other-keys)
                                (push (list method url headers content) ,asked)
                                (funcall ,answer url headers content))
       ,@body)))

(defun dg-call (asked &optional (index 0))
  "The INDEXth call ASKED recorded, as (values METHOD URL HEADERS BODY)."
  (let ((call (nth index (reverse asked))))
    (values (first call) (second call) (third call) (fourth call))))

(defun dg-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun dg-query (url name)
  "The NAME parameter of URL's query string, or NIL."
  (let* ((mark (position #\? url))
         (query (and mark (subseq url (1+ mark)))))
    (when query
      (loop for part in (uiop:split-string query :separator "&")
            when (uiop:string-prefix-p (format nil "~a=" name) part)
              do (return (subseq part (1+ (length name))))))))

;;; --- handcrafted bytes -------------------------------------------------------------

(defun dg-mp3 (&optional (frames 4))
  "FRAMES MPEG-1 Layer III headers, enough for the signature check."
  (let ((out (make-array (* frames 417) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (index frames)
      (setf (aref out (* index 417)) #xFF
            (aref out (+ 1 (* index 417))) #xFB
            (aref out (+ 2 (* index 417))) #x90))
    out))

(defun dg-wav ()
  "A RIFF/WAVE header, which is what a wav call must be saved from."
  (let ((out (nodecode-deepgram-voice::octets-of "RIFF0000WAVEfmt ")))
    (concatenate '(vector (unsigned-byte 8)) out
                 (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))))

(defun dg-transcript-json (&key (text "hello there, this is a recording"))
  "Deepgram's own Listen answer shape."
  (format nil "{\"metadata\":{\"duration\":3.4,\"channels\":1},~
               \"results\":{\"channels\":[{\"detected_language\":\"en\",~
               \"alternatives\":[{\"transcript\":~s,\"confidence\":0.99}]}]}}" text))

(defun dg-recording (&optional (path (merge-pathnames "note.mp3" (dg-directory))))
  "A small recording written where the test can read it back."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
    (write-sequence (dg-mp3 2) out))
  path)

(defun dg-answer-path (answer)
  "The file the answer names on its first line."
  (subseq answer 6 (or (position #\Newline answer) (length answer))))

(defun dg-file-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

;;; --- speak ------------------------------------------------------------------------

(deftest deepgram-voice-speaks-text-into-a-file-it-names ()
  (with-voice ("api_key" "test-key")
    (let ((asked '()) (text "The tide comes in at four."))
      (with-dg-endpoint (asked)
        (let ((answer (dg-tool "deepgram_speak" "text" text "speed" 1.1)))
          (multiple-value-bind (method url headers body) (dg-call asked)
            (is (eq :post method) "one POST")
            (is (search "/speak?" url) "to the speak endpoint")
            (is (equal "aura-2-thalia-en" (dg-query url "model")) "with the section's voice as model")
            (is (equal "mp3" (dg-query url "encoding")) "and mp3 as the encoding")
            (is (equal "1.10" (dg-query url "speed")) "and the speed asked for")
            (is (equal "Token test-key" (dg-header headers "authorization")) "with the key as a Token")
            (is (equal "application/json" (dg-header headers "content-type")))
            (is (equal text (nlk:json-value (nlk:decode-json body) :text "text")) "the text rides as JSON"))
          (let ((path (dg-answer-path answer)))
            (is (probe-file path) "the answer names a file that is there")
            (is (= (* 4 417) (dg-file-bytes path)) "holding exactly the bytes that came back")
            (is (equal "mp3" (pathname-type (pathname path))) "named for the container asked for"))
          (is (search "The tide comes in at four." answer) "the answer quotes what it said")
          (is (search "aura-2-thalia-en, mp3" answer) "and names the voice and the format")
          (is (= 1 (length asked)) "one call, nothing more"))))))

(deftest deepgram-voice-speaks-each-container-and-refuses-bytes-that-are-not-audio ()
  (with-voice ("api_key" "test-key")
    (let ((asked '()))
      (with-dg-endpoint (asked
                          :answer (lambda (&rest call) (declare (ignore call))
                                    (values (dg-wav) 200 nil)))
        (let ((answer (dg-tool "deepgram_speak" "text" "a line" "format" "wav")))
          (multiple-value-bind (method url headers content) (dg-call asked)
            (declare (ignore content))
            (is (eq :post method))
            (is (equal "linear16" (dg-query url "encoding")) "wav is linear16")
            (is (equal "wav" (dg-query url "container")) "in a wav container")
            (is (equal "Token test-key" (dg-header headers "authorization"))))
          (is (equal "wav" (pathname-type (pathname (dg-answer-path answer)))) "saved with a wav name")))
      (with-dg-endpoint (asked
                          :answer (lambda (&rest call) (declare (ignore call))
                                    (values (nodecode-deepgram-voice::octets-of "{\"no\":\"audio\"}") 200 nil)))
        (let ((answer (dg-tool "deepgram_speak" "text" "a line")))
          (is (search "not mp3 audio" answer) "a 200 that is not audio is refused, not saved")))
      (is (= 0 (length (directory (merge-pathnames "*.mp3"
                                                   (nodecode-deepgram-voice::setting :directory))))) "a 200 that was not audio leaves no mp3 file"))))

(deftest deepgram-voice-refuses-an-oversize-text-and-a-speed-out-of-range ()
  (with-voice ("api_key" "test-key")
    (let ((asked '()))
      (with-dg-endpoint (asked)
        (let ((answer (dg-tool "deepgram_speak" "text" (make-string 2500 :initial-element #\a))))
          (is (search "2000" answer) "the ceiling is named")
          (is (search "split" answer) "and the fix"))
        (let ((answer (dg-tool "deepgram_speak" "text" "a line" "speed" 3)))
          (is (search "speed" answer))
          (is (search "0.7" answer) "the range is named"))
        (is (= 0 (length asked)) "nothing was sent, so nothing was billed"))
      (is (signals-error nlk:contract-refusal (dg-tool "deepgram_speak")) "a call with no text is refused by the declaration"))))

;;; --- transcribe -------------------------------------------------------------------

(deftest deepgram-voice-transcribes-a-recording ()
  (with-voice ("api_key" "test-key")
    (let ((asked '()) (recording (dg-recording))
          (words "hello there, this is a recording"))
      (nlk:with-cleanup ((ignore-errors (delete-file recording)))
        (with-dg-endpoint (asked
                            :answer (lambda (&rest call) (declare (ignore call))
                                      (values (nodecode-deepgram-voice::octets-of
                                               (dg-transcript-json :text words))
                                              200 nil)))
          (let ((answer (dg-tool "deepgram_transcribe" "file" (namestring recording))))
            (multiple-value-bind (method url headers content) (dg-call asked)
              (is (eq :post method) "one POST")
              (is (search "/listen?" url) "to the listen endpoint")
              (is (equal "nova-3" (dg-query url "model")) "with the section's model")
              (is (equal "true" (dg-query url "smart_format")) "smart formatting on by default")
              (is (equal "true" (dg-query url "detect_language")) "and language detection when no code is given")
              (is (null (dg-query url "language")) "no language forced")
              (is (equal "audio/mpeg" (dg-header headers "content-type")) "the recording is posted as the audio it is")
              (is (equal "Token test-key" (dg-header headers "authorization")))
              (is (= (length (dg-mp3 2)) (length content)) "the body is the recording's own bytes"))
            (is (search words answer) "the answer carries the words")
            (is (search "language en" answer) "and the language Deepgram detected")
            (is (search "3.4 s" answer) "and how much audio was read"))
          (let ((answer (dg-tool "deepgram_transcribe" "file" (namestring recording)
                                             "language" "pt-BR" "diarize" t)))
            (multiple-value-bind (method url headers content) (dg-call asked 1)
              (declare (ignore method headers content))
              (is (equal "pt-BR" (dg-query url "language")) "a named language is sent")
              (is (null (dg-query url "detect_language")) "and detection is off")
              (is (equal "true" (dg-query url "diarize")))
              (is (equal "true" (dg-query url "paragraphs")) "paragraphs ride with diarization")))
          (is (= 2 (length asked)) "two calls, one per recording read"))))))

(deftest deepgram-voice-a-missing-recording-and-a-refused-call-answer-in-words ()
  (with-voice ("api_key" "test-key")
    (let ((asked '()))
      (with-dg-endpoint (asked)
        (let ((answer (dg-tool "deepgram_transcribe" "file" "/nonexistent/nowhere.mp3")))
          (is (search "no file at" answer) "a missing recording is named")
          (is (search "path" answer) "with the fix"))
        (is (null asked) "and nothing was uploaded"))
      (let ((recording (dg-recording)))
        (nlk:with-cleanup ((ignore-errors (delete-file recording)))
          (with-dg-endpoint (asked
                              :answer (lambda (&rest call) (declare (ignore call))
                                        (values (nodecode-deepgram-voice::octets-of
                                                 "{\"err_code\":\"INVALID_AUTH\",\"err_msg\":\"Invalid credentials\"}")
                                                401 nil)))
            (let ((answer (dg-tool "deepgram_transcribe" "file" (namestring recording))))
              (is (search "HTTP 401" answer))
              (is (search "Invalid credentials" answer) "Deepgram's own sentence")
              (is (search "not valid" answer) "and the fix its code names")))))
      (with-dg-endpoint (asked
                          :answer (lambda (&rest call) (declare (ignore call))
                                    (values (nodecode-deepgram-voice::octets-of
                                             "{\"results\":{\"channels\":[{\"alternatives\":[{\"transcript\":\"\"}]}]}}")
                                            200 nil)))
        (let ((recording (dg-recording)))
          (nlk:with-cleanup ((ignore-errors (delete-file recording)))
            (let ((answer (dg-tool "deepgram_transcribe" "file" (namestring recording))))
              (is (search "heard no words" answer) "an empty transcript is said, not passed on empty"))))))))

;;; --- the key, and /say ----------------------------------------------------------------

(deftest deepgram-voice-without-a-key-refuses-and-sends-nothing ()
  (with-voice ("api_key" "" "api_key_env" "DEEPGRAM_VOICE_TEST_KEY_UNSET")
    (let ((asked '())
          (nodecode-deepgram-voice::*getenv* (lambda (name) (declare (ignore name)) nil)))
      (with-dg-endpoint (asked
                          :answer (lambda (&rest call) (declare (ignore call))
                                    (error "no request should have gone out")))
        (let ((answer (dg-tool "deepgram_speak" "text" "a line")))
          (is (search "ERROR:" answer) "the call answers a failure")
          (is (search "DEEPGRAM_VOICE_TEST_KEY_UNSET" answer) "naming the variable to set")
          (is (search "Nothing was sent" answer)))
        (is (null asked) "and nothing was dialled")))))

(deftest deepgram-voice-the-say-command-answers-the-voices-and-speaks-a-line ()
  (with-voice ("api_key" "test-key")
    (let ((listing (nodecode-deepgram-voice::run-say "" "s1")))
      (is (search "/say <text>" listing) "with no argument /say says how to use it")
      (is (search "aura-2-thalia-en" listing) "and names the voices")
      (is (search "mp3" listing) "and the containers"))
    (let ((asked '()))
      (with-dg-endpoint (asked)
        (let ((answer (nodecode-deepgram-voice::run-say "good morning" "s1")))
          (is (search "saved " answer) "/say speaks the line into a file")
          (is (probe-file (dg-answer-path answer)) "that is there")
          (is (equal "good morning" (nlk:json-value (nlk:decode-json (fourth (first asked))) :text "text")) "with the line as the text that was sent"))))))
