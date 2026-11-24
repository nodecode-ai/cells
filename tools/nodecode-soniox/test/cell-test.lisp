;;;; cell-test.lisp --- the two billed calls, the files they touch, and every
;;;; refusal, with the endpoints stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials: NLK:HTTP is a stub that records the call and answers
;;;; handcrafted bytes, the environment is a stub, the poll's clock and sleep are
;;;; a counter the test advances, and every spoken file is written into a folder
;;;; of this test's own under the scratch home. What is pinned is what a user
;;;; gets -- text becomes a real file the answer names, a recording becomes its
;;;; words and is deleted from Soniox afterwards -- and what a refused call
;;;; never costs: a missing key, an oversize text, a 200 that is not audio and a
;;;; missing file all answer before, or instead of, a second request.

(in-package #:nodecode.test)

(define-test-slice "soniox" "SONIOX-" :start nodecode-soniox:start-cell)

;;; --- the cell's own lifecycle -----------------------------------------------------

(define-cell-lifecycle-tests "soniox"
  (:config "api_key" "test-key"
           "directory" (namestring (nlk:home "soniox-cell-test/")))
  (:help :soniox)
  (:command "soniox")
  (:running (is (equal "test-key" (nodecode-soniox::setting :api-key)) "the section's own key")
            (is (equal "Adrian" (nodecode-soniox::setting :voice)) "the documented default voice")
            (is (equal "tts-rt-v2" (nodecode-soniox::setting :model)) "and the default speech model")
            (is (equal "stt-async-v6" (nodecode-soniox::setting :stt-model)) "and the default transcription model")
            (is-present (entry (nle::find-tool "soniox_speak"))
                        "the speak tool is registered under its own name"
                        (is (equal "soniox_speak" (first entry))))
            (is-present (entry (nle::find-tool "soniox_transcribe"))
                        "and the transcribe tool"
                        (is (equal "soniox_transcribe" (first entry)))))
  (:stopped (is (null (nle::find-tool "soniox_speak")) "stop takes the speak tool back out")
            (is (null (nle::find-tool "soniox_transcribe")) "and the transcribe tool"))
  (:refused ("api_base_url" "ftp://api.soniox.com/v1")
            ("api_base_url" "http://api.soniox.com/v1")
            ("poll_timeout_seconds" 1))
  (:idle nodecode-soniox:soniox-error (nodecode-soniox::settings-text)))

;;; --- fixtures ---------------------------------------------------------------------

(defun sz-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "sz-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defun sz-start (&rest pairs)
  "The cell started on PAIRS, in a fresh folder unless PAIRS names one."
  (apply #'soniox-start
         (append (unless (member "directory" pairs :test #'equal)
                   (list "directory" (namestring (sz-directory))))
                 (copy-list pairs))))

(defmacro with-soniox ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, a flat key/value list; its folder is
removed afterwards."
  `(let* ((started (sz-start ,@pairs))
          (directory (nodecode-soniox::setting :directory)))
     (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t
                                                            :if-does-not-exist :ignore)))
       ,@body)))

(defmacro with-sz-clock (&body body)
  "BODY with the poll's clock a counter of the test's own: sleeping advances
it, and no test waits a second of real time."
  `(let* ((clock 0.0)
          (nodecode-soniox::*monotonic* (lambda () clock))
          (nodecode-soniox::*sleep* (lambda (seconds) (incf clock seconds))))
     ,@body))

(defun sz-tool (name &rest pairs)
  "The tool NAME as the engine calls it: the declared contract decoded, then
the body."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-sz-endpoint ((asked &key (answer '(lambda (url headers content)
                                                    (declare (ignore url headers content))
                                                    (values (sz-mp3) 200 nil))))
                             &body body)
  "BODY with the endpoints stubbed: ASKED collects (METHOD URL HEADERS CONTENT)
oldest first, and ANSWER -- evaluated once, so a scripted answer keeps its
place -- answers (values BODY STATUS HEADERS)."
  `(let ((,asked '())
         (answer-fn ,answer))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content &allow-other-keys)
                                (push (list method url headers content) ,asked)
                                (funcall answer-fn url headers content))
       ,@body)))

(defun sz-call (asked &optional (index 0))
  "The INDEXth call ASKED recorded, as (values METHOD URL HEADERS BODY)."
  (let ((call (nth index (reverse asked))))
    (values (first call) (second call) (third call) (fourth call))))

(defun sz-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

;;; --- handcrafted bytes --------------------------------------------------------------

(defun sz-json (text)
  "TEXT as the octets an endpoint answers with, so a body is never a Lisp string."
  (nodecode-soniox::octets-of text))

(defun sz-mp3 (&optional (frames 4))
  "FRAMES MPEG-1 Layer III headers, enough for the signature check."
  (let ((out (make-array (* frames 417) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (index frames)
      (setf (aref out (* index 417)) #xFF
            (aref out (+ 1 (* index 417))) #xFB
            (aref out (+ 2 (* index 417))) #x90))
    out))

(defun sz-wav ()
  "A RIFF/WAVE header, which is what a wav call must be saved from."
  (let ((out (nodecode-soniox::octets-of "RIFF0000WAVEfmt ")))
    (concatenate '(vector (unsigned-byte 8)) out
                 (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))))

(defun sz-ogg ()
  "An Ogg page carrying an Opus header, which is what an opus call may answer."
  (nodecode-soniox::octets-of "OggS0000OpusHead0000"))

(defun sz-close (value wanted)
  "Whether VALUE is WANTED as a pace, to three places: a JSON number comes
back as whichever float type the wire wrote, so = would compare the types."
  (and (numberp value) (< (abs (- (float value 1.0d0) wanted)) 1d-3)))

(defun sz-path (answer)
  "The file the answer names on its first line."
  (subseq answer 6 (or (position #\Newline answer) (length answer))))

(defun sz-file-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

(defun sz-recording (&optional (path (merge-pathnames "note.mp3" (sz-directory))))
  "A small recording written where the test can read it back."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
    (write-sequence (sz-mp3 2) out))
  path)

(defun sz-conversation (&key (statuses '("completed")) (text "hello there, this is a recording")
                             (tokens nil))
  "Soniox's whole asynchronous conversation, stubbed: the upload, the create,
STATUSES in the order the status reads come back, the last one repeating, the
transcript, and the two deletes."
  (let ((pending (copy-list statuses)))
    (lambda (url headers content)
      (declare (ignore headers content))
      (cond
        ((uiop:string-suffix-p url "/transcript")
         (values (sz-json (format nil "{\"text\":~s}" text)) 200 nil))
        ((uiop:string-suffix-p url "/transcriptions")
         (values (sz-json "{\"id\":\"transcription-1\"}") 200 nil))
        ((uiop:string-suffix-p url "/files")
         (values (sz-json "{\"id\":\"file-1\"}") 200 nil))
        ((search "/transcriptions/" url)
         (let ((state (or (pop pending) (car (last statuses)) "completed")))
           (if (equal state "error")
               (values (sz-json "{\"status\":\"error\",\"error_type\":\"audio_duration_exceeded\",\"error_message\":\"the audio is too long\"}") 200 nil)
               (values (sz-json (if tokens
                                     (format nil "{\"status\":~s,\"tokens\":[{\"text\":\"hall\"},{\"text\":\"o\"}]}" state)
                                     (format nil "{\"status\":~s}" state)))
                       200 nil))))
        (t (values (sz-json "{}") 200 nil))))))

;;; --- speak ---------------------------------------------------------------------------

(deftest soniox-speaks-text-into-a-file-it-names ()
  (with-soniox ("api_key" "test-key")
    (let ((asked '()) (text "The tide comes in at four."))
      (with-sz-endpoint (asked)
        (let ((answer (sz-tool "soniox_speak" "text" text "speed" 1.1)))
          (multiple-value-bind (method url headers content) (sz-call asked)
            (is (eq :post method) "one POST")
            (is (equal "https://tts-rt.soniox.com/tts" url) "to the text-to-speech endpoint")
            (is (equal "Bearer test-key" (sz-header headers "authorization")) "with the key as a bearer")
            (is (equal "application/json" (sz-header headers "content-type")))
            (let ((payload (nlk:decode-json content)))
              (is (equal text (nlk:json-value payload :text "text")) "the text rides as JSON")
              (is (equal "tts-rt-v2" (nlk:json-value payload :text "model")) "with the section's model")
              (is (equal "Adrian" (nlk:json-value payload :text "voice")) "and voice")
              (is (equal "en" (nlk:json-value payload :text "language")) "and language")
              (is (equal "mp3" (nlk:json-value payload :text "audio_format")))
              (is (sz-close (nlk:json-value payload :number "speed") 1.1) "and the pace asked for")))
          (let ((path (sz-path answer)))
            (is (probe-file path) "the answer names a file that is there")
            (is (= (* 4 417) (sz-file-bytes path)) "holding exactly the bytes that came back")
            (is (equal "mp3" (pathname-type (pathname path))) "named for the container asked for"))
          (is (search "The tide comes in at four." answer) "the answer quotes what it said")
          (is (search "voice Adrian, mp3" answer) "and names the voice and the format")
          (is (= 1 (length asked)) "one call, nothing more"))))))

(deftest soniox-speaks-each-container-and-clamps-the-pace ()
  (with-soniox ("api_key" "test-key")
    (let ((asked '()))
      (with-sz-endpoint (asked
                         :answer (lambda (&rest call) (declare (ignore call))
                                   (values (sz-wav) 200 nil)))
        (let ((answer (sz-tool "soniox_speak" "text" "a line" "format" "wav" "speed" 2.5)))
          (let ((payload (nlk:decode-json (fourth (first asked)))))
            (is (equal "wav" (nlk:json-value payload :text "audio_format")) "wav is asked for as wav")
            (is (sz-close (nlk:json-value payload :number "speed") 1.3) "and a pace past the top is clamped, not refused"))
          (is (equal "wav" (pathname-type (pathname (sz-path answer)))) "saved with a wav name")))
      (with-sz-endpoint (asked
                         :answer (lambda (&rest call) (declare (ignore call))
                                   (values (sz-ogg) 200 nil)))
        (let ((answer (sz-tool "soniox_speak" "text" "a line" "format" "ogg")))
          (is (equal "opus" (nlk:json-value (nlk:decode-json (fourth (first asked))) :text "audio_format")) "ogg asks Soniox for opus")
          (is (equal "ogg" (pathname-type (pathname (sz-path answer)))) "and an Ogg answer is saved as .ogg")))
      (with-sz-endpoint (asked
                         :answer (lambda (&rest call) (declare (ignore call))
                                   (values (sz-json "{\"error_type\":\"invalid_request\"}") 200 nil)))
        (let ((answer (sz-tool "soniox_speak" "text" "a line")))
          (is (search "not mp3 audio" answer) "a 200 that is not audio is refused, not saved"))))))

(deftest soniox-refuses-an-oversize-text-and-answers-a-refused-status ()
  (with-soniox ("api_key" "test-key")
    (let ((asked '()))
      (with-sz-endpoint (asked)
        (let ((answer (sz-tool "soniox_speak" "text" (make-string 5100 :initial-element #\a))))
          (is (search "5000" answer) "the ceiling is named")
          (is (search "split" answer) "and the fix"))
        (is (= 0 (length asked)) "nothing was sent, so nothing was billed"))
      (with-sz-endpoint (asked
                         :answer (lambda (&rest call) (declare (ignore call))
                                   (values (sz-json "{\"error_type\":\"invalid_api_key\",\"error_message\":\"bad key test-key\"}")
                                           401 nil)))
        (let ((answer (sz-tool "soniox_speak" "text" "a line")))
          (is (search "HTTP 401" answer) "a refused status is answered in words")
          (is (search "bad key ***" answer) "with the key redacted out of Soniox's sentence")
          (is (search "not valid" answer) "and the fix its error type names")))
      (is (signals-error nlk:contract-refusal (sz-tool "soniox_speak")) "a call with no text is refused by the declaration"))))

;;; --- transcribe ------------------------------------------------------------------------

(deftest soniox-transcribes-a-recording-and-deletes-it ()
  (with-soniox ("api_key" "test-key")
    (let ((asked '()) (recording (sz-recording))
          (words "hello there, this is a recording"))
      (nlk:with-cleanup ((ignore-errors (delete-file recording)))
        (with-sz-clock
          (with-sz-endpoint (asked :answer (sz-conversation :statuses '("queued" "completed")))
            (let ((answer (sz-tool "soniox_transcribe" "file" (namestring recording))))
              (multiple-value-bind (method url headers content) (sz-call asked)
                (is (eq :post method) "the upload is a POST")
                (is (equal "https://api.soniox.com/v1/files" url) "to the files endpoint")
                (is (equal "Bearer test-key" (sz-header headers "authorization")))
                (is (equal "file" (car (first content))) "the recording rides as the file part")
                (is (equal "note.mp3" (file-namestring (cdr (first content)))) "named as the file it is"))
              (multiple-value-bind (method url headers content) (sz-call asked 1)
                (declare (ignore method headers))
                (is (equal "https://api.soniox.com/v1/transcriptions" url) "then a transcription is created")
                (let ((payload (nlk:decode-json content)))
                  (is (equal "file-1" (nlk:json-value payload :text "file_id")) "of the uploaded file")
                  (is (equal "stt-async-v6" (nlk:json-value payload :text "model")) "with the section's model")
                  (is (null (nlk:json-value payload :any "language_hints")) "and no language hint when none is set")))
              (multiple-value-bind (method url headers content) (sz-call asked 2)
                (declare (ignore method headers content))
                (is (equal "https://api.soniox.com/v1/transcriptions/transcription-1" url) "the status is polled")
                (is (equal "Bearer test-key" (sz-header headers "authorization"))))
              (is (search words answer) "the answer carries the words")
              (is (search "from note.mp3" answer) "and names the recording it read")
              (let ((deletes (loop for call in (reverse asked)
                                   when (eq (first call) :delete) collect (second call))))
                (is (equal '("https://api.soniox.com/v1/transcriptions/transcription-1"
                             "https://api.soniox.com/v1/files/file-1")
                           deletes) "the transcription and the file are deleted from Soniox afterwards")))))))))

(deftest soniox-sends-language-hints-and-a-prompt-context ()
  (with-soniox ("api_key" "test-key")
    (let ((asked '()) (recording (sz-recording)))
      (nlk:with-cleanup ((ignore-errors (delete-file recording)))
        (with-sz-clock
          (with-sz-endpoint (asked :answer (sz-conversation))
            (sz-tool "soniox_transcribe" "file" (namestring recording)
                                        "language" "zh-CN, en" "prompt" "Nodecode, Soniox")
            (let ((payload (nlk:decode-json (fourth (second (reverse asked))))))
              (is (equalp #("zh" "en") (nlk:json-value payload :any "language_hints")) "BCP-47 values are reduced to bare ISO codes, in order")
              (is (equal "Nodecode, Soniox" (nlk:json-value payload :text "context" "text")) "and the prompt rides as context text"))))))))

(deftest soniox-a-recording-that-fails-or-times-out-answers-in-words ()
  (with-soniox ("api_key" "test-key" "poll_timeout_seconds" 10)
    (let ((asked '()) (recording (sz-recording)))
      (nlk:with-cleanup ((ignore-errors (delete-file recording)))
        (with-sz-clock
          (with-sz-endpoint (asked :answer (sz-conversation :statuses '("error")))
            (let ((answer (sz-tool "soniox_transcribe" "file" (namestring recording))))
              (is (search "transcription failed" answer))
              (is (search "the audio is too long" answer) "with Soniox's own sentence")
              (let ((deletes (loop for call in (reverse asked)
                                   when (eq (first call) :delete) collect t)))
                (is (= 2 (length deletes)) "and the file is still cleaned up"))))
          (with-sz-endpoint (asked :answer (sz-conversation :statuses '("processing")))
            (let ((answer (sz-tool "soniox_transcribe" "file" (namestring recording))))
              (is (search "timed out after 10 s" answer) "a poll past its limit times out in words"))))))))

;;; --- the key, and the command ---------------------------------------------------------

(deftest soniox-without-a-key-refuses-and-sends-nothing ()
  (with-soniox ("api_key" "" "api_key_env" "SONIOX_TEST_KEY_UNSET")
    (let ((asked '())
          (nodecode-soniox::*getenv* (lambda (name) (declare (ignore name)) nil)))
      (with-sz-endpoint (asked
                         :answer (lambda (&rest call) (declare (ignore call))
                                   (error "no request should have gone out")))
        (let ((answer (sz-tool "soniox_speak" "text" "a line")))
          (is (search "ERROR:" answer) "the call answers a failure")
          (is (search "SONIOX_TEST_KEY_UNSET" answer) "naming the variable to set")
          (is (search "Nothing was sent" answer)))
        (is (null asked) "and nothing was dialled")))))

(deftest soniox-the-command-answers-the-settings-and-speaks-a-line ()
  (with-soniox ("api_key" "test-key")
    (let ((listing (nodecode-soniox::run-command "" "s1")))
      (is (search "/soniox <text>" listing) "with no argument /soniox says how to use it")
      (is (search "Adrian" listing) "and names the voices")
      (is (search "mp3" listing) "and the containers"))
    (let ((asked '()))
      (with-sz-endpoint (asked)
        (let ((answer (nodecode-soniox::run-command "good morning" "s1")))
          (is (search "saved " answer) "/soniox speaks the line into a file")
          (is (probe-file (sz-path answer)) "that is there")
          (is (equal "good morning" (nlk:json-value (nlk:decode-json (fourth (first asked))) :text "text")) "with the line as the text that was sent"))))))
