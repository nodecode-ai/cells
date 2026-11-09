;;;; cell-test.lisp --- the three tools against a stubbed Fish Audio, and the
;;;; refusals that cost nothing.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here dials: NLK:HTTP is a stub that records the call and answers
;;;; handcrafted bytes, the environment is a stub, and every file is written
;;;; into a folder of this test's own under the scratch home. What is pinned is
;;;; what a user gets -- a text becomes a real file with its path in the
;;;; answer, a voice search is a table of ids, a local audio file is uploaded
;;;; -- and what a refused call never costs: no key, no consent, nothing sent.

(in-package #:nodecode.test)

(define-test-slice "fish-audio" "FISH-AUDIO-" :start nodecode-fish-audio:start-cell)

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "fish-audio"
  (:config "api_key" "test-key"
           "directory" (namestring (nlk:home "fish-cell-test/")))
  (:help :fish)
  (:command "fish")
  (:running (is (equal "test-key" (nodecode-fish-audio::setting :api-key)) "the section's own key")
            (is (equal "mp3" (nodecode-fish-audio::setting :output-format)) "mp3 by default")
            (is (equal "https://api.fish.audio" (nodecode-fish-audio::setting :api-base-url)) "Fish's own endpoint")
            (let ((entry (nle::find-tool "fish_speak")))
              (is-present entry "the first tool is registered under the plugin's name"
                (is (equal "fish_speak" (first entry)))))
            (is-present (nle::find-tool "fish_voices") "the voice library is reachable")
            (is-present (nle::find-tool "fish_transcribe") "and dictation"))
  (:stopped (is (null (nle::find-tool "fish_speak")) "stop takes the tools back out")
            (is (null (nle::find-tool "fish_transcribe"))))
  (:refused ("api_base_url" "ftp://api.fish.audio")
            ("api_base_url" "http://api.fish.audio"))
  (:idle nodecode-fish-audio:fish-audio-error (nodecode-fish-audio::status-text)))

;;; --- fixtures ---------------------------------------------------------------------

(defun fish-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "fish-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defmacro with-fish ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS in a folder of its own, the section's
members as a flat key/value list; the folder goes when BODY does."
  `(let* ((started (fish-audio-start "directory" (namestring (fish-directory)) ,@(copy-list pairs)))
          (directory (nodecode-fish-audio::setting :directory)))
     (nlk:with-cleanup ((progn (funcall started)
                                (uiop:delete-directory-tree directory :validate t
                                                             :if-does-not-exist :ignore)))
       ,@body)))

(defun fish-tool (name &rest pairs)
  "The tool as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-fish-endpoint ((asked &key (answer '(lambda (&rest call)
                                                    (declare (ignore call))
                                                    (values "{}" 200 nil))))
                             &body body)
  "BODY with Fish Audio stubbed: ASKED collects (METHOD URL HEADERS BODY) oldest
first, and ANSWER answers (values BODY STATUS HEADERS)."
  `(let ((,asked '()))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content &allow-other-keys)
                                (push (list method url headers content) ,asked)
                                (funcall ,answer method url headers content))
       ,@body)))

(defun fish-call (asked &optional (index 0))
  "The INDEXth call ASKED recorded, as (values METHOD URL HEADERS BODY)."
  (let ((call (nth index (reverse asked))))
    (values (first call) (second call) (third call) (fourth call))))

(defun fish-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun fish-mp3 (&optional (frames 4))
  "FRAMES MPEG-1 Layer III frames at 128 kbps, 417 bytes each: enough for the
container check, which reads the first byte."
  (let ((frame (make-array 417 :element-type '(unsigned-byte 8) :initial-element 0))
        (out (make-array (* frames 417) :element-type '(unsigned-byte 8))))
    (setf (aref frame 0) #xFF (aref frame 1) #xFB (aref frame 2) #x90 (aref frame 3) #x00)
    (dotimes (index frames) (replace out frame :start1 (* index 417)))
    out))

(defun fish-mp3-file (&rest frames)
  "A real .mp3 file in a folder of this test's own, as a pathname."
  (let ((path (merge-pathnames (format nil "upload-~12,'0x.mp3" (random (expt 16 10)))
                                (nlk:home "fish-uploads/"))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
      (write-sequence (apply #'fish-mp3 frames) out))
    path))

(defun fish-answer-path (answer)
  "The file the tool's answer names on its first line."
  (subseq answer 6 (or (position #\Newline answer) (length answer))))

(defun fish-json-text (&rest pairs)
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

;;; --- text becomes a file ---------------------------------------------------------

(deftest fish-audio-a-text-becomes-a-saved-audio-file ()
  (with-fish ("api_key" "test-key")
    (let ((asked '()))
      (with-fish-endpoint (asked :answer (lambda (&rest call)
                                           (declare (ignore call))
                                           (values (fish-mp3 4) 200 nil)))
        (let ((answer (fish-tool "fish_speak" "text" "hello [excited] there"
                                               "voice" "abc12345def6")))
          (multiple-value-bind (method url headers body) (fish-call asked)
            (is (eq :post method) "one POST")
            (is (string= "https://api.fish.audio/v1/tts" url) "to the synthesis endpoint")
            (is (equal "Bearer test-key" (fish-header headers "authorization")) "with the section's key")
            (is (equal "s2.1-pro" (fish-header headers "model")) "and the section's model, in Fish's model header")
            (is (equal "application/json" (fish-header headers "content-type")))
            (let ((sent (nlk:decode-json body)))
              (is (equal "hello [excited] there" (nlk:json-value sent :text "text")) "the text rides as text")
              (is (equal "mp3" (nlk:json-value sent :text "format")) "the section's format")
              (is (equal "abc12345def6" (nlk:json-value sent :text "reference_id")) "the named voice")
              (is (= 1.0 (nlk:json-value sent :number "prosody" "speed")) "speed 1.0 unless sent")))
          (let ((path (fish-answer-path answer)))
            (is (probe-file path) "the answer names a file that is there")
            (is (= (* 4 417) (with-open-file (in path :element-type '(unsigned-byte 8))
                                (file-length in)))
                "holding exactly the bytes that came back")
            (is (search "s2.1-pro" answer) "the answer names the model")
            (is (search "voice abc12345def6" answer) "and the voice, so the operator can make another")))
        (is (= 1 (length asked)) "one billed call")))))

(deftest fish-audio-a-multipart-answer-is-not-saved-and-a-200-that-is-not-audio-is-refused ()
  (with-fish ("api_key" "test-key")
    (let ((asked '()))
      (with-fish-endpoint (asked :answer (lambda (&rest call)
                                           (declare (ignore call))
                                           (values "{\"message\":\"bad key\"}" 401 nil)))
        (let ((answer (fish-tool "fish_speak" "text" "hello")))
          (is (search "HTTP 401" answer))
          (is (search "https://fish.audio/app/api-keys" answer) "the fix the status names")
          (is (= 1 (length asked)) "one call was made")))
      (with-fish-endpoint (asked :answer (lambda (&rest call)
                                           (declare (ignore call))
                                           (values "{\"text\":\"not audio\"}" 200 nil)))
        (let ((answer (fish-tool "fish_speak" "text" "hello")))
          (is (search "not mp3 audio" answer) "a 200 carrying JSON is refused, not saved")
          (is (= 1 (length asked)) "one call was made")))
      (is (null (uiop:directory-files (nodecode-fish-audio::setting :directory)))
          "the folder holds nothing"))))

;;; --- the voice library -------------------------------------------------------------

(deftest fish-audio-voices-search-lists-ids-and-get-reads-one ()
  (with-fish ("api_key" "test-key")
    (let ((asked '())
          (listing (fish-json-text
                    "items" (vector (nlk:make-json-object
                                     "_id" "abc12345def6" "title" "Rain on a tin roof"
                                     "languages" (vector "en") "like_count" 12 "task_count" 40
                                     "author" (nlk:make-json-object "nickname" "someone"))
                                    (nlk:make-json-object
                                     "_id" "fedc54321abc" "title" "Quiet narrator"
                                     "languages" (vector "en" "es") "like_count" 3 "task_count" 5))
                    "total" 2 "has_more" nil)))
      (with-fish-endpoint (asked :answer (lambda (&rest call)
                                           (declare (ignore call))
                                           (values listing 200 nil)))
        (let ((answer (fish-tool "fish_voices" "action" "search" "query" "rain")))
          (multiple-value-bind (method url headers body) (fish-call asked)
            (declare (ignore headers body))
            (is (eq :get method) "a search is a GET")
            (is (search "/model?page_number=1&page_size=20&sort_by=score&title=rain" url)
                "with the query Fish Audio reads, and no key in the URL"))
          (is (search "abc12345def6" answer) "the answer names the id a call takes")
          (is (search "Rain on a tin roof" answer) "and the title")
          (is (search "2 total" answer) "and the total")))
      (with-fish-endpoint (asked :answer (lambda (&rest call)
                                           (declare (ignore call))
                                           (values (fish-json-text
                                                    "_id" "abc12345def6" "title" "Rain on a tin roof"
                                                    "description" "steady rain" "visibility" "public"
                                                    "languages" (vector "en"))
                                           200 nil)))
        (let ((answer (fish-tool "fish_voices" "action" "get" "voice_id" "abc12345def6")))
          (multiple-value-bind (method url headers body) (fish-call asked)
            (declare (ignore headers body))
            (is (eq :get method))
            (is (string= "https://api.fish.audio/model/abc12345def6" url) "one voice by id"))
          (is (search "steady rain" answer) "the description is shown")
          (is (search "fish_speak with voice abc12345def6" answer) "and how to use it"))))))

(deftest fish-audio-a-clone-needs-consent-and-uploads-the-samples ()
  (with-fish ("api_key" "test-key")
    (let ((asked '())
          (sample (fish-mp3-file 2)))
      (nlk:with-cleanup ((uiop:delete-file-if-exists sample))
        (with-fish-endpoint (asked)
          (let ((answer (fish-tool "fish_voices" "action" "clone" "title" "My voice"
                                                "sample_paths" (vector (namestring sample)))))
            (is (search "consent" answer) "without consent it refuses")
            (is (null asked) "and sends nothing"))
          (let ((answer (fish-tool "fish_voices" "action" "clone" "title" "My voice"
                                                "consent" t
                                                "sample_paths" (vector (namestring sample)))))
            (multiple-value-bind (method url headers body) (fish-call asked)
              (declare (ignore headers))
              (is (eq :post method) "a clone is a POST")
              (is (string= "https://api.fish.audio/model" url) "to the model collection")
              (is (equal "My voice" (cdr (assoc "title" body :test #'equal))))
              (is (equal "private" (cdr (assoc "visibility" body :test #'equal))) "private unless asked otherwise")
              (is (pathnamep (cdr (assoc "voices" body :test #'equal))) "the sample rides as a file part"))
            (is (search "cloned voice" answer) "the answer names the new voice")))))))

;;; --- dictation ---------------------------------------------------------------------

(deftest fish-audio-transcribe-uploads-one-file-and-answers-its-text ()
  (with-fish ("api_key" "test-key")
    (let ((asked '())
          (sample (fish-mp3-file 2)))
      (nlk:with-cleanup ((uiop:delete-file-if-exists sample))
        (let ((answer-for (lambda (&rest call)
                             (declare (ignore call))
                             (values (fish-json-text
                                      "text" "hello there" "duration" 2.5 "language_code" "en"
                                      "segments" (vector (nlk:make-json-object
                                                          "text" "hello" "start" 0 "end" 1.25)
                                                         (nlk:make-json-object
                                                          "text" "there" "start" 1.25 "end" 2.5)))
                                     200 nil))))
          (with-fish-endpoint (asked :answer answer-for)
            (let ((answer (fish-tool "fish_transcribe" "file_path" (namestring sample))))
              (multiple-value-bind (method url headers body) (fish-call asked)
                (declare (ignore headers))
                (is (eq :post method) "a transcription is a POST")
                (is (string= "https://api.fish.audio/v1/asr" url) "to the ASR endpoint")
                (is (equal "transcribe-1-pro" (fish-header headers "model")) "the section's model")
                (is (pathnamep (cdr (assoc "audio" body :test #'equal))) "the file rides as the audio part")
                (is (equal "false" (cdr (assoc "ignore_timestamps" body :test #'equal)))
                    "timestamps are wanted by default")
                (is (equal "auto" (cdr (assoc "diarize" body :test #'equal)))))
              (is (search "hello there" answer) "the text is the answer")
              (is (search "2.5" answer) "with the length Fish Audio measured")))
          (with-fish-endpoint (asked :answer answer-for)
            (let ((answer (fish-tool "fish_transcribe" "file_path" (namestring sample) "srt" t)))
              (is (search "subtitles: " answer) "an SRT is written when it is asked for")
              (let* ((from (+ 11 (search "subtitles: " answer)))
                     (path (subseq answer from (or (position #\Newline answer :start from)
                                                   (length answer)))))
                (is (probe-file path) "the file is there")
                (let ((text (uiop:read-file-string path)))
                  (is (search "00:00:00,000 --> 00:00:01,250" text) "with Fish's cue times")
                  (is (search "hello" text)))))))))))

(deftest fish-audio-a-missing-file-and-an-unknown-kind-are-refused-before-the-upload ()
  (with-fish ("api_key" "test-key")
    (let ((asked '()))
      (with-fish-endpoint (asked :answer (lambda (&rest call)
                                           (declare (ignore call))
                                           (error "no request should have gone out")))
        (let ((answer (fish-tool "fish_transcribe" "file_path" "/nowhere/at/all.mp3")))
          (is (search "file_path" answer))
          (is (search "no readable file" answer)))
        (let ((notes (nlk:home "fish-notes.txt")))
          (ensure-directories-exist notes)
          (with-open-file (out notes :direction :output :if-exists :supersede) (write-line "notes" out))
          (nlk:with-cleanup ((uiop:delete-file-if-exists notes))
            (let ((answer (fish-tool "fish_transcribe" "file_path" (namestring notes))))
              (is (search "transcribes" answer) "a kind Fish Audio does not read is named"))))
        (is (null asked) "and nothing was dialled")))))

;;; --- what a cell without a key answers ---------------------------------------------

(deftest fish-audio-without-a-key-refuses-and-sends-nothing ()
  (with-fish ("api_key" "" "api_key_env" "FISH_AUDIO_TEST_KEY_UNSET")
    (let ((asked '()))
      (with-fish-endpoint (asked :answer (lambda (&rest call)
                                           (declare (ignore call))
                                           (error "no request should have gone out")))
        (let ((nodecode-fish-audio::*getenv* (lambda (name) (declare (ignore name)) nil)))
          (let ((answer (fish-tool "fish_speak" "text" "hello")))
            (is (search "ERROR:" answer) "the call answers a failure")
            (is (search "FISH_AUDIO_TEST_KEY_UNSET" answer) "naming the variable to set")
            (is (search "Nothing was sent" answer)))
          (is (null asked) "and nothing was dialled"))))))
