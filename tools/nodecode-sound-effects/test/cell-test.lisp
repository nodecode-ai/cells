;;;; cell-test.lisp --- the billed call, the saved file, the library and every
;;;; refusal, with the endpoint stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials: NLE::HTTP is a stub that records the call and answers
;;;; handcrafted bytes, the environment is a stub, and every sound is written
;;;; into a folder of this test's own under the scratch home. What is pinned is
;;;; what a user gets -- a description becomes a real file with its own length
;;;; in the answer, a line in the library, /sfx listing it -- and what a refused
;;;; call never costs: a missing key, a length in the prompt, an out-of-range
;;;; number and a non-audio 200 all answer before, or instead of, a second
;;;; request.

(in-package #:nodecode.test)

(define-test-slice "sound-effects" "SOUND-EFFECTS-" :start nodecode-sound-effects:start-cell)

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "sound-effects"
  (:config "api_key" "test-key"
           "directory" (namestring (nlk:home "sfx-cell-test/")))
  (:help :sfx)
  (:command "sfx")
  (:running (is (equal "test-key" (nodecode-sound-effects::setting :api-key)) "the section's own key")
            (is (equal "mp3_44100_128" (nodecode-sound-effects::setting :output-format)) "mp3 by default")
            (let ((entry (nle::find-tool "elevenlabs_sound_effect")))
              (is-present entry "the tool is registered under the plugin's name"
                (is (equal "elevenlabs_sound_effect" (first entry))))))
  (:stopped (is (null (nle::find-tool "elevenlabs_sound_effect")) "stop takes the tool back out"))
  (:refused ("api_base_url" "ftp://api.elevenlabs.io")
            ("api_base_url" "http://api.elevenlabs.io"))
  (:idle nodecode-sound-effects:sound-effects-error (nodecode-sound-effects::library-text)))

;;; --- fixtures ---------------------------------------------------------------------

(defun sfx-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "sfx-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defun sfx-start (&rest pairs)
  "The cell started on PAIRS, in a fresh folder unless PAIRS names one."
  (apply #'sound-effects-start
         (append (unless (member "directory" pairs :test #'string=)
                   (list "directory" (namestring (sfx-directory))))
                 (copy-list pairs))))

(defmacro with-sounds ((&body pairs) &body body)
  "BODY with the cell started on PAIRS, the section's members as a flat
key/value list (or one list of them); its folder and its settings go after."
  (let ((flat (if (every #'consp pairs)
                  (loop for pair in pairs append pair)
                  pairs)))
    `(let* ((started (sfx-start ,@flat))
          (directory (nodecode-sound-effects::setting :directory)))
     (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t
                                                            :if-does-not-exist :ignore)))
       ,@body))))

(defun sfx-tool (&rest pairs)
  "The tool as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool "elevenlabs_sound_effect")))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-sfx-endpoint ((asked &key (answer '(lambda (url headers content)
                                                    (declare (ignore url headers content))
                                                    (values (sfx-mp3) 200 nil))))
                             &body body)
  "BODY with the endpoint stubbed: ASKED collects (URL HEADERS BODY) oldest
first, and ANSWER answers (values BODY STATUS HEADERS)."
  `(let ((,asked '()))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content &allow-other-keys)
                                (push (list method url headers content) ,asked)
                                (funcall ,answer url headers content))
       ,@body)))

(defun sfx-call (asked &optional (index 0))
  "The INDEXth call ASKED recorded, as (values METHOD URL HEADERS BODY)."
  (let ((call (nth index (reverse asked))))
    (values (first call) (second call) (third call) (fourth call))))

(defun sfx-json (headers name)
  (cdr (assoc name headers :test #'string-equal)))

;;; --- handcrafted containers --------------------------------------------------------

(defun sfx-mp3 (&optional (frames 10))
  "FRAMES MPEG-1 Layer III frames at 128 kbps and 44.1 kHz, 417 bytes each."
  (let ((frame (make-array 417 :element-type '(unsigned-byte 8) :initial-element 0))
        (out (make-array (* frames 417) :element-type '(unsigned-byte 8))))
    (setf (aref frame 0) #xFF (aref frame 1) #xFB (aref frame 2) #x90 (aref frame 3) #x00)
    (dotimes (index frames) (replace out frame :start1 (* index 417)))
    out))

(defun sfx-wav (data-bytes &key (rate 44100) (channels 1))
  "A canonical RIFF/WAVE header over DATA-BYTES of silence."
  (let* ((out (make-array (+ 44 data-bytes) :element-type '(unsigned-byte 8) :initial-element 0))
         (byte-rate (* rate channels 2)))
    (flet ((text (at string) (replace out (nodecode-sound-effects::ascii string) :start1 at))
           (put (at n bytes)
             (loop for i below bytes do (setf (aref out (+ at i)) (ldb (byte 8 (* 8 i)) n)))))
      (text 0 "RIFF") (put 4 (- (+ 44 data-bytes) 8) 4) (text 8 "WAVE")
      (text 12 "fmt ") (put 16 16 4) (put 20 1 2) (put 22 channels 2) (put 24 rate 4)
      (put 28 byte-rate 4) (put 32 (* channels 2) 2) (put 34 16 2)
      (text 36 "data") (put 40 data-bytes 4))
    out))

(defun sfx-ogg (granule &optional (pre-skip 312))
  "Two Ogg Opus pages: one carrying OpusHead, one whose granule position is GRANULE."
  (flet ((page (granule payload)
           (let ((out (make-array (+ 28 (length payload)) :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
             (replace out (nodecode-sound-effects::ascii "OggS") :start1 0)
             (loop for i below 8 do (setf (aref out (+ 6 i)) (ldb (byte 8 (* 8 i)) granule)))
             (setf (aref out 26) 1 (aref out 27) (length payload))
             (replace out payload :start1 28)
             out)))
    (let ((head (make-array 19 :element-type '(unsigned-byte 8) :initial-element 0)))
      (replace head (nodecode-sound-effects::ascii "OpusHead") :start1 0)
      (setf (aref head 8) 1 (aref head 9) 1)
      (setf (aref head 10) (ldb (byte 8 0) pre-skip) (aref head 11) (ldb (byte 8 8) pre-skip))
      (concatenate '(vector (unsigned-byte 8))
                   (page 0 head)
                   (page granule (make-array 4 :element-type '(unsigned-byte 8)))))))

(defun sfx-answer-path (answer)
  "The file the tool's answer names on its first line."
  (subseq answer 6 (or (position #\Newline answer) (length answer))))

(defun sfx-file-length (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

;;; --- the call --------------------------------------------------------------------

(deftest sound-effects-a-described-sound-becomes-a-file-and-a-library-line ()
  (with-sounds (("api_key" "test-key"))
    (let ((asked '()) (prompt "heavy wooden door creaking open in a stone hall"))
      (with-sfx-endpoint (asked
                          :answer (lambda (url headers content)
                                    (declare (ignore url headers content))
                                    (values (sfx-mp3 10) 200 (list (cons "character-cost" "47")))))
        (let ((answer (sfx-tool "prompt" prompt "duration_seconds" 2 "loop" t)))
          (multiple-value-bind (method url headers body) (sfx-call asked)
            (is (eq :post method) "one POST")
            (is (search "/v1/sound-generation?output_format=mp3_44100_128" url) "to the sound-generation endpoint")
            (is (equal "test-key" (sfx-json headers "xi-api-key")) "with the section's key, and no other place to put it")
            (is (equal "audio/*" (sfx-json headers "accept")))
            (let ((sent (nlk:decode-json body)))
              (is (equal prompt (nlk:json-value sent :text "text")) "the prompt rides as text")
              (is (= 2 (nlk:json-value sent :number "duration_seconds")) "the named length rides as duration_seconds")
              (is (eq t (nlk:json-value sent :boolean "loop")))
              (is (equal "eleven_text_to_sound_v2" (nlk:json-value sent :text "model_id")) "the section's model")))
          (let ((path (sfx-answer-path answer)))
            (is (probe-file path) "the answer names a file that is there")
            (is (= (* 10 417) (sfx-file-length path)) "holding exactly the bytes that came back"))
          (is (search "0.26 s" answer) "the answer's length is the file's, read from its frames")
          (is (search "47 characters billed" answer) "and what ElevenLabs billed")
          (is (search "returned 0.26 s for the 2.00 s asked for" answer) "a length that differs from the request is said")
          (let ((rows (nodecode-sound-effects::records)))
            (is (= 1 (length rows)) "one library line")
            (is (equal prompt (nlk:json-value (first rows) :text "prompt")))
            (is (= (* 10 417) (nlk:json-value (first rows) :integer "bytes"))))
          (let ((listing (nodecode-sound-effects::run-slash "" "s1")))
            (is (search "Sounds: 1" listing) "/sfx counts it")
            (is (search prompt listing) "and shows the prompt")
            (let ((id (nlk:json-value (first (nodecode-sound-effects::records)) :string "id")))
              (is (search id listing) "with the id /sfx takes")))
          (let ((detail (nodecode-sound-effects::run-slash
                         (subseq prompt 0 12) "s1")))
            (is (search prompt detail) "/sfx <words> finds one sound")
            (is (search "file " detail) "and names its file")))))))

;;; --- what is refused before anything is sent -----------------------------------------

(deftest sound-effects-without-a-key-it-refuses-and-sends-nothing ()
  (with-sounds (("api_key" "") ("api_key_env" "SOUND_EFFECTS_TEST_KEY_UNSET"))
    (let ((asked '()))
      (with-sfx-endpoint (asked :answer (lambda (&rest call) (declare (ignore call))
                                       (error "no request should have gone out")))
        (let ((nodecode-sound-effects::*getenv* (lambda (name) (declare (ignore name)) nil)))
          (let ((answer (sfx-tool "prompt" "a short retro coin pickup")))
            (is (search "ERROR:" answer) "the call answers a failure")
            (is (search "SOUND_EFFECTS_TEST_KEY_UNSET" answer) "naming the variable to set")
            (is (search "Nothing was sent" answer)))
          (is (null asked) "and nothing was dialled"))))))

(deftest sound-effects-refuses-a-length-in-the-prompt-and-out-of-range-numbers ()
  (with-sounds (("api_key" "test-key"))
    (let ((asked '()))
      (with-sfx-endpoint (asked)
        (let ((answer (sfx-tool "prompt" "about 2 seconds of rain on a tin roof")))
          (is (search "duration_seconds" answer))
          (is (search "2 seconds" answer) "the length the prompt named is quoted back"))
        (let ((answer (sfx-tool "prompt" "a beep" "duration_seconds" 45)))
          (is (search "0.5" answer))
          (is (search "30" answer) "the range the API takes is named"))
        (let ((answer (sfx-tool "prompt" "a beep" "prompt_influence" 2)))
          (is (search "prompt_influence" answer)))
        (let ((answer (sfx-tool "prompt" "a door creaking open somewhere in a large stone hall")))
          (is (search "saved " answer) "a prompt with no length in it goes through"))
        (is (= 1 (length asked)) "only the call that could be billed was made"))
      (is (signals-error nlk:contract-refusal (sfx-tool)) "a call with no prompt is refused by the declaration"))))

(deftest sound-effects-reports-what-elevenlabs-refused-and-keeps-nothing ()
  (with-sounds (("api_key" "test-key"))
    (let ((asked '()))
      (with-sfx-endpoint (asked
                          :answer (lambda (&rest call) (declare (ignore call))
                                    (values (nodecode-sound-effects::octets-of
                                             "{\"detail\":{\"status\":\"missing_permissions\",\"message\":\"missing permission\"}}")
                                            401 nil)))
        (let ((answer (sfx-tool "prompt" "a coin pickup")))
          (is (= 1 (length asked)) "one call was made")
          (is (search "HTTP 401" answer))
          (is (search "missing permission" answer) "ElevenLabs' own sentence")
          (is (search "Sound Effects" answer) "and the fix its status names")))
      (with-sfx-endpoint (asked
                          :answer (lambda (&rest call) (declare (ignore call))
                                    (values (nodecode-sound-effects::octets-of "{\"detail\":\"no\"}") 200 nil)))
        (let ((answer (sfx-tool "prompt" "a coin pickup")))
          (is (= 1 (length asked)) "one call was made")
          (is (search "not mp3 audio" answer) "a 200 that is not audio is refused, not saved")))
      (is (null (nodecode-sound-effects::records)) "the library holds nothing from either"))))

(deftest sound-effects-reads-the-length-out-of-the-bytes ()
  (let ((wav (nodecode-sound-effects::audio-seconds (sfx-wav 88200) "wav"))
        (frames (nodecode-sound-effects::audio-seconds (sfx-mp3 10) "mp3"))
        (opus (nodecode-sound-effects::audio-seconds (sfx-ogg (+ 48000 312) 312) "opus")))
    (is (< (abs (- 1.0 wav)) 0.001) "a WAV's byte rate and data chunk give its length")
    (is (< (abs (- (/ 11520 44100) frames)) 0.001) "an MP3's frames are counted")
    (is (< (abs (- 1.0 opus)) 0.001) "an Ogg Opus stream is its last granule less the pre-skip"))
  (is (null (nodecode-sound-effects::audio-seconds (nodecode-sound-effects::octets-of "x") "mp3")) "no frames, no length"))
