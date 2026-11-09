;;;; cell-test.lisp --- the call, the file it writes, and every refusal, with the
;;;; runtime, the player and the PATH stubbed.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here executes anything: *RUN* is a stub that records the command
;;;; line and writes handcrafted bytes where the runtime would, *LAUNCH* records
;;;; the player, and the PATH is a fixture folder of this test's own holding an
;;;; empty file where a program would be. What is pinned is what a user gets --
;;;; a voice value resolved the way the plugin resolved it, a text file holding
;;;; the words, a WAV found when ffmpeg fell back, the answer naming it -- and
;;;; what a refused call never does: run anything.

(in-package #:nodecode.test)

(define-test-slice "omnivoice" "OMNIVOICE-" :start nodecode-omnivoice:start-cell)

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "omnivoice"
  (:config "command" "omnivoice"
           "directory" (namestring (nlk:home "omnivoice-cell-test/")))
  (:help :omnivoice)
  (:command "say")
  (:running (is (equal "omnivoice" (nodecode-omnivoice::setting :command)) "the section's own command")
            (is (equal 1.0d0 (nodecode-omnivoice::setting :speed)) "the speed parsed as a number")
            (let ((entry (nle::find-tool "omnivoice_say")))
              (is-present entry "the tool is registered under the plugin's own name"
                (is (equal "omnivoice_say" (first entry))))))
  (:stopped (is (null (nle::find-tool "omnivoice_say")) "stop takes the tool back out"))
  (:refused ("format" "mp3x")
            ("speed" "brisk")
            ("guidance_scale" "high"))
  (:idle nodecode-omnivoice:omnivoice-error (nodecode-omnivoice::speak "hello there")))

;;; --- fixtures ---------------------------------------------------------------------

(defun ov-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "omnivoice-~12,'0x/" (random (expt 16 12))) (nlk:home)))

(defun ov-start (&rest pairs)
  "The cell started on PAIRS, in a folder of this test's own unless PAIRS names one."
  (apply #'omnivoice-start
         (append (unless (member "directory" pairs :test #'string=)
                   (list "directory" (namestring (ov-directory))))
                 (copy-list pairs))))

(defmacro with-omnivoice ((&body pairs) &body body)
  "BODY with the cell started on PAIRS (a flat key/value list, or one list of
pairs); its speech folder goes with it."
  (let ((flat (if (every #'consp pairs) (loop for pair in pairs append pair) pairs)))
    `(let* ((started (ov-start ,@flat))
            (directory (nodecode-omnivoice::setting :directory)))
       (nlk:with-cleanup ((progn (funcall started)
                                 (uiop:delete-directory-tree directory :validate t
                                                              :if-does-not-exist :ignore)))
         ,@body))))

(defun ov-bin ()
  "A fixture folder holding an empty file where the runtime's command would be.
It is only looked for -- nothing here executes it."
  (let ((folder (merge-pathnames (format nil "omnivoice-bin-~12,'0x/" (random (expt 16 12)))
                                  (nlk:home))))
    (ensure-directories-exist (merge-pathnames "omnivoice-hermes-tts" folder))
    (dolist (program '("omnivoice-hermes-tts" "ffplay"))
      (with-open-file (out (merge-pathnames program folder)
                            :direction :output :if-exists :supersede)
        (write-string "" out)))
    (namestring folder)))

(defmacro with-omnivoice-path ((&optional bin-form) &body body)
  "BODY with PATH answering BIN-FORM (or a fixture folder), and everything else
read from the real environment."
  (let ((bin (gensym "BIN")))
    `(let* ((,bin (or ,bin-form (ov-bin)))
            (nodecode-omnivoice::*getenv*
              (lambda (name)
                (if (string= name "PATH") ,bin (uiop:getenv name)))))
       ,@body)))

(defun ov-tool (&rest pairs)
  "The tool as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool "omnivoice_say")))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun ov-arg (arguments name)
  "ARGUMENTS' value for the flag NAME."
  (second (member name arguments :test #'string=)))

(defmacro with-ov-run ((calls &key (writer 'nil)) &body body)
  "BODY with *RUN* stubbed: CALLS collects (PROGRAM ARGUMENTS ENVIRONMENT)
oldest first, and WRITER is called with the arguments and answers (values TEXT
STATUS); nil, it writes one second of silence where --output says."
  `(let ((,calls '()))
     (let ((nodecode-omnivoice::*run*
             (lambda (program arguments &key environment)
               (push (list program arguments environment) ,calls)
               (if ,writer
                   (funcall ,writer arguments)
                   (progn (ov-write-wav (ov-arg arguments "--output") 44100)
                          (values "" 0))))))
       ,@body)))

(defun ov-call (calls &optional (index 0))
  "The INDEXth call CALLS recorded, as (values PROGRAM ARGUMENTS ENVIRONMENT)."
  (let ((call (nth index (reverse calls))))
    (values (first call) (second call) (third call))))

;;; --- handcrafted audio -------------------------------------------------------------

(defun ov-wav (bytes &key (rate 44100))
  "A canonical RIFF/WAVE file of BYTES of silence."
  (let ((out (make-array (+ 44 bytes) :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((text (at string) (replace out (map '(vector (unsigned-byte 8)) #'char-code string) :start1 at))
           (put (at number size)
             (loop for index below size
                   do (setf (aref out (+ at index)) (ldb (byte 8 (* 8 index)) number)))))
      (text 0 "RIFF") (put 4 (+ 36 bytes) 4) (text 8 "WAVE")
      (text 12 "fmt ") (put 16 16 4) (put 20 1 2) (put 22 1 2) (put 24 rate 4)
      (put 28 (* rate 2) 4) (put 32 2 2) (put 34 16 2)
      (text 36 "data") (put 40 bytes 4))
    out))

(defun ov-write-wav (path bytes)
  "OV-WAV's bytes at PATH, wherever the runtime said it would write."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
    (write-sequence (ov-wav bytes) out))
  path)

(defun ov-first-line (answer)
  "The file the tool's answer names on its first line."
  (subseq answer 6 (or (position #\Newline answer) (length answer))))

;;; --- a voice value is read the way the plugin read it -------------------------------

(deftest omnivoice-a-voice-value-resolves-to-a-mode-and-its-prompt-arguments ()
  (multiple-value-bind (mode instruct ref)
      (nodecode-omnivoice::resolve-voice "auto" "older male" nil nil)
    (is (eq :auto mode) "auto lets the model pick")
    (is (null instruct) "and clears a described voice")
    (is (null ref)))
  (multiple-value-bind (mode instruct)
      (nodecode-omnivoice::resolve-voice "instruct:female, low pitch, british accent" nil nil nil)
    (is (eq :instruct mode) "instruct: designs a voice")
    (is (equal "female, low pitch, british accent" instruct) "with the words after the colon"))
  (multiple-value-bind (mode instruct ref)
      (nodecode-omnivoice::resolve-voice "female, whisper" "older male" nil "a transcript")
    (is (eq :instruct mode) "a bare description is a design instruction")
    (is (equal "female, whisper" instruct) "the description itself"))
  (multiple-value-bind (mode instruct ref text)
      (nodecode-omnivoice::resolve-voice "ref:~/clips/voice.wav" "older male" nil "a transcript")
    (is (eq :ref mode) "ref: clones from a clip")
    (is (uiop:string-suffix-p ref "clips/voice.wav") "with the clip's path, ~ resolved")
    (is (equal "a transcript" text) "and the transcript kept"))
  (let ((clip (merge-pathnames "clip.wav" (ov-directory))))
    (ensure-directories-exist clip)
    (ov-write-wav clip 1000)
    (multiple-value-bind (mode instruct ref)
        (nodecode-omnivoice::resolve-voice (namestring clip) nil nil nil)
      (declare (ignore instruct))
      (is (eq :ref mode) "a file that is there is a clip to clone")
      (is (equal (namestring clip) ref))))
  (multiple-value-bind (mode instruct)
      (nodecode-omnivoice::resolve-voice nil "older male" nil nil)
    (is (eq :instruct mode) "no voice value falls back on the section's")
    (is (equal "older male" instruct))))

;;; --- the call ----------------------------------------------------------------------

(deftest omnivoice-a-text-becomes-a-file-the-runtime-wrote ()
  (with-omnivoice (("command" "omnivoice-hermes-tts") ("language" "th") ("format" "wav"))
    (with-omnivoice-path ()
      (let ((calls '()) (launched '()) (read-back nil) (answer nil))
        (with-ov-run (calls :writer (lambda (arguments)
                                     (setf read-back
                                           (uiop:read-file-string (ov-arg arguments "--text-file")))
                                     (ov-write-wav (ov-arg arguments "--output") 88200)
                                     (values "" 0)))
          (let ((nodecode-omnivoice::*launch*
                  (lambda (program arguments)
                    (push (cons program arguments) launched))))
            (progn (setf answer (ov-tool "text" "Hello from OmniVoice."
                                          "voice" "instruct:female, british accent"
                                          "play" t))
              (is (search "wrote " answer) "the answer names the file it wrote")
              (is (probe-file (ov-first-line answer)) "and the file is there")
              (is (search "1.00 s" answer) "with the length read out of the WAV header")
              (is (search "designed: female, british accent" answer) "and the voice it resolved to"))
            (multiple-value-bind (program arguments environment) (ov-call calls)
              (is (uiop:string-suffix-p program "omnivoice-hermes-tts") "the runtime's command ran")
              (is (equal "--text-file" (first arguments)))
              (is (equal "instruct:female, british accent" (ov-arg arguments "--voice")) "the resolved voice rides the plugin's own flag")
              (is (equal "wav" (ov-arg arguments "--format")))
              (is (equal "k2-fsa/OmniVoice" (ov-arg arguments "--model")))
              (is (equal "1.0000" (ov-arg arguments "--speed")) "the section's speed, as the flag carries it")
              (is (member "OMNIVOICE_LANGUAGE=th" environment :test #'string=) "the language is the runtime's own variable")
              (is (member "OMNIVOICE_MODEL=k2-fsa/OmniVoice" environment :test #'string=))
              (is (equal "Hello from OmniVoice." read-back) "the text file holds the words")
              (is (null (probe-file (ov-arg arguments "--text-file"))) "and is gone once the call returns"))
            (is (= 1 (length launched)) "one player was started")
            (is (equal (ov-first-line answer) (car (last (first launched)))) "on the file the runtime wrote")))))))

(deftest omnivoice-a-wav-stands-in-for-an-mp3-without-ffmpeg ()
  (with-omnivoice (("command" "omnivoice-hermes-tts") ("format" "mp3"))
    (with-omnivoice-path ()
      (let ((calls '()))
        (with-ov-run (calls :writer (lambda (arguments)
                                     ;; The plugin asks soundfile for mp3 and falls back to WAV.
                                     (ov-write-wav (make-pathname :type "wav"
                                                                   :defaults (ov-arg arguments "--output"))
                                                   44100)
                                     (values "" 0)))
          (let ((answer (ov-tool "text" "A sentence worth hearing." "format" "mp3" "play" nil)))
            (is (search ".wav" answer) "the answer names the wav that stands in")
            (is (uiop:string-suffix-p (ov-first-line answer) ".wav"))
            (is (search "not played" answer) "and says it was not played")
            (is (equal "mp3" (ov-arg (second (first calls)) "--format")) "though mp3 is what the runtime was asked for")))))))

(deftest omnivoice-a-missing-runtime-refuses-and-runs-nothing ()
  (with-omnivoice (("command" "omnivoice-not-installed-here"))
    (let ((empty (merge-pathnames (format nil "omnivoice-empty-~12,'0x/" (random (expt 16 12)))
                                   (nlk:home))))
      (ensure-directories-exist (merge-pathnames "keep" empty))
      (with-omnivoice-path ((namestring empty))
        (let ((calls '()))
          (with-ov-run (calls)
            (let ((answer (ov-tool "text" "hello")))
              (is (search "ERROR:" answer) "the call answers a failure")
              (is (search "omnivoice-not-installed-here" answer) "naming the command it looked for")
              (is (search "command" answer) "and the setting to fix")
              (is (search "Nothing was run" answer)))
            (is (null calls) "and nothing was executed")))))))

(deftest omnivoice-a-failed-run-reports-what-the-runtime-said ()
  (with-omnivoice (("command" "omnivoice-hermes-tts"))
    (with-omnivoice-path ()
      (let ((calls '()))
        (with-ov-run (calls :writer (lambda (arguments)
                                     (declare (ignore arguments))
                                     (values "torch.cuda.OutOfMemoryError: CUDA out of memory" 1)))
          (let ((answer (ov-tool "text" "hello" "play" nil)))
            (is (search "ERROR:" answer))
            (is (search "exited 1" answer) "the exit status is named")
            (is (search "CUDA out of memory" answer) "with the runtime's own last words")))
        (with-ov-run (calls :writer (lambda (arguments)
                                     (declare (ignore arguments))
                                     (values "" 0)))
          (let ((answer (ov-tool "text" "hello" "play" nil)))
            (is (search "wrote no audio" answer) "a run that wrote nothing is refused")))))))

;;; --- /say ---------------------------------------------------------------------------

(deftest omnivoice-the-slash-command-speaks-and-shows-what-it-runs-on ()
  (with-omnivoice (("command" "omnivoice-hermes-tts"))
    (with-omnivoice-path ()
      (let ((calls '()))
        (let ((nodecode-omnivoice::*launch* (lambda (program arguments)
                                               (declare (ignore program arguments)) nil)))
          (with-ov-run (calls)
            (let ((answer (nodecode-omnivoice::run-slash "Speak this, please." "s1")))
              (is (search "wrote " answer) "/say <text> speaks it")
              (is (probe-file (ov-first-line answer)) "and the file is there"))
            (let ((listing (nodecode-omnivoice::run-slash "" "s1")))
              (is (search "OmniVoice speech on this machine" listing) "/say alone shows the section")
              (is (search "voice auto" listing) "with the voice it would use")
              (is (search "speech_" listing) "and the files it made"))))))))
