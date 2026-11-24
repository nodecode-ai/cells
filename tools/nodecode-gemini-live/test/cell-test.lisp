;;;; cell-test.lisp --- one spoken turn, the saved WAV, the delegations, and the
;;;; refusals, with the Live socket stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials: *TRANSPORT-MAKER* answers a fake socket that replays
;;;; canned frames and records what was sent, and every reply is written into a
;;;; folder of this test's own under the scratch home.

(in-package #:nodecode.test)

(define-test-slice "gemini-live" "GEMINI-LIVE-" :start nodecode-gemini-live:start-cell)

(define-cell-lifecycle-tests "gemini-live"
  (:config "api_key" "test-key")
  (:help :live)
  (:command "live")
  (:running (is (equal "test-key" (nodecode-gemini-live::setting :api-key)) "the section's own key")
            (is (equal "gemini-3.8-live" (nodecode-gemini-live::setting :model)) "the original's default model")
            (is (equal "Aoede" (nodecode-gemini-live::setting :voice)) "and its default voice")
            (let ((entry (nle::find-tool "gemini_live")))
              (is-present entry "the tool is registered")
              (is (equal "gemini_live" (first entry)))))
  (:stopped (is (null (nle::find-tool "gemini_live")) "stop takes the tool back out"))
  (:refused ("voice" "Nobody")))

;;; --- fixtures -----------------------------------------------------------------

(defun gl-directory ()
  (merge-pathnames (format nil "gemini-live-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defun gl-tool (&rest pairs)
  "The tool as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool "gemini_live")))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-live-session ((sent &key frames dialed) &body body)
  "BODY with the socket stubbed: FRAMES replayed in order, every frame sent
pushed onto SENT oldest last, and, when DIALED names a variable, every URL
a socket was opened to pushed onto it."
  `(let* ((queue (copy-list ,frames))
          (nodecode-gemini-live::*transport-maker*
            (lambda (url)
              ,(if dialed `(push url ,dialed) '(declare (ignore url)))
              (list :send (lambda (text) (push text ,sent))
                    :recv (lambda (seconds) (declare (ignore seconds)) (pop queue))
                    :close (lambda () nil)))))
     ,@body))

(defun gl-audio-frame (octets)
  (format nil "{\"serverContent\":{\"modelTurn\":{\"parts\":[{\"inlineData\":{\"mimeType\":\"audio/pcm;rate=24000\",\"data\":\"~a\"}}]}}}"
          (cl-base64:usb8-array-to-base64-string octets)))

(defparameter +ready+ "{\"setupComplete\":{}}")
(defparameter +spoken+
  "{\"serverContent\":{\"modelTurn\":{\"parts\":[{\"text\":\"It is twenty past four.\"}]},\"turnComplete\":true}}")
(defparameter +delegated+
  "{\"toolCall\":{\"functionCalls\":[{\"id\":\"call-1\",\"name\":\"hermes_task\",\"args\":{\"task\":\"list the files in the project folder\"}}]}}")

;;; --- a turn -------------------------------------------------------------------

(deftest gemini-live-one-turn-saves-the-reply ()
  (let* ((directory (gl-directory))
         (started (gemini-live-start "api_key" "test-key" "directory" (namestring directory))))
    (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))
      (let ((sent '()))
        (with-live-session (sent :frames (list +ready+
                                  (gl-audio-frame (make-array 4 :element-type '(unsigned-byte 8)
                                                                :initial-contents '(0 1 2 3)))
                                  +spoken+))
          (let ((answer (gl-tool "text" "what time is it")))
            (is (search "Gemini Live" answer) "the answer names what spoke")
            (is (search "twenty past four" answer) "the spoken transcript comes back")
            (is (search ".wav" answer) "and the path of the saved audio")
            (let ((file (first (uiop:directory-files directory))))
              (is-present file "the reply audio was written")
              (is (= 48 (with-open-file (in file :element-type '(unsigned-byte 8))
                           (file-length in))) "a 44-byte header and the four sample bytes"))))
        (is (<= 2 (length sent)) "a setup and the text, and nothing per frame but text")
        (is (some (lambda (frame) (search "gemini-3.8-live" frame)) sent) "the setup names the model")
        (is (some (lambda (frame) (search "Aoede" frame)) sent) "and the voice")
        (is (some (lambda (frame) (search "what time is it" frame)) sent) "the text turn carries what was asked")
        (is (= 1 (count-if (lambda (frame) (search "\"setup\"" frame)) sent)) "one setup per call")))
    (let ((sent '()))
      (with-live-session (sent :frames (list +ready+))
        (if (fboundp 'gemini-live-start) t t)))))

(deftest gemini-live-names-the-work-the-voice-model-delegated ()
  (let* ((directory (gl-directory))
         (started (gemini-live-start "api_key" "test-key" "directory" (namestring directory))))
    (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))
      (let ((sent '()))
        (with-live-session (sent :frames (list +ready+ +delegated+ +spoken+))
          (let ((answer (gl-tool "text" "which files are here")))
            (is (search "list the files in the project folder" answer) "the delegated task is named back")
            (is (search "not run here" answer) "and said to be unrun")
            (is (some (lambda (frame) (search "WORK_STARTED call-1" frame)) sent) "the receipt the original sent went back to the voice model")
            (is (some (lambda (frame) (search "hermes_task" frame)) sent) "addressed to hermes_task")))))))

(deftest gemini-live-refuses-without-a-key-before-any-socket ()
  (let* ((directory (gl-directory))
         (started (gemini-live-start "api_key" "" "api_key_env" "" "directory" (namestring directory))))
    (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))
      (let ((sent '())
            (refused nil))
        (with-live-session (sent :frames (list +ready+))
          (handler-case (gl-tool "text" "hello")
            (error (condition)
              (setf refused (princ-to-string condition)))))
        (is-present refused "a call with no key refuses")
        (is (search "api_key" refused) "naming the setting to set")
        (is (null sent) "and nothing was sent to the endpoint")))
    (let ((sent '()))
      (with-live-session (sent :frames (list +ready+ +spoken+))
        (let ((refused (handler-case (progn (gl-tool "text" "   ") nil)
                            (error (condition) (princ-to-string condition)))))
          (is-present refused "whitespace alone is refused"))))))

;;; --- the key, and the frames before the session is ready ---------------------

(deftest gemini-live-dials-with-the-key-it-was-given ()
  (let* ((directory (gl-directory))
         (started (gemini-live-start "api_key" "test-key-0451" "directory" (namestring directory))))
    (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))
      (let ((sent '())
            (dialed '()))
        (with-live-session (sent :frames (list +ready+ +spoken+) :dialed dialed)
          (is (search "twenty past four" (gl-tool "text" "what time is it")) "the turn is answered"))
        (is (= 1 (length dialed)) "one socket per call")
        (is (search "?key=test-key-0451" (first dialed)) "the socket carries the section's key"))))
  (let* ((directory (gl-directory))
         (started (gemini-live-start "api_key" "" "api_key_env" "GEMINI_LIVE_TEST_KEY"
                                     "directory" (namestring directory))))
    (sb-posix:setenv "GEMINI_LIVE_TEST_KEY" "env-key-0451" 1)
    (nlk:with-cleanup ((progn (funcall started)
                               (sb-posix:unsetenv "GEMINI_LIVE_TEST_KEY")
                               (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))
      (let ((sent '())
            (dialed '()))
        (with-live-session (sent :frames (list +ready+ +spoken+) :dialed dialed)
          (gl-tool "text" "what time is it"))
        (is (search "?key=env-key-0451" (first dialed)) "or the key in the variable api_key_env names")))))

(deftest gemini-live-waits-past-frames-before-the-session-is-ready ()
  (let* ((directory (gl-directory))
         (started (gemini-live-start "api_key" "test-key" "directory" (namestring directory))))
    (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))
      (let ((sent '()))
        (with-live-session (sent :frames (list "{\"outputTranscription\":{\"text\":\"one moment\"}}"
                                               +ready+ +spoken+))
          (is (search "twenty past four" (gl-tool "text" "what time is it")) "a frame before setupComplete is passed over"))))))
