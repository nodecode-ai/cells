;;;; cell-test.lisp --- the gemini-live-bridge cell, end to end over scripted seams.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

;;; --- lifecycle and config ------------------------------------------------------------

(define-cell-lifecycle-tests "gemini-live-bridge"
  (:help :gemini-live-bridge)
  (:command "gemini-live-bridge")
  (:refused ("api_key_env" 42)))

;;; --- the catalog ---------------------------------------------------------------------

(deftest gemini-live-bridge-cell-catalog-keeps-the-originals-order ()
  (with-live ("api_key_env" "GEMINI_LIVE_BRIDGE_TEST_KEY")
    (live-key)
    (multiple-value-bind (ask calls)
        (stub-http :models (models-body "models/gemini-2.5-flash-native-audio-preview-09-2025"
                                         "models/gemini-3.8-live-extended-thinking"
                                         "models/gemini-2.5-flash-native-audio-latest"
                                         "models/gemini-3.8-live"
                                         "models/gemini-2.5-flash"))
      (let* ((nodecode-gemini-live-bridge::*ask* ask)
             (got (nodecode-gemini-live-bridge::live-models)))
        (is (equal '("models/gemini-3.8-live"
                     "models/gemini-3.8-live-extended-thinking"
                     "models/gemini-2.5-flash-native-audio-latest"
                     "models/gemini-2.5-flash-native-audio-preview-09-2025")
                   got)
            (format nil "3.8-live, extended-thinking, native-audio-latest, preview; got ~s" got))
        (is (search "pageSize=200" (second (first (funcall calls)))) "ListModels is read as the original read it")))))

(deftest gemini-live-bridge-cell-catalog-falls-back-to-the-default ()
  (with-live ("api_key_env" "GEMINI_LIVE_BRIDGE_TEST_KEY")
    (live-key)
    (multiple-value-bind (ask calls)
        (stub-http :models (models-body "models/gemini-2.5-flash"))
      (declare (ignore calls))
      (let ((nodecode-gemini-live-bridge::*ask* ask))
        (is (equal (list nodecode-gemini-live-bridge::+default-model+)
                   (nodecode-gemini-live-bridge::live-models)) "a key that lists no Live model still dials the original's default")))))

;;; --- a turn --------------------------------------------------------------------------

(defun answer-frames (&key (text "hello there") (tokens 120))
  "The frames one Live turn arrives as."
  (list (frame-of (nlk:json-object "setupComplete" (nlk:json-object)))
        (frame-of (nlk:json-object
                   "serverContent"
                   (nlk:json-object "modelTurn"
                                    (nlk:json-object
                                     "parts" (vector (nlk:json-object "text" text))))))
        (frame-of (nlk:json-object "usageMetadata" (nlk:json-object "totalTokenCount" tokens)))
        (frame-of (nlk:json-object "sessionResumptionUpdate"
                                    (nlk:json-object "newHandle" "h-1" "resumable" t)))
        (frame-of (nlk:json-object "serverContent" (nlk:json-object "turnComplete" t)))))

(defmacro with-scripted-live ((scripted &key (dials 'dials) (turns 1)) &body body)
  "Run BODY with the one HTTP stub in place and *DIAL* answering SCRIPTED's
frames for TURNS turns, counting dials in DIALS."
  `(progn
     (multiple-value-bind (ask calls)
         (stub-http :models (models-body "models/gemini-3.8-live"))
       (declare (ignore calls))
       (let ((nodecode-gemini-live-bridge::*ask* ask)
             (,dials 0))
         (let ((nodecode-gemini-live-bridge::*dial*
                 (lambda (url)
                   (declare (ignore url))
                   (incf ,dials)
                   (apply #'scripted-link ,scripted
                          (loop repeat ,turns append (answer-frames))))))
           ,@body)))))

(deftest gemini-live-bridge-cell-one-turn-answers-through-the-scripted-socket ()
  (with-live ("api_key_env" "GEMINI_LIVE_BRIDGE_TEST_KEY")
    (live-key)
    (let ((scripted (make-scripted)))
      (with-scripted-live (scripted)
        (multiple-value-bind (reply conversation)
            (nodecode-gemini-live-bridge::say "hi")
          (is (equal "hello there" reply) "the model's text is the answer")
          (is (equal 120 (nodecode-gemini-live-bridge::token-count
                          (nodecode-gemini-live-bridge::conversation-usage conversation))) "the token meter carries Google's own count")
          (is (equal "h-1" (nodecode-gemini-live-bridge::conversation-handle conversation)) "the resumption handle is kept for the next dial")
          (is (= 1 dials) "one dial for the first turn")
          (is (search "clientContent" (first (scripted-sent scripted))) "the turn went out as clientContent"))))))

(deftest gemini-live-bridge-cell-the-session-stays-open-between-turns ()
  (with-live ("api_key_env" "GEMINI_LIVE_BRIDGE_TEST_KEY")
    (live-key)
    (let ((scripted (make-scripted)))
      (with-scripted-live (scripted :turns 2)
        (nodecode-gemini-live-bridge::say "hi")
        (nodecode-gemini-live-bridge::say "again")
        (is (= 1 dials) "the second turn reuses the open session")
        (is (= 2 (nodecode-gemini-live-bridge::conversation-turns
                  nodecode-gemini-live-bridge::*conversation*)) "two turns are counted in the meter")
        (nodecode-gemini-live-bridge::close-conversation)
        (is (= 1 (scripted-closes scripted)) "stop closes the socket")))))

(deftest gemini-live-bridge-cell-a-missing-key-refuses-the-call ()
  (with-live ("api_key_env" "GEMINI_LIVE_BRIDGE_TEST_KEY")
    (live-key nil)
    (is (search "ERROR: no API key" (nodecode-gemini-live-bridge::models-tool nil)) "without the key the tool answers an error, not a dial")
    (is (search "no API key" (nodecode-gemini-live-bridge::run-slash "models" nil)) "/gemini-live-bridge says the same")))

(deftest gemini-live-bridge-cell-an-upstream-error-is-reported ()
  (with-live ("api_key_env" "GEMINI_LIVE_BRIDGE_TEST_KEY")
    (live-key)
    (multiple-value-bind (ask calls)
        (stub-http :models (models-body "models/gemini-3.8-live"))
      (declare (ignore calls))
      (let* ((nodecode-gemini-live-bridge::*ask* ask)
             (scripted (make-scripted))
             (nodecode-gemini-live-bridge::*dial*
               (lambda (url)
                 (declare (ignore url))
                 (scripted-link scripted
                                 (frame-of (nlk:json-object
                                            "error" (nlk:json-object
                                                     "message" "model not found")))))))
        (let ((answer (nodecode-gemini-live-bridge::talk-tool
                       (nodecode-gemini-live-bridge::decode-gemini-live-bridge-turn
                        (nlk:json-object "prompt" "hi")))))
          (is (search "ERROR: models/gemini-3.8-live: model not found" answer)
              (format nil "a setup error names the model and what Google said; got ~s" answer)))))))
