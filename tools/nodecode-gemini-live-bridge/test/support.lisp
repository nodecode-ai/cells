;;;; support.lisp --- the slice runner and the two scripted seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test runs with the cell started on a literal config (the key's
;;;; variable need not exist: the tools, not the start, read the key) and
;;;; with *ASK* and *DIAL* bound, so no test opens a socket.

(in-package #:nodecode.test)

(define-test-slice "gemini-live-bridge" "GEMINI-LIVE-BRIDGE-CELL-" :start nodecode-gemini-live-bridge:start-cell
  :defaults (("api_key_env" "GEMINI_LIVE_BRIDGE_TEST_KEY")))

(defmacro with-live ((&rest config-pairs) &body body)
  "Run BODY with the cell started on CONFIG-PAIRS, stopping it on unwind."
  `(with-cell-stop ((gemini-live-bridge-start ,@config-pairs))
     (unwind-protect (progn ,@body)
       (setf (uiop:getenv "GEMINI_LIVE_BRIDGE_TEST_KEY") ""))))

(defun live-key (&optional (value "test-key"))
  "Put VALUE where the section's api_key_env points; an empty string is absent."
  (setf (uiop:getenv "GEMINI_LIVE_BRIDGE_TEST_KEY") (or value "")))

(defun frame-of (object)
  "The frame text the socket would carry for OBJECT."
  (nlk:encode-json-object object))

(defstruct (scripted (:constructor make-scripted ()))
  "The log and the frames a fake dial answers from."
  (sent '())
  (closes 0)
  (queue '()))

(defun scripted-link (scripted &rest frames)
  "A LINK over SCRIPTED answering FRAMES in order, recording what was sent."
  (setf (scripted-queue scripted) (copy-list frames))
  (nodecode-gemini-live-bridge::make-link
   :send-fn (lambda (text) (push text (scripted-sent scripted)))
   :recv-fn (lambda (timeout)
              (declare (ignore timeout))
              (if (scripted-queue scripted)
                  (pop (scripted-queue scripted))
                  :closed))
   :close-fn (lambda () (incf (scripted-closes scripted)))))

(defun models-body (&rest names)
  "The ListModels answer for NAMES, each live and bidi-capable."
  (frame-of (nlk:json-object
             "models"
             (coerce (loop for name in names
                           collect (nlk:json-object
                                    "name" name
                                    "supportedGenerationMethods"
                                    (vector "generateContent" "bidiGenerateContent")))
                     'vector))))

(defun stub-http (&key models (token "ephemeral-1") (models-status 200))
  "ASK's stand-in: ListModels answers MODELS at MODELS-STATUS, auth_tokens
answers TOKEN. Answers (VALUES FUNCTION CALLS), CALLS the urls asked, oldest first."
  (let ((calls '()))
    (values (lambda (method url &key headers content timeout)
              (declare (ignore headers content timeout))
              (push (list method url) calls)
              (cond ((search "auth_tokens" url)
                     (values (frame-of (nlk:json-object "name" token)) 200))
                    (t (values models models-status))))
            (lambda () (reverse calls)))))
