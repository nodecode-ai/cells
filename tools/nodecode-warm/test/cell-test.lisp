;;;; cell-test.lisp --- the warm cell against a stubbed endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No request leaves the process: the session's kept body is a literal and
;;;; the transport records what it was handed. Each test drives the real
;;;; seams - the :FRAME hook, TICK and the /warm command - so what the
;;;; operator and the worker run is what is asserted.

(in-package #:nodecode.test)

(define-test-slice "warm" "WARM-CELL-"
  :start nodecode-warm:start-cell
  :defaults (("base_url" "https://api.example.test/v1")
             ("models" (vector "model-one"))))

;;; --- what a session sent, and what the endpoint answers ---------------------------

(defparameter +request+
  (nlk:encode-json-object
   (nlk:json-object
    "model" "model-one"
    "stream" t
    "max_tokens" 4096
    "tools" (vector (nlk:json-object "type" "function"
                                      "function" (nlk:json-object "name" "sh")))
    "messages" (vector (nlk:json-object "role" "user" "content" "what is a monad"))))
  "The body a session's last round sent, as the kernel keeps it.")

(defparameter +answer+
  "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"OK\"}}],\"usage\":{\"prompt_tokens\":1200,\"completion_tokens\":1,\"prompt_tokens_details\":{\"cached_tokens\":1024}}}"
  "What a warmed endpoint answers: one token, most of the prefix cached.")

(defparameter *sent* '()
  "Every warming request this cell made, (URL BODY), oldest first.")

(defmacro with-warming ((&key (body '+request+) (answer '+answer+)) &body forms)
  "FORMS with the session's kept body stubbed to BODY and the transport
answering ANSWER, so nothing reaches a network."
  `(let ((nodecode-warm::*body-fn*
           (lambda (session-id)
             (declare (ignore session-id))
             (sb-ext:string-to-octets ,body :external-format :utf-8)))
         (nodecode-warm::*transport*
           (lambda (settings sent)
             (push (list (nodecode-warm::warming-url settings) sent) *sent*)
             (nodecode-warm::usage-of ,answer)))
         (*sent* '()))
     ,@forms))

(defun warm-frame (type session-id)
  "The :FRAME op of one durable fact of TYPE about SESSION-ID."
  (list :session-id session-id
        :kind "session_stream_event"
        :durable-p t
        :cursor nil
        :payload (nlk:json-object
                  "frame" (nlk:json-object
                           "event" (nlk:json-object "type" type)
                           "turn_id" "turn-1"))))

(defun lose-patience (session-id)
  "Wait out SESSION-ID's interval without waiting: make it due now."
  (let ((session (gethash session-id (nodecode-warm::warmer-sessions nodecode-warm::*warmer*))))
    (is (not (null session)) "the session is kept")
    (when session
      (setf (nodecode-warm::warm-session-due session) 0))))

(defun call-warm (&optional (argument ""))
  "The /warm command, driven the way a shell drives it."
  (funcall (nle::slash-command-headless (nle::find-registered-command "warm")) argument "s1"))

(defun warm-sent-payload (request)
  (nlk:decode-json (second request)))

(defun frame-next (op)
  (declare (ignore op))
  t)

;;; --- the lifecycle, from the one declaration --------------------------------------

(define-cell-lifecycle-tests "warm"
  (:config "base_url" "https://api.example.test/v1" "models" (vector "model-one"))
  (:hooks :frame)
  (:help :warm)
  (:command "warm")
  (:refused ("base_url" "ftp://api.example.test/v1")
            ("base_url" "http://api.example.test/v1")
            ("models" (vector)))
  (:idle nodecode-warm::warm-error
         (nodecode-warm::run-command "" "s1")
         (nodecode-warm::tick)))

;;; --- a turn ends, and its request goes back ---------------------------------------

(deftest warm-cell-resends-the-request-a-turn-sent ()
  (with-warming ()
    (with-cell-stop ((warm-start "base_url" "https://api.example.test/v1"
                                  "models" (vector "model-one")))
      (nodecode-warm::observe-frame (warm-frame "turn.completed" "s1") #'frame-next)
      (is (zerop (length *sent*)) "nothing is sent while the session is warm")
      (lose-patience "s1")
      (nodecode-warm::tick)
      (is (= 1 (length *sent*)) "the idle session's request goes back once")
      (let* ((request (first *sent*))
             (payload (warm-sent-payload request)))
        (is (equal "https://api.example.test/v1/chat/completions" (first request)))
        (is (equal "model-one" (nlk:json-value payload :string "model")))
        (is (= 1 (nlk:json-value payload :integer "max_tokens")) "one completion token")
        (is (null (gethash "tools" payload)) "no tools travel")
        (is (null (gethash "stream" payload)) "and it does not stream")
        (is (equal "what is a monad"
                   (nlk:json-value (aref (gethash "messages" payload) 0) :string "content")) "the history the session sent is the history that goes"))
      (nodecode-warm::tick)
      (is (= 1 (length *sent*)) "and not again until the interval passes")
      (is (search "1 sent" (call-warm)) "the command answers the counters")
      (is (search "1024" (call-warm)) "including what the cache saved"))))

(deftest warm-cell-leaves-another-model-alone ()
  (with-warming (:body (nlk:encode-json-object
                        (nlk:json-object "model" "model-two"
                                         "messages" (vector (nlk:json-object "role" "user"
                                                                             "content" "hi")))))
    (with-cell-stop ((warm-start "base_url" "https://api.example.test/v1"
                                  "models" (vector "model-one")))
      (nodecode-warm::observe-frame (warm-frame "turn.completed" "s1") #'frame-next)
      (nodecode-warm::tick)
      (is (null *sent*) "a model outside the allowlist is never resent")
      (is (search "1 skipped" (call-warm)) "and the command counts it as skipped"))))

(deftest warm-cell-stops-when-asked-and-forgets-the-session ()
  (with-warming ()
    (with-cell-stop ((warm-start "base_url" "https://api.example.test/v1"
                                  "models" (vector "model-one")))
      (nodecode-warm::observe-frame (warm-frame "turn.completed" "s1") #'frame-next)
      (lose-patience "s1")
      (is (search "stopped" (call-warm "stop")) "the command stops it")
      (nodecode-warm::tick)
      (is (null *sent*) "nothing goes after the stop")
      (nodecode-warm::observe-frame (warm-frame "turn.completed" "s1") #'frame-next)
      (is (search "0 session" (call-warm)) "and no session is kept"))))

(deftest warm-cell-forgets-a-session-that-started-a-turn ()
  (with-warming ()
    (with-cell-stop ((warm-start "base_url" "https://api.example.test/v1"
                                  "models" (vector "model-one")))
      (nodecode-warm::observe-frame (warm-frame "turn.completed" "s1") #'frame-next)
      (nodecode-warm::observe-frame (warm-frame "turn.started" "s1") #'frame-next)
      (is (search "0 session" (call-warm)) "a new turn takes the old request away")
      (nodecode-warm::tick)
      (is (null *sent*) "so nothing is sent under the model's feet"))))
