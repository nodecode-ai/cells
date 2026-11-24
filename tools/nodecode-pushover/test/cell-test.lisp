;;;; cell-test.lisp --- the pushover cell against a stubbed API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Pushover request is a stubbed NLK:HTTP and both secrets a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Pushover, the environment or the
;;;; operator's files. The tool is called the way a model's call reaches it —
;;;; through the real dispatch, contract and all — so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "pushover" "PUSHOVER-CELL-" :start nodecode-pushover:start-cell
  :defaults (("app_token_env" "PUSHOVER_APP_TOKEN")
             ("user_key_env" "PUSHOVER_USER_KEY")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter *posts* nil
  "Every request the stub answered, oldest first, as (URL FORM-BODY). It is a
global on purpose: a lifecycle push goes out on a thread of its own, and a
thread sees the global value, not the caller's binding.")

(defmacro with-pushover (&body body)
  "BODY with both secrets answering, and the request log emptied."
  `(let ((saved *posts*))
     (setf *posts* '())
     (nlk:with-cleanup ((setf *posts* saved))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                   (cond ((equal name "PUSHOVER_APP_TOKEN") "app-token")
                                         ((equal name "PUSHOVER_USER_KEY") "user-key")))
         ,@body))))

(defmacro with-pushover-wire ((&key (answer "{\"status\":1,\"request\":\"req-1\"}") (status 200))
                              &body body)
  "BODY with NLK:HTTP answering ANSWER and STATUS to every Pushover request,
and the request read off the fields a test asserts on."
  `(with-stubbed-fdefinition (nlk:http (method target &rest options)
                              (push (list target (getf options :content)) *posts*)
                              (values ,answer ,status))
     ,@body))

(defun push-answer (id arguments)
  "The tool's answer to ARGUMENTS, as the model reads it: the real dispatch."
  (nle::message-content (execute-wire-call id "pushover_notify" arguments)))

(defun push-object (id arguments)
  (nlk:decode-json (push-answer id arguments)))

(defun form-field (body name)
  "NAME's value in a form-encoded BODY, or NIL."
  (let ((head (format nil "~a=" name)))
    (loop for part in (uiop:split-string (or body "") :separator '(#\&))
          when (uiop:string-prefix-p head part)
            return (quri:url-decode (subseq part (length head)) :encoding :utf-8))))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "pushover"
  (:config)
  (:help :pushover)
  (:command "pushover")
  (:hooks :frame)
  (:fixture with-pushover)
  (:refused ("timeout" 0) ("device" 5) ("notify_on" (vector "lunch"))
            ("priority" 7)))

(deftest pushover-cell-without-a-token-refuses ()
  ;; The one thing no verb can work without: the section names a variable, and
  ;; a variable that is unset stops the start before anything installs.
  (with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "PUSHOVER_APP_TOKEN") "app-token"))
    (let ((condition (signals-error nlk:config-refusal (pushover-start))))
      (is (search "PUSHOVER_USER_KEY" (princ-to-string condition)) "the refusal names the unset variable")
      (is (null (nle::find-tool "pushover_notify")) "and installs nothing"))))

;;; --- pushover_notify ----------------------------------------------------------------

(deftest pushover-cell-the-tool-sends-a-push (with-pushover)
  (with-cell-stop ((pushover-start))
    (with-pushover-wire ()
      (let ((object (push-object "push-1"
                                 "{\"message\":\"the deploy is live\",\"title\":\"Deploy\",\"url\":\"https://example.com\"}")))
        (is (eq t (nlk:json-value object :boolean "sent")))
        (is (equal "req-1" (nlk:json-value object :string "request")))
        (is (= 1 (length *posts*)) "one HTTPS exchange, no retry")
        (let ((target (first (first *posts*)))
              (body (second (first *posts*))))
          (is (equal "https://api.pushover.net/1/messages.json" target) "Pushover's one endpoint")
          (is (equal "app-token" (form-field body "token")) "the application token rides the form")
          (is (equal "user-key" (form-field body "user")) "and the operator's user key with it")
          (is (equal "the deploy is live" (form-field body "message")))
          (is (equal "Deploy" (form-field body "title")))
          (is (equal "https://example.com" (form-field body "url")))
          (is (equal "0" (form-field body "priority")) "the section's priority is the default"))))))

(deftest pushover-cell-says-what-pushover-refused (with-pushover)
  (with-cell-stop ((pushover-start))
    (with-pushover-wire (:answer "{\"status\":0,\"errors\":[\"user key is invalid\"]}" :status 400)
      (let ((answer (push-answer "push-2" "{\"message\":\"hello\"}")))
        (is (search "ERROR:" answer) "a refused push is a failed call, not a success")
        (is (search "user key is invalid" answer) "in Pushover's own words")))))

(deftest pushover-cell-an-emergency-push-repeats (with-pushover)
  (with-cell-stop ((pushover-start))
    (with-pushover-wire ()
      (push-object "push-3" "{\"message\":\"the disk is full\",\"priority\":2}")
      (let ((body (second (first *posts*))))
        (is (equal "2" (form-field body "priority")))
        (is (equal "60" (form-field body "retry")) "Pushover requires the repeat interval")
        (is (equal "3600" (form-field body "expire")) "and the ceiling with it"))
      (push-object "push-4" "{\"message\":\"the disk is full\",\"priority\":2,\"retry\":30,\"expire\":120}")
      (let ((body (second (first *posts*))))
        (is (equal "30" (form-field body "retry")) "the caller's own interval wins")))))

(deftest pushover-cell-refuses-an-empty-message (with-pushover)
  (with-cell-stop ((pushover-start))
    (with-pushover-wire ()
      (is (search "ERROR:" (push-answer "push-5" "{\"message\":\"   \"}")) "blank is not a push")
      (is (null *posts*) "and it goes out before any request"))))

(deftest pushover-cell-a-message-without-the-field-is-refused-before-the-body (with-pushover)
  (with-cell-stop ((pushover-start))
    (with-pushover-wire ()
      (let ((answer (push-answer "push-6" "{\"title\":\"nothing said\"}")))
        (is (search "preflight" answer) "the contract refuses it before the body")
        (is (search "message" answer))
        (is (null *posts*))))))

;;; --- the command ---------------------------------------------------------------------

(deftest pushover-cell-the-command-answers-through-stubbed-io (with-pushover)
  (with-cell-stop ((pushover-start "notify_on" (vector "turn-end")))
    (with-pushover-wire ()
      (let ((line (cell-entry "nodecode-pushover" "pushover" "")))
        (is (search "PUSHOVER_APP_TOKEN" line) "the status names the variable the token is read from")
        (is (search "turn-end" line) "and the events that push by themselves")
        (is (null *posts*) "status sends nothing"))
      (is (search "test push went" (cell-entry "nodecode-pushover" "pushover" "test")))
      (is (= 1 (length *posts*)) "test sends exactly one")
      (is (search "test" (string-downcase (form-field (second (first *posts*)) "message"))) "a test push")
      (is (search "Usage" (cell-entry "nodecode-pushover" "pushover" "wobble")) "an unknown verb says what the verbs are"))))

;;; --- the frames ----------------------------------------------------------------------

(deftest pushover-cell-a-turn-end-pushes-when-the-section-asks (with-pushover)
  (with-cell-stop ((pushover-start "notify_on" (vector "turn-end")))
    (with-pushover-wire ()
      (cell-publish "nodecode-pushover" (cell-frame-op "turn.completed"))
      (is (await (:timeout 10) (= 1 (length *posts*))) "the turn's end became one push")
      (is (search "Ready for input" (form-field (second (first *posts*)) "message")) "and says the session waits"))))

(deftest pushover-cell-pushes-nothing-by-itself-by-default (with-pushover)
  (with-cell-stop ((pushover-start))
    (with-pushover-wire ()
      (cell-publish "nodecode-pushover" (cell-frame-op "turn.completed"))
      (cell-publish "nodecode-pushover" (cell-frame-op "turn.failed"))
      (is (null *posts*) "an empty notify_on pushes nothing, and never starts a thread"))))

(deftest pushover-cell-a-failed-turn-says-why (with-pushover)
  (with-cell-stop ((pushover-start "notify_on" (vector "turn-failed")))
    (with-pushover-wire ()
      (cell-publish "nodecode-pushover"
                     (cell-frame-op "turn.failed"
                                    :payload (nlk:json-object "detail" "the provider refused the key")))
      (is (await (:timeout 10) (= 1 (length *posts*))))
      (is (search "the provider refused the key" (form-field (second (first *posts*)) "message")) "the failure's own words ride the push"))))

(deftest pushover-cell-a-question-pushes-once (with-pushover)
  (with-cell-stop ((pushover-start "notify_on" (vector "questions")))
    (with-pushover-wire ()
      (let ((op (list :session-id "s1" :kind "organism.questions"
                      :payload
                      (nlk:json-object
                       "questions"
                       (vector (nlk:json-object "question_id" "q-1"
                                                 "session_id" "s1"
                                                 "title" "Allow the deploy?"
                                                 "detail" "npm publish"
                                                 "choices" (vector "Allow" "Deny")))))))
        (cell-publish "nodecode-pushover" op)
        (cell-publish "nodecode-pushover" op)
        (is (await (:timeout 10) (= 1 (length *posts*))) "the same question pushes once")
        (let ((message (form-field (second (first *posts*)) "message")))
          (is (search "Allow the deploy?" message))
          (is (search "Allow / Deny" message) "its choices ride along"))))))

;;; --- a verb with nothing started --------------------------------------------------------

(deftest pushover-cell-a-verb-refuses-while-not-running (with-pushover)
  (setf nodecode-pushover::*pushover* nil)
  (with-pushover-wire ()
    (let ((answer (nodecode-pushover:send-notification
                   (nodecode-pushover::decode-notification
                    (nlk:make-json-object "message" "hello")))))
      (is (search "not running" answer) "the refusal says the cell is not on")
      (is (null *posts*) "and nothing left the machine"))))

;;; --- the same push, from eval -----------------------------------------------------------

(deftest pushover-cell-eval-sends-the-same-push (with-pushover)
  (with-cell-stop ((pushover-start))
    (with-pushover-wire ()
      (is (search "req-1" (nodecode-pushover:notify "the build is green" :priority 1)) "notify is the tool's own body")
      (let ((body (second (first *posts*))))
        (is (equal "the build is green" (form-field body "message")))
        (is (equal "1" (form-field body "priority")) "a keyword argument reaches the form")))))
