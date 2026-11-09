;;;; cell-test.lisp --- the telegram-notify cell against a stubbed Bot API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Bot API request is a stubbed NLK:HTTP and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Telegram, the environment or the
;;;; operator's files. The tool is called the way a model's call reaches it —
;;;; through the real dispatch, contract and all — so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "telegram-notify" "TELEGRAM-NOTIFY-CELL-"
  :start nodecode-telegram-notify:start-cell
  :defaults (("bot_token_env" "TELEGRAM_BOT_TOKEN")
             ("chat_id" "12345")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter *posts* nil
  "Every request the stub answered, oldest first, as (URL BODY). It is a global
on purpose: a lifecycle message goes out on a thread of its own, and a thread
sees the global value, not the caller's binding.")

(defmacro with-telegram (&body body)
  "BODY with the bot token answering, and the request log emptied."
  `(let ((saved *posts*))
     (setf *posts* '())
     (nlk:with-cleanup ((setf *posts* saved))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "TELEGRAM_BOT_TOKEN") "bot-token"))
         ,@body))))

(defmacro with-telegram-wire ((&key (answer "{\"ok\":true,\"result\":{\"message_id\":7}}") (status 200))
                              &body body)
  "BODY with NLK:HTTP answering ANSWER and STATUS to every Bot API request, and
the request read off what a test asserts on."
  `(with-stubbed-fdefinition (nlk:http (method target &rest options)
                              (declare (ignore method))
                              (push (list target (getf options :content)) *posts*)
                              (values ,answer ,status))
     ,@body))

(defun telegram-answer (id arguments)
  "The tool's answer to ARGUMENTS, as the model reads it: the real dispatch."
  (nle::message-content (execute-wire-call id "telegram_notify" arguments)))

(defun sent-object (request)
  "The JSON body of one logged REQUEST, as an object."
  (nlk:decode-json (second request)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "telegram-notify"
  (:config)
  (:help :telegram-notify)
  (:command "telegram-notify")
  (:hooks :frame)
  (:fixture with-telegram)
  (:refused ("timeout" 0) ("chat_id" 5) ("notify_on" (vector "lunch"))))

(deftest telegram-notify-cell-without-a-token-refuses ()
  ;; The one thing no verb can work without: the section names a variable, and
  ;; a variable that is unset stops the start before anything installs.
  (with-stubbed-fdefinition (nle::credential-env (name)
                              (declare (ignore name))
                              nil)
    (let ((condition (signals-error nlk:config-refusal (telegram-notify-start))))
      (is (search "TELEGRAM_BOT_TOKEN" (princ-to-string condition)) "the refusal names the unset variable")
      (is (null (nle::find-tool "telegram_notify")) "and installs nothing"))))

(deftest telegram-notify-cell-without-a-chat-refuses ()
  (with-telegram
    (let ((condition (signals-error nlk:config-refusal (telegram-notify-start "chat_id" ""))))
      (is (search "chat_id" (princ-to-string condition)) "the refusal names the empty member")
      (is (search "chats" (princ-to-string condition)) "and the command that finds it"))))

;;; --- telegram_notify ----------------------------------------------------------------

(deftest telegram-notify-cell-the-tool-sends-a-message (with-telegram)
  (with-cell-stop ((telegram-notify-start))
    (with-telegram-wire ()
      (let ((object (nlk:decode-json (telegram-answer "tg-1" "{\"message\":\"the deploy is live\"}"))))
        (is (eq t (nlk:json-value object :boolean "sent")))
        (is (= 7 (nlk:json-value object :integer "message_id")))
        (is (= 1 (length *posts*)) "one HTTPS exchange, no retry")
        (let ((request (first *posts*)))
          (is (equal "https://api.telegram.org/botbot-token/sendMessage" (first request)) "the Bot API's one endpoint, the token in its path")
          (let ((body (sent-object request)))
            (is (equal "12345" (nlk:json-value body :string "chat_id")) "the section's chat")
            (is (equal "the deploy is live" (nlk:json-value body :string "text")))
            (is (eq t (nlk:json-value body :boolean "disable_web_page_preview")))
            (is (null (nlk:json-value body :boolean "disable_notification")) "a normal message rings")))))))

(deftest telegram-notify-cell-a-silent-message-and-another-chat (with-telegram)
  (with-cell-stop ((telegram-notify-start))
    (with-telegram-wire ()
      (telegram-answer "tg-2" "{\"message\":\"done\",\"chat_id\":\"-100200\",\"silent\":true}")
      (let ((body (sent-object (first *posts*))))
        (is (equal "-100200" (nlk:json-value body :string "chat_id")) "the caller's chat wins")
        (is (eq t (nlk:json-value body :boolean "disable_notification")) "and silent means no sound")))))

(deftest telegram-notify-cell-says-what-telegram-refused (with-telegram)
  (with-cell-stop ((telegram-notify-start))
    (with-telegram-wire (:answer "{\"ok\":false,\"description\":\"Bad Request: chat not found\"}" :status 400)
      (let ((answer (telegram-answer "tg-3" "{\"message\":\"hello\"}")))
        (is (search "ERROR:" answer) "a refused message is a failed call, not a success")
        (is (search "chat not found" answer) "in Telegram's own words")))))

(deftest telegram-notify-cell-refuses-an-empty-message (with-telegram)
  (with-cell-stop ((telegram-notify-start))
    (with-telegram-wire ()
      (is (search "ERROR:" (telegram-answer "tg-4" "{\"message\":\"   \"}")) "blank is not a message")
      (is (null *posts*) "and it goes out before any request"))))

(deftest telegram-notify-cell-a-message-without-the-field-is-refused-before-the-body (with-telegram)
  (with-cell-stop ((telegram-notify-start))
    (with-telegram-wire ()
      (let ((answer (telegram-answer "tg-5" "{\"chat_id\":\"1\"}")))
        (is (search "preflight" answer) "the contract refuses it before the body")
        (is (search "message" answer))
        (is (null *posts*))))))

;;; --- the command ---------------------------------------------------------------------

(deftest telegram-notify-cell-the-command-answers-through-stubbed-io (with-telegram)
  (with-cell-stop ((telegram-notify-start "notify_on" (vector "turn-end")))
    (with-telegram-wire ()
      (let ((line (cell-entry "nodecode-telegram-notify" "telegram-notify" "")))
        (is (search "TELEGRAM_BOT_TOKEN" line) "the status names the variable the token is read from")
        (is (search "12345" line) "and the chat a message would go to")
        (is (search "turn-end" line) "and the events that message by themselves")
        (is (null *posts*) "status sends nothing"))
      (is (search "test message went" (cell-entry "nodecode-telegram-notify" "telegram-notify" "test")))
      (is (= 1 (length *posts*)) "test sends exactly one")
      (is (search "test message" (nlk:json-value (sent-object (first *posts*)) :string "text")) "a test message")
      (is (search "Usage" (cell-entry "nodecode-telegram-notify" "telegram-notify" "wobble")) "an unknown verb says what the verbs are"))))

(deftest telegram-notify-cell-the-chat-list-reads-getupdates (with-telegram)
  (with-cell-stop ((telegram-notify-start))
    (with-telegram-wire (:answer "{\"ok\":true,\"result\":[{\"message\":{\"chat\":{\"id\":987,\"username\":\"someone\"}}}]}")
      (let ((line (cell-entry "nodecode-telegram-notify" "telegram-notify" "chats")))
        (is (search "987" line) "the id a person pastes into chat_id")
        (is (search "someone" line) "with the name Telegram knows it by")
        (is (equal "https://api.telegram.org/botbot-token/getUpdates" (first (first *posts*))) "the one other Bot API method the cell calls")))))

;;; --- the frames ----------------------------------------------------------------------

(deftest telegram-notify-cell-a-turn-end-carries-the-answer (with-telegram)
  (with-cell-stop ((telegram-notify-start "notify_on" (vector "turn-end")))
    (with-telegram-wire ()
      (cell-publish "nodecode-telegram-notify"
                     (cell-frame-op "turn.assistant_message_completed" :turn "t1"
                                    :payload (nlk:json-object "message" "the deploy is live")))
      (cell-publish "nodecode-telegram-notify" (cell-frame-op "turn.completed" :turn "t1"))
      (is (await (:timeout 10) (= 1 (length *posts*))) "the turn's end became one message")
      (let ((text (nlk:json-value (sent-object (first *posts*)) :string "text")))
        (is (search "turn done" text) "it says the turn ended")
        (is (search "the deploy is live" text) "and carries the answer the turn produced")))))

(deftest telegram-notify-cell-one-turn-is-one-message (with-telegram)
  (with-cell-stop ((telegram-notify-start "notify_on" (vector "turn-end")))
    (with-telegram-wire ()
      (cell-publish "nodecode-telegram-notify" (cell-frame-op "turn.completed" :turn "t9"))
      (cell-publish "nodecode-telegram-notify" (cell-frame-op "turn.completed" :turn "t9"))
      (is (await (:timeout 10) (= 1 (length *posts*))) "a republished end does not message twice"))))

(deftest telegram-notify-cell-messages-nothing-by-itself-by-default (with-telegram)
  (with-cell-stop ((telegram-notify-start))
    (with-telegram-wire ()
      (cell-publish "nodecode-telegram-notify" (cell-frame-op "turn.started" :turn "t2"))
      (cell-publish "nodecode-telegram-notify" (cell-frame-op "turn.completed" :turn "t2"))
      (cell-publish "nodecode-telegram-notify" (cell-frame-op "turn.failed" :turn "t3"))
      (is (null *posts*) "an empty notify_on messages nothing, and never starts a thread"))))

(deftest telegram-notify-cell-a-failed-turn-says-why (with-telegram)
  (with-cell-stop ((telegram-notify-start "notify_on" (vector "turn-failed")))
    (with-telegram-wire ()
      (cell-publish "nodecode-telegram-notify"
                     (cell-frame-op "turn.failed" :turn "t4"
                                    :payload (nlk:json-object "detail" "the provider refused the key")))
      (is (await (:timeout 10) (= 1 (length *posts*))))
      (is (search "the provider refused the key"
                  (nlk:json-value (sent-object (first *posts*)) :string "text")) "the failure's own words ride the message"))))

(deftest telegram-notify-cell-a-turn-start-is-opt-in (with-telegram)
  (with-cell-stop ((telegram-notify-start "notify_on" (vector "turn-start")))
    (with-telegram-wire ()
      (cell-publish "nodecode-telegram-notify" (cell-frame-op "turn.started" :turn "t5"))
      (is (await (:timeout 10) (= 1 (length *posts*))))
      (is (search "turn started" (nlk:json-value (sent-object (first *posts*)) :string "text"))))))

(deftest telegram-notify-cell-a-question-messages-once (with-telegram)
  (with-cell-stop ((telegram-notify-start "notify_on" (vector "questions")))
    (with-telegram-wire ()
      (let ((op (list :session-id "s1" :kind "organism.questions"
                      :payload
                      (nlk:json-object
                       "questions"
                       (vector (nlk:json-object "question_id" "q-1"
                                                 "session_id" "s1"
                                                 "title" "Allow the deploy?"
                                                 "detail" "npm publish"
                                                 "choices" (vector "Allow" "Deny")))))))
        (cell-publish "nodecode-telegram-notify" op)
        (cell-publish "nodecode-telegram-notify" op)
        (is (await (:timeout 10) (= 1 (length *posts*))) "the same question messages once")
        (let ((text (nlk:json-value (sent-object (first *posts*)) :string "text")))
          (is (search "Allow the deploy?" text))
          (is (search "Allow / Deny" text) "its choices ride along"))))))

;;; --- a verb with nothing started --------------------------------------------------------

(deftest telegram-notify-cell-a-verb-refuses-while-not-running (with-telegram)
  (setf nodecode-telegram-notify::*telegram-notify* nil)
  (with-telegram-wire ()
    (let ((answer (nodecode-telegram-notify:send-notification
                   (nodecode-telegram-notify::decode-notification
                    (nlk:make-json-object "message" "hello")))))
      (is (search "not running" answer) "the refusal says the cell is not on")
      (is (null *posts*) "and nothing left the machine"))))

;;; --- the same message, from eval ---------------------------------------------------------

(deftest telegram-notify-cell-eval-sends-the-same-message (with-telegram)
  (with-cell-stop ((telegram-notify-start))
    (with-telegram-wire ()
      (is (search "message_id" (nodecode-telegram-notify:notify "the build is green" :silent t)) "notify is the tool's own body")
      (let ((body (sent-object (first *posts*))))
        (is (equal "the build is green" (nlk:json-value body :string "text")))
        (is (eq t (nlk:json-value body :boolean "disable_notification")) "a keyword argument reaches the body")))))
