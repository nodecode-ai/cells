;;;; cell-test.lisp --- Telegram Business against a stubbed Bot API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Bot API call is a stubbed NLK:HTTP and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Telegram, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it
;;;; - through the real dispatch, contract and all - so the schema the
;;;; provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "telegram-business" "TELEGRAM-BUSINESS-CELL-")

(defun telegram-business-config (&rest pairs)
  "A whole shared config holding the `telegram-business' section PAIRS make."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "bot_token_env" section "TG_BIZ_TOKEN")
    (nlk:json-object "telegram-business" section)))

(defun telegram-business-start (&rest pairs)
  "START-CELL against a literal config, from a state file no earlier test left."
  (ignore-errors (delete-file (nodecode-telegram-business::state-path)))
  (nodecode-telegram-business:start-cell (apply #'telegram-business-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-tg-token (&body body)
  "BODY with the token's variable answering one fixed token."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "TG_BIZ_TOKEN") "123:test-token"))
     ,@body))

(defmacro with-tg-api ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by method name. ROUTES are (FRAGMENT FORM),
tried in order; FORM runs with METHOD, URL and OPTIONS bound and answers
(values TEXT STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT),
oldest first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Bot API request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +connection-update+
  (concatenate 'string
               "{\"ok\":true,\"result\":[{\"update_id\":10,\"business_connection\":{"
               "\"id\":\"bc-1\",\"user\":{\"id\":555,\"first_name\":\"Sam\"},"
               "\"user_chat_id\":555,\"is_enabled\":true,\"can_reply\":true}}]}")
  "One poll's answer: the connection, reported.")

(defparameter +customer-updates+
  (concatenate 'string
               "{\"ok\":true,\"result\":[{\"update_id\":10,\"business_connection\":{"
               "\"id\":\"bc-1\",\"user\":{\"id\":555,\"first_name\":\"Sam\"},"
               "\"user_chat_id\":555,\"is_enabled\":true,\"can_reply\":true}},"
               "{\"update_id\":11,\"business_message\":{\"message_id\":7,\"date\":1700000000,"
               "\"business_connection_id\":\"bc-1\","
               "\"chat\":{\"id\":777,\"type\":\"private\",\"first_name\":\"Robin\"},"
               "\"from\":{\"id\":777,\"first_name\":\"Robin\"},"
               "\"text\":\"Are you free Friday afternoon?\"}}]}")
  "One poll's answer: the connection, and one customer.")

(defparameter +send-press+
  (concatenate 'string
               "{\"ok\":true,\"result\":[{\"update_id\":12,\"callback_query\":{"
               "\"id\":\"q1\",\"from\":{\"id\":555},\"data\":\"bd:send:d1\","
               "\"message\":{\"message_id\":901,\"chat\":{\"id\":555}}}}]}")
  "One poll's answer: the owner pressed Send on the first draft.")

(defparameter +stranger-press+
  (concatenate 'string
               "{\"ok\":true,\"result\":[{\"update_id\":13,\"callback_query\":{"
               "\"id\":\"q2\",\"from\":{\"id\":999},\"data\":\"bd:send:d1\","
               "\"message\":{\"message_id\":901,\"chat\":{\"id\":555}}}}]}")
  "One poll's answer: somebody who is not the owner pressed Send.")

(defparameter +sent-message+ "{\"ok\":true,\"result\":{\"message_id\":901}}")
(defparameter +ok+ "{\"ok\":true,\"result\":true}")

(defun tg-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun tg-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded; the text itself when it is a refusal."
  (let ((text (tg-answer id tool arguments)))
    (or (ignore-errors (nlk:decode-json text))
        (error "~a" text))))

(defun tg-started ()
  "START-CELL, past one poll that reported the connection."
  (let ((stop (telegram-business-start)))
    (with-tg-api (asked ("getUpdates" (values +customer-updates+ 200)))
      (tg-object "c0" "tg_business_poll" "{}"))
    stop))

(defun asked-content (asked fragment)
  "The payload of the first call ASKED whose URL carries FRAGMENT, decoded."
  (let ((entry (find-if (lambda (entry) (search fragment (second entry))) asked)))
    (and entry (nlk:decode-json (third entry)))))

;;; --- the lifecycle, from the one declaration --------------------------------------

(define-cell-lifecycle-tests "telegram-business"
  (:config "bot_token_env" "TG_BIZ_TOKEN")
  (:help :telegram-business)
  (:command "biz")
  (:fixture with-tg-token)
  (:refused ("bot_token_env" "")))

;;; --- tg_business_poll -------------------------------------------------------------

(deftest telegram-business-cell-polls-a-customer-message (with-tg-token)
  (with-cell-stop ((telegram-business-start))
    (with-tg-api (asked ("getUpdates" (values +customer-updates+ 200)))
      (let* ((object (tg-object "p1" "tg_business_poll" "{}"))
             (events (nlk:json-array object "events")))
        (is (= 2 (length events)) "the connection and the customer are two events")
        (is (equal "connection" (nlk:json-value (aref events 0) :string "kind")))
        (is (equal "customer" (nlk:json-value (aref events 1) :string "kind")))
        (is (equal "777" (nlk:json-value (aref events 1) :string "chat_id")))
        (is (equal "Are you free Friday afternoon?"
                   (nlk:json-value (aref events 1) :string "text")))
        (is (search "short, friendly" (nlk:json-value object :string "persona"))
            "the answer carries the persona to draft in")
        (let ((payload (asked-content asked "getUpdates")))
          (is (= 0 (nlk:json-value payload :integer "offset")))
          (is (= 0 (nlk:json-value payload :integer "timeout"))))
        (is (= 12 (gethash "offset" (nodecode-telegram-business::state)))
            "the offset moves past the last update read")))))

(deftest telegram-business-cell-skips-its-own-echo-and-a-muted-chat (with-tg-token)
  (with-cell-stop ((telegram-business-start))
    (with-tg-api (asked
                   ("getUpdates"
                    (values (concatenate 'string
                                         "{\"ok\":true,\"result\":["
                                         "{\"update_id\":20,\"business_message\":{\"message_id\":8,"
                                         "\"business_connection_id\":\"bc-1\","
                                         "\"sender_business_bot\":{\"id\":42},"
                                         "\"chat\":{\"id\":777},\"from\":{\"id\":777},"
                                         "\"text\":\"the reply we sent\"}}]}")
                            200)))
      (is (zerop (length (nlk:json-array (tg-object "p2" "tg_business_poll" "{}") "events")))
          "our own business reply is not a customer message"))
    (with-tg-api (asked ("getUpdates" (values +customer-updates+ 200)))
      (tg-object "p3" "tg_business_poll" "{}"))
    (is (search "muted" (nodecode-telegram-business:run-command "off 777" "s1")))
    (with-tg-api (asked ("getUpdates" (values +customer-updates+ 200)))
      (let ((events (nlk:json-array (tg-object "p4" "tg_business_poll" "{}") "events")))
        (is (= 1 (length events)) "the muted chat's message makes no event")
        (is (equal "connection" (nlk:json-value (aref events 0) :string "kind")))))))

;;; --- tg_business_draft ------------------------------------------------------------

(deftest telegram-business-cell-draft-dms-the-owner (with-tg-token)
  (with-cell-stop ((tg-started))
    (with-tg-api (asked ("sendMessage" (values +sent-message+ 200)))
      (let ((object (tg-object "d1" "tg_business_draft"
                                "{\"connection_id\":\"bc-1\",\"chat_id\":\"777\",\"customer_message_id\":7,\"customer_text\":\"Are you free Friday afternoon?\",\"text\":\"Friday afternoon works. How about 2pm?\"}")))
        (is (equal "d1" (nlk:json-value object :string "draft_id")))
        (is (eq t (nlk:json-value object :boolean "can_reply")))
        (is (= 901 (nlk:json-value object :integer "owner_message_id")))
        (let ((payload (asked-content asked "sendMessage")))
          (is (= 555 (nlk:json-value payload :integer "chat_id")) "the draft goes to the owner's DM")
          (is (search "Friday afternoon works" (nlk:json-value payload :string "text")))
          (is (search "Robin wrote" (nlk:json-value payload :string "text")))
          (let* ((markup (nlk:json-value payload :object "reply_markup"))
                 (rows (nlk:json-array markup "inline_keyboard"))
                 (buttons (aref rows 0)))
            (is (= 3 (length buttons)) "Send, Edit and Discard")
            (is (equal "bd:send:d1" (nlk:json-value (aref buttons 0) :string "callback_data"))))))
      (is (equal "pending" (nlk:json-value (nodecode-telegram-business::draft "d1")
                                            :string "status"))))))

;;; --- the owner's Send press ---------------------------------------------------------

(deftest telegram-business-cell-send-press-delivers-to-the-customer (with-tg-token)
  (with-cell-stop ((tg-started))
    (with-tg-api (asked ("sendMessage" (values +sent-message+ 200)))
      (tg-object "d2" "tg_business_draft"
                 "{\"connection_id\":\"bc-1\",\"chat_id\":\"777\",\"customer_text\":\"Are you free Friday?\",\"text\":\"Friday works.\"}"))
    (with-tg-api (asked ("getUpdates" (values +send-press+ 200))
                         ("sendMessage" (values +sent-message+ 200))
                         ("answerCallbackQuery" (values +ok+ 200))
                         ("editMessageText" (values +sent-message+ 200)))
      (let ((events (nlk:json-array (tg-object "p5" "tg_business_poll" "{}") "events")))
        (is (= 1 (length events)))
        (is (equal "send" (nlk:json-value (aref events 0) :string "choice"))))
      (let ((payload (asked-content asked "sendMessage")))
        (is (equal "777" (nlk:json-value payload :string "chat_id")))
        (is (equal "Are you free Friday afternoon?"
                   (nlk:json-value (aref events 1) :string "text")))
        (is (search "short, friendly" (nlk:json-value object :string "persona"))
            "the answer carries the persona to draft in")
        (let ((payload (asked-content asked "getUpdates")))
          (is (= 0 (nlk:json-value payload :integer "offset")))
          (is (= 0 (nlk:json-value payload :integer "timeout"))))
        (is (= 12 (gethash "offset" (nodecode-telegram-business::state)))
            "the offset moves past the last update read")))))

(deftest telegram-business-cell-skips-its-own-echo-and-a-muted-chat (with-tg-token)
  (with-cell-stop ((telegram-business-start))
    (with-tg-api (asked
                   ("getUpdates"
                    (values (concatenate 'string
                                         "{\"ok\":true,\"result\":["
                                         "{\"update_id\":20,\"business_message\":{\"message_id\":8,"
                                         "\"business_connection_id\":\"bc-1\","
                                         "\"sender_business_bot\":{\"id\":42},"
                                         "\"chat\":{\"id\":777},\"from\":{\"id\":777},"
                                         "\"text\":\"the reply we sent\"}}]}")
                            200)))
      (is (zerop (length (nlk:json-array (tg-object "p2" "tg_business_poll" "{}") "events")))
          "our own business reply is not a customer message"))
    (with-tg-api (asked ("getUpdates" (values +customer-updates+ 200)))
      (tg-object "p3" "tg_business_poll" "{}"))
    (is (search "muted" (nodecode-telegram-business:run-command "off 777" "s1")))
    (with-tg-api (asked ("getUpdates" (values +customer-updates+ 200)))
      (let ((events (nlk:json-array (tg-object "p4" "tg_business_poll" "{}") "events")))
        (is (= 1 (length events)) "the muted chat's message makes no event")
        (is (equal "connection" (nlk:json-value (aref events 0) :string "kind")))))))

;;; --- tg_business_draft ------------------------------------------------------------

(deftest telegram-business-cell-draft-dms-the-owner (with-tg-token)
  (with-cell-stop ((tg-started))
    (with-tg-api (asked ("sendMessage" (values +sent-message+ 200)))
      (let ((object (tg-object "d1" "tg_business_draft"
                                "{\"connection_id\":\"bc-1\",\"chat_id\":\"777\",\"customer_message_id\":7,\"customer_text\":\"Are you free Friday afternoon?\",\"text\":\"Friday afternoon works. How about 2pm?\"}")))
        (is (equal "d1" (nlk:json-value object :string "draft_id")))
        (is (eq t (nlk:json-value object :boolean "can_reply")))
        (is (= 901 (nlk:json-value object :integer "owner_message_id")))
        (let ((payload (asked-content asked "sendMessage")))
          (is (= 555 (nlk:json-value payload :integer "chat_id")) "the draft goes to the owner's DM")
          (is (search "Friday afternoon works" (nlk:json-value payload :string "text")))
          (is (search "Robin wrote" (nlk:json-value payload :string "text")))
          (let* ((markup (nlk:json-value payload :object "reply_markup"))
                 (rows (nlk:json-array markup "inline_keyboard"))
                 (buttons (aref rows 0)))
            (is (= 3 (length buttons)) "Send, Edit and Discard")
            (is (equal "bd:send:d1" (nlk:json-value (aref buttons 0) :string "callback_data"))))))
      (is (equal "pending" (nlk:json-value (nodecode-telegram-business::draft "d1")
                                            :string "status"))))))

;;; --- the owner's Send press ---------------------------------------------------------

(deftest telegram-business-cell-send-press-delivers-to-the-customer (with-tg-token)
  (with-cell-stop ((tg-started))
    (with-tg-api (asked ("sendMessage" (values +sent-message+ 200)))
      (tg-object "d2" "tg_business_draft"
                 "{\"connection_id\":\"bc-1\",\"chat_id\":\"777\",\"customer_text\":\"Are you free Friday?\",\"text\":\"Friday works.\"}"))
    (with-tg-api (asked ("getUpdates" (values +send-press+ 200))
                         ("sendMessage" (values +sent-message+ 200))
                         ("answerCallbackQuery" (values +ok+ 200))
                         ("editMessageText" (values +sent-message+ 200)))
      (let ((events (nlk:json-array (tg-object "p5" "tg_business_poll" "{}") "events")))
        (is (= 1 (length events)))
        (is (equal "send" (nlk:json-value (aref events 0) :string "choice"))))
      (let ((payload (asked-content asked "sendMessage")))
        (is (equal "777" (nlk:json-value payload :string "chat_id"))
            "the owner's press delivers to the customer")
        (is (equal "bc-1" (nlk:json-value payload :string "business_connection_id")))
        (is (equal "Friday works." (nlk:json-value payload :string "text"))))
      (is (search "Sent" (nlk:json-value (asked-content asked "editMessageText") :string "text"))
          "the owner's DM says it was sent"))
    (is (equal "sent" (nlk:json-value (nodecode-telegram-business::draft "d1") :string "status")))
    (with-tg-api (asked)
      (let ((answer (tg-answer "d3" "tg_business_send" "{\"draft_id\":\"d1\"}")))
        (is (search "ERROR" answer) "a sent draft is not sent twice")
        (is (search "already sent" answer))))))

(deftest telegram-business-cell-stranger-cannot-press-send (with-tg-token)
  (with-cell-stop ((tg-started))
    (with-tg-api (asked ("sendMessage" (values +sent-message+ 200)))
      (tg-object "d4" "tg_business_draft"
                 "{\"connection_id\":\"bc-1\",\"chat_id\":\"777\",\"customer_text\":\"Hi\",\"text\":\"Hello!\"}"))
    (with-tg-api (asked ("getUpdates" (values +stranger-press+ 200))
                         ("sendMessage" (values +sent-message+ 200))
                         ("answerCallbackQuery" (values +ok+ 200)))
      (let ((events (nlk:json-array (tg-object "p6" "tg_business_poll" "{}") "events")))
        (is (equal "forbidden" (nlk:json-value (aref events 0) :string "choice")))
        (is (null (find-if (lambda (entry)
                             (and (search "sendMessage" (second entry))
                                  (= 777 (or (ignore-errors
                                              (nlk:json-value (nlk:decode-json (third entry))
                                                              :integer "chat_id"))
                                             0))))
                           asked))
            "nothing reached the customer")))
    (is (equal "pending" (nlk:json-value (nodecode-telegram-business::draft "d1") :string "status")))))

;;; --- tg_business_send --------------------------------------------------------------

(deftest telegram-business-cell-expired-draft-is-refused (with-tg-token)
  (with-cell-stop ((tg-started))
    (with-tg-api (asked ("sendMessage" (values +sent-message+ 200)))
      (tg-object "d5" "tg_business_draft"
                 "{\"connection_id\":\"bc-1\",\"chat_id\":\"777\",\"customer_text\":\"Hi\",\"text\":\"Hello!\"}"))
    (setf (nodecode-telegram-business::field (nodecode-telegram-business::draft "d1") "expires_at")
          (- (nlk:unix-now) 60))
    (with-tg-api (asked)
      (let ((answer (tg-answer "d6" "tg_business_send" "{\"draft_id\":\"d1\"}")))
        (is (search "ERROR" answer))
        (is (search "expired" answer))))))

(deftest telegram-business-cell-send-carries-an-owners-own-text (with-tg-token)
  (with-cell-stop ((tg-started))
    (with-tg-api (asked ("sendMessage" (values +sent-message+ 200)))
      (tg-object "d7" "tg_business_draft"
                 "{\"connection_id\":\"bc-1\",\"chat_id\":\"777\",\"customer_text\":\"Hi\",\"text\":\"Hello!\"}"))
    (with-tg-api (asked ("sendMessage" (values +sent-message+ 200))
                         ("editMessageText" (values +sent-message+ 200)))
      (let ((object (tg-object "d8" "tg_business_send"
                                "{\"draft_id\":\"d1\",\"text\":\"See you at two.\"}")))
        (is (equal "edited" (nlk:json-value object :string "status")))
        (is (equal "See you at two." (nlk:json-value object :string "sent_text")))
        (let ((payload (asked-content asked "sendMessage")))
          (is (equal "See you at two." (nlk:json-value payload :string "text")))
          (is (equal "bc-1" (nlk:json-value payload :string "business_connection_id"))))))))

;;; --- /biz ----------------------------------------------------------------------------

(deftest telegram-business-cell-biz-command-reports-the-connection (with-tg-token)
  (with-cell-stop ((tg-started))
    (let ((status (nodecode-telegram-business:run-command "" "s1")))
      (is (search "Telegram Business" status))
      (is (search "0 drafts pending" status)))
    (is (search "muted" (nodecode-telegram-business:run-command "off 777" "s1")))
    (is (search "re-enabled" (nodecode-telegram-business:run-command "on 777" "s1")))
    (is (search "isn't a chat id" (nodecode-telegram-business:run-command "off abc" "s1")))
    (is (search "Paused drafting" (nodecode-telegram-business:run-command "pause" "s1")))
    (is (eq nil (nodecode-telegram-business::auto-draft-p
                 (nodecode-telegram-business::connection "bc-1"))))))
