;;;; cell-test.lisp --- the OneBot lane against a scripted executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call is the scripted executor's; nothing reaches a bridge. A
;;;; message is a segment array or one CQ string, as the two bridges send.

(in-package #:nodecode.test)

(define-test-slice "onebot" "ONEBOT-CELL-")

(defun onebot-section (&rest pairs)
  "A channels.onebot section pointed at a make-believe bridge, PAIRS over it."
  (apply #'nlk:make-json-object pairs))

(defun onebot-login-answer (&key (id 999) (name "Bot"))
  (nlk:json-object "status" "ok" "retcode" 0
                    "data" (nlk:json-object "user_id" id "nickname" name)))

(defun onebot-group-answer (&rest groups)
  (nlk:json-object "status" "ok" "retcode" 0
                    "data" (coerce
                            (loop for group in groups
                                  collect (nlk:json-object "group_id" (parse-integer group)
                                                            "group_name" (format nil "group ~a" group)))
                            'vector)))

(defun onebot-history-answer (&rest messages)
  (nlk:json-object "status" "ok" "retcode" 0
                    "data" (nlk:json-object "messages" (coerce messages 'vector))))

(defun onebot-message (id message &key (user 42) (name "Kim") (time nil))
  (nlk:json-object "message_id" (parse-integer id)
                    "user_id" user
                    "time" (or time (- (get-universal-time) 2208988800))
                    "message" message
                    "sender" (nlk:json-object "nickname" name)))

;;; --- the section -----------------------------------------------------------------

(deftest onebot-cell-section-refuses-without-a-base-url ()
  (let ((problems (nlk:section-problems (nlk:find-section '("channels" "onebot"))
                                        (onebot-section "allowed_groups" (vector "1")))))
    (is (plusp (length problems)) "a section with no base_url refuses")
    (is (search "base_url" (format nil "~{~a~^ ~}" problems)) "and the refusal names the key")))

;;; --- what a message says ---------------------------------------------------------

(deftest onebot-cell-cq-string-is-read ()
  (let ((parsed (nodecode-channel-onebot:onebot-parse-message
                 "[CQ:reply,id=88][CQ:at,qq=999,name=bot] why is the build red? &#91;now&#93;"
                 :self-id "999")))
    (is (eq t (getf parsed :to-bot)) "an at of the bot's own QQ number addresses it")
    (is (equal "88" (getf parsed :reply)) "a reply segment names the message it answers")
    (is (search "why is the build red? [now]" (getf parsed :text)) "the words come through, the escapes decoded")
    (is (not (search "@bot" (getf parsed :text))) "the bot's own name is not left in the words")))

(deftest onebot-cell-segment-array-is-read ()
  (let ((parsed (nodecode-channel-onebot:onebot-parse-message
                 (coerce (list (nlk:json-object "type" "at"
                                                 "data" (nlk:json-object "qq" "42" "name" "Kim"))
                               (nlk:json-object "type" "text"
                                                 "data" (nlk:json-object "text" " hi"))
                               (nlk:json-object "type" "image"
                                                 "data" (nlk:json-object "file" "a.png")))
                         'vector)
                 :self-id "999")))
    (is (equal "@Kim hi(image)" (getf parsed :text)) "a mention of someone else stays as @name")
    (is (null (getf parsed :to-bot)) "and does not address the bot")
    (is (= 1 (getf parsed :images)) "a picture is counted")))

(deftest onebot-cell-outbound-defuses-a-cq-code ()
  (let ((text (nodecode-channel-onebot:onebot-render-outbound
               "done [CQ:at,qq=all] please look")))
    (is (search "［CQ:at,qq=all］" text) "a CQ code the model wrote is made inert")
    (is (not (search "[CQ:" text)) "so a group cannot be pinged")
    (is (equal "..." (nodecode-channel-onebot:onebot-render-outbound "  ")) "an empty answer still says something")))

;;; --- what a message becomes -------------------------------------------------------

(deftest onebot-cell-only-an-addressed-group-line-is-taken ()
  (let ((adapter (nodecode-channel-onebot::make-onebot-adapter :self-id "999" :nickname "Bot")))
    (let ((addressed (nodecode-channel-onebot:onebot-message-candidate
                      adapter (onebot-message "5" "[CQ:at,qq=999] deploy it?") "group" "123")))
      (is (hash-table-p addressed) "an at of the bot is an ask")
      (is (equal "deploy it?" (nlk:json-value addressed :string "text")))
      (is (eq t (nlk:json-value addressed :boolean "source" "addressed")) "and is addressed"))
    (is (null (nodecode-channel-onebot:onebot-message-candidate
               adapter (onebot-message "6" "lunch?") "group" "123")) "an unaddressed group line is not")
    (is (null (nodecode-channel-onebot:onebot-message-candidate
               adapter (onebot-message "7" "lunch?" :user 999) "group" "123")) "nor is the bot's own post")))

(deftest onebot-cell-a-reply-to-the-bot-is-an-ask ()
  (let* ((adapter (nodecode-channel-onebot::make-onebot-adapter :self-id "999" :nickname "Bot"))
         (state (nodecode-channel-onebot::onebot-adapter-state adapter)))
    (nodecode-channel-onebot:onebot-remember-sent state "9")
    (let ((candidate (nodecode-channel-onebot:onebot-message-candidate
                       adapter (onebot-message "10" "[CQ:reply,id=9] and now?") "group" "123")))
      (is (hash-table-p candidate) "a reply to the bot's own message is an ask")
      (is (equal "9" (nlk:json-value candidate :string "source" "reply_to_message_id")))
      (is (eq t (nodecode-channel-onebot:onebot-sent-by-bot-p state "9")) "the bot knows which postings are its own"))))

(deftest onebot-cell-a-free-group-and-a-private-chat-answer-anything ()
  (let ((adapter (nodecode-channel-onebot::make-onebot-adapter
                  :self-id "999" :free-groups '("123") :require-mention t)))
    (is (hash-table-p (nodecode-channel-onebot:onebot-message-candidate
                        adapter (onebot-message "5" "lunch?") "group" "123")) "a free-response group takes an unaddressed line")
    (is (hash-table-p (nodecode-channel-onebot:onebot-message-candidate
                        adapter (onebot-message "6" "hello") "private" "42")) "a private chat takes what is typed there")))

;;; --- the plans -------------------------------------------------------------------

(deftest onebot-cell-send-plan-posts-to-the-group ()
  (let* ((adapter (nodecode-channel-onebot::make-onebot-adapter
                   :token "t0k" :self-id "999" :allowed-groups '("123")))
         (plan (nodecode-channel-onebot:onebot-send-plan
                adapter (list :channel-id "123") "hello [CQ:at,qq=all]" :ping t)))
    (is (equal "send_group_msg" (nck:request-plan-path plan)) "a group answer goes to the group")
    (is (equal "POST" (nck:request-plan-method plan)) "as a post")
    (is (equal "Bearer t0k" (cdr (assoc "authorization" (nck:request-plan-headers plan)
                                         :test #'string=))) "the access token rides the header, never the body")
    (let ((body (nck:request-plan-body plan)))
      (is (equal "123" (nlk:json-value body :string "group_id")) "the group is named")
      (is (eq t (nlk:json-value body :boolean "auto_escape")) "auto_escape keeps a CQ code as words")
      (is (search "［CQ:at,qq=all］" (nlk:json-value body :string "message")) "and it is defused as well")))
  (let ((adapter (nodecode-channel-onebot::make-onebot-adapter :self-id "999")))
    (is (equal "send_private_msg"
               (nck:request-plan-path
                (nodecode-channel-onebot:onebot-send-plan adapter (list :channel-id "42") "hi"))) "a private answer goes to the person")))

(deftest onebot-cell-a-refused-send-is-not-ok ()
  (let ((executor (nck:make-recording-executor
                   :responses (list (nck:make-scripted-response 200 (nlk:json-object "retcode" 0))
                                    (nck:make-scripted-response 200 (nlk:json-object "retcode" 100))))))
    (is (eq t (and (nodecode-channel-onebot:onebot-ok-p
                    (nck:execute-plan executor (nodecode-channel-onebot:onebot-login-plan nil)))
                   t)) "retcode 0 is an answer")
    (let ((refused (nck:execute-plan executor (nodecode-channel-onebot:onebot-login-plan nil))))
      (is (null (nodecode-channel-onebot:onebot-ok-p refused)) "a bridge refusal is not")
      (is (equal "1" (nodecode-channel-onebot:onebot-message-id-of
                      (nlk:json-object "data" (nlk:json-object "message_id" 1)))) "and a sent message answers its id"))))

;;; --- the lap ---------------------------------------------------------------------

(deftest onebot-cell-lap-marks-and-then-answers ()
  (let* ((executor
           (nck:make-recording-executor
            :responses
            (list (nck:make-scripted-response 200 (onebot-history-answer (onebot-message "5" "[CQ:at,qq=999] before")))
                  (nck:make-scripted-response 200 (onebot-history-answer (onebot-message "5" "[CQ:at,qq=999] before")
                                                                          (onebot-message "6" "[CQ:at,qq=999] hi"))))))
         (adapter (nodecode-channel-onebot::make-onebot-adapter
                   :self-id "999" :nickname "Bot" :allowed-groups '("123")))
         (host (nck:make-host-from-section
                (onebot-section "base_url" "http://127.0.0.1:3000"
                                "allowed_groups" (vector "123") "allowed_users" (vector "42"))
                (nodecode-channel-onebot:onebot-platform :adapter adapter)
                :executor executor
                :policy (nodecode-channel-onebot:onebot-inbound-policy adapter '())
                :owners '("42")))
         (seen '()))
    (setf (nodecode-channel-onebot::onebot-adapter-host adapter) host)
    (with-stubbed-fdefinition (nck:handle-candidate (host candidate)
                               (push candidate seen))
      (is (integerp (nodecode-channel-onebot:onebot-lap adapter (constantly nil))) "the first lap polls")
      (is (null seen) "and only marks where the chat stood")
      (is (integerp (nodecode-channel-onebot:onebot-lap adapter (constantly nil))) "the second lap polls too")
      (is (= 1 (length seen)) "and hands the one new ask over")
      (is (equal "hi" (nlk:json-value (first seen) :string "text")) "with its words"))
    (is (nck:channel-status "onebot") "the lap left the channel reporting a state")))

;;; --- start and stop ---------------------------------------------------------------

(deftest onebot-cell-starts-and-stops ()
  (let ((executor (nck:make-recording-executor
                   :responses (list (nck:make-scripted-response 200 (onebot-login-answer))
                   (nck:make-scripted-response 200 (onebot-group-answer "123"))))))
    (let ((stop (nodecode-channel-onebot:start-channel
                 (onebot-section "base_url" "http://127.0.0.1:3000"
                                 "allowed_groups" (vector "123") "allowed_users" (vector "42")
                                 "owner" (vector "42"))
                 :executor executor)))
      (is (functionp stop) "the lane started")
      (is (eq t (and (nck:channel-status "onebot") t)) "the channel reports a state")
      (is (eq t (funcall stop)) "and stops")
      (is (null nodecode-channel-onebot::*onebot-adapter*) "with no lane left behind"))))

(deftest onebot-cell-start-refuses-without-an-allowlist ()
  (let ((refused (handler-case
                     (progn (nodecode-channel-onebot:start-channel
                             (onebot-section "base_url" "http://127.0.0.1:3000")
                             :executor (nck:make-recording-executor))
                            nil)
                   (error (condition) (princ-to-string condition)))))
    (is (and refused
             (or (search "refuses to start open" refused) (search "allowed_groups" refused))) "no allowlist, no lane")))
