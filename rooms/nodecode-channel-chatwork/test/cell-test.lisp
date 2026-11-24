;;;; cell-test.lisp --- the Chatwork lane against a scripted executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call is the scripted executor's; nothing reaches a server, and the
;;;; token is a file in the run's own temporary folder.

(in-package #:nodecode.test)

(define-test-slice "chatwork" "CHATWORK-CELL-")

(defun chatwork-token-file ()
  "A token file of this run's own, holding one fixed token."
  (let ((path (merge-pathnames "chatwork-cell-token" (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "cw-token-for-tests~%"))
    (namestring path)))

(defun chatwork-section (&rest pairs)
  "A channels.chatwork section pointed at a make-believe account, PAIRS over it."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "token_file" section (chatwork-token-file))
    section))

(defun chatwork-room (id &key (name "general") (type "group") (messages 3))
  "One GET /rooms entry."
  (nlk:json-object "room_id" (parse-integer id) "name" name "type" type
                    "last_update_time" 1700000000 "message_num" messages))

(defun chatwork-message (id body &key (account 42) (name "Kim") (sent (get-universal-time)))
  "One GET /rooms/<id>/messages entry."
  (nlk:json-object "message_id" (parse-integer id)
                    "body" body
                    "send_time" sent
                    "account" (nlk:json-object "account_id" account "name" name)))

(defun chatwork-rooms-answer (&rest rooms)
  (coerce rooms 'vector))

(defun chatwork-messages-answer (&rest messages)
  (coerce messages 'vector))

;;; --- the section -----------------------------------------------------------------

(deftest chatwork-cell-section-refuses-without-a-token ()
  (let ((problems (nlk:section-problems (nlk:find-section '("channels" "chatwork"))
                                        (nlk:json-object "allowed_rooms" (vector "1")))))
    (is (plusp (length problems)) "an unset token refuses")
    (is (search "token" (format nil "~{~a~^ ~}" problems)) "and the refusal names the key")))

;;; --- the notation ---------------------------------------------------------------

(deftest chatwork-cell-inbound-is-read ()
  (let ((parsed (nodecode-channel-chatwork:chatwork-parse-inbound
                 "[To:7] Bot さん
why is the build red?

[code]
make test
[/code]
[qt]old words[/qt][info]title[/info][toall]"
                 :account-id "7" :account-names '("Bot"))))
    (is (eq t (getf parsed :to-bot)) "a [To:] of our own account addresses the bot")
    (is (equal "why is the build red?" (car (ppcre:split "\\n" (getf parsed :text)))) "the display name a To button inserts is not part of the words")
    (is (search "```" (getf parsed :text)) "a [code] block reads as a fence")
    (is (search "> old words" (getf parsed :text)) "a [qt] block reads as a quote")
    (is (not (search "[toall]" (getf parsed :text))) "a [toall] tag is dropped")
    (let ((notice (nodecode-channel-chatwork:chatwork-parse-inbound
                   "[dtext:kim joined]ok" :account-id "7")))
      (is (eq t (getf notice :system-p)) "a system notice is recognised, and never answered"))))

(deftest chatwork-cell-outbound-becomes-a-chatwork-body ()
  (let ((body (nodecode-channel-chatwork:chatwork-render-outbound
               "# 結果
**done** in 3s

```
make test
```
[toall] please look")))
    (is (search "【結果】" body) "a heading becomes 【...】")
    (is (not (search "**" body)) "bold markers are dropped")
    (is (search "[code]make test[/code]" body) "a fence becomes a code box")
    (is (search "［toall］" body) "a notification tag is defused, never pings")
    (is (not (search "# 結果" body)) "and no heading marker survives")))

;;; --- what a message becomes -------------------------------------------------------

(deftest chatwork-cell-only-an-addressed-room-line-is-taken ()
  (let* ((adapter (nodecode-channel-chatwork::make-chatwork-adapter
                   :account-id "7" :account-name "Bot"))
         (addressed (nodecode-channel-chatwork:chatwork-message-candidate
                     adapter (chatwork-message "5" "[To:7] Bot さん
deploy it?") "1")))
    (is (hash-table-p addressed) "a [To:the bot] line is an ask")
    (is (equal "deploy it?" (nlk:json-value addressed :string "text")))
    (is (eq t (nlk:json-value addressed :boolean "source" "addressed")) "and is addressed")
    (is (null (nodecode-channel-chatwork:chatwork-message-candidate
               adapter (chatwork-message "6" "lunch?") "1")) "an unaddressed group line is not")
    (is (null (nodecode-channel-chatwork:chatwork-message-candidate
               adapter (chatwork-message "7" "lunch?") "1"
               :now (+ (get-universal-time) 10))) "and stays so however the clock moves")))

(deftest chatwork-cell-a-reply-to-the-bot-is-an-ask ()
  (let* ((adapter (nodecode-channel-chatwork::make-chatwork-adapter
                   :account-id "7" :account-name "Bot"))
         (state (nodecode-channel-chatwork::chatwork-adapter-state adapter)))
    (nodecode-channel-chatwork::chatwork-remember-sent state "9")
    (let ((candidate (nodecode-channel-chatwork:chatwork-message-candidate
                       adapter (chatwork-message "10" "[rp aid=7 to=1-9]Botさん
and now?") "1")))
      (is (hash-table-p candidate) "a reply to the bot's own message is an ask")
      (is (eq t (nlk:json-value candidate :boolean "source" "addressed")))
      (is (equal "9" (nlk:json-value candidate :string "source" "reply_to_message_id")))
      (is (eq t (nodecode-channel-chatwork::chatwork-sent-by-bot-p state "9")) "the bot knows which postings are its own"))))

;;; --- the plans -------------------------------------------------------------------

(deftest chatwork-cell-message-plan-posts-to-the-room ()
  (let* ((adapter (nodecode-channel-chatwork::make-chatwork-adapter
                   :token "t0k" :account-id "7" :account-name "Bot"))
         (ask (nodecode-channel-chatwork:chatwork-message-candidate
               adapter (chatwork-message "5" "[To:7] Bot さん
hi") "1"))
         (plan (nodecode-channel-chatwork:chatwork-message-plan
                adapter (nck:channel-target ask) "hello"
                :reply-to "5" :ping t)))
    (is (equal "rooms/1/messages" (nck:request-plan-path plan)) "the answer goes to the room")
    (is (equal "POST" (nck:request-plan-method plan)) "as a post")
    (is (equal "t0k" (cdr (assoc "x-chatworktoken" (nck:request-plan-headers plan)
                                   :test #'string=))) "the token rides the header, never the body")
    (is (null (assoc "x-chatworktoken" (nck:request-plan-body plan) :test #'equal)) "and no body field carries it")
    (let ((body (cdr (assoc "body" (nck:request-plan-body plan) :test #'string=))))
      (is (search "[rp aid=42 to=1-5]Kimさん" body) "the answer notifies the asker as Reply does")
      (is (search "hello" body) "and carries the words"))))

;;; --- the lap ---------------------------------------------------------------------

(deftest chatwork-cell-lap-marks-and-then-answers ()
  (let* ((executor
           (nck:make-recording-executor
            :responses
            (list (nck:make-scripted-response 200 (chatwork-rooms-answer (chatwork-room "1")))
                  (nck:make-scripted-response
                   200 (chatwork-messages-answer (chatwork-message "5" "[To:7] Bot さん
before")))
                  (nck:make-scripted-response
                   200 (chatwork-rooms-answer (chatwork-room "1" :messages 4)))
                  (nck:make-scripted-response
                   200 (chatwork-messages-answer
                        (chatwork-message "5" "[To:7] Bot さん
before")
                        (chatwork-message "6" "[To:7] Bot さん
hi"))))))
         (adapter (nodecode-channel-chatwork::make-chatwork-adapter
                   :token "t0k" :account-id "7" :account-name "Bot"
                   :allowed-rooms '("1") :state-file nil))
         (host (nck:make-host-from-section
                (chatwork-section "allowed_rooms" (vector "1") "allowed_users" (vector "42"))
                (nodecode-channel-chatwork:chatwork-platform :adapter adapter)
                :executor executor
                :policy (nodecode-channel-chatwork:chatwork-policy adapter '())
                :owners '("42")))
         (seen '()))
    (setf (nodecode-channel-chatwork::chatwork-adapter-host adapter) host)
    (with-stubbed-fdefinition (nck:handle-candidate (host candidate)
                               (push candidate seen))
      (is (integerp (nodecode-channel-chatwork:chatwork-lap adapter (constantly nil))) "the first lap polls")
      (is (null seen) "and only marks where the room stood")
      (is (integerp (nodecode-channel-chatwork:chatwork-lap adapter (constantly nil))) "the second lap polls too")
      (is (= 1 (length seen)) "and hands the one new ask over")
      (is (equal "hi" (nlk:json-value (first seen) :string "text")) "with its words"))
    (is (nck:channel-status "chatwork") "the lap left the channel reporting a state")))

;;; --- start and stop ---------------------------------------------------------------

(deftest chatwork-cell-starts-and-stops ()
  (let ((executor (nck:make-recording-executor
                   :responses
                   (list (nck:make-scripted-response
                          200 (nlk:json-object "account_id" 7 "name" "Bot"))))))
    (let ((stop (nodecode-channel-chatwork:start-channel
                 (chatwork-section "allowed_rooms" (vector "1") "allowed_users" (vector "42")
                                   "owner" (vector "42"))
                 :executor executor)))
      (is (functionp stop) "the lane started")
      (is (eq t (and (nck:channel-status "chatwork") t)) "the channel reports a state")
      (is (eq t (funcall stop)) "and stops")
      (is (null nodecode-channel-chatwork::*chatwork-adapter*) "with no lane left behind"))))

(deftest chatwork-cell-start-refuses-without-an-allowlist ()
  (let ((refused (handler-case
                     (progn (nodecode-channel-chatwork:start-channel
                             (chatwork-section) :executor (nck:make-recording-executor))
                            nil)
                   (error (condition) (princ-to-string condition)))))
    (is (and refused
             (or (search "refuses to start open" refused) (search "allowed_rooms" refused))) "no allowlist, no lane")))
