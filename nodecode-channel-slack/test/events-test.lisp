;;;; events-test.lisp --- Slack events and socket frames as the kit reads them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure: the event shapes are docs.slack.dev's, built by the fake's helpers.

(in-package #:nodecode.test)

(deftest channel-slack-plain-text-reads-as-typed ()
  ;; A mention stays the handle the model writes back; a channel link, a
  ;; link, @here and the escaped characters read as a person sees them.
  (is (equal "<@U1> see #general and docs (https://x.dev) or https://y.dev, @here: a < b & c > d"
             (ncs:slack-plain-text
              "<@U1> see <#C1|general> and <https://x.dev|docs> or <https://y.dev>, <!here>: a &lt; b &amp; c &gt; d")))
  (is (equal "mail kim@x.dev" (ncs:slack-plain-text "mail <mailto:kim@x.dev|kim@x.dev>")))
  (is (equal "" (ncs:slack-plain-text nil))))

(deftest channel-slack-mention-is-the-user-handle ()
  (is (ncs:slack-mention-p "hey <@UBOT> look" "UBOT"))
  (is (ncs:slack-mention-p "<@UBOT|nodecode> look" "UBOT"))
  (is (not (ncs:slack-mention-p "hey <@UOTHER> look" "UBOT")))
  (is (not (ncs:slack-mention-p "hey UBOT" "UBOT")))
  (is (equal "look here" (ncs:slack-strip-mention "<@UBOT> look here" "UBOT")))
  (is (equal "look" (ncs:slack-strip-mention "<@UBOT|nodecode> look" "UBOT"))))

(deftest channel-slack-message-maps-to-the-candidate ()
  (is-present (dm (ncs:slack-message-candidate
                   (slack-message "D1" "1.1" "hi &amp; bye" :channel-type "im")
                   :bot-user-id "UBOT" :user-name "kim"))
      "a DM is something said"
    (is (equal "hi & bye" (gethash "text" dm)))
    (is-source dm "platform" "slack" "chat_kind" "direct_message" "channel_id" "D1"
               "message_id" "1.1" "user_id" "U1" "user_name" "kim" "workspace_id" "T1"
               "reply_to_message_id" nil)
    (is (not (nck:candidate-addressed-p dm))))
  ;; A reply in a thread answers the thread's first message, and is addressed
  ;; when the thread hangs off the bot or the bot speaks in it.
  (is-present (reply (ncs:slack-message-candidate
                      (slack-message "C1" "2.2" "and more" :thread "1.1" :parent-user "UBOT")
                      :bot-user-id "UBOT"))
      "a reply under the bot's message"
    (is-source reply "chat_kind" "thread" "reply_to_message_id" "1.1")
    (is (nck:candidate-addressed-p reply)))
  (let ((seen '()))
    (is-present (reply (ncs:slack-message-candidate
                        (slack-message "C1" "2.3" "go on" :thread "1.1")
                        :bot-user-id "UBOT"
                        :bot-thread-p (lambda (channel root) (push (list channel root) seen) t)))
        "a reply in a thread the bot answers in"
      (is (nck:candidate-addressed-p reply))
      (is (equal '(("C1" "1.1")) seen))))
  (is-present (parent (ncs:slack-message-candidate
                       (slack-message "C1" "1.1" "the ask" :thread "1.1") :bot-user-id "UBOT"))
      "a thread's own first message replies to nothing"
    (is-source parent "reply_to_message_id" nil))
  (is (null (ncs:slack-message-candidate (slack-message "C1" "3.3" "x" :subtype "message_changed")))
      "an edit is bookkeeping")
  (is (null (ncs:slack-message-candidate (slack-message "C1" "3.4" "x" :subtype "channel_join"))))
  (is-present (bot (ncs:slack-message-candidate (slack-message "C1" "3.5" "beep" :bot-id "B2")))
      "another bot's line maps, and says it is a bot's"
    (is (eq t (gethash "is_bot" (gethash "source" bot)))))
  (is-present (shared (ncs:slack-message-candidate
                       (slack-message "C1" "4.4" "look" :subtype "file_share"
                                                        :files (vector (nlk:json-object
                                                                        "id" "F1" "name" "shot.png"
                                                                        "mimetype" "image/png" "size" 12
                                                                        "url_private_download" "https://files/F1")
                                                                       (nlk:json-object
                                                                        "id" "F2" "name" "a.zip"
                                                                        "mimetype" "application/zip"
                                                                        "url_private_download" "https://files/F2")))))
      "a file share carries what the lane can read, and only that"
    (let ((files (gethash "attachments" shared)))
      (is (= 1 (length files)))
      (let ((file (aref files 0)))
        (is-shape file ("id" "F1") ("download" "https://files/F1")
                  ("media_type" "image/png") ("filename" "shot.png") ("size" = 12))))))

(deftest channel-slack-command-is-the-line-it-spells ()
  (flet ((command (text)
           (ncs:slack-command-candidate
            (nlk:json-object "command" "/nodecode" "text" text "channel_id" "C1" "user_id" "U1"
                             "user_name" "kim" "team_id" "T1" "response_url" "http://r/1"))))
    (is (equal "/models grok" (gethash "text" (command "models grok"))))
    (is (equal "/stop" (gethash "text" (command "/stop"))))
    (is (equal "/help" (gethash "text" (command "  "))))
    (let ((candidate (command "help")))
      (is (nck:candidate-addressed-p candidate))
      (is-source candidate "response_url" "http://r/1" "chat_kind" "channel" "message_id" nil))
    (is-source (ncs:slack-command-candidate
                (nlk:json-object "text" "help" "channel_id" "D9" "user_id" "U1"))
               "chat_kind" "direct_message")))

(deftest channel-slack-button-press-is-the-kit-press ()
  (let ((press (ncs:slack-control-press
                (nlk:json-object "type" "block_actions" "trigger_id" "tr"
                                 "user" (nlk:json-object "id" "U1")
                                 "container" (nlk:json-object "message_ts" "5.5" "channel_id" "C1")
                                 "actions" (vector (nlk:json-object "action_id" "nodecode-0"
                                                                    "value" "nck:stop:s1"
                                                                    "action_ts" "6.6"))))))
    (is (equal '(:id "6.6" :data "nck:stop:s1" :user-id "U1" :message-id "5.5" :channel-id "C1")
               press)))
  (is (null (ncs:slack-control-press (nlk:json-object "type" "view_submission")))))

(deftest channel-slack-socket-frames ()
  (is (equal '((:hello)) (ncs:read-socket-frame (nlk:json-object "type" "hello"))))
  (is (equal '((:reconnect "refresh_requested"))
             (ncs:read-socket-frame (nlk:json-object "type" "disconnect" "reason" "refresh_requested"))))
  (is (eq :fatal (first (first (ncs:read-socket-frame
                                (nlk:json-object "type" "disconnect" "reason" "link_disabled"))))))
  ;; An envelope is acknowledged first, whatever it holds.
  (let* ((payload (nlk:json-object "type" "event_callback"))
         (actions (ncs:read-socket-frame (slack-envelope "e1" "events_api" payload))))
    (is (equal '(:ack "e1") (first actions)))
    (is (equal (list :envelope "events_api" payload) (second actions))))
  (is (equal '((:ack "e2")) (ncs:read-socket-frame (nlk:json-object "envelope_id" "e2" "type" "x"))))
  (is (null (ncs:read-socket-frame (nlk:json-object "type" "mystery"))))
  (is (equal "{\"envelope_id\":\"e1\"}" (nlk:encode-json-object (ncs:ack-frame "e1")))))
