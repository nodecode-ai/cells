;;;; events-test.lisp --- Matrix events as candidates, pure.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun matrix-flag (candidate key)
  "CANDIDATE's boolean source member KEY: NCK:SOURCE-FIELD answers strings only."
  (gethash key (gethash "source" candidate)))

(deftest channel-matrix-words-read-a-pill-as-its-id ()
  (let ((content (nlk:json-object
                  "msgtype" "m.text" "body" "bot: what &amp; why"
                  "format" "org.matrix.custom.html"
                  "formatted_body" "<mx-reply><blockquote>old words</blockquote></mx-reply><a href=\"https://matrix.to/#/%40bot%3Afake\">bot</a>: what &amp; <b>why</b><br>next")))
    (is (equal (format nil "@bot:fake: what & why~%next") (ncm:matrix-plain-text content))
        "the quoted reply is dropped, the pill is the id, the tags and entities read plain")
    (is (ncm:matrix-mention-p (ncm:matrix-plain-text content) "@bot:fake"))
    (is (equal (format nil "what & why~%next")
               (ncm:matrix-strip-mention (ncm:matrix-plain-text content) "@bot:fake"))))
  (let ((content (nlk:json-object "msgtype" "m.text"
                                  "body" (format nil "> <@kim:fake> earlier~%> more~%~%the reply")
                                  "m.relates_to" (nlk:json-object "m.in_reply_to" (nlk:json-object "event_id" "$a")))))
    (is (equal "the reply" (ncm:matrix-plain-text content)) "an old client's reply fallback is dropped")))

(deftest channel-matrix-a-bare-command-is-addressed ()
  (flet ((addressed-p (event &rest keys)
           (matrix-flag (apply #'ncm:matrix-message-candidate event "!room:fake" :bot-user-id "@bot:fake" keys)
                        "addressed"))
         (ours (room name) (and (equal room "!room:fake") (equal name "models"))))
    (is (eq t (addressed-p (matrix-text "@kim:fake" "/models grok" :id "$1") :command-p #'ours))
        "a bare slash line naming the bot's command is addressed to it")
    (is (null (addressed-p (matrix-text "@kim:fake" "/models" :id "$2")))
        "without the bot's word on whose it is, it is not")
    (is (null (addressed-p (matrix-text "@kim:fake" "/shrug" :id "$3") :command-p #'ours))
        "a command the bot lacks is words")
    (is (null (addressed-p (matrix-text "@kim:fake" "/models" :id "$4" :mentions '("@other:fake"))
                           :command-p #'ours))
        "a line mentioning someone else is theirs")
    (is (null (addressed-p (matrix-text "@kim:fake" "/models" :id "$5" :thread "$root")
                           :command-p #'ours :bot-event-p (constantly nil)))
        "a line in a thread the bot is not in is that thread's")
    (is (null (addressed-p (matrix-text "@kim:fake" "see /models" :id "$6") :command-p #'ours))
        "a line that only names a command is words")))

(deftest channel-matrix-a-message-is-a-candidate ()
  (let ((candidate (ncm:matrix-message-candidate
                    (matrix-text "@kim:fake" "hi" :id "$1") "!dm:fake"
                    :bot-user-id "@bot:fake" :direct-p t :user-name "kim")))
    (is-source candidate :text "hi" "platform" "matrix" "chat_kind" "direct_message"
               "channel_id" "!dm:fake" "user_id" "@kim:fake" "user_name" "kim"
               "message_id" "$1" "reply_to_message_id" nil)
    (is (null (matrix-flag candidate "addressed")) "a direct message needs no address to be answered")
    (is (null (matrix-flag candidate "is_bot"))))
  (let ((candidate (ncm:matrix-message-candidate
                    (matrix-text "@kim:fake" "and more" :id "$3" :thread "$root") "!room:fake"
                    :bot-user-id "@bot:fake"
                    :bot-event-p (lambda (room event) (and (equal room "!room:fake") (equal event "$root"))))))
    (is-source candidate "chat_kind" "thread" "reply_to_message_id" "$root")
    (is (eq t (matrix-flag candidate "addressed")) "a line in a thread the bot speaks in is addressed"))
  (let ((candidate (ncm:matrix-message-candidate
                    (matrix-text "@kim:fake" "look" :id "$4" :mentions '("@bot:fake")) "!room:fake"
                    :bot-user-id "@bot:fake")))
    (is-source candidate "chat_kind" "channel")
    (is (eq t (matrix-flag candidate "addressed")) "m.mentions naming the bot addresses the line"))
  (let ((edit (matrix-text "@kim:fake" "* fixed" :id "$5")))
    (setf (gethash "m.relates_to" (gethash "content" edit))
          (nlk:json-object "rel_type" "m.replace" "event_id" "$1"))
    (is (null (ncm:matrix-message-candidate edit "!room:fake")) "an edit is no ask"))
  (let ((notice (matrix-text "@other:fake" "beep" :id "$6")))
    (setf (gethash "msgtype" (gethash "content" notice)) "m.notice")
    (is (eq t (matrix-flag (ncm:matrix-message-candidate notice "!room:fake") "is_bot"))
        "m.notice is how a bot speaks"))
  (let ((picture (matrix-event "@kim:fake" "m.room.message"
                               (nlk:json-object "msgtype" "m.image" "body" "cat.png" "url" "mxc://fake/c1"
                                                "info" (nlk:json-object "mimetype" "image/png" "size" 42))
                               :id "$7")))
    (let ((candidate (ncm:matrix-message-candidate picture "!dm:fake" :direct-p t)))
      (is (equal "" (gethash "text" candidate)) "a bare body is the file's name, not words")
      (is (equal "mxc://fake/c1" (nlk:json-value (aref (gethash "attachments" candidate) 0) :string "download"))))))

(deftest channel-matrix-a-reaction-presses-a-control ()
  (let* ((controls '(("Stop" "stop:lane" :danger) ("Details" "details:lane")))
         (controls-of (lambda (room event) (and (equal room "!r:fake") (equal event "$card") controls))))
    (flet ((reaction (key target)
             (matrix-event "@kim:fake" "m.reaction"
                           (nlk:json-object "m.relates_to" (nlk:json-object "rel_type" "m.annotation"
                                                                            "event_id" target "key" key))
                           :id "$react")))
      (is (equal '(:id "$react" :data "stop:lane" :user-id "@kim:fake" :message-id "$card" :channel-id "!r:fake")
                 (ncm:matrix-control-press (reaction (string (code-char #x23F9)) "$card") "!r:fake" controls-of))
          "a glyph without its variation selector still presses")
      (is (equal "details:lane"
                 (getf (ncm:matrix-control-press (reaction (ncm:control-glyph (second controls) 1) "$card")
                                                 "!r:fake" controls-of)
                       :data)))
      (is (null (ncm:matrix-control-press (reaction "👍" "$card") "!r:fake" controls-of)) "any other reaction is no press")
      (is (null (ncm:matrix-control-press (reaction (ncm:control-glyph (first controls) 0) "$other")
                                          "!r:fake" controls-of))
          "a message without controls takes no press"))))

(deftest channel-matrix-an-invite-names-its-sender ()
  (let ((invite (nlk:json-object "invite_state"
                                 (nlk:json-object "events"
                                                  (vector (nlk:json-object "type" "m.room.member" "state_key" "@bot:fake"
                                                                           "sender" "@kim:fake"
                                                                           "content" (nlk:json-object "membership" "invite"
                                                                                                      "is_direct" t)))))))
    (is (equal '("@kim:fake" t) (multiple-value-list (ncm:invite-sender invite "@bot:fake"))))
    (is (null (ncm:invite-sender invite "@other:fake")))))
