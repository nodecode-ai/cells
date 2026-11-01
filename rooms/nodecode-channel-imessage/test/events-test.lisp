;;;; events-test.lisp --- BlueBubbles messages as candidates, pure.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun bb-message (&rest pairs &key (guid "G1") (chat "iMessage;-;+15550001") (sender "+15550001")
                                 text from-me root reply-to type target attachments emoji
                                 &allow-other-keys)
  (declare (ignore pairs))
  (nlk:json-object "guid" guid "originalROWID" 7 "text" text
                   "handle" (nlk:json-object "address" sender)
                   "isFromMe" (and from-me t) "itemType" 0
                   "associatedMessageGuid" (and target (format nil "p:0/~a" target))
                   "associatedMessageType" type
                   :opt "associatedMessageEmoji" emoji
                   "threadOriginatorGuid" root "replyToGuid" reply-to
                   "attachments" (coerce attachments 'vector)
                   "chats" (vector (nlk:json-object "guid" chat))))

(deftest channel-imessage-handles-compare-one-way ()
  (is (equal "+15551234567" (nci:normalize-handle " +1 (555) 123-4567 ")))
  (is (equal "kim@icloud.com" (nci:normalize-handle "Kim@iCloud.com")))
  (is (nci:direct-chat-p "iMessage;-;+15550001"))
  (is (not (nci:direct-chat-p "iMessage;+;chat42"))))

(deftest channel-imessage-a-message-is-a-candidate ()
  (let ((candidate (nci:imessage-message-candidate
                    (bb-message :text (format nil "hi ~a there" (code-char #xFFFC)) :sender "+1 555 000 1")
                    :user-name "Kim")))
    (is (equal "hi  there" (gethash "text" candidate)) "the attachment mark is gone")
    (is-source candidate "platform" "imessage" "chat_kind" "direct_message" "channel_id" "iMessage;-;+15550001"
               "user_id" "+15550001" "user_name" "Kim" "message_id" "G1" "reply_to_message_id" nil
               "addressed" nil))
  (let* ((mine (lambda (id) (equal id "BOT1")))
         (in-thread (nci:imessage-message-candidate
                     (bb-message :chat "iMessage;+;chat42" :text "and more" :root "BOT1") :bot-message-p mine))
         (quoting (nci:imessage-message-candidate
                   (bb-message :chat "iMessage;+;chat42" :text "re" :root "ASK" :reply-to "BOT1") :bot-message-p mine))
         (plain (nci:imessage-message-candidate (bb-message :chat "iMessage;+;chat42" :text "chat") :bot-message-p mine)))
    (is-source in-thread "chat_kind" "thread" "reply_to_message_id" "BOT1")
    (is (nck:candidate-addressed-p in-thread) "a line in the bot's thread is said to it")
    (is-source quoting "reply_to_message_id" "ASK")
    (is (nck:candidate-addressed-p quoting) "so is a reply quoting the bot")
    (is-source plain "chat_kind" "group")
    (is (not (nck:candidate-addressed-p plain))))
  (is (null (nci:imessage-message-candidate (bb-message :text "mine" :from-me t))) "the bot's own is no ask")
  (is (null (nci:imessage-message-candidate (bb-message :text "" :type "like" :target "X"))) "a tapback is no ask")
  (is (null (nci:imessage-message-candidate (bb-message :text "  "))) "nothing said is no ask"))

(deftest channel-imessage-files-come-as-their-download ()
  (let* ((candidate (nci:imessage-message-candidate
                     (bb-message :text "" :attachments (list (nlk:json-object "guid" "A1" "mimeType" "image/heic"
                                                                               "transferName" "IMG_1.HEIC" "totalBytes" 900)
                                                             (nlk:json-object "guid" "A2" "mimeType" "audio/x-caf"
                                                                               "transferName" "Audio Message.caf")
                                                             (nlk:json-object "guid" "A3" "mimeType" "text/plain"
                                                                               "transferName" "x.pluginPayloadAttachment")))))
         (files (nlk:json-array candidate "attachments")))
    (is (= 2 (length files)) "a link preview's payload is no file")
    (is (equal '("image/jpeg" "IMG_1.jpg" 900 "A1")
               (list (gethash "media_type" (aref files 0)) (gethash "filename" (aref files 0))
                     (gethash "size" (aref files 0)) (gethash "download" (aref files 0)))))
    (is (equal '("audio/mpeg" "Audio Message.mp3")
               (list (gethash "media_type" (aref files 1)) (gethash "filename" (aref files 1)))))))

(deftest channel-imessage-tapbacks-read-every-spelling ()
  (flet ((tap (&rest keys) (multiple-value-list (nci:imessage-tapback (apply #'bb-message keys)))))
    (is (equal '("👍" t "X") (tap :type "2001" :target "X")))
    (is (equal '("👍" t "X") (tap :type 2001 :target "X")))
    (is (equal '("❤️" nil "X") (tap :type "3000" :target "X")))
    (is (equal '("😂" t "X") (tap :type "laugh" :target "X")))
    (is (equal '("‼️" nil "X") (tap :type "-emphasize" :target "X")))
    (is (equal '("🔥" t "X") (tap :type "2006" :target "X" :emoji "🔥")) "an emoji tapback is its emoji")
    (is (equal '(nil) (tap :type "1000" :target "X")) "a sticker is no tapback")
    (is (equal '(nil) (tap :text "hi")) "words are no tapback"))
  (let ((candidate (nci:imessage-reaction-candidate (bb-message :type "3001" :target "BOT1" :guid "T1"))))
    (is (equal "👍" (gethash "text" candidate)))
    (is-source candidate "reply_to_message_id" "BOT1" "reaction" "remove" "user_id" "+15550001"))
  (is (equal "emphasize" (nci:emoji-tapback "👀")) "the kit's eyes are the emphasis")
  (is (equal "like" (nci:emoji-tapback "✅")))
  (is (equal "dislike" (nci:emoji-tapback "❌")))
  (is (equal "love" (nci:emoji-tapback "❤️")))
  (is (null (nci:emoji-tapback "🦀")) "Messages leaves six"))

(deftest channel-imessage-names-call-the-bot ()
  (is (nci:imessage-mention-p "hey @Nodecode, look" '("nodecode")))
  (is (nci:imessage-mention-p "nodecode: look" '("nodecode")))
  (is (not (nci:imessage-mention-p "nodecoder look" '("nodecode"))) "a longer word is not the name")
  (is (not (nci:imessage-mention-p "anything" '())))
  (is (equal "what is up" (nci:imessage-strip-mention "@nodecode: what is up" '("nodecode"))))
  (is (equal "so, what is up" (nci:imessage-strip-mention "so, nodecode what is up" '("nodecode")))))

(deftest channel-imessage-choices-are-numbered-lines ()
  (let ((items (nci:choice-items '((("Yes" "d1" :primary) ("No" "d2" :secondary) ("Off" "d3" :secondary t))
                                   ("Stop" "stop:x" :danger)
                                   (:menu "pick" (("Opus" "m1" "big" t) ("Flash" "m2" nil nil)))))))
    (is (equal '(("Yes" "d1") ("No" "d2") ("Stop" "stop:x") ("Opus - big (current)" "m1") ("Flash" "m2")) items))
    (is (equal '("No" "d2") (nci:choice-picked "2" items)))
    (is (equal '("No" "d2") (nci:choice-picked " #2. " items)))
    (is (equal '("Stop" "stop:x") (nci:choice-picked "stop" items)) "a label as written picks it")
    (is (null (nci:choice-picked "9" items)))
    (is (null (nci:choice-picked "yes please" items)) "other words pick nothing"))
  (is (null (nci:choice-items :clear))))
