;;;; events-test.lisp --- the driver's events as candidates, pure.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun wa-flag (candidate key)
  "CANDIDATE's boolean source member KEY: NCK:SOURCE-FIELD answers strings only."
  (gethash key (gethash "source" candidate)))

(deftest channel-whatsapp-numbers-and-jids ()
  (is (equal "15551234567" (ncw:normalize-phone "+1 (555) 123-4567")))
  (is (equal "123@lid" (ncw:normalize-phone " 123@LID ")) "a JID is kept, lowercased")
  (is (null (ncw:normalize-phone "  ")))
  (is (equal "15551234567@s.whatsapp.net" (ncw:user-jid "+15551234567")))
  (is (equal +wa-group+ (ncw:chat-jid +wa-group+)) "a group JID is itself")
  (is (ncw:whatsapp-mention-p "hey @15550000000 look" "15550000000"))
  (is (not (ncw:whatsapp-mention-p "hey @155500000001" "15550000000")) "no mention by a prefix")
  (is (equal "what is up" (ncw:whatsapp-strip-mention "@15550000000: what is up" '("15550000000" "9999@lid"))))
  (is (equal "and you" (ncw:whatsapp-strip-mention "@9999 and you" '("15550000000" "9999@lid")))
      "the LID mention goes too"))

(deftest channel-whatsapp-a-message-is-a-candidate ()
  (let ((candidate (ncw:whatsapp-message-candidate (wa-message +wa-kim+ "hi" :id "A1"))))
    (is-source candidate :text "hi" "platform" "whatsapp" "chat_kind" "direct_message"
               "channel_id" (wa-jid +wa-kim+) "user_id" +wa-kim+ "user_name" "kim"
               "message_id" "A1" "reply_to_message_id" nil)
    (is (null (wa-flag candidate "addressed")) "a direct message needs no address to be answered")
    (is (integerp (wa-flag candidate "sent_ms")) "when it was said rides along"))
  (let ((candidate (ncw:whatsapp-message-candidate
                    (wa-message +wa-kim+ "and why" :id "A2" :group +wa-group+
                                                   :quote (list "BOT7" t "the answer")))))
    (is-source candidate "chat_kind" "group" "channel_id" +wa-group+ "reply_to_message_id" "BOT7")
    (is (eq t (wa-flag candidate "addressed")) "quoting the bot addresses it")
    (is (equal "the answer" (nlk:json-value candidate :string "reply" "text")) "the quoted words come along")
    (is (equal +wa-bot+ (nlk:json-value candidate :string "reply" "user_id"))))
  (is (eq t (wa-flag (ncw:whatsapp-message-candidate
                      (wa-message +wa-kim+ "@15550000000 hi" :group +wa-group+ :mentions-me t))
                     "addressed"))
      "an @mention of the bot addresses it")
  (is (null (wa-flag (ncw:whatsapp-message-candidate (wa-message +wa-kim+ "chatter" :group +wa-group+))
                     "addressed"))
      "group chatter does not")
  (let* ((candidate (ncw:whatsapp-message-candidate
                     (wa-message +wa-kim+ "" :id "V1" :kind "voice"
                                             :media (wa-media "audio" :mime "audio/ogg; codecs=opus" :filename "voice.ogg"
                                                                      :seconds 4 :ptt t))))
         (attachment (aref (nlk:json-array candidate "attachments") 0)))
    (is (equal "audio/ogg" (gethash "media_type" attachment)) "the declared type, its parameters dropped")
    (is (equal "voice.ogg" (gethash "filename" attachment)))
    (is (eql 4 (gethash "seconds" attachment)) "a recording's length rides along")
    (is (equal "/v/t62/fake" (nlk:json-value attachment :string "media" "direct_path"))
        "and the driver's reference to download it by"))
  (let ((candidate (ncw:whatsapp-message-candidate
                    (wa-message +wa-kim+ "what does this say" :quote (list "Q1" nil "" (wa-media "audio"))))))
    (is (= 1 (length (nlk:json-array candidate "reply" "attachments"))) "a quoted file comes along"))
  (is (null (ncw:whatsapp-message-candidate (nlk:json-object "event" "message" "chat" "x")))
      "no sender, no candidate"))

(deftest channel-whatsapp-reactions-and-votes ()
  (let ((reaction (nlk:json-object "event" "reaction" "chat" (wa-jid +wa-kim+) "group" nil
                                   "from" (wa-person +wa-kim+) "push_name" "kim"
                                   "target_id" "BOT1" "target_from_me" t "emoji" "🔥")))
    (is-source (ncw:whatsapp-reaction-candidate reaction nil)
               :text "🔥" "reaction" "add" "reply_to_message_id" "BOT1" "user_id" +wa-kim+)
    (setf (gethash "emoji" reaction) "")
    (is-source (ncw:whatsapp-reaction-candidate reaction "🔥") :text "🔥" "reaction" "remove")
    (is (null (ncw:whatsapp-reaction-candidate reaction nil)) "a take-back nobody can name is nothing")
    (setf (gethash "emoji" reaction) "👍" (gethash "target_from_me" reaction) nil)
    (is (null (ncw:whatsapp-reaction-candidate reaction nil)) "a reaction on someone else's message is not ours"))
  (let* ((yes (nck:press-token "yes please"))
         (choices (list (cons "Yes" yes) (cons "Raise limit" "nck:lift")))
         (vote (nlk:json-object "event" "poll_vote" "chat" +wa-group+ "group" t
                                "from" (wa-person +wa-kim+) "push_name" "kim"
                                "poll_id" "BOT9" "options" (vector "Yes"))))
    (multiple-value-bind (kind candidate) (ncw:whatsapp-vote-press vote choices)
      (is (eq :line kind) "a vote on a choice says its line")
      (is-source candidate :text "yes please" "press_data" yes "reply_to_message_id" "BOT9" "chat_kind" "group")
      (is (eq t (wa-flag candidate "pressed"))))
    (setf (gethash "options" vote) (vector "Raise limit"))
    (multiple-value-bind (kind payload) (ncw:whatsapp-vote-press vote choices)
      (is (eq :control kind) "a vote on any other control is the kit's press")
      (is (equal "nck:lift" (getf payload :data)))
      (is (equal +wa-kim+ (getf payload :user-id))))
    (setf (gethash "options" vote) #())
    (is (null (ncw:whatsapp-vote-press vote choices)) "a vote taken back presses nothing")))

(deftest channel-whatsapp-policy-mentions-and-chats ()
  (let ((policy (ncw:whatsapp-inbound-policy :allowed-users (list +wa-kim+) :require-mention t
                                             :allow-private-chats t :allow-group-chats t
                                             :bot-ids (list +wa-bot+) :mention-patterns '("nodecode"))))
    (is (eq t (funcall (nck::inbound-policy-mention-test policy) "hi @15550000000" nil)) "the bot's @number")
    (is-inbound policy (ncw:whatsapp-message-candidate (wa-message +wa-kim+ "hi")) :answer)
    (is-inbound policy (ncw:whatsapp-message-candidate (wa-message +wa-kim+ "chatter" :group +wa-group+)) :observe
                "mention_required" "group chatter is context")
    (is-inbound policy (ncw:whatsapp-message-candidate (wa-message +wa-kim+ "hey Nodecode, look" :group +wa-group+))
                :answer nil "a wake word addresses the bot")
    (is-inbound policy (ncw:whatsapp-message-candidate (wa-message +wa-lee+ "hi")) :reject "user_not_allowed")))
