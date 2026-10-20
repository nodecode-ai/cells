;;;; events.lisp --- the driver's events as the kit's candidates. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A message the driver reports becomes the shared candidate shape: the chat
;;;; is the room, the sender the speaker, a quote the reply gesture. Nothing
;;;; here touches the driver: who the account is, what a poll's options say
;;;; and what someone reacted with before are the adapter's to know, passed in.
;;;;
;;;; Names. WhatsApp addresses a person two ways, by phone number
;;;; (15551234567@s.whatsapp.net) and by LID (an opaque number@lid that groups
;;;; increasingly use). The driver names each person by the phone number's
;;;; digits when it knows them -- what an allowlist and an owner list say --
;;;; and by the LID JID only when it does not. A chat is its JID: a group's
;;;; ends in @g.us, a direct chat is the person's own phone JID.
;;;;
;;;; Threads. WhatsApp has none: a quote ("reply") is the only pointer one
;;;; message keeps to another, which is exactly the kit's reply gesture -- a
;;;; reply to anything a lane posted continues that lane.

(in-package #:nodecode-channel-whatsapp)

(defun normalize-phone (value)
  "VALUE as a person or chat is named here: a phone number's digits
(\"+1 (555) 123-4567\" is \"15551234567\"), a JID as written, lowercased;
NIL for nothing."
  (let ((text (nlk:trimmed (or value ""))))
    (cond ((zerop (length text)) nil)
          ((find #\@ text) (string-downcase text))
          (t (let ((digits (remove-if-not #'digit-char-p text)))
               (and (plusp (length digits)) digits))))))

(defun user-jid (id)
  "The JID a person ID names: digits are a phone number's JID, a JID is itself."
  (let ((id (normalize-phone id)))
    (cond ((null id) nil)
          ((find #\@ id) id)
          (t (format nil "~a@s.whatsapp.net" id)))))

(defun chat-jid (id)
  "The JID a chat ID names: a direct chat is its person's JID, a group its own."
  (user-jid id))

(defun jid-user (jid)
  "The part of JID before its @: what a mention spells after its @."
  (let ((jid (or jid "")))
    (subseq jid 0 (or (position #\@ jid) (length jid)))))

(defun whatsapp-mention-p (text target)
  "Whether TEXT mentions TARGET, the bot's number: @<number> in the words."
  (and (stringp text) (stringp target) (plusp (length target))
       (ppcre:scan (format nil "@~a(?!\\d)" (ppcre:quote-meta-chars (jid-user target))) text)
       t))

(defun whatsapp-strip-mention (text ids)
  "TEXT with every @mention of IDS -- the bot's number and LID -- removed,
with the colon or comma a mention opening a line carries: the model reads
the ask, not the handle that routed it."
  (let ((text (or text "")))
    (dolist (id ids)
      (when (plusp (length (jid-user id)))
        (setf text (ppcre:regex-replace-all
                    (format nil "@~a(?!\\d)[:,]?" (ppcre:quote-meta-chars (jid-user id))) text ""))))
    (nlk:trimmed (ppcre:regex-replace-all "  +" text " "))))

;;; --- messages --------------------------------------------------------------------

(defun person-id (person)
  (normalize-phone (nlk:json-value person :string "id")))

(defun media-attachments (media id)
  "The file a message carries as the candidate's attachments array, or NIL:
its type, name and size as WhatsApp declared them, and the driver's
reference to it, which the adapter's fetcher downloads by."
  (when (hash-table-p media)
    (vector (nlk:json-object
             "id" (format nil "~a:~a" id (nlk:json-value media :string "type"))
             "media" media
             "media_type" (let ((mime (nlk:json-value media :string "mime")))
                            (if (plusp (length mime))
                                (string-trim " " (subseq mime 0 (or (position #\; mime) (length mime))))
                                "application/octet-stream"))
             "filename" (or (nlk:json-value media :string "filename") "")
             "size" (or (nlk:json-value media :integer "file_length") 0)
             :when (nlk:json-value media :integer "seconds")
             "seconds" (nlk:json-value media :integer "seconds")))))

(defun speaker-name (event)
  (let ((name (nlk:json-value event :text "push_name")))
    (if (plusp (length name)) name (person-id (gethash "from" event)))))

(defun whatsapp-message-candidate (event &key names)
  "One message EVENT the driver reported as the shared candidate, or NIL
without a chat, a sender or an id. NAMES answers a person id's known name,
for a quoted message the driver could not name."
  (let* ((chat (nlk:json-value event :string "chat"))
         (id (nlk:json-value event :string "id"))
         (from (gethash "from" event))
         (user (and (hash-table-p from) (person-id from)))
         (quoted (nlk:json-value event :object "quote"))
         (quoted-from (and quoted (gethash "from" quoted)))
         (quoted-user (and (hash-table-p quoted-from) (person-id quoted-from)))
         (group-p (and (gethash "group" event) t)))
    (when (and chat id user)
      (nlk:json-object
       "text" (or (nlk:json-value event :string "text") "")
       "attachments" (media-attachments (gethash "media" event) id)
       "reply" (and quoted
                    (nlk:json-object
                     "id" (nlk:json-value quoted :string "id")
                     "user_id" quoted-user
                     "user_name" (or (let ((name (nlk:json-value quoted :text "push_name")))
                                       (and (plusp (length name)) name))
                                     (and names quoted-user (funcall names quoted-user))
                                     quoted-user)
                     "text" (or (nlk:json-value quoted :string "text") "")
                     ;; A reply to a voice note is about what it says.
                     "attachments" (media-attachments (gethash "media" quoted)
                                                      (nlk:json-value quoted :string "id"))))
       "source"
       (nlk:json-object
        "platform" "whatsapp"
        "chat_kind" (if group-p "group" "direct_message")
        "channel_id" chat
        "user_id" user
        "user_name" (speaker-name event)
        "user_jid" (nlk:json-value from :string "jid")
        "message_id" id
        ;; The reply gesture: a quote of anything a lane posted continues it.
        "reply_to_message_id" (and quoted (nlk:json-value quoted :string "id"))
        ;; Said to the bot: it is @mentioned, or the line quotes one of its
        ;; messages -- the platform routed it to us, as a reply on Telegram.
        "addressed" (and (or (gethash "mentions_me" event)
                             (and quoted (gethash "from_me" quoted)))
                         t)
        "forwarded" (and (gethash "forwarded" event) t)
        ;; When it was said, for the catch-up after the device was away.
        "sent_ms" (nlk:json-value event :integer "timestamp_ms")
        "is_bot" nil)))))

;;; --- reactions -------------------------------------------------------------------

(defun whatsapp-reaction-candidate (event previous)
  "One reaction EVENT on one of the bot's messages as the kit reads a
reaction -- the emoji its text, the message it sits on its reply gesture --
or NIL for a reaction on anybody else's message, or one taken back whose
emoji (PREVIOUS, the one that person left before) nobody knows."
  ;; WhatsApp keeps one reaction a person: a new one replaces theirs, an
  ;; empty one takes it back without saying what it was.
  (let* ((chat (nlk:json-value event :string "chat"))
         (from (gethash "from" event))
         (user (and (hash-table-p from) (person-id from)))
         (target (nlk:json-value event :string "target_id"))
         (emoji (or (nlk:json-value event :string "emoji") ""))
         (removed-p (zerop (length emoji))))
    (when (and chat user target (gethash "target_from_me" event)
               (not (gethash "from_me" event))
               (or (not removed-p) (plusp (length previous))))
      (nlk:json-object
       "text" (if removed-p previous emoji)
       "source"
       (nlk:json-object
        "platform" "whatsapp"
        "chat_kind" (if (gethash "group" event) "group" "direct_message")
        "channel_id" chat
        "user_id" user
        "user_name" (speaker-name event)
        "reply_to_message_id" target
        "reaction" (if removed-p "remove" "add")
        "addressed" t)))))

;;; --- choices ---------------------------------------------------------------------
;;; WhatsApp draws no buttons for an account like this one. It draws polls: one
;;; tap on an option votes. So the choices a message carries -- an answer's
;;; ANSWER-CHOICES, a dialog's rows and menus -- ride under it as a poll, and a
;;; vote is the press. The book keeps each poll's options against the data
;;; their presses carry (PLANS.LISP, POLL-OF).

(defun whatsapp-vote-press (event choices)
  "One poll_vote EVENT on a poll the bot posted, CHOICES its (LABEL . DATA)
options: (values :LINE CANDIDATE) for a vote on a choice -- the voter saying
its line where the poll is, a reply to it -- or (values :CONTROL PAYLOAD) for
any other control, the kit's normal press; NIL for a vote taken back, on an
option the poll never had, or on a poll that is not the bot's."
  (let* ((chat (nlk:json-value event :string "chat"))
         (from (gethash "from" event))
         (user (and (hash-table-p from) (person-id from)))
         (poll (nlk:json-value event :string "poll_id"))
         (picked (loop for name across (nlk:json-array event "options")
                       for hit = (assoc name choices :test #'equal)
                       when hit return hit))
         (data (cdr picked)))
    (when (and chat user poll picked (not (gethash "from_me" event)))
      (let ((line (said-line data)))
        (if line
            (values :line
                    (nlk:json-object
                     "text" line
                     "source"
                     (nlk:json-object
                      "platform" "whatsapp"
                      "chat_kind" (if (gethash "group" event) "group" "direct_message")
                      "channel_id" chat
                      "user_id" user
                      "user_name" (speaker-name event)
                      "pressed" t
                      "press_data" data
                      "reply_to_message_id" poll
                      "addressed" t
                      "is_bot" nil)))
            (values :control
                    (list :id (format nil "~a:~a" poll user)
                          :data data
                          :user-id user
                          :user-name (speaker-name event)
                          :message-id poll
                          :channel-id chat)))))))

;;; --- the policy ------------------------------------------------------------------

(defun whatsapp-inbound-policy (&key allowed-chats allowed-users allowed-roles require-mention
                                     free-response-chats require-mention-chats ignored-chats
                                     mention-patterns allow-private-chats allow-group-chats
                                     bot-ids &allow-other-keys)
  "The kit policy over WhatsApp's chats: chats by JID, people by phone
number, the mention as the bot's @number or a wake word. BOT-IDS is the
account's number and LID, or a function answering them when the test runs:
the adapter learns them only once its driver has started."
  ;; The bot's own messages never come back to it -- the driver hears only
  ;; what other devices send -- so no self id is named: in the chat with
  ;; yourself (self_chat) the owner's number is the bot's own.
  (make-inbound-policy
   :allowed-channels allowed-chats
   :allowed-users allowed-users
   :allowed-roles allowed-roles
   :dm-policy (if allow-private-chats :enabled :disabled)
   :group-policy (if allow-group-chats :enabled :disabled)
   :require-mention require-mention
   :free-response-channels free-response-chats
   :require-mention-channels require-mention-chats
   :ignored-channels ignored-chats
   ;; The target only has to be there: the test reads BOT-IDS itself.
   :mention-target "the linked account"
   :mention-test (lambda (text target)
                   (declare (ignore target))
                   (or (some (lambda (id) (whatsapp-mention-p text id))
                             (if (functionp bot-ids) (funcall bot-ids) bot-ids))
                       (some (lambda (pattern)
                               (and (stringp pattern) (plusp (length pattern))
                                    (search pattern (or text "") :test #'char-equal)))
                             mention-patterns)))
   :channel-match :exact
   :reasons '(:channel-not-allowed "chat_not_allowed"
              :dm-disabled "private_chats_disabled"
              :group-disabled "group_chats_disabled"
              :mention-required-without-user "mention_required_without_bot_number")))
