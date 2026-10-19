;;;; events.lisp --- BlueBubbles messages as the kit's candidates. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A message the server read out of the Mac's Messages database becomes the
;;;; shared candidate shape (the kit names the room and the lane from it); a
;;;; tapback on one of the bot's messages becomes the kit's reaction; a reply
;;;; that picks one of the numbered choices under the bot's message becomes
;;;; the press of that choice. Nothing here touches the network: a person's
;;;; contact name, which messages are the bot's and which choices a chat has
;;;; open are the adapter's to know, passed in.
;;;;
;;;; Chats. A chat's guid says its kind: `iMessage;-;+15551234567' is a
;;;; direct message with one person, `iMessage;+;chat123...' a group (SMS and
;;;; RCS chats read the same, under their own service). A person is their
;;;; handle -- a phone number or an email address -- compared one way
;;;; (NORMALIZE-HANDLE), so an allowed list may write a number as a person
;;;; would.
;;;;
;;;; Threads. iMessage's inline reply puts a message in the thread of the
;;;; message it answers, every one naming the thread's first message as its
;;;; threadOriginatorGuid. The kit's lane is the same shape -- one ask and what
;;;; follows from it -- so a line in a thread is normalized as a reply to the
;;;; thread's root.

(in-package #:nodecode-channel-imessage)

;;; --- people and chats -------------------------------------------------------------

(defun normalize-handle (handle)
  "HANDLE, a phone number or an email address, the one way handles are
compared: an email address lowercased, a phone number its leading + and its
digits."
  (let ((text (nlk:trimmed (or handle ""))))
    (cond ((find #\@ text) (string-downcase text))
          ((some #'digit-char-p text)
           (concatenate 'string (if (eql 0 (position #\+ text)) "+" "")
                        (remove-if-not #'digit-char-p text)))
          (t text))))

(defun direct-chat-p (chat-guid)
  "Whether CHAT-GUID names a direct message: its middle part is -, a group's +."
  (and (stringp chat-guid) (search ";-;" chat-guid) t))

(defun message-chat-guid (message)
  "The guid of the chat MESSAGE was said in."
  (let ((chats (nlk:json-array message "chats")))
    (and (plusp (length chats)) (nlk:json-value (aref chats 0) :string "guid"))))

(defun message-sender (message)
  "The handle MESSAGE came from, normalized, or NIL."
  (nlk:when-let (address (nlk:json-value message :string "handle" "address"))
    (normalize-handle address)))

(defun imessage-text (message)
  "MESSAGE's words: the server's text without the object-replacement marks
Messages puts where an attachment sits, trimmed."
  (nlk:trimmed (remove (code-char #xFFFC) (or (nlk:json-value message :string "text") ""))))

;;; --- tapbacks -----------------------------------------------------------------------

(defparameter +tapbacks+
  '(("love" . "❤️") ("like" . "👍") ("dislike" . "👎") ("laugh" . "😂")
    ("emphasize" . "‼️") ("question" . "❓"))
  "The six tapbacks, in Messages' own order (associated types 2000-2005), and
the emoji each is drawn as.")

(defparameter +emoji-tapbacks+
  '(("❤" . "love") ("♥" . "love") ("👍" . "like") ("✅" . "like") ("👎" . "dislike")
    ("❌" . "dislike") ("😂" . "laugh") ("‼" . "emphasize") ("❗" . "emphasize")
    ("👀" . "emphasize") ("❓" . "question") ("?" . "question"))
  "The tapback each emoji the kit marks with is left as: Messages carries six,
so the kit's eyes (seen) are the emphasis, its tick (done) the thumb up and
its cross (failed) the thumb down.")

(defun bare-emoji (emoji)
  "EMOJI without variation selectors: two clients spell one emoji apart."
  (remove (code-char #xFE0F) (or emoji "")))

(defun emoji-tapback (emoji)
  "The tapback EMOJI is left as, or NIL for one Messages cannot leave."
  (cdr (assoc (bare-emoji emoji) +emoji-tapbacks+ :test #'string=)))

(defun tapback-emoji (tapback)
  "The emoji TAPBACK, one of the six names, is drawn as."
  (cdr (assoc tapback +tapbacks+ :test #'string=)))

(defun associated-target (guid)
  "The message an associated GUID points at: `p:0/GUID' names part 0 of
GUID, `bp:GUID' the whole of it."
  (cond ((null guid) nil)
        ((uiop:string-prefix-p "bp:" guid) (subseq guid 3))
        ((and (uiop:string-prefix-p "p:" guid) (position #\/ guid)) (subseq guid (1+ (position #\/ guid))))
        (t guid)))

(defun imessage-tapback (message)
  "(values EMOJI ADDED-P TARGET) when MESSAGE is a tapback -- the emoji it
shows, whether it was left or taken back, the guid of the message it is on --
else NIL."
  ;; The server spells the associated type as a name (love, -love) or as
  ;; Messages' own number (2000-2005 left, 3000-3005 taken back; 2006/3006 an
  ;; emoji of the person's choosing, iOS 18, the emoji alongside).
  (let* ((raw (nlk:json-value message :any "associatedMessageType"))
         (type (cond ((integerp raw) raw)
                     ((and (stringp raw) (plusp (length raw)) (every #'digit-char-p raw))
                      (parse-integer raw))
                     ((stringp raw) raw)))
         (target (associated-target (nlk:json-value message :string "associatedMessageGuid")))
         (emoji (nlk:json-value message :string "associatedMessageEmoji")))
    (when target
      (multiple-value-bind (name added-p)
          (cond ((and (integerp type) (<= 2000 type 2005)) (values (car (nth (- type 2000) +tapbacks+)) t))
                ((and (integerp type) (<= 3000 type 3005)) (values (car (nth (- type 3000) +tapbacks+)) nil))
                ((and (eql type 2006) emoji) (values :emoji t))
                ((and (eql type 3006) emoji) (values :emoji nil))
                ((stringp type)
                 (let ((name (string-left-trim "-" type)))
                   (cond ((assoc name +tapbacks+ :test #'string=)
                          (values name (not (uiop:string-prefix-p "-" type))))
                         ((and emoji (equal name "emoji"))
                          (values :emoji (not (uiop:string-prefix-p "-" type))))))))
        (when name
          (values (if (eq name :emoji) emoji (tapback-emoji name)) added-p target))))))

;;; --- mentions --------------------------------------------------------------------
;;; iMessage has no bot accounts and no mention a client spells as an id: in a
;;; group the bot is called by a name of the operator's choosing (the
;;; section's mention_names), typed with or without @.

(defun mention-regex (names)
  (format nil "(?i)(^|[^\\w@])@?(~{~a~^|~})(?!\\w)" (mapcar #'ppcre:quote-meta-chars names)))

(defun imessage-mention-p (text names)
  "Whether TEXT calls the bot by one of NAMES."
  (and (stringp text) names (ppcre:scan (mention-regex names) text) t))

(defun imessage-strip-mention (text names)
  "TEXT without the name that called the bot, and the colon or comma a person
puts after a name that opens a line: the model reads the ask."
  (nlk:trimmed
   (if names
       (replace-all " {2,}" (replace-all (concatenate 'string (mention-regex names) "[:,]?") (or text "") "\\1")
                    " ")
       (or text ""))))

;;; --- messages --------------------------------------------------------------------

(defun said-p (message)
  "Whether MESSAGE is something a person said: not a tapback, a sticker, a
group event, an unsent message or one of the bot's own."
  (and (not (nlk:json-value message :boolean "isFromMe"))
       (null (nlk:json-value message :any "associatedMessageGuid"))
       (not (nlk:json-value message :boolean "isSystemMessage"))
       (member (or (nlk:json-value message :integer "itemType") 0) '(0))
       (null (nlk:json-value message :integer "dateRetracted"))))

(defparameter +converted-types+
  '(("image/heic" . "image/jpeg") ("image/heif" . "image/jpeg") ("audio/x-caf" . "audio/mpeg"))
  "What the server's download turns a file of each type into: Messages keeps
photos as HEIC and voice notes as CAF, the download serves JPEG and MP3.")

(defun imessage-attachments (message)
  "MESSAGE's files as the candidate's attachments array, or NIL: each one's
guid, the type its download comes as, its name and size. A link preview's
payload is no file. The fetch thunk is the adapter's to add."
  (let ((files (loop for attachment across (nlk:json-array message "attachments")
                     for guid = (nlk:json-value attachment :string "guid")
                     for name = (or (nlk:json-value attachment :string "transferName") "")
                     for type = (nlk:json-value attachment :string "mimeType")
                     when (and guid (not (search "pluginPayloadAttachment" name))
                               (not (nlk:json-value attachment :boolean "hideAttachment")))
                       collect (let ((served (or (cdr (assoc type +converted-types+ :test #'equal)) type)))
                                 (nlk:json-object
                                  "id" guid
                                  "download" guid
                                  "media_type" served
                                  "filename" (if (equal served type)
                                                 name
                                                 (format nil "~a.~a" (pathname-name name)
                                                         (if (uiop:string-prefix-p "audio/" served) "mp3" "jpg")))
                                  "size" (or (nlk:json-value attachment :integer "totalBytes") 0))))))
    (and files (coerce files 'vector))))

(defun thread-root (message)
  "The first message of the thread MESSAGE was said in, or NIL."
  (let ((root (nlk:json-value message :string "threadOriginatorGuid")))
    (and (plusp (length root)) root)))

(defun imessage-message-candidate (message &key bot-message-p user-name)
  "One MESSAGE as the shared candidate, or NIL for one that is nothing said.
BOT-MESSAGE-P answers whether a guid is one of the bot's messages or the root
of a thread it speaks in: a line aimed there is addressed the way a mention
is, so a conversation the bot is in needs no name to go on."
  (let* ((guid (nlk:json-value message :string "guid"))
         (chat (message-chat-guid message))
         (root (thread-root message))
         (quoted (nlk:json-value message :string "replyToGuid"))
         (direct-p (direct-chat-p chat))
         (attachments (imessage-attachments message)))
    (when (and guid chat (said-p message)
               (or attachments (plusp (length (imessage-text message)))))
      (nlk:json-object
       "text" (imessage-text message)
       "attachments" attachments
       "source"
       (nlk:json-object
        "platform" "imessage"
        "chat_kind" (cond (direct-p "direct_message") (root "thread") (t "group"))
        "channel_id" chat
        "user_id" (message-sender message)
        "user_name" user-name
        "message_id" guid
        "reply_to_message_id" root
        "addressed" (and bot-message-p
                         (or (and root (funcall bot-message-p root))
                             (and quoted (funcall bot-message-p quoted)))
                         t)
        "is_bot" nil)))))

(defun imessage-reaction-candidate (message &key user-name)
  "A tapback MESSAGE as the kit's reaction candidate (NCK:REACTION-NOTICED):
the emoji as its text, the message it is on as its reply gesture -- or NIL."
  (multiple-value-bind (emoji added-p target) (imessage-tapback message)
    (let ((chat (message-chat-guid message)))
      (when (and emoji chat (not (nlk:json-value message :boolean "isFromMe")))
        (nlk:json-object
         "text" emoji
         "source" (nlk:json-object
                   "platform" "imessage"
                   "chat_kind" (if (direct-chat-p chat) "direct_message" "group")
                   "channel_id" chat
                   "user_id" (message-sender message)
                   "user_name" user-name
                   "message_id" (nlk:json-value message :string "guid")
                   "reply_to_message_id" target
                   "reaction" (if added-p "add" "remove")))))))

;;; --- choices ---------------------------------------------------------------------
;;; Messages draws no buttons. A message carrying the kit's choices carries
;;; them as numbered lines under its words; a reply that is one of those
;;; numbers, or a choice's label as written, presses it.

(defun choice-items (controls)
  "CONTROLS, the kit's rows -- buttons (LABEL DATA STYLE DISABLED), a row of
them, a menu (:MENU PLACEHOLDER OPTIONS), a line (:ROW MARK LABEL DETAIL
CHOICE) -- as the ((LABEL DATA) ...) a person can pick, in order; a disabled
button is left out."
  (loop for control in (unless (eq controls :clear) controls)
        append (cond ((eq (first control) :menu)
                      (loop for (label data description current) in (third control)
                            collect (list (format nil "~a~@[ - ~a~]~:[~; (current)~]" label description current)
                                          data)))
                     ((eq (first control) :row)
                      (destructuring-bind (mark label detail choice) (rest control)
                        (and choice (not (fourth choice))
                             (list (list (format nil "~@[~a ~]~a~@[ - ~a~]" mark label detail)
                                         (second choice))))))
                     (t (loop for (label data nil disabled) in (if (consp (first control))
                                                                    control
                                                                    (list control))
                              unless disabled collect (list label data))))))

(defun choice-lines (items)
  "ITEMS as the numbered lines a person picks from."
  (format nil "~{~a~^~%~}" (loop for (label) in items for n from 1
                                  collect (format nil "~d. ~a" n label))))

(defun choice-picked (text items)
  "The (LABEL DATA) of ITEMS that TEXT, a reply, picks -- its number alone
(`2', `2.', `#2') or a label as written -- or NIL."
  (let* ((said (string-trim '(#\Space #\Tab #\Newline #\. #\# #\)) (or text "")))
         (number (and (plusp (length said)) (every #'digit-char-p said) (parse-integer said))))
    (cond (number (and (<= 1 number (length items)) (nth (1- number) items)))
          ((plusp (length said))
           (find said items :key #'first :test #'string-equal)))))

;;; --- the policy ------------------------------------------------------------------

(defun imessage-inbound-policy (&key allowed-chats allowed-users require-mention mention-names
                                     free-response-chats ignored-chats dm-policy group-policy
                                     &allow-other-keys)
  "The kit policy over iMessage's chats: chats by guid, people by handle, the
mention as one of MENTION-NAMES."
  (make-inbound-policy
   :allowed-channels allowed-chats
   :allowed-users allowed-users
   :dm-policy (if (equal dm-policy "disabled") :disabled :enabled)
   :group-policy (if (equal group-policy "disabled") :disabled :enabled)
   :require-mention require-mention
   :free-response-channels free-response-chats
   :ignored-channels ignored-chats
   :mention-target (first mention-names)
   :mention-test (lambda (text target)
                   (declare (ignore target))
                   (imessage-mention-p text mention-names))
   :allow-bots :none
   :channel-match :exact
   :reasons '(:mention-required-without-user "mention_required_without_mention_names")))
