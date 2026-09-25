;;;; events.lisp --- Slack events as the kit's candidates. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A message event becomes the shared candidate shape (the kit names the
;;;; room and the lane from it), a slash command the command line it spells,
;;;; a button press the kit's normal control press. Nothing here touches the
;;;; network: a person's display name and whether the bot already speaks in a
;;;; thread are the adapter's to know, passed in.
;;;;
;;;; Rooms and threads. A Slack thread is not a channel of its own: it is the
;;;; replies hung off one message, named by that message's ts. The kit's lane
;;;; is the same shape -- one ask and what follows from it -- so a message in a
;;;; thread is normalized as a reply to the thread's first message, in the
;;;; channel's room. The ask that opened a thread is a lane's address, so a
;;;; reply in the thread continues that lane; the bot answers in the thread
;;;; (rest.lisp threads every reply to its root).

(in-package #:nodecode-channel-slack)

;;; --- text ------------------------------------------------------------------------

(defun slack-plain-text (text)
  "Slack's message TEXT as a person reads it: a channel link as its #name, a
link as its label and address, @here and a group mention as typed, the three
escaped characters as themselves. A user mention stays <@U...>, the handle
the model writes to mention that person back."
  (flet ((reference (match inner)
           (let ((label (let ((bar (position #\| inner))) (and bar (subseq inner (1+ bar)))))
                 (head (subseq inner 0 (or (position #\| inner) (length inner)))))
             (cond ((uiop:string-prefix-p "@" inner) match)
                   ((uiop:string-prefix-p "#" inner) (if label (format nil "#~a" label) match))
                   ((uiop:string-prefix-p "!subteam^" inner) (or label match))
                   ((uiop:string-prefix-p "!date^" inner) (or label match))
                   ((uiop:string-prefix-p "!" inner) (format nil "@~a" (subseq head 1)))
                   ((uiop:string-prefix-p "mailto:" head) (or label (subseq head 7)))
                   ((or (null label) (string= label head)) head)
                   (t (format nil "~a (~a)" label head))))))
    (let ((plain (ppcre:regex-replace-all
                  "<([^<>]*)>" (or text "")
                  (lambda (target start end match-start match-end reg-starts reg-ends)
                    (declare (ignore start end))
                    (reference (subseq target match-start match-end)
                               (subseq target (aref reg-starts 0) (aref reg-ends 0)))))))
      (uiop:frob-substrings plain '("&lt;" "&gt;" "&amp;")
                            (lambda (entity emit)
                              (funcall emit (cond ((string= entity "&lt;") "<")
                                                  ((string= entity "&gt;") ">")
                                                  (t "&"))))))))

(defun slack-mention-p (text bot-user-id)
  "Whether TEXT mentions BOT-USER-ID: Slack writes a mention as <@U...>."
  (and (stringp text) (stringp bot-user-id) (plusp (length bot-user-id))
       (or (search (format nil "<@~a>" bot-user-id) text)
           (search (format nil "<@~a|" bot-user-id) text))
       t))

(defun slack-strip-mention (text bot-user-id)
  "TEXT with our own mention removed: the model reads the ask, not the
handle that routed it."
  (when (and (stringp bot-user-id) (plusp (length bot-user-id)))
    (setf text (ppcre:regex-replace-all
                (format nil "<@~a(?:\\|[^>]*)?>" (ppcre:quote-meta-chars bot-user-id))
                (or text "") "")))
  (nlk:trimmed text))

;;; --- messages --------------------------------------------------------------------

(defparameter +message-subtypes+ '("file_share" "thread_broadcast" "bot_message" "me_message")
  "The message subtypes that are something said; every other subtype -- an
edit, a deletion, a join, a topic change -- is the channel's bookkeeping.")

(defun slack-chat-kind (channel-type channel-id)
  "The kit's chat kind for a Slack channel: a DM, a group DM, or a channel,
public or private."
  ;; An event names the kind; a slash command only the id, whose D says DM.
  (cond ((equal channel-type "im") "direct_message")
        ((equal channel-type "mpim") "group")
        (channel-type "channel")
        ((uiop:string-prefix-p "D" (or channel-id "")) "direct_message")
        (t "channel")))

(defun slack-attachments (event)
  "EVENT's files the lane can read, as the candidate's attachments array: the
file's id, its declared type, name and size, and the address its bytes are
fetched from with the bot token."
  ;; NIL when it carries none. The fetch thunk is the adapter's to add
  ;; (ATTACH-SLACK-FILE-FETCHERS): the address alone answers a sign-in page.
  (let ((entries (loop for file across (nlk:json-array event "files")
                       for type = (nlk:json-value file :string "mimetype")
                       for url = (or (nlk:json-value file :string "url_private_download")
                                     (nlk:json-value file :string "url_private"))
                       when (and url (readable-media-type-p type))
                         collect (nlk:json-object
                                  "id" (nlk:json-value file :string "id")
                                  "download" url
                                  "media_type" type
                                  "filename" (or (nlk:json-value file :string "name") "")
                                  "size" (or (nlk:json-value file :integer "size") 0)))))
    (and entries (coerce entries 'vector))))

(defun slack-message-candidate (event &key team-id bot-user-id bot-thread-p user-name)
  "One message EVENT as the shared candidate, or NIL for an event that is
not something said: the words, the files, and the source coordinates, the
thread as the reply gesture."
  ;; BOT-THREAD-P answers whether the bot already speaks in the thread a
  ;; (CHANNEL THREAD-TS) names: a message there is addressed, the way a reply
  ;; to the bot is, so a thread the bot answers in needs no mention to go on.
  ;; A thread under one of the bot's own messages is addressed the same way.
  (let* ((subtype (nlk:json-value event :string "subtype"))
         (channel (nlk:json-value event :string "channel"))
         (ts (nlk:json-value event :string "ts"))
         (thread (nlk:json-value event :string "thread_ts"))
         (root (and thread (not (equal thread ts)) thread)))
    (when (and (equal "message" (nlk:json-value event :string "type"))
               channel ts
               (or (null subtype) (member subtype +message-subtypes+ :test #'equal)))
      (nlk:json-object
       "text" (slack-plain-text (nlk:json-value event :string "text"))
       "attachments" (slack-attachments event)
       "source"
       (nlk:json-object
        "platform" "slack"
        ;; A reply in a channel's thread is in a thread already: the kit
        ;; opens none for it, and the reply gesture finds the lane.
        "chat_kind" (let ((kind (slack-chat-kind (nlk:json-value event :string "channel_type")
                                                 channel)))
                      (if (and root (not (equal kind "direct_message"))) "thread" kind))
        "workspace_id" (or (nlk:json-value event :string "team") team-id)
        "channel_id" channel
        "user_id" (nlk:json-value event :string "user")
        "user_name" user-name
        "message_id" ts
        ;; The thread is the reply gesture: its first message is what this
        ;; one answers, and a lane that ask opened goes on here.
        "reply_to_message_id" root
        "addressed" (and root
                         (or (and bot-user-id
                                  (equal bot-user-id (nlk:json-value event :string "parent_user_id")))
                             (and bot-thread-p (funcall bot-thread-p channel root)))
                         t)
        "is_bot" (and (or (nlk:json-value event :string "bot_id")
                          (equal subtype "bot_message"))
                      t))))))

;;; --- the slash command and the button --------------------------------------------

(defun slack-command-candidate (payload)
  "A slash command PAYLOAD -- the app's one command, /nodecode, and what was
typed after it -- as the candidate for the command line it spells: `/nodecode
models grok' is `/models grok', a bare `/nodecode' is `/help'."
  ;; Addressed by construction: the person picked this app's command. The
  ;; source carries the response_url, the return path the platform's
  ;; PLAN-RESPOND answers through; a command is not a message and has no id.
  (let* ((typed (nlk:trimmed (or (nlk:json-value payload :string "text") "")))
         (channel (nlk:json-value payload :string "channel_id")))
    (when channel
      (nlk:json-object
       "text" (cond ((zerop (length typed)) "/help")
                    ((char= #\/ (char typed 0)) typed)
                    (t (format nil "/~a" typed)))
       "source"
       (nlk:json-object
        "platform" "slack"
        "chat_kind" (slack-chat-kind nil channel)
        "workspace_id" (nlk:json-value payload :string "team_id")
        "channel_id" channel
        "user_id" (nlk:json-value payload :string "user_id")
        "user_name" (nlk:json-value payload :string "user_name")
        "response_url" (nlk:json-value payload :string "response_url")
        "addressed" t
        "is_bot" nil)))))

(defun slack-control-press (payload)
  "A block_actions PAYLOAD -- a press on a button a lane posted -- as the
kit's normal control press, or NIL for any other interaction."
  ;; The socket's acknowledgement is the press's answer: there is no spinner
  ;; to stop, so the platform's control ack plans nothing.
  (let ((action (and (equal "block_actions" (nlk:json-value payload :string "type"))
                     (find-if #'hash-table-p (nlk:json-array payload "actions")))))
    (when (and action (nlk:json-value action :string "value"))
      (list :id (or (nlk:json-value action :string "action_ts")
                    (nlk:json-value payload :string "trigger_id"))
            :data (nlk:json-value action :string "value")
            :user-id (nlk:json-value payload :string "user" "id")
            :message-id (or (nlk:json-value payload :string "container" "message_ts")
                            (nlk:json-value payload :string "message" "ts"))
            :channel-id (or (nlk:json-value payload :string "container" "channel_id")
                            (nlk:json-value payload :string "channel" "id"))))))

;;; --- the policy ------------------------------------------------------------------

(defun slack-inbound-policy (&key allowed-channels allowed-users require-mention
                                  free-response-channels require-mention-channels
                                  ignored-channels allow-bot-authors dm-policy group-policy
                                  bot-user-id &allow-other-keys)
  "The kit policy over Slack's rooms: channels by id, the mention as <@U...>."
  (make-inbound-policy
   :allowed-channels allowed-channels
   :allowed-users allowed-users
   :dm-policy (if (equal dm-policy "disabled") :disabled :enabled)
   :group-policy (if (equal group-policy "disabled") :disabled :enabled)
   :require-mention require-mention
   :free-response-channels free-response-channels
   :require-mention-channels require-mention-channels
   :ignored-channels ignored-channels
   :mention-target bot-user-id
   :mention-test #'slack-mention-p
   :allow-bot-authors allow-bot-authors
   :self-id bot-user-id
   :channel-match :exact
   :reasons '(:mention-required-without-user "mention_required_without_bot_user")))
