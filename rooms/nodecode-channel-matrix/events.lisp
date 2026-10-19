;;;; events.lisp --- Matrix room events as the kit's candidates. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A room message becomes the shared candidate shape (the kit names the
;;;; room and the lane from it), a reaction on a message that carries
;;;; controls the kit's normal control press. Nothing here touches the
;;;; network: a person's display name, whether a room is a direct message and
;;;; which events and threads are the bot's are the adapter's to know, passed
;;;; in.
;;;;
;;;; Threads. A Matrix thread is the events whose m.relates_to says
;;;; rel_type m.thread under one root event. The kit's lane is the same shape --
;;;; one ask and what follows from it -- so a message in a thread is
;;;; normalized as a reply to the thread's root, in the room's room. A plain
;;;; reply (m.in_reply_to) outside a thread is the reply gesture too, aimed at
;;;; the event it quotes.
;;;;
;;;; Mentions. A client writes a mention twice: the person's display name in
;;;; `body', and a matrix.to link -- a pill -- in `formatted_body'; a client
;;;; that speaks intentional mentions (spec v1.7) also lists the ids under
;;;; m.mentions. The words the lane reads come from formatted_body, each pill
;;;; spelled as the user id it links, so the kit's mention test is a plain
;;;; search for the bot's id, and an m.mentions naming the bot addresses the
;;;; line whatever its words say.

(in-package #:nodecode-channel-matrix)

;;; --- text ------------------------------------------------------------------------

(defparameter +html-entities+
  '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&#39;" . "'") ("&#x27;" . "'")
    ("&nbsp;" . " ") ("&amp;" . "&"))
  "The entities a client's HTML spells characters with, &amp; last.")

(defun html-plain-text (html)
  "HTML as a person reads it: the quoted reply dropped, a user pill as the
user id it links, line-ending tags as line breaks, every other tag gone and
the entities read."
  (let ((text (ppcre:regex-replace-all "(?is)<mx-reply>.*?</mx-reply>" html "")))
    (setf text (ppcre:regex-replace-all
                "(?is)<a\\s[^>]*href=[\"']https://matrix\\.to/#/((?:@|%40)[^\"'?]+)[^\"']*[\"'][^>]*>.*?</a>"
                text
                (lambda (target start end match-start match-end reg-starts reg-ends)
                  (declare (ignore start end match-start match-end))
                  (quri:url-decode (subseq target (aref reg-starts 0) (aref reg-ends 0))))))
    (setf text (ppcre:regex-replace-all "(?i)<br\\s*/?>" text (string #\Newline)))
    (setf text (ppcre:regex-replace-all "(?i)<li[^>]*>" text "- "))
    (setf text (ppcre:regex-replace-all "(?i)</(p|li|h[1-6]|blockquote|pre|tr|div)>" text
                                        (string #\Newline)))
    (setf text (ppcre:regex-replace-all "<[^>]*>" text ""))
    (dolist (entity +html-entities+)
      (setf text (uiop:frob-substrings text (list (car entity))
                                       (lambda (match emit)
                                         (declare (ignore match))
                                         (funcall emit (cdr entity))))))
    (nlk:trimmed (ppcre:regex-replace-all "\\n{3,}" text (format nil "~%~%")))))

(defun reply-target (content)
  "The event CONTENT's m.in_reply_to names, or NIL."
  (nlk:json-value content :string "m.relates_to" "m.in_reply_to" "event_id"))

(defun body-without-fallback (body)
  "BODY with the quoted reply an older client puts in front of it dropped:
its leading `> ' lines and the blank line after them."
  (let ((lines (uiop:split-string body :separator '(#\Newline))))
    (if (and lines (uiop:string-prefix-p "> " (first lines)))
        (let ((rest (member-if-not (lambda (line) (uiop:string-prefix-p ">" line)) lines)))
          (format nil "~{~a~^~%~}" (if (and rest (blank-p (first rest))) (rest rest) rest)))
        body)))

(defun matrix-plain-text (content)
  "A message CONTENT's words as the lane reads them: its HTML spelled plain
(HTML-PLAIN-TEXT) when it carries some, else its body, a quoted reply's
fallback dropped either way."
  (let ((html (and (equal "org.matrix.custom.html" (nlk:json-value content :string "format"))
                   (nlk:json-value content :string "formatted_body")))
        (body (or (nlk:json-value content :string "body") "")))
    (if html
        (html-plain-text html)
        (nlk:trimmed (if (reply-target content) (body-without-fallback body) body)))))

(defun matrix-mention-p (text bot-user-id)
  "Whether TEXT mentions BOT-USER-ID: a pill reads as the id it links."
  (and (stringp text) (stringp bot-user-id) (plusp (length bot-user-id))
       (search bot-user-id text)
       t))

(defun matrix-strip-mention (text bot-user-id)
  "TEXT with our own mention removed, and the colon or comma a client puts
after a mention that opens a line: the model reads the ask, not the handle
that routed it."
  (when (and (stringp bot-user-id) (plusp (length bot-user-id)))
    (setf text (ppcre:regex-replace-all
                (format nil "~a[:,]?" (ppcre:quote-meta-chars bot-user-id)) (or text "") "")))
  (nlk:trimmed text))

;;; --- messages --------------------------------------------------------------------

(defparameter +said-msgtypes+ '("m.text" "m.notice" "m.emote" "m.image" "m.file" "m.audio" "m.video")
  "The message types that are something said; a location, a poll, a key
request are not asks.")

(defparameter +file-msgtypes+ '("m.image" "m.file" "m.audio" "m.video")
  "The message types whose `url' is the thing sent.")

(defun mentioned-ids (content)
  "The user ids CONTENT's m.mentions names."
  (loop for id across (nlk:json-array content "m.mentions" "user_ids")
        when (stringp id) collect id))

(defun thread-root (content)
  "The root event of the thread CONTENT is in, or NIL."
  (and (equal "m.thread" (nlk:json-value content :string "m.relates_to" "rel_type"))
       (nlk:json-value content :string "m.relates_to" "event_id")))

(defun matrix-attachments (content)
  "CONTENT's file as the candidate's attachments array, or NIL: its mxc://
address, declared type, name and size. The fetch thunk is the adapter's to
add: the address alone is no download."
  (let ((url (nlk:json-value content :string "url"))
        (type (nlk:json-value content :string "info" "mimetype")))
    (when (and (member (nlk:json-value content :string "msgtype") +file-msgtypes+ :test #'equal)
               (stringp url) (uiop:string-prefix-p "mxc://" url))
      (vector (nlk:json-object
               "id" url
               "download" url
               "media_type" type
               "filename" (or (nlk:json-value content :string "filename")
                              (nlk:json-value content :string "body") "")
               "size" (or (nlk:json-value content :integer "info" "size") 0))))))

(defun file-caption (content)
  "The words sent with a file: its body when a filename beside it says the
body is a caption (spec v1.10), else none -- a bare body is the file's name."
  (let ((filename (nlk:json-value content :string "filename"))
        (body (nlk:json-value content :string "body")))
    (if (and filename body (string/= filename body)) (matrix-plain-text content) "")))

(defun matrix-message-candidate (event room-id &key bot-user-id direct-p bot-event-p user-name)
  "One timeline EVENT in ROOM-ID as the shared candidate, or NIL for an event
that is not something said -- an edit, a redaction, a state change, a type
no ask is made of."
  ;; DIRECT-P says the room is a direct message. BOT-EVENT-P answers whether
  ;; (ROOM EVENT-ID) is one of the bot's own messages or the root of a thread
  ;; the bot speaks in: a reply to it, or a line in that thread, is addressed
  ;; the way a mention is, so a conversation the bot is in needs no mention to
  ;; go on.
  (let* ((content (nlk:json-value event :object "content"))
         (msgtype (and content (nlk:json-value content :string "msgtype")))
         (event-id (nlk:json-value event :string "event_id"))
         (root (and content (thread-root content)))
         (quoted (and content (not (nlk:json-value content :boolean "m.relates_to" "is_falling_back"))
                      (reply-target content)))
         (aimed (or root quoted)))
    (when (and (equal "m.room.message" (nlk:json-value event :string "type"))
               event-id content
               (member msgtype +said-msgtypes+ :test #'equal)
               (not (equal "m.replace" (nlk:json-value content :string "m.relates_to" "rel_type"))))
      (let ((kind (if direct-p "direct_message" "channel")))
        (nlk:json-object
         "text" (if (member msgtype +file-msgtypes+ :test #'equal)
                    (file-caption content)
                    (matrix-plain-text content))
         "attachments" (matrix-attachments content)
         "source"
         (nlk:json-object
          "platform" "matrix"
          ;; A line in a room's thread is in a thread already: the kit opens
          ;; none for it, and the reply gesture finds the lane.
          "chat_kind" (if (and root (not direct-p)) "thread" kind)
          "channel_id" room-id
          "user_id" (nlk:json-value event :string "sender")
          "user_name" user-name
          "message_id" event-id
          "reply_to_message_id" aimed
          "addressed" (and (or (and bot-user-id (member bot-user-id (mentioned-ids content)
                                                         :test #'equal))
                               (and aimed bot-event-p (funcall bot-event-p room-id aimed)))
                           t)
          ;; m.notice is the msgtype a bot speaks in (the spec's own advice).
          "is_bot" (and (equal msgtype "m.notice") t)))))))

;;; --- controls --------------------------------------------------------------------
;;; Matrix draws no buttons. A message that carries the kit's controls carries
;;; them as reactions the bot puts on it, one glyph per control; a person who
;;; adds the same reaction -- a click on the bot's, in Element -- presses it.

(defparameter +control-glyphs+ '(("stop" . "⏹️") ("details" . "ℹ️"))
  "The glyph a control is pressed with, by its label; any other control
takes its number's keycap.")

(defun control-glyph (control index)
  "The reaction that presses CONTROL, the kit's (LABEL DATA STYLE), the
INDEXth on its message."
  (or (cdr (assoc (string-downcase (first control)) +control-glyphs+ :test #'string=))
      (format nil "~d~a~a" (1+ (mod index 9)) (code-char #xFE0F) (code-char #x20E3))))

(defun bare-glyph (key)
  "KEY without its variation selectors: two clients spell one emoji apart."
  (remove (code-char #xFE0F) (or key "")))

(defun matrix-control-press (event room-id controls-of)
  "An m.reaction EVENT on a message carrying controls as the kit's normal
control press, or NIL. CONTROLS-OF answers the controls (ROOM EVENT-ID)
carries."
  (let* ((relation (nlk:json-value event :object "content" "m.relates_to"))
         (target (and relation (nlk:json-value relation :string "event_id")))
         (key (and relation (nlk:json-value relation :string "key")))
         (controls (and target (equal "m.annotation" (nlk:json-value relation :string "rel_type"))
                        (funcall controls-of room-id target))))
    (when (equal "m.reaction" (nlk:json-value event :string "type"))
      (loop for control in controls
            for index from 0
            when (string= (bare-glyph key) (bare-glyph (control-glyph control index)))
              return (list :id (nlk:json-value event :string "event_id")
                           :data (second control)
                           :user-id (nlk:json-value event :string "sender")
                           :message-id target
                           :channel-id room-id)))))

;;; --- invites ---------------------------------------------------------------------

(defun invite-sender (invite bot-user-id)
  "(values SENDER DIRECT-P): who invited BOT-USER-ID to the room whose
invite section INVITE is, and whether they made it a direct message."
  (loop for event across (nlk:json-array invite "invite_state" "events")
        when (and (equal "m.room.member" (nlk:json-value event :string "type"))
                  (equal bot-user-id (nlk:json-value event :string "state_key"))
                  (equal "invite" (nlk:json-value event :string "content" "membership")))
          return (values (nlk:json-value event :string "sender")
                         (and (nlk:json-value event :boolean "content" "is_direct") t))))

;;; --- the policy ------------------------------------------------------------------

(defun matrix-inbound-policy (&key allowed-rooms allowed-users require-mention
                                   free-response-rooms ignored-rooms allow-bots
                                   dm-policy group-policy bot-user-id &allow-other-keys)
  "The kit policy over Matrix's rooms: rooms by id, the mention as the bot's
user id."
  (make-inbound-policy
   :allowed-channels allowed-rooms
   :allowed-users allowed-users
   :dm-policy (if (equal dm-policy "disabled") :disabled :enabled)
   :group-policy (if (equal group-policy "disabled") :disabled :enabled)
   :require-mention require-mention
   :free-response-channels free-response-rooms
   :ignored-channels ignored-rooms
   :mention-target bot-user-id
   :mention-test #'matrix-mention-p
   :allow-bots (cond ((equal allow-bots "all") :all)
                     ((equal allow-bots "mentions") :mentions)
                     (t :none))
   :self-id bot-user-id
   :channel-match :exact
   :reasons '(:mention-required-without-user "mention_required_without_bot_user")))
