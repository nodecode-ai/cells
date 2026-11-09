;;;; updates.lisp --- the updates a business connection carries.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One update in, the events worth the model's look out: a customer wrote,
;;;; a connection changed, the owner pressed a draft's button, the owner
;;;; typed the text an Edit waits for. The presses are applied here, exactly
;;;; as the original plugin applied them inside its own handler.

(in-package #:nodecode-telegram-business)

(defun allowed-updates ()
  (vector "business_connection" "business_message" "edited_business_message"
          "callback_query" "message"))

(defun remember-connection (object)
  "Store what Telegram says about one business connection, and answer the
event that reports it."
  (let* ((id (or (nlk:json-value object :string "id") ""))
         (user (nlk:json-value object :object "user"))
         (user-id (nlk:json-value user :integer "id"))
         (chat-id (nlk:json-value object :integer "user_chat_id"))
         (can-reply (nlk:json-value object :boolean "can_reply"))
         (enabled (nlk:json-value object :boolean "is_enabled"))
         (known (connection id))
         (record (or known (new-table))))
    (setf (field record "connection_id") id
          (field record "owner_user_id") (and user-id (princ-to-string user-id))
          (field record "owner_chat_id") (or chat-id (field record "owner_chat_id"))
          (field record "user_name") (or (nonblank (display-name user))
                                          (field record "user_name") "")
          (field record "can_reply") (and can-reply t)
          (field record "enabled") (and enabled t))
    (when (null (gethash "auto_draft" record))
      (setf (field record "auto_draft") t))
    (setf (gethash id (connections)) record)
    (nlk:json-object
     "kind" "connection"
     "connection_id" id
     "owner_chat_id" (or (field record "owner_chat_id") 0)
     "user_name" (field record "user_name")
     "enabled" (enabled-p record)
     "can_reply" (can-reply-p record))))

(defun supersede-pending (connection-id chat-id)
  "The one-pending-draft rule: a newer customer message retires the draft the
owner was still looking at in this chat."
  (let ((chat (chat-key chat-id)))
    (dolist (record (pending-drafts))
      (when (and (field record "status")
                 (string= (field record "status") "pending")
                 (string= (or (field record "connection_id") "") (or connection-id ""))
                 (string= (or (field record "customer_chat_id") "") chat))
        (settle record "superseded")))))

(defun customer-event (message edited-p)
  "MESSAGE as the events it makes: one customer event, or none when the
message is our own reply, the owner typing, or a muted chat."
  (let* ((connection-id (or (nlk:json-value message :string "business_connection_id") ""))
         (sender-bot (nlk:json-value message :any "sender_business_bot"))
         (chat (nlk:json-value message :object "chat"))
         (chat-id (nlk:json-value chat :integer "id"))
         (from (nlk:json-value message :object "from"))
         (from-id (nlk:json-value from :integer "id"))
         (conn (connection connection-id))
         (text (or (nlk:json-value message :string "text")
                   (nlk:json-value message :string "caption")
                   "")))
    (when (and chat-id (nonblank (display-name from)))
      (setf (gethash (chat-key chat-id) (names)) (display-name from)))
    (cond
      ((or sender-bot (null chat-id)) '())
      ((owner-p conn from-id) '())
      ((paused-p chat-id) '())
      (t
       (let ((text (nlk:clip text (setting :max-customer-text-chars))))
         (supersede-pending connection-id chat-id)
         (list (nlk:json-object
                "kind" "customer"
                "connection_id" connection-id
                "chat_id" (chat-key chat-id)
                "message_id" (or (nlk:json-value message :integer "message_id") 0)
                "from_name" (or (display-name from) "")
                "text" text
                "edited" (and edited-p t)
                "draft" (if conn (auto-draft-p conn) t))))))))

(defun push-edit (owner-chat draft-id)
  (let* ((key (chat-key owner-chat))
         (table (pending-edits))
         (stack (or (gethash key table) (vector))))
    (setf (gethash key table) (concatenate 'vector stack (vector draft-id)))))

(defun pop-edit (key)
  (let* ((table (pending-edits))
         (stack (gethash key table)))
    (cond ((null stack) nil)
          ((= (length stack) 1) (remhash key table) (aref stack 0))
          (t (let ((id (aref stack (1- (length stack)))))
               (setf (gethash key table) (subseq stack 0 (1- (length stack))))
               id)))))

(defun handle-callback (query)
  "The owner pressed a draft's button: apply it, and answer the event."
  (let* ((query-id (or (nlk:json-value query :string "id") ""))
         (data (nlk:json-value query :string "data"))
         (caller (nlk:json-value (nlk:json-value query :object "from") :integer "id")))
    (multiple-value-bind (choice draft-id) (parse-callback data)
      (let ((record (draft draft-id)))
        (labels ((event (choice)
                   (nlk:json-object "kind" "decision" "choice" choice
                                    "draft_id" (or draft-id ""))))
          (cond
            ((null record)
             (answer-callback query-id "That draft is gone.")
             (event "missing"))
            ((draft-expired-p record)
             (settle record "expired")
             (rerender record "expired")
             (answer-callback query-id "That draft has expired.")
             (event "expired"))
            ((not (owner-p record caller))
             (answer-callback query-id
                              "\u26d4 Only the connected account owner can use these buttons.")
             (event "forbidden"))
            ((string= choice "discard")
             (settle record "discarded")
             (answer-callback query-id "\u2715 Discarded")
             (rerender record "discarded")
             (event "discard"))
            ((string= choice "edit")
             (settle record "awaiting_edit")
             (push-edit (field record "owner_chat_id") draft-id)
             (answer-callback query-id "\u270e Send me the text to deliver")
             (rerender record "awaiting_edit")
             (event "edit"))
            ((string= choice "send")
             (if (not (can-reply-p (connection (field record "connection_id"))))
                 (progn
                   (answer-callback query-id
                                    "\u26a0\ufe0f Send-on-your-behalf is OFF: enable it in Telegram \u2192 Business \u2192 Chatbots, then try again.")
                   (event "no-reply-right"))
                 (multiple-value-bind (ok note)
                     (deliver-approved record (field record "draft_text") "sent")
                   (answer-callback query-id (if ok "\u2713 Sent" note))
                   (if ok
                       (event "send")
                       (nlk:json-object "kind" "decision" "choice" "send-failed"
                                        "draft_id" draft-id "note" note)))))
            (t (answer-callback query-id "Unknown action.")
               (event "unknown"))))))))

(defun owner-text-event (message)
  "The owner's DM: when a draft waits for their text, it is the outgoing
message. => the events it makes."
  (let* ((chat (nlk:json-value message :object "chat"))
         (chat-id (nlk:json-value chat :integer "id"))
         (from (nlk:json-value message :object "from"))
         (from-id (nlk:json-value from :integer "id"))
         (key (chat-key chat-id))
         (text (nlk:trimmed (or (nlk:json-value message :string "text") ""))))
    (if (or (null key) (zerop (length text)) (uiop:string-prefix-p "/" text))
        '()
        (let ((draft-id (pop-edit key)))
          (when draft-id
            (let ((record (draft draft-id)))
              (cond
                ((or (null record) (draft-expired-p record))
                 (when record (settle record "expired") (rerender record "expired"))
                 (note-owner key "That draft has expired or was already resolved.")
                 (list (nlk:json-object "kind" "decision" "choice" "expired"
                                         "draft_id" draft-id)))
                ((not (member (field record "status") +answerable+ :test #'string=))
                 (note-owner key "That draft has expired or was already resolved.")
                 (list (nlk:json-object "kind" "decision" "choice" "settled"
                                         "draft_id" draft-id)))
                ((not (owner-p record from-id)) '())
                ((not (can-reply-p (connection (field record "connection_id"))))
                 (note-owner key "\u26a0\ufe0f Send-on-your-behalf is OFF in Telegram \u2192 Business \u2192 Chatbots. Copy the text and send it manually.")
                 '())
                (t
                 (multiple-value-bind (ok note)
                     (deliver-approved record text "edited")
                   (if ok
                       (progn
                         (note-owner key (format nil "\u2713 Sent (edited):~%~%~a" text))
                         (list (nlk:json-object "kind" "decision" "choice" "edited"
                                                 "draft_id" draft-id "text" text)))
                       (progn
                         (note-owner key note)
                         (list (nlk:json-object "kind" "decision" "choice" "send-failed"
                                                 "draft_id" draft-id "note" note)))))))))))))

(defun handle-update (update)
  (let ((connection (nlk:json-value update :object "business_connection"))
        (message (nlk:json-value update :object "business_message"))
        (edited (nlk:json-value update :object "edited_business_message"))
        (callback (nlk:json-value update :object "callback_query"))
        (direct (nlk:json-value update :object "message")))
    (cond
      (connection (list (remember-connection connection)))
      (message (customer-event message nil))
      (edited (customer-event edited t))
      (callback (list (handle-callback callback)))
      (direct (owner-text-event direct))
      (t '()))))

(defun connections-list ()
  (sort (loop for record being the hash-values of (connections) collect record)
        (lambda (a b) (string< (or (field a "connection_id") "")
                               (or (field b "connection_id") "")))))
