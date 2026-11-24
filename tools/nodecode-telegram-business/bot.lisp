;;;; bot.lisp --- the Bot API, the owner's DM, and delivery on their behalf.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-telegram-business)

;;; --- one call ---------------------------------------------------------------

(defun api-call (method &key payload extra-timeout)
  "One Telegram Bot API call => its RESULT as decoded JSON (an object, an
array, a number or true), or NIL. A dial that fails, or an answer whose ok is
false, is a refusal carrying Telegram's own description."
  (let ((url (format nil "~a/bot~a/~a" +api-base+ (setting :token) method))
        (timeout (+ (setting :timeout) (or extra-timeout 0))))
    (multiple-value-bind (body status)
        (handler-case
            (nlk:http :post url
                      :headers (list (cons "accept" "application/json")
                                     (cons "content-type" "application/json"))
                      :content (if payload (nlk:encode-json-object payload) "{}")
                      :timeout timeout
                      :connect-timeout (setting :timeout))
          (error (condition)
            (fail "the Bot API call ~a did not answer: ~a" method
                  (nlk:one-line (princ-to-string condition)))))
      (let* ((text (if (stringp body) body (nlk:body-text body)))
             (object (and (stringp text) (plusp (length (nlk:trimmed text)))
                          (ignore-errors (nlk:decode-json text))))
             (ok (and (hash-table-p object)
                      (nlk:json-value object :boolean "ok"))))
        (cond
          ((and ok (or (null status) (and (integerp status) (< status 400))))
           (nlk:json-value object :any "result"))
          (t
           (fail "~a answered HTTP ~a~@[: ~a~]" method status
                 (or (and (hash-table-p object)
                          (nlk:json-value object :string "description"))
                     (and text (nlk:clip text 300))))))))))

;;; --- records -----------------------------------------------------------------

(defun display-name (user)
  "A person's name as Telegram tells it: first and last, else the @handle."
  (when (hash-table-p user)
    (let* ((first (nonblank (nlk:json-value user :string "first_name")))
           (last (nonblank (nlk:json-value user :string "last_name")))
           (handle (nonblank (nlk:json-value user :string "username"))))
      (or (nonblank (format nil "~@[~a~]~@[ ~a~]" first last))
          (and handle (format nil "@~a" handle))))))

(defun owner-p (record user-id)
  "Whether USER-ID is the owner RECORD belongs to. A caller the cell cannot
name is not the owner: the check never fails open."
  (let ((owner (and record (field record "owner_user_id")))
        (user (and user-id (princ-to-string user-id))))
    (and owner user (string= (princ-to-string owner) user))))

(defun enabled-p (conn) (if (null (field conn "enabled")) nil t))
(defun auto-draft-p (conn) (if (null (field conn "auto_draft")) nil t))
(defun can-reply-p (conn) (and conn (field conn "can_reply") t))

(defun paused-p (chat-id)
  (and chat-id (gethash (princ-to-string chat-id) (paused)) t))

(defun paused-list ()
  (sort (loop for key being the hash-keys of (paused) collect key) #'string<))

(defun draft-expired-p (record)
  (let ((expires (field record "expires_at")))
    (and (integerp expires) (>= (nlk:unix-now) expires))))

(defun settle (record status)
  "Put RECORD in STATUS, and answer it."
  (setf (field record "status") status)
  record)

(defun new-draft-id ()
  (let* ((state (state))
         (n (1+ (or (gethash "next_draft" state) 0))))
    (setf (gethash "next_draft" state) n)
    (format nil "d~d" n)))

(defun drafts-for (predicate)
  (loop for record being the hash-values of (drafts)
        when (funcall predicate record) collect record))

(defun pending-drafts ()
  (drafts-for (lambda (record)
                 (member (field record "status") +answerable+ :test #'string=))))

;;; --- the owner's DM ----------------------------------------------------------

(defun callback-data (choice draft-id)
  (format nil "~a~a:~a" +callback-prefix+ choice draft-id))

(defun parse-callback (data)
  "DATA as (values CHOICE DRAFT-ID), or NIL when it is not this cell's button."
  (when (and (stringp data) (uiop:string-prefix-p +callback-prefix+ data))
    (let* ((rest (subseq data (length +callback-prefix+)))
           (colon (position #\: rest)))
      (when colon
        (values (subseq rest 0 colon) (subseq rest (1+ colon)))))))

(defun button (text data)
  (nlk:json-object "text" text "callback_data" data))

(defun draft-keyboard (record)
  "The draft DM's keyboard: Send (only when the connection may reply for the
owner), Edit and Discard."
  (let ((id (field record "draft_id"))
        (row '()))
    (when (can-reply-p (connection (field record "connection_id")))
      (push (button "✓ Send" (callback-data "send" id)) row))
    (push (button "✎ Edit" (callback-data "edit" id)) row)
    (push (button "✕ Discard" (callback-data "discard" id)) row)
    (nlk:json-object "inline_keyboard"
                     (vector (coerce (nreverse row) 'vector)))))

(defun render-draft (record)
  (format nil "💬 ~a wrote:~%~~> ~a~%~%📝 Suggested reply:~%~a~@[~%~%~a~]"
          (or (nonblank (field record "customer_name")) "A customer")
          (or (nonblank (nlk:clip (or (field record "customer_text") "")
                                   +max-owner-quote+))
              "(no text)")
          (or (field record "draft_text") "")
          (unless (can-reply-p (connection (field record "connection_id")))
            "⚠️ Send-on-your-behalf is off for this connection: copy the reply and send it yourself.")))

(defparameter +status-heads+
  (list (cons "sent" "✓ Sent")
        (cons "edited" "✓ Sent (edited)")
        (cons "discarded" "✕ Discarded")
        (cons "expired" "⌛ Expired")
        (cons "superseded" "↻ Superseded by a newer draft")
        (cons "awaiting_edit" "✎ Waiting for the owner's text"))
  "How a settled draft reads in the owner's DM.")

(defun status-head (status)
  (or (cdr (assoc status +status-heads+ :test #'string=)) status))

(defun resolved-text (record status)
  (format nil "~a~%~%~a"
          (status-head status)
          (or (field record "final_sent_text") (field record "draft_text") "")))

(defun send-owner-message (chat-id text &optional keyboard)
  "One DM to the owner => its message id, or NIL when Telegram refused it."
  (let ((result (api-call "sendMessage"
                           :payload (nlk:json-object
                                     "chat_id" chat-id
                                     "text" text
                                     "disable_web_page_preview" t
                                     "reply_markup" (or keyboard
                                                        (nlk:json-object "remove_keyboard" t))))))
    (and (hash-table-p result)
         (nlk:json-value result :integer "message_id"))))

(defun note-owner (chat-id text)
  (ignore-errors (send-owner-message chat-id text)))

(defun rerender (record status)
  "Best effort: the owner's DM says what happened, its buttons gone."
  (let ((chat (field record "owner_chat_id"))
        (message (field record "owner_message_id")))
    (when (and chat (integerp message) (plusp message))
      (ignore-errors
        (api-call "editMessageText"
                  :payload (nlk:json-object
                            "chat_id" chat
                            "message_id" message
                            "text" (resolved-text record status)
                            "reply_markup" (nlk:json-object "inline_keyboard" (vector))))))))

(defun answer-callback (query-id text)
  (ignore-errors
    (api-call "answerCallbackQuery"
              :payload (nlk:json-object "callback_query_id" query-id "text" text))))

(defun deliver (record text)
  "Send TEXT to the draft's customer on the owner's behalf."
  (api-call "sendMessage"
            :payload (nlk:json-object
                      "chat_id" (field record "customer_chat_id")
                      "text" text
                      "business_connection_id" (field record "connection_id"))))

(defun deliver-approved (record text status)
  "Deliver TEXT for RECORD, settle it as STATUS and redraw the owner's DM.
=> (values OK NOTE), NOTE the words for the owner."
  (handler-case
      (progn
        (deliver record text)
        (setf (field record "final_sent_text") text)
        (settle record status)
        (rerender record status)
        (values t (if (string= status "edited") "✓ Sent (edited)" "✓ Sent")))
    (telegram-business-error (condition)
      (values nil (format nil "⚠️ Send failed: ~a"
                          (telegram-business-error-detail condition))))))
