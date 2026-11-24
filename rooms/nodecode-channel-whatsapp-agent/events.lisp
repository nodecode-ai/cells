;;;; events.lisp --- an inbound update as the kit's candidate. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The platform delivers messages in the webhook shape it uses elsewhere:
;;;; one entry per message, its sender in "from", its words under "text"
;;;; ("body"), a media message's under its type's "caption". A message the
;;;; agent itself sent comes back naming the agent in "from"; the adapter
;;;; drops those before admission.

(in-package #:nodecode-channel-whatsapp-agent)

(defun text-of-message (message)
  "MESSAGE's words: a text message's body, another kind's caption, else an
empty string."
  (let ((type (nlk:json-value message :string "type")))
    (or (nlk:json-value message :string "text" "body")
        (and (plusp (length type)) (nlk:json-value message :string type "caption"))
        "")))

(defun attachment-entry (message type id)
  "The file MESSAGE carries, as the kit's attachment entry, or NIL: what it
is called, of what type and how big, the platform's own object for it, and
the media id -- under the key download -- the adapter puts a fetch thunk on."
  (let ((media (and (plusp (length type))
                    (not (string= type "text"))
                    (nlk:json-value message :object type))))
    (when media
      (let ((media-id (nlk:json-value media :string "id")))
        (vector (nlk:json-object
                 "id" (or (and media-id (plusp (length media-id))
                                (format nil "~a" media-id))
                           (format nil "~a:~a" id type))
                 "media_type" (or (nlk:json-value media :string "mime_type") type)
                 "filename" (or (nlk:json-value media :string "filename") "")
                 "size" (or (nlk:json-value media :integer "file_size") 0)
                 "media" media
                 :when (and media-id (plusp (length media-id))) "download" media-id))))))

(defun reply-of (message)
  "The message MESSAGE quotes as the kit's reply, or NIL."
  (let ((context (nlk:json-value message :object "context")))
    (when context
      (let ((id (nlk:json-value context :string "id")))
        (when (and id (plusp (length id)))
          (nlk:json-object "id" id
                           "user_id" (or (nlk:json-value context :string "from") "")
                           "text" ""))))))

(defparameter +non-ask-types+ '("reaction")
  "The message kinds that carry no ask: a reaction says what someone
thought of a message, not a thing to answer, and answering it would be an
empty turn of its own.")

(defun message-candidate (message &key (agent-id ""))
  "One inbound MESSAGE as the shared candidate, or NIL without an id or a
sender. The chat is the sender: an agent chat is a direct message."
  (let* ((id (nlk:json-value message :string "id"))
         (from (nlk:json-value message :string "from"))
         (type (nlk:json-value message :string "type"))
         (name (nlk:json-value message :string "profile" "name"))
         (sent (nlk:json-value message :integer "timestamp")))
    (when (and id (plusp (length id)) from (plusp (length from))
               (not (member type +non-ask-types+ :test #'string=))
               (or (zerop (length agent-id)) (string/= from agent-id)))
      (nlk:json-object
       "text" (text-of-message message)
       "attachments" (attachment-entry message type id)
       "reply" (reply-of message)
       "source" (nlk:json-object
                 "platform" "whatsapp-agent"
                 "chat_kind" "direct_message"
                 "chat_id" from
                 "channel-id" from
                 "chat_name" (or name from)
                 "user_id" from
                 "user-id" from
                 "user_name" (or name from)
                 "message_id" id
                 "message-id" id
                 "addressed" t
                 :when (and sent (plusp sent))
                 "sent_ms" (* 1000 sent))))))

(defun message-candidates (message &key (agent-id ""))
  "The candidates one delivered message is: itself, when it is someone
else's."
  (let ((candidate (message-candidate message :agent-id agent-id)))
    (and candidate (list candidate))))

(defun outbound-messages (body)
  "The messages an updates answer carries, as a list."
  (let ((messages (and (hash-table-p body) (gethash "messages" body))))
    (cond ((vectorp messages) (coerce messages 'list))
          ((listp messages) messages)
          (t nil))))
