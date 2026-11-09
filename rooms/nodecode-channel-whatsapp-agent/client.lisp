;;;; client.lisp --- the API's own calls, as request plans.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Meta's WhatsApp Agent Platform answers under /agent/v1: a long-polling
;;;; GET updates, a POST messages per message, and a POST statuses for a read
;;;; receipt and the typing indicator. A key is the whole identity, so it
;;;; rides on the executor's headers and never in a plan: a plan is what may
;;;; be printed.

(in-package #:nodecode-channel-whatsapp-agent)

(defparameter +api-base+ "https://api.whatsapp.com/agent/v1"
  "Meta's Agent Platform API version and root.")

(defparameter +text-limit+ 4000
  "Characters one message may carry: WhatsApp's own ceiling.")

(defun api-executor (key &key (base +api-base+))
  "The live executor: every plan under BASE with the API KEY's bearer."
  (make-dexador-executor
   :base-url base
   :headers (list (cons "authorization" (format nil "bearer ~a" key)))
   :redact-prefixes (list key)))

(defun updates-plan (&key (offset nil) (timeout 20) (limit 50))
  "One long poll: the updates after OFFSET, waiting up to TIMEOUT seconds."
  (rest-plan "GET"
             (format nil "updates?timeout=~d&limit=~d~@[&offset=~a~]" timeout limit offset)
             "updates" nil (1+ timeout) nil))

(defun send-text-plan (to text &key reply-to (timeout 30))
  "One text message to TO, quoted onto REPLY-TO when it is not NIL."
  (flet ((plan (context)
           (rest-plan "POST" "messages" "send" t timeout
                      (nlk:json-object
                       "messaging_product" "whatsapp"
                       "recipient_type" "individual"
                       "to" to
                       "type" "text"
                       "text" (nlk:json-object "body" text)
                       :when context
                       "context" (nlk:json-object "message_id" (or context ""))))))
    (if reply-to (plan reply-to) (plan nil))))

(defun status-plan (message-id &key (typing nil) (timeout 20))
  "A read receipt on MESSAGE-ID, carrying the typing indicator when TYPING.
The platform's only sign of a turn in progress: a message it took cannot be
edited."
  (flet ((plan (indicator)
           (rest-plan "POST" "statuses" "status" nil timeout
                      (nlk:json-object
                       "messaging_product" "whatsapp"
                       "status" "read"
                       "message_id" message-id
                       :when indicator
                       "typing_indicator" (nlk:json-object "type" "text")))))
    (if typing (plan t) (plan nil))))

(defun message-id-of (body)
  "The id a send's answer gives: messages[0].id."
  (nlk:json-value body :string "messages" 0 "id"))
