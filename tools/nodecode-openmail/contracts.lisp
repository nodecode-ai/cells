;;;; contracts.lisp --- the nine tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-openmail)

(nlk:define-contract mail-send
  (:doc "Start a new email thread from the agent's own inbox. Use it to reach a person or a service: contacting support, signing up, asking for a reply. To answer mail that arrived, use openmail_reply instead - it keeps the thread and the subject. The answer is the sent message, with its id and thread id; keep the thread id to read the reply later.")
  ("to" :text :required t
   :doc "the one recipient address (support@example.com)")
  ("subject" :text :required t
   :doc "the subject line; it starts the thread")
  ("body" :text :required t
   :doc "the plain-text body, sent verbatim")
  ("cc" :array :of :string
   :doc "extra recipients to copy")
  ("inbox_id" :string
   :doc "the inbox to send from; defaults to the operator's inbox_id, or to the key's only inbox"))

(nlk:define-contract mail-reply
  (:doc "Answer inside an existing OpenMail thread: the subject is the thread's, and the recipient comes from the last inbound message when `to` is not given. Use it for every answer to mail that arrived; a new openmail_send would start a second thread. The answer is the sent message.")
  ("thread_id" :text :required t
   :doc "the thread to answer, as openmail_list_threads or openmail_read_thread answered it")
  ("body" :text :required t
   :doc "the plain-text body, sent verbatim")
  ("to" :string
   :doc "override the recipient; default is the sender of the thread's last inbound message")
  ("cc" :array :of :string
   :doc "extra recipients to copy")
  ("inbox_id" :string
   :doc "the inbox to send from; default is the inbox that received the thread's last inbound message")
  ("quote" :boolean :slot quote-previous :default t
   :doc "false to leave the quoted previous message out of the body"))

(nlk:define-contract mail-threads
  (:doc "List the email threads of an inbox, newest first. Ask for unread ones to find what is new; openmail_read_thread marks a thread read.")
  ("inbox_id" :string
   :doc "the inbox to list; defaults to the operator's inbox_id, or to the key's only inbox")
  ("read_state" :string :options '("any" "unread" "read") :default "any"
   :doc "unread to list only threads nobody has read, read for only those already read, any for both")
  ("limit" :integer :default 20
   :doc "how many threads to answer at most")
  ("offset" :integer
   :doc "how many threads to skip, for a second page"))

(nlk:define-contract mail-thread-read
  (:doc "Read every message of one thread, oldest first, with each message's sender, subject, bodyText and attachments. It marks the thread read unless mark_read is false. Read bodyText: when it is empty the sender wrote HTML only, and bodyHtml carries it.")
  ("thread_id" :text :required t
   :doc "the thread to read")
  ("mark_read" :boolean :default t
   :doc "false to read the thread without marking it read"))

(nlk:define-contract mail-messages
  (:doc "List individual messages of an inbox, newest first, filtered by direction. It has no notion of seen - use openmail_list_threads with read_state unread to find new mail.")
  ("inbox_id" :string
   :doc "the inbox to list; defaults to the operator's inbox_id, or to the key's only inbox")
  ("direction" :string :options '("inbound" "outbound")
   :doc "inbound for mail that arrived, outbound for mail this inbox sent; omit for both")
  ("limit" :integer :default 20
   :doc "how many messages to answer at most")
  ("offset" :integer
   :doc "how many messages to skip, for a second page"))

(nlk:define-contract mail-attachment
  (:doc "Plain text extracted from one attachment, server-side (PDF, DOCX, XLSX, and images by OCR). Take message_id and filename from the attachments list openmail_read_thread answered.")
  ("message_id" :text :required t
   :doc "the message the attachment arrived with")
  ("filename" :text :required t
   :doc "the attachment's filename, exactly as the message lists it"))

(nlk:define-contract mail-inbox-create
  (:doc "Create a new inbox - a new email address for the agent. Needs a pod- or account-scoped key. The address is live immediately; target it with inbox_id on send, reply and list.")
  ("display_name" :string
   :doc "the display name the inbox sends under, e.g. Support")
  ("mailbox_name" :string
   :doc "the local part of the address, e.g. sales for sales@omail.sh")
  ("pod_id" :string
   :doc "the pod to create the inbox in; omit to use the key's own"))
