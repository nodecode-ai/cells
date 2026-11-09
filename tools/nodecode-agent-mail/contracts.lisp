;;;; contracts.lisp --- the arguments of each tool, declared once.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A contract is the one declaration the provider's schema and the body's
;;;; record both come from: the tool cannot advertise one shape and dispatch
;;;; another. Field names are the gateway's own JSON names, so a prompt
;;;; written for its REST or MCP surface reads the same here.

(in-package #:nodecode-agent-mail)

(nlk:define-contract message-filter
  (:doc "List the mailbox's newest mail, INBOX only. Read mail before guessing: unread only shows what you have not opened, text searches older mail too.")
  ("unread" :boolean :doc "only unread messages (true) or only read ones (false)")
  ("text" :string :doc "words anywhere in the subject, sender or body")
  ("from" :string :doc "sender address or part of it")
  ("subject" :string :doc "words in the subject")
  ("since" :string :doc "on or after this time: YYYY-MM-DD, or a date-time (no offset is read in the mailbox time zone)")
  ("before" :string :doc "before this time, in the same forms as since")
  ("limit" :integer :doc "how many messages, 1 to 50 (20 unless sent)")
  ("cursor" :string :doc "next_cursor from the previous page, for older mail"))

(nlk:define-contract message-read
  (:doc "One message with its body as Markdown. Marks it as read unless mark_read is false; peek with false when you want to handle it later.")
  ("id" :text :required t :doc "the message id from mail_list")
  ("mark_read" :boolean :default t :doc "false to read without marking it read"))

(nlk:define-contract message-mark
  (:doc "Put a message back on the to-do pile (unread true) or mark it read (false).")
  ("id" :text :required t :doc "the message id")
  ("unread" :boolean :default t :doc "true marks it unread again, false marks it read"))

(nlk:define-contract message-delete
  (:doc "Move a message to Trash. Only when the mailbox allows it, and only when you were asked to.")
  ("id" :text :required t :doc "the message id"))

(nlk:define-contract message-attachment
  (:doc "One attachment of a received message, by the index mail_read listed. Text files (csv, txt, md, json, ics) come back as readable text; anything else as a note naming its type and size.")
  ("id" :text :required t :doc "the message id")
  ("index" :integer :required t :doc "the attachment's index, as mail_read lists it"))

(nlk:define-contract message-send
  (:doc "Send a new message. Write normal Markdown in body_markdown: it is sent as formatted HTML with a plain-text copy. Every send counts towards the mailbox's hourly limit, so combine updates into one message.")
  ("to" :array :of :string :required t :doc "recipient addresses, each one inside the mailbox's allow_send_to list")
  ("subject" :text :required t :doc "one clear subject")
  ("body_markdown" :text :required t :doc "the body, as Markdown")
  ("cc" :array :of :string :doc "copied recipients")
  ("bcc" :array :of :string :doc "blind-copied recipients")
  ("attachments" :array :doc "files, one of {filename, content_type, content_text}, {filename, content_type, content_base64}, or {from_message: {id, index}} to re-attach a received file")
  ("reply_to_id" :string :doc "a received message id, to keep the new mail in that thread"))

(nlk:define-contract message-reply
  (:doc "Answer a received message in its own thread: the recipients (the sender, or everyone in To/Cc with reply_all) and the Re: subject are filled in for you.")
  ("id" :text :required t :doc "the message id to answer")
  ("body_markdown" :text :required t :doc "the answer, as Markdown")
  ("reply_all" :boolean :doc "true to also answer everyone in To and Cc")
  ("attachments" :array :doc "files, in the same three forms as mail_send"))

(nlk:define-contract message-forward
  (:doc "Send a received message on to someone else, with the original's text and its attachments.")
  ("id" :text :required t :doc "the message id to forward")
  ("to" :array :of :string :required t :doc "recipient addresses, each one inside allow_send_to")
  ("body_markdown" :string :doc "an optional note above the forwarded text")
  ("cc" :array :of :string :doc "copied recipients")
  ("bcc" :array :of :string :doc "blind-copied recipients")
  ("include_attachments" :boolean :default t :doc "false to forward the text alone"))

(nlk:define-contract message-rsvp
  (:doc "Answer a calendar invitation you received. The organizer's calendar only updates with this tool, never with a normal email answer.")
  ("id" :text :required t :doc "the id of the message that carries the invitation")
  ("response" :text :required t :options '("accept" "decline" "tentative") :doc "accept, decline or tentative")
  ("comment" :string :doc "an optional line for the organizer"))

(nlk:define-contract events-query
  (:doc "The mailbox's calendar events, newest first, with the ids mail_update_event and mail_cancel_event take; or one event when id is given, including who accepted, declined or answered tentatively.")
  ("id" :string :doc "one event's id, for that event alone")
  ("limit" :integer :doc "how many events (the gateway's own default unless sent)"))

(nlk:define-contract event-create
  (:doc "Create an event and invite its attendees. Remember the returned id: moving or changing the meeting means mail_update_event with it, never a second event.")
  ("title" :text :required t :doc "the meeting's title")
  ("start" :text :required t :doc "start, e.g. 2026-10-05T14:00 in the mailbox time zone, or with an offset")
  ("end" :text :required t :doc "end, in the same forms as start")
  ("attendees" :array :of :string :required t :doc "attendee addresses, each one inside allow_send_to")
  ("location" :string :doc "where it is")
  ("description_markdown" :string :doc "what it is about, as Markdown")
  ("timezone" :string :doc "the time zone for times without an offset, e.g. Europe/Berlin"))

(nlk:define-contract event-update
  (:doc "Change an existing event: send only the fields that change. Attendees you remove get a cancellation automatically.")
  ("id" :text :required t :doc "the event id")
  ("title" :string :doc "a new title")
  ("start" :string :doc "a new start")
  ("end" :string :doc "a new end")
  ("attendees" :array :of :string :doc "the new attendee list; anyone missing from it is cancelled")
  ("location" :string :doc "a new place")
  ("description_markdown" :string :doc "a new description")
  ("timezone" :string :doc "the time zone for times without an offset"))

(nlk:define-contract event-cancel
  (:doc "Cancel an event for everyone who was invited.")
  ("id" :text :required t :doc "the event id"))
