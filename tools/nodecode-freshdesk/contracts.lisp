;;;; contracts.lisp --- the eleven tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-freshdesk)

(nlk:define-contract ticket-list
  (:doc "List Freshdesk tickets using bounded pagination (100 a page). With no updated_since, Freshdesk returns only the tickets created in the last 30 days. The description field is omitted unless include_description is true, so the answer stays small. A list cut by the page cap is marked truncated rather than silently short.")
  ("updated_since" :string :default ""
   :doc "an ISO 8601 timestamp (2026-01-31T09:00:00Z): only tickets updated since then; empty, Freshdesk's own default window of the last 30 days")
  ("include_description" :boolean :default nil
   :doc "include each ticket's description; off by default, because it is the largest field"))

(nlk:define-contract ticket-read
  (:doc "Get one Freshdesk ticket by id: subject, description, status, priority, requester, tags and attachments, as Freshdesk reports it.")
  ("ticket_id" :integer :required t
   :doc "the ticket's id, a positive integer (from a list or a conversation)"))

(nlk:define-contract ticket-search
  (:doc "Search Freshdesk tickets by status (2-5), priority (1-4), tag, type, agent_id, group_id or a created/updated date range. At least one filter is required: this is a read-only search, and subject, description and free-text keyword search are not supported. The query is built and URL-encoded here, so no raw query string is taken.")
  ("status" :integer
   :doc "ticket status: 2 open, 3 pending, 4 resolved, 5 closed")
  ("priority" :integer
   :doc "ticket priority: 1 low, 2 medium, 3 high, 4 urgent")
  ("tag" :string
   :doc "a tag, up to 64 characters of letters, digits, spaces, _ and -")
  ("type" :string :slot type-of-ticket
   :doc "the ticket type, e.g. Question or Incident, up to 64 characters of letters, digits, spaces, _ and -")
  ("agent_id" :integer
   :doc "the id of the agent the tickets are assigned to")
  ("group_id" :integer
   :doc "the id of the group the tickets belong to")
  ("created_after" :string
   :doc "created on or after this date, YYYY-MM-DD")
  ("created_before" :string
   :doc "created before this date, YYYY-MM-DD; it must be after created_after")
  ("updated_after" :string
   :doc "updated on or after this date, YYYY-MM-DD")
  ("updated_before" :string
   :doc "updated before this date, YYYY-MM-DD; it must be after updated_after"))

(nlk:define-contract conversation-list
  (:doc "List every conversation of a Freshdesk ticket: replies, notes and forwards, oldest first, each with its body, its privacy and who wrote it. A conversation list that reaches the page cap is an error rather than a short list, so nothing is silently missing.")
  ("ticket_id" :integer :required t
   :doc "the ticket's id, a positive integer"))

(nlk:define-contract contact-read
  (:doc "Get one Freshdesk contact by id: name, email, phone, company and the other fields Freshdesk keeps.")
  ("contact_id" :integer :required t
   :doc "the contact's id, a positive integer"))

(nlk:define-contract attachment-download
  (:doc "Download one attachment of a Freshdesk ticket (its own, or one of its conversations) under the operator's download directory. The URL comes from the ticket's attachment object and must be a Freshdesk or Freshdesk CDN/S3 origin: no caller-supplied URL is ever fetched. The answer names the file written and its size in bytes.")
  ("ticket_id" :integer :required t
   :doc "the ticket the attachment belongs to")
  ("attachment_id" :integer :required t
   :doc "the attachment's id, as the ticket or its conversations report it"))

(nlk:define-contract note-add
  (:doc "Add a private note to a Freshdesk ticket: plain text, HTML-escaped, visible to agents only. The note is read back through the ticket's conversations before success is reported. A person approves the write first.")
  ("ticket_id" :integer :required t
   :doc "the ticket to note on")
  ("body" :text :required t
   :doc "the note's text, plain (newlines become line breaks); it is escaped, so HTML in it is shown, not interpreted"))

(nlk:define-contract create-fields
  (:doc "The new ticket's fields: subject, description, priority, status, and exactly one of email or requester_id.")
  (:unknown :refuse)
  ("subject" :text :required t
   :doc "the ticket's subject")
  ("description" :text :required t
   :doc "the ticket's description as plain text; it is HTML-escaped before it is sent")
  ("priority" :integer :required t
   :doc "1 low, 2 medium, 3 high, 4 urgent")
  ("status" :integer :required t
   :doc "2 open, 3 pending, 4 resolved, 5 closed")
  ("email" :string
   :doc "the requester's email address; give this or requester_id, not both")
  ("requester_id" :integer
   :doc "an existing contact's id as the requester; give this or email, not both"))

(nlk:define-contract ticket-create
  (:doc "Create a Freshdesk ticket from plain text, then read it back and compare every field before reporting success. A person approves the write first.")
  ("fields" :object :of create-fields :required t
   :doc "subject, description, priority, status, and exactly one of email or requester_id"))

(nlk:define-contract ticket-reply
  (:doc "Send a public reply to the requester of a Freshdesk ticket; this emails them. The reply is read back through the ticket's conversations before success is reported. A person approves the write first.")
  ("ticket_id" :integer :required t
   :doc "the ticket to reply on")
  ("body" :text :required t
   :doc "the reply's text, plain (newlines become line breaks)"))

(nlk:define-contract time-entry-add
  (:doc "Add a finished (non-timer) time entry to a Freshdesk ticket, then read it back and compare it. A person approves the write first.")
  ("ticket_id" :integer :required t
   :doc "the ticket the time was spent on")
  ("time_spent" :text :required t
   :doc "the duration as H:MM through HHHH:MM, minutes 00-59 (1:30 and 000:01 are sent as 01:30 and 00:01); zero is refused")
  ("note" :text :required t
   :doc "what the time was spent on")
  ("billable" :boolean :required t
   :doc "whether the time is billable")
  ("agent_id" :integer
   :doc "the agent the entry belongs to; omitted, Freshdesk uses the API key's own agent"))

(nlk:define-contract ticket-update
  (:doc "Update a Freshdesk ticket's status and/or priority, then read the ticket back and compare it. A person approves the write first.")
  ("ticket_id" :integer :required t
   :doc "the ticket to update")
  ("status" :integer
   :doc "the new status: 2 open, 3 pending, 4 resolved, 5 closed")
  ("priority" :integer
   :doc "the new priority: 1 low, 2 medium, 3 high, 4 urgent"))
