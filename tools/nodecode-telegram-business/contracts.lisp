;;;; contracts.lisp --- the three tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-telegram-business)

(nlk:define-contract business-poll
  (:doc "Read what arrived on the Telegram business connection(s): new customer messages, connection changes, and the owner's button presses on the draft replies this cell DMed them. Call it with no arguments for what is waiting; raise timeout to hold the call open (long polling) when told to wait for the next customer. Each customer event carries the connection, the chat and the message the reply must be drafted for; answer the owner's send/edit/discard presses are applied here, so call this again after a draft to learn what the owner chose.")
  ("timeout" :integer :default 0
   :doc "seconds to hold the call open waiting for the next update, 0 to answer at once (Telegram caps it at 50)")
  ("limit" :integer :default 50
   :doc "the most updates to read in one call"))

(nlk:define-contract business-draft
  (:doc "Record a reply drafted for one customer message and DM it to the owner with Send, Edit and Discard buttons. Nothing is delivered to the customer until the owner acts. Draft in the owner's voice: short, warm, personal, the customer's own language, the persona the poll answer carries. One pending draft per customer chat: a newer draft supersedes the older one.")
  ("connection_id" :text :required t
   :doc "the business connection the customer wrote through, as a poll event names it")
  ("chat_id" :text :required t
   :doc "the customer's chat id, as a poll event names it")
  ("customer_message_id" :integer
   :doc "the id of the customer message this replies to")
  ("customer_text" :string :default ""
   :doc "what the customer wrote, quoted in the owner's DM; at most 4000 characters, longer text is cut")
  ("text" :text :required t
   :doc "the reply to deliver if the owner approves it"))

(nlk:define-contract business-send
  (:doc "Deliver a recorded draft to its customer now, on the owner's behalf: the path for a draft the owner approved in conversation instead of by button, or one they want changed first. It refuses a draft that expired, was discarded, was superseded, or was already sent, and refuses when Telegram does not grant the connection the right to reply for the owner.")
  ("draft_id" :text :required t
   :doc "the draft to deliver, as business-draft answered it")
  ("text" :string :default ""
   :doc "the text to deliver instead of the drafted one; empty delivers the draft as recorded"))
