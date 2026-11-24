;;;; contracts.lisp --- the one tool's arguments.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for the tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, the value the body runs on is materialized
;;;; from it, and a call that does not satisfy it is refused before the body
;;;; sees it.

(in-package #:nodecode-telegram-notify)

(nlk:define-contract notification
  (:doc "Send one Telegram message through the operator's own bot. Use it for what the person should know now and is not watching the session for: a long job finished, a deploy went out, a check failed. The message is the message body — plain text, at most 4096 characters, and it is read on a lock screen, so lead with the fact and leave the narration to the transcript. Do not send one per turn, and never as a substitute for answering: the answer itself is what the transcript carries.")
  ("message" :text :required t
   :doc "the message body, plain text, at most 4096 characters: the fact, first")
  ("chat_id" :string :default ""
   :doc "the chat to deliver to when it is not the configured one — an id the bot can see; empty means the section's chat")
  ("silent" :boolean
   :doc "deliver without a notification sound, for something that can wait"))
