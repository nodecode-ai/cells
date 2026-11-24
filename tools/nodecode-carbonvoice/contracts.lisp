;;;; contracts.lisp --- the four tools' arguments.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for each tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, the value the body runs on is materialized
;;;; from it, and a call that does not satisfy it is refused before the body
;;;; sees it.

(in-package #:nodecode-carbonvoice)

(nlk:define-contract recent-request
  (:doc "List the messages that came into Carbon Voice lately, newest last, with the transcript Carbon Voice made of each voice memo. Text messages carry their words. A message whose transcript is empty is still being transcribed: read it again with carbonvoice_message in a few seconds. Use it to find who wrote what and which conversation to answer in; carbonvoice_send posts the answer back into that conversation, threaded under the message it replies to.")
  ("since_minutes" :integer
   :doc "how far back to look, in minutes; the section's own window when empty")
  ("limit" :integer
   :doc "how many messages to return at most, newest first; the section's own limit when empty")
  ("conversation" :string :default ""
   :doc "one channel_guid to read, the id carbonvoice_recent reports for each message; empty reads every conversation"))

(nlk:define-contract message-request
  (:doc "Read one Carbon Voice message by its id: its transcript, who sent it and which conversation it is in. A voice memo arrives in two pieces — the audio first, the transcript a few seconds later — so an empty transcript here means Carbon Voice is still transcribing it and the same call answers it once it is done.")
  ("message_id" :text :required t
   :doc "the id carbonvoice_recent reported for the message"))

(nlk:define-contract send-request
  (:doc "Send a text message into a Carbon Voice conversation, threaded under the message being answered. Carbon Voice shows it to everyone in that conversation as the agent's own account, and a voice-memo conversation plays it as text with its own transcript. Answer in the conversation the message came from and thread it under that message, so the person sees it where they spoke.")
  ("conversation_id" :text :required t
   :doc "the channel_guid Carbon Voice gave the message: the conversation_id carbonvoice_recent reported")
  ("text" :text :required t
   :doc "what to say, plain text, at most 8000 characters; Carbon Voice keeps the line breaks")
  ("reply_to" :string :default ""
   :doc "the id of the message being answered, so the reply lands in its thread; empty posts a new top-level message"))

(nlk:define-contract channel-request
  (:doc "Read one Carbon Voice conversation: its name, its kind (a direct message, a workspace channel, a customer conversation, an async meeting) and who is in it, by display name and user id. Use it to turn the sender ids carbonvoice_recent reports into names, or to check who will see a reply before sending it.")
  ("conversation_id" :text :required t
   :doc "the channel_guid of the conversation"))
