;;;; contracts.lisp --- the two tools' arguments.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One authority per tool: the JSON Schema the model is shown is projected
;;;; from these, the value the body runs on is materialized from them, and a
;;;; call that does not satisfy one is refused before the body sees it.

(in-package #:nodecode-call-me)

(nlk:define-contract spoken-call
  (:doc "Ring the operator's phone through /call-me, say TEXT out loud, and wait for their spoken answer, which comes back as a transcript. Use it when a text is not enough: something urgent, or an answer you need to hear now. Prefer callme_text for everything else — a call interrupts a person. Keep the text short and end with a question that can be answered in one sentence. Only numbers listed in the section's allowed_users can be rung. The call ends when the person hangs up, or when the wait runs out: the answer then says missed.")
  ("text" :text :required t
   :doc "what to say out loud, spoken verbatim, at most 2000 characters: one or two sentences ending in a question")
  ("to" :string :default ""
   :doc "who to ring: one of the allowed 10-digit /call-me numbers, or a name from the section's names; empty when only one number is allowed")
  ("wait_seconds" :integer
   :doc "how long to wait for the call to finish, 30 to 300 seconds; the section's call_wait when empty"))

(nlk:define-contract text-message
  (:doc "Text the operator's phone through /call-me: the message arrives in the /call-me iPhone app's thread for this agent, and their reply comes back through the same thread when the room is running. Use it for what a person should read when they look at their phone rather than for something that cannot wait — that is callme_call. Only numbers listed in the section's allowed_users can be texted, so a called-off agent cannot reach a stranger.")
  ("message" :text :required t
   :doc "the text to send, plain text, no markdown, at most 2000 characters: it is read on a phone, so lead with the fact")
  ("to" :string :default ""
   :doc "who to text: one of the allowed 10-digit /call-me numbers, or a name from the section's names; empty when only one number is allowed"))
