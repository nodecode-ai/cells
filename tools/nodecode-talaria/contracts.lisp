;;;; contracts.lisp --- the four tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it.

(in-package #:nodecode-talaria)

(nlk:define-contract server-info
  (:doc "Report what the Hermes API server's talaria-webui plugin supports: its version, whether session changes and history search are available, how many commands it exposes, and the agent name it reads. Call this first when a call answers 'not enabled' or an action is missing.")
  ("full" :boolean :default nil
   :doc "answer the server's whole capability object instead of the summary fields"))

(nlk:define-contract history-search
  (:doc "Search the message history of every session the API key can see on the Hermes server, and answer the matches as pages of twenty: a session id, its title, the role of the message and a snippet. Use it to find where an earlier conversation covered a subject; then talaria_reply_details or talaria_context_usage reads one session further.")
  ("query" :text :required t
   :doc "what to look for: a word or phrase, 1 to 200 characters")
  ("offset" :integer :default 0
   :doc "how many matches to skip, for the next page"))

(nlk:define-contract reply-details
  (:doc "Read one reply's own details from the server: when it finished, why it stopped, which model answered, how many tokens it spent and whether it carried reasoning. Use it after talaria_search_history names a message, or to explain a turn that cost more than expected.")
  ("session_id" :text :required t
   :doc "the session the reply belongs to")
  ("message_id" :integer :required t
   :doc "the message id the reply was stored as"))

(nlk:define-contract context-usage
  (:doc "Read how full one session's context window is: the input tokens the model was last given, the model's maximum, and when that was observed. A session with no observed reply answers that nothing has been recorded yet.")
  ("session_id" :text :required t
   :doc "the session to read"))
