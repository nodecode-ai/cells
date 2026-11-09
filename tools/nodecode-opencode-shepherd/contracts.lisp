;;;; contracts.lisp --- the seven tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-opencode-shepherd)

(nlk:define-contract session-list
  (:doc "List the person's opencode sessions, most recently updated first: each one's id, title, working directory, whether its turn is running, whether it is a sub-session and how long since it moved. This is the inventory to read before tailing, steering or answering one; opencode_waiting says which of them needs the person.")
  ("limit" :integer :default 20
   :doc "how many sessions to answer, most recent first"))

(nlk:define-contract session-waiting
  (:doc "What is waiting on the person right now across their opencode sessions: a question the agent asked, a permission request it needs answered, or a turn that finished and sits waiting for input. Answers, for each one, the session id and title, the pending question with its fields and options or the permission with its action and resources, and a one-line text summary. A session waiting on a background shell is not reported: opencode resumes it by itself when the shell exits, and neither is a sub-session's finished turn, which its parent reports.")
  ("session" :string :default ""
   :doc "limit the answer to one session, named by its id or a fragment of its id or title; omit for every recent session")
  ("window_hours" :integer :default 12
   :doc "how far back to look for a session that is not running")
  ("max_sessions" :integer :default 20
   :doc "at most how many sessions to inspect"))

(nlk:define-contract session-tail
  (:doc "The last messages of one opencode session, oldest first, one line each: the assistant's text, the tools it called with their status, what the person typed, and how a turn ended. Read this before steering a session, to know what it is doing.")
  ("session" :text :required t
   :doc "the session, by its id or a fragment of its id or title")
  ("count" :integer :default 10 :slot messages
   :doc "how many of the last messages to answer"))

(nlk:define-contract session-send
  (:doc "Send a message to an opencode session, as the person would from its own input box. By default the message steers the turn that is running now; with queue=1 it is delivered when that turn ends. This is the reply to a session that asked something, and the way to start work in a session that finished.")
  ("session" :text :required t
   :doc "the session, by its id or a fragment of its id or title")
  ("text" :text :required t
   :doc "the message, in the person's own words")
  ("queue" :boolean :default nil
   :doc "wait for the running turn to end instead of steering into it"))

(nlk:define-contract permission-reply
  (:doc "Answer the first pending permission request of an opencode session: once (this request only), always (for the rest of the session) or reject. This is a real approval the agent acts on, so relay exactly what the person decided and nothing more; opencode_waiting shows the request, its action and its resources first.")
  ("session" :text :required t
   :doc "the session, by its id or a fragment of its id or title")
  ("decision" :text :required t :options '("once" "always" "reject")
   :doc "once, always or reject")
  ("message" :string :default ""
   :doc "what to tell the agent with the decision, e.g. why it was rejected"))

(nlk:define-contract form-reply
  (:doc "Answer the first pending question of an opencode session, one member per field: the field's key as opencode_waiting names it, and the value to answer. opencode_waiting lists the fields and the options each accepts, so a question is answered without guessing.")
  ("session" :text :required t
   :doc "the session, by its id or a fragment of its id or title")
  ("answers" :object :required t
   :doc "one member per field key: {\"q0\": \"the answer\"}"))

(nlk:define-contract session-interrupt
  (:doc "Interrupt the turn an opencode session is running, as its stop button does. The session stays open with what it has so far, and can be steered afterwards.")
  ("session" :text :required t
   :doc "the session, by its id or a fragment of its id or title"))
