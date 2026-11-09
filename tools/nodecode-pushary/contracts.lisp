;;;; contracts.lisp --- the six tools' arguments, once each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for each tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value the body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it.

(in-package #:nodecode-pushary)

(nlk:define-contract notice
  (:doc "Send a push notification to the operator's phone through Pushary. Use it when a task finishes, a long job ends, a check fails, or something needs a person and they are away from the machine: the notification is read on a lock screen, so lead with the fact. context_type 'success', 'error' or 'info' with summary, details, files_changed, error_message and next_steps adds a detail page behind the notification. Do not narrate: the transcript already carries everything.")
  ("body" :text :required t
   :doc "the notification's body, plain text, at most 500 characters and best under 200: the fact, first")
  ("title" :string :default ""
   :doc "a short headline over the body, at most 100 characters; the cell's own title when empty")
  ("context_type" :string :default ""
   :doc "adds a detail page: success, error or info; empty sends the notification alone")
  ("summary" :string :default ""
   :doc "one line shown on the detail page")
  ("details" :string :default ""
   :doc "bullet points for the detail page, one per line")
  ("files_changed" :array :of :string
   :doc "the files this turn changed, for the detail page")
  ("error_message" :string :default ""
   :doc "the error's own words, for context_type error")
  ("next_steps" :string :default ""
   :doc "what the person might do next, for the detail page"))

(nlk:define-contract ask
  (:doc "Ask the operator a question on their phone through Pushary and wait for the answer. Types: confirm (yes/no), select (pick one of options, 2 to 6), input (free text). By default the call blocks: it asks, and when the answer has not come back it polls once, up to timeout_ms. Returns the answer as JSON — value, answered, correlationId — or answered false when the person has not answered yet; pushary_wait then continues the wait. For a dangerous tool call, pass tool_name, action and blocker so the phone shows what, and why, it is being asked.")
  ("question" :text :required t
   :doc "the question, plainly, at most 8000 characters: what you need decided, and the one fact that decides it")
  ("type" :string :slot kind :default "confirm"
   :doc "confirm (yes/no, the default), select (one of options) or input (free text)")
  ("options" :array :of :string
   :doc "the choices for type select, 2 to 6 of them")
  ("placeholder" :string :default ""
   :doc "the hint inside the text field for type input")
  ("context" :string :default ""
   :doc "what this is about, shown above the question")
  ("wait" :boolean :default t
   :doc "wait for the answer before returning (the default); false answers a correlationId at once, for pushary_wait to poll")
  ("timeout_ms" :integer :default 30000
   :doc "how long one wait may block, in milliseconds, at most 55000")
  ("tool_name" :string :default ""
   :doc "the tool this approval is for (for example sh), so the phone can offer always-allow")
  ("tool_target" :string :default ""
   :doc "the compact target of that call, such as the command head")
  ("intent" :string :default ""
   :doc "the task as the person stated it, one line")
  ("action" :string :default ""
   :doc "the concrete operation about to happen, one line")
  ("blocker" :string :default ""
   :doc "the one reason you stopped, one line")
  ("action_body" :string :default ""
   :doc "the diff or the whole command, shown collapsed on the phone"))

(nlk:define-contract poll
  (:doc "Poll once for the answer to a question pushary_ask answered with wait false, through Pushary. Pass the correlationId pushary_ask returned; do not loop — ask again in a later turn, or answer the person another way.")
  ("correlation_id" :text :required t
   :doc "the correlationId pushary_ask returned")
  ("timeout_ms" :integer :default 30000
   :doc "how long this one poll may block, in milliseconds, at most 55000"))

(nlk:define-contract cancel
  (:doc "Cancel a pending Pushary question so it can no longer be answered: use it when the question stopped being relevant, before asking a different one.")
  ("correlation_id" :text :required t
   :doc "the correlationId of the question to cancel"))

(nlk:define-contract reminder
  (:doc "Schedule, list or cancel a personal reminder on Pushary, which reaches the phone even after this session ends. Give body plus in_minutes or at (an ISO 8601 time with an explicit UTC offset, within 30 days); never guess the person's timezone. No arguments lists the pending reminders; cancel_reminder_id cancels one.")
  ("body" :string :default ""
   :doc "what to remind them of, at most 180 characters")
  ("title" :string :default ""
   :doc "a short headline, at most 100 characters")
  ("in_minutes" :integer
   :doc "how many minutes from now the reminder should come")
  ("at" :string :default ""
   :doc "when it should come, as ISO 8601 with Z or a UTC offset")
  ("cancel_reminder_id" :string :default ""
   :doc "the id of a scheduled reminder to cancel")
  ("reminder_id" :string :default ""
   :doc "the id of a reminder to look up instead of scheduling or cancelling"))

(nlk:define-contract scope
  (:doc "Propose what this run will touch and block until the operator ratifies it on their phone: the paths you intend to change, the ones you promise to leave alone, and your definition of done, agreed in one tap. After that, editing a file outside the agreed paths becomes a separate request to widen the scope rather than a silent approval. Only file paths are enforced.")
  ("done_when" :text :required t
   :doc "what finished means for this run, one or two lines")
  ("allowed_paths" :array :of :string
   :doc "the globs you intend to change, such as src/** or docs/*.md")
  ("off_limits_paths" :array :of :string
   :doc "the globs you promise not to touch; they win wherever they overlap allowed_paths")
  ("promises" :array :of :string
   :doc "what you promise holds for a run that changes no files, when there are no paths to agree")
  ("timeout_ms" :integer :default 50000
   :doc "how long to block waiting for the ratification, in milliseconds, at most 55000"))
