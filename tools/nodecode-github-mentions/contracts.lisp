;;;; contracts.lisp --- the three tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-github-mentions)

(nlk:define-contract mention-list
  (:doc "List the GitHub notifications that mention you by @login and that the operator's allowed_authors lets you answer: unread threads whose reason is a mention, each with the text of the mention. Start here, then answer one with github_mention_reply (or read the thread around it first with github_mention_thread).")
  ("repo" :string :default ""
   :doc "only mentions in this repository, as \"owner/name\"; empty for every repository the token reaches")
  ("max" :integer :slot max-mentions
   :doc "the most mentions to answer for this call; overrides the operator's max_mentions"))

(nlk:define-contract mention-thread
  (:doc "Read the GitHub thread a mention came from: its title, its body and its most recent comments, so an answer fits the conversation. The mention itself is already in the github_mentions answer; this is the context around it.")
  ("notification_id" :text :required t
   :doc "the notification_id github_mentions answered for the mention (GitHub's number, e.g. \"1234567\")"))

(nlk:define-contract mention-reply
  (:doc "Answer a GitHub @mention in its own thread, as the GitHub user the token belongs to, then mark the notification read so it is not listed again. Pass the notification_id github_mentions answered. An empty body marks the mention read without answering (dismiss). A mention in a pull request's inline review is answered as an inline reply in the same review thread. Write Markdown; say what you did and what you need, in the thread's own language.")
  ("notification_id" :text :required t
   :doc "the notification_id github_mentions answered for the mention (GitHub's number, e.g. \"1234567\")")
  ("body" :string :default ""
   :doc "the comment to post, in Markdown; empty to mark the mention read without answering")
  ("mark_read" :boolean :default t
   :doc "mark the notification read after posting, so the next github_mentions call does not list it again"))
