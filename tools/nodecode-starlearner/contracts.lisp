;;;; contracts.lisp --- the three tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a call's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-starlearner)

(nlk:define-contract sync-request
  (:doc "Fetch the repositories the user has starred on GitHub (read-only, with the token the starlearner section names) and, INCREMENTALLY (a persistent seen ledger - a repository is never processed twice): categorize each NEW star into one of twelve domains, assess it on two tracks (worth integrating into the user's own makeup / business-opportunity value), and write a skill note for it under the cell's data directory. Use it when the user mentions their GitHub stars, starring repositories, or refreshing their skill library from GitHub. It answers counts, the new items and the paths of the notes it wrote.")
  ("max_pages" :integer :default 0
   :doc "how many pages of 100 stars to fetch this run; 0 means the section's max_pages (the hard cap on the fetch)")
  ("generate_limit" :integer :default -1
   :doc "how many skill notes to write per category this run, highest stars first; -1 means the section's generate_limit, 0 assesses without writing any note")
  ("dry_run" :boolean
   :doc "fetch and diff only: categorize and assess the new stars, write no note and leave the ledger unchanged"))

(nlk:define-contract review-request
  (:doc "Review what the syncs produced, item by item: each starred repository with its description, category, two-track assessment (integrate-into-makeup / business-opportunity plus the rationale) and the skill note written for it. This is the human-in-the-loop surface - an LLM-derived opportunity lands here, never silently in the ledger.")
  ("since" :string :default ""
   :doc "ISO-8601 time; only items first seen at or after it (empty means every item, newest first)")
  ("category" :string :default ""
   :doc "one of the twelve domain keys (bitcoin-lightning, ai-ml, privacy-security, finance-trading, development-tools, social-media, health-wellness, travel-exploration, voice-audio, video-streaming, education-learning, gaming-entertainment, or uncategorized)")
  ("opportunities_only" :boolean
   :doc "only the items whose assessment found a business opportunity")
  ("limit" :integer :default 50
   :doc "the most rows to answer, newest first")
  ("include_skill_md" :boolean
   :doc "carry the head of each generated skill note in its row"))
