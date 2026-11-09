;;;; contracts.lisp --- the two tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-marketing)

(nlk:define-contract skill-search
  (:doc "Find the marketing skills that answer a question, by words in their name, folder or one-sentence description. The library holds 207 skills in three groups: strategy (positioning, SEO/AEO/GEO, paid media, CRM, analytics, pricing, launches), content (long-form articles, fact-checking, style guides, humanizing, editing) and social (a content calendar, captions, carousels, images and video). Search before you answer any marketing question: one skill is usually a whole procedure with checklists, templates and numbers. Then read the one that fits with marketing_skill.")
  ("query" :string
   :doc "words to look for, e.g. \"technical seo\", \"ab test sample size\", \"linkedin carousel\", \"press release\". Every word must appear in the skill's name, folder or description; an empty query lists the library from the top")
  ("group" :string
   :doc "limit the search to one group: strategy, content or social. Empty searches all three")
  ("limit" :integer :slot limit
   :doc "the most skills to answer, 20 when unset"))

(nlk:define-contract skill-read
  (:doc "Read one marketing skill: the whole SKILL.md the library holds for it — its procedure, checklists, worked numbers and examples. Call marketing_skills first and read the skill it names. A skill that names a script (scripts/*.py) was written to run it: this cell carries the prose only, so do the step by hand or say the script is needed.")
  ("name" :text :required t
   :doc "the skill's name as marketing_skills answered it, e.g. ab-test-plan; a group-qualified name (strategy/ab-test-plan) or the folder name works too")
  ("section" :string
   :doc "read only the part of the skill under the first heading holding these words, e.g. \"quality gates\" or \"output format\". Use it on a long skill, or to take one step at a time. Empty reads from the top")
  ("max_chars" :integer :slot max-chars
   :doc "how much of the skill to hand over, 0 for the operator's max_chars setting; a clipped answer lists the headings further down so the next read can name one"))
