;;;; contracts.lisp --- the five tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-nexus)

(defparameter +categories+ '("fact" "preference" "belief" "rule" "session" "entity" "temp")
  "The categories the original stores a memory under.")

(defparameter +levels+ '("public" "trusted" "private")
  "The access levels, least to most restrictive. A memory, a rule or an
entity without a known level counts as the most restrictive.")

(nlk:define-contract nexus-recall
  (:doc "Search Nexus Memory for the memories nearest the query: what the user said before, as notes with their ids. Use it before answering anything that depends on earlier context (preferences, names, decisions, projects), and before nexus_remember so the same fact is not stored twice. An empty answer is not a failure: nothing is stored about it yet.")
  ("query" :text :required t
   :doc "what to look for, in words - a question or a phrase, not keywords")
  ("limit" :integer
   :doc "the most memories to answer; overrides the section's recall_limit for this call")
  ("as_of" :string :default ""
   :doc "point in time, YYYY-MM-DD: only memories created on or before that date, empty for now"))

(nlk:define-contract nexus-remember
  (:doc "Store one durable fact in Nexus Memory, shared with every agent on the same collection. Write it as one self-contained sentence that still makes sense weeks later, with the name it is about (\"Prefers green tea over coffee\", not \"prefers that\"). The answer carries the id nexus_forget takes. A rule stored here protects what it names: nexus_guardrail_check blocks a destructive command that touches it.")
  ("text" :text :required t
   :doc "the fact, one self-contained sentence")
  ("category" :string :default "fact" :options +categories+
   :doc "what kind of memory this is; a rule is also a protection rule")
  ("access_level" :string :default "private" :options +levels+
   :doc "who may read it: public, trusted or private")
  ("source" :string :default ""
   :doc "where the fact came from, when it is not this conversation")
  ("source_url" :string :default ""
   :doc "a URL that verifies the fact, when there is one")
  ("confidence" :number :default 0.7
   :doc "how sure this is true, 0.0 to 1.0")
  ("salience" :number
   :doc "how much it matters, 0.0 to 1.0; 0.8 and above is immune to decay, absent takes the category's default"))

(nlk:define-contract nexus-forget
  (:doc "Delete one memory from Nexus Memory for good, by the id nexus_recall or nexus_remember answered. Recall it first so the id is the memory meant, and say what was forgotten.")
  ("memory_id" :text :required t
   :doc "the id nexus_recall answered for the memory to forget"))

(nlk:define-contract nexus-guardrail-check
  (:doc "Check whether a command is safe before running it: Nexus Memory's protection rules are matched against the command's targets, and a target a rule protects comes back as verdict block, with the rule that matched. Call it before anything destructive - a recursive delete, a drop, a truncate, a kill, an overwrite - and stop when the verdict is block unless the user explicitly authorised that exact action.")
  ("command" :text :required t
   :doc "the command line, as it would run")
  ("tool_name" :string :default ""
   :doc "the tool or shell the command runs in, when it is not a shell"))

(nlk:define-contract nexus-find-entities
  (:doc "List the entities (devices, services, people, places, software) Nexus Memory has stored, with their ids and attributes. Use it to find what the user's setup is made of before asking about one piece of it.")
  ("entity_type" :string :default ""
   :doc "only entities of this type: device, service, person, location, organization, concept, software, protocol; empty for all")
  ("limit" :integer :default 50
   :doc "the most entities to answer"))
