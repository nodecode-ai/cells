;;;; contracts.lisp --- the four tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-qdrant-memory)

(nlk:define-contract memory-search
  (:doc "Search the user's durable memories in Qdrant by meaning: answers the stored notes nearest the query, with their ids. Use it before answering anything that depends on what the user said earlier (preferences, names, decisions, projects), and before qdrant_memory_add so the same fact is not stored twice. An empty answer is not a failure: nothing has been stored about it yet.")
  ("query" :text :required t
   :doc "what to look for, in words — a question or a phrase, not keywords")
  ("limit" :integer
   :doc "the most memories to answer; overrides qdrant_memory.recall_limit for this call")
  ("namespace" :string :default ""
   :doc "the memory space to search; empty means the cell's configured namespace"))

(nlk:define-contract memory-add
  (:doc "Store one durable fact about the user in long-term memory. Write it as one self-contained sentence that will still make sense weeks later, with the name it is about (\"Prefers green tea over coffee\", not \"prefers that\"). The answer carries the id that qdrant_memory_update and qdrant_memory_delete take; an identical fact already stored answers stored=false with that id instead of adding a second copy.")
  ("text" :text :required t
   :doc "the fact, one self-contained sentence")
  ("tags" :array :of :string
   :doc "optional labels to file it under (work, preferences, people)")
  ("source" :string :default ""
   :doc "where the fact came from, when it is not this conversation (a file, a meeting, a url)"))

(nlk:define-contract memory-update
  (:doc "Correct one stored memory in place: its text (which is re-embedded, so it is found by the new wording), or nothing at all to stamp it as reviewed. Read the memory with qdrant_memory_search first and keep its id. The answer carries the updated memory.")
  ("id" :text :required t
   :doc "the id qdrant_memory_search or qdrant_memory_add answered")
  ("text" :string :default ""
   :doc "the corrected fact, one self-contained sentence; empty changes nothing about the text"))

(nlk:define-contract memory-delete
  (:doc "Forget one stored memory for good, by id. It is gone from Qdrant, so a later search cannot find it; use it when the user says something is wrong or no longer true. Read it with qdrant_memory_search first so the id is the memory meant, and say what was deleted.")
  ("id" :text :required t
   :doc "the id qdrant_memory_search answered for the memory to forget"))
