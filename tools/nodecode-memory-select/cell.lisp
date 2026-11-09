;;;; cell.lisp --- the manual, the section, the tool, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "memory-select": {"max_results": 10, "max_tokens": 4000, "min_relevance": 0.15}
;;;; Nothing is required and nothing reaches the network: the selection is
;;;; deterministic, so the same memories and the same query answer the same
;;;; list in every session.

(in-package #:nodecode-memory-select)

;;; --- the manual ---------------------------------------------------------------
;;; (help :memory-select) answers it while the cell runs; a request carries the
;;; one line the :HELP clause below gives, never this text.

(defparameter *primer*
  "A retrieved memory list is filtered before it reaches the prompt by the
nodecode-memory-select cell: one model tool, memory_select, and the same call in
the MEMORY-SELECT package from eval.
  (memory-select:select \"where does the app keep its data\"
                        '((:id \"db\" :text \"PostgreSQL, on the build host\"
                           :importance 0.7 :created_at \"2026-11-01T09:00:00Z\")
                          (:id \"ui\" :text \"The settings page uses a dark theme\")))
Give it the memories you already retrieved and what the next turn is about. It
answers what to keep: each memory with its score, the tokens it costs and the
reasons it was kept (relevance=, recency=, importance=), and every memory left
behind with the one reason it was (out_of_scope, expired, duplicate_input,
low_relevance, max_items, budget, duplicate_of:ID). A memory that contradicts a
kept one is kept anyway and marked possible_conflict:ID.
The score is lexical relevance to the query (0.5), freshness on a half-life in
days (0.3) and the caller's own importance (0.2); ties go to the newer memory.
Two memories that share four fifths of their words keep only the first. The
budget is estimated at about four characters to the token, and stops the list
rather than cutting a memory in half. A key-shaped string in a memory (sk-...,
a private key block, a password assignment) answers as a marker, never the key.
What it does NOT do: it retrieves nothing (give it the candidates), it writes
no prose, and it needs no key and no network - the same list answers the same
way every time."
  "What (help :memory-select) answers while the cell runs.")

;;; --- the boundary ---------------------------------------------------------------

(nlk:define-contract memory-selection-request
  (:doc "Filter a list of memories you already retrieved, keeping the ones that
answer the query inside a token budget. Use it before putting many memories in a
prompt: give the candidates and what the turn is about, and send only what comes
back as selected.")
  ("query" :text :required t
   :doc "what the next turn is about; a memory sharing no word with it scores zero")
  ("memories" :array :required t
   :doc "the candidate memories, each an object with id and text (or content), and optional created_at (ISO-8601), importance (0 to 1), tags, scope, expires_at, similarity")
  ("max_memories" :integer
   :doc "how many to keep at most; the section's max_results answers when this is absent")
  ("max_tokens" :integer
   :doc "the token budget to stay inside; the section's max_tokens answers when this is absent")
  ("scope" :string
   :doc "the scope to select from, \"default\" when absent; a memory in another scope is dropped"))

(defun memory-select-tool (request)
  "The memory_select tool: the selection the request asks for, as JSON."
  (to-json (select (memory-selection-request-query request)
                    (memory-selection-request-memories request)
                    :scope (memory-selection-request-scope request)
                    :max-memories (memory-selection-request-max-memories request)
                    :max-tokens (memory-selection-request-max-tokens request))))

;;; --- the section's numbers --------------------------------------------------------------

(defun section-number (text default)
  "TEXT as a double, or DEFAULT when it is not a number. A decimal read here
is a double, so 0.8 is exactly the 0.8 a dedupe threshold compares against."
  (let* ((*read-eval* nil)
         (*read-default-float-format* 'double-float)
         (value (cond ((realp text) text)
                      ((stringp text) (ignore-errors (read-from-string text))))))
    (if (realp value) (coerce value 'double-float) default)))

(defun settings-from (values table)
  "The section's VALUES as the plist SELECT runs on. A section carries no
decimal type, so min_relevance, recency_days and dedupe are declared as text
and read here; one that is not a number reads as its default."
  (declare (ignore table))
  (list :max-results (getf values :max-results)
        :max-tokens (getf values :max-tokens)
        :min-relevance (max 0d0 (min 1d0 (section-number (getf values :min-relevance) 0.15d0)))
        :recency-days (max 0.5d0 (section-number (getf values :recency-days) 14d0))
        :dedupe (max 0d0 (min 1d0 (section-number (getf values :dedupe) 0.8d0)))
        :redact (getf values :redact)))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell memory-select
  (:section ("memory-select")
    (:guide "nothing here is required: the defaults keep ten memories of about 4000 tokens, and min_relevance drops a memory that shares no word with the query")
    ("max_results" :integer :default 10 :min 1
                   :doc "how many memories one selection may keep (default 10)")
    ("max_tokens" :integer :default 4000 :min 1
                  :doc "the token budget one selection may spend (default 4000)")
    ("min_relevance" :string :default "0.15"
                     :doc "the share of the query's words a memory must carry: a number from 0 to 1 (default 0.15; 0 keeps everything)")
    ("recency_days" :string :default "14"
                    :doc "the half-life in days of a memory's freshness: a number, at least 0.5 (default 14)")
    ("dedupe" :string :default "0.8"
              :doc "how much of its words two memories must share to keep only the first: a number from 0 to 1 (default 0.8; 0 turns the test off)")
    ("redact" :boolean :default t
              :doc "answer a key-shaped string in a memory as a marker instead of the key"))
  (:settings #'settings-from)
  (:tool "memory_select" (nlk:contract 'memory-selection-request) #'memory-select-tool)
  (:help :memory-select
   "memory-select: keep the memories that fit the query and the token budget - the memory_select tool, and (memory-select:select \"query\" memories)"
   *primer*))
