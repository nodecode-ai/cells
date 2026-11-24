;;;; cell.lisp --- the section, the tool, the command, the declaration.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The plugin was a web-search provider behind Hermes's `web.search_backend'
;;;; switch, plus a settings card in its desktop half. Nodecode's websearch cell
;;;; keeps its engines in a fixed list a cell cannot add one to, so the fan-out
;;;; here is the cell's own tool, federated_search, and /federated-search is the
;;;; operator's way in. The section carries the plugin's own keys, and the
;;;; backends array its settings form edited.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "federated-search": {"timeout": 10, "max_results": 8, "min_backends": 1,
;;;;                        "pool_strategy": "fill_first", "max_chars": 400,
;;;;                        "backends": [{"name": "tavily"}, ...]}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-federated-search)

(nle:define-cell federated-search
  (:section ("federated-search")
    (:guide "backends is the one thing this cell cannot run without: an array of search lines, each an object naming at least its name -- tavily, firecrawl and a searxng instance are known by name, and any other endpoint taking the query as a parameter is a line of its own (base_url, search_path, query_param, api_key_env or credentials, auth_style); keys are environment variable names, never values; timeout bounds the whole fan-out, max_results caps one answer, min_backends is how many lines must answer for the search to count, and a line marked required must answer whatever min_backends says")
    ("timeout" :integer :default 10 :min 1
     :doc "seconds for the whole fan-out: lines still unanswered when it passes are dropped, and the ones that answered make up a partial answer")
    ("max_results" :integer :default 8 :min 1
     :doc "the most ranked results one answer carries; a call's limit caps it further")
    ("min_backends" :integer :default 1 :min 1
     :doc "how many lines must return results for a search to succeed; a line marked required must always")
    ("pool_strategy" :choice :options '("fill_first" "round_robin" "random") :default "fill_first"
     :doc "which of a line's several keys a request spends: fill_first holds one until it fails, round_robin rotates, random picks; a line may say its own")
    ("max_chars" :integer :default 400 :min 40
     :doc "how much of one result's snippet an answer carries"))
  (:settings 'read-settings)
  (:help :federated-search
         "federated_search: one query across every search backend you configured - (help :federated-search)"
         +manual+)
  (:command "federated-search" 'run-command
            :description "Federated search: the lines and their keys, or a search across them"
            :argument-hint "QUERY"
            :session nil)
  (:tool "federated_search" (nlk:contract 'federated-call) #'tool-search)
  (:start (lambda ()
            (clrhash *cooldowns*)
            (clrhash *rotation*))))
