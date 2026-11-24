;;;; cell.lisp --- the primer, the section, the four tools, /openrouter-everything.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin put all of OpenRouter's non-chat APIs behind one key inside
;;;; Hermes: System One decisions and extractions, reranking, embeddings, and
;;;; server-tool web search, plus five tools and a model switcher. The model
;;;; switcher was one line of Hermes' config.yaml per capability, which is
;;;; this cell's section; what is left is the five calls, so they are five
;;;; verbs in the OPENROUTER-EVERYTHING package -- four of them also tools,
;;;; for the ones with a shape simple enough to advertise -- and one manual
;;;; topic the model reads.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "openrouter-everything": {"api_key": "", "api_key_env": "OPENROUTER_API_KEY",
;;;;                              "base_url": "", "decision_model": "",
;;;;                              "extraction_model": "", "rerank_model": "",
;;;;                              "embeddings_model": "", "web_search_model": "",
;;;;                              "timeout_seconds": 60}
;;;; An empty model is the plugin's curated default; an empty base_url is
;;;; https://openrouter.ai/api/v1. A vetoed section installs nothing.

(in-package #:nodecode-openrouter-everything)

(defparameter +primer+
  "OpenRouter's non-chat APIs are available through the nodecode-openrouter-everything cell, on the
one OpenRouter key this home already holds: five verbs in the OPENROUTER-EVERYTHING package, called
through eval. Each spends that key.
  (openrouter-everything:decide STATE :options MAP)          one decision, with probabilities
  (openrouter-everything:extract-fields STATE FIELDS)        typed fields with spans and confidence
  (openrouter-everything:rerank QUERY DOCUMENTS :top-n N)    documents ranked by relevance
  (openrouter-everything:embed TEXT-OR-LIST)                 vectors, dims and a similarity matrix
  (openrouter-everything:search QUERY :limit 5)              web search, with citations
Use them where one answer beats a conversation. decide and extract-fields answer in one forward pass
over the state you pass, so put the whole context in and ask once; a decision's options are a map of
option name to its criteria, and a multi-question schema (:questions) asks several independent
questions of the same state, which is cheaper than several calls. extract-fields wants a map of field
name to spec: (:type \"text\"|\"number\"|\"boolean\"|\"category\"|\"list\" :unit \"USD\" :choices (...) :items text).
rerank takes a list of candidate documents and answers them best first; embed takes several strings in
one call and compares them for you, which is how to cluster or dedupe without pulling the vectors.
search is OpenRouter's web_search server tool: it answers a synthesis plus the urls it read, so cite
them; the shipped websearch cell needs no key and is the cheaper default for a plain lookup.
Decisions and extractions live under /api/alpha, the rest under /api/v1, both derived from base_url.
Every call bills the key; a refused status or a missing key comes back as an
OPENROUTER-EVERYTHING-ERROR naming what to fix. The operator runs /openrouter-everything to see the
base, whether the key is set and which model each verb uses."
  "What (help :openrouter-everything) answers while the cell runs.")

(defparameter +verbs+ '("decide" "extract-fields" "rerank" "embed" "search")
  "The package's verbs, in the order the manual names them.")

;;; --- the tools ------------------------------------------------------------------

(nlk:define-contract decide-call
  (:doc "Answer one decision about a state: calibrated probabilities over candidate choices, from OpenRouter's Decisions API.")
  ("state" :text :required t
   :doc "the context, user query or document to judge")
  ("options" :object
   :doc "map of option name to the criteria that describe it, for one choice question")
  ("questions" :object
   :doc "OpenRouter's own multi-question schema: question name to (:type, :instructions, :criteria)")
  ("instructions" :string :default ""
   :doc "what the single choice question asks; ignored when questions is given")
  ("model" :string :default ""
   :doc "the decision model; empty is the configured one"))

(defun tool-decide (call)
  "The openrouter_decide tool: the decision, as the model reads it."
  (multiple-value-bind (answers model usage)
      (decide (decide-call-state call)
              :options (decide-call-options call)
              :questions (decide-call-questions call)
              :instructions (decide-call-instructions call)
              :model (decide-call-model call))
    (format nil "~a answered~@[ (~a)~]~%~a" model (usage-line usage)
            (nlk:encode-json-object answers))))

(nlk:define-contract extract-call
  (:doc "Extract typed fields, with spans and confidence, from a state, through OpenRouter's Extractions API.")
  ("state" :text :required t :doc "the source text or document")
  ("fields" :object :required t
   :doc "map of field name to its spec: type (text, number, boolean, category, list) and any of items, decimals, unit, choices")
  ("model" :string :default "" :doc "the extraction model; empty is the configured one"))

(defun tool-extract-fields (call)
  "The openrouter_extract_fields tool: the extracted fields, as the model reads them."
  (multiple-value-bind (answers model usage)
      (extract-fields (extract-call-state call) (extract-call-fields call)
                       :model (extract-call-model call))
    (format nil "~a answered~@[ (~a)~]~%~a" model (usage-line usage)
            (nlk:encode-json-object answers))))

(nlk:define-contract rerank-call
  (:doc "Rank candidate documents by relevance to a query, through OpenRouter's Rerank API.")
  ("query" :text :required t :doc "the question the documents answer")
  ("documents" :array :of :string :required t :doc "the candidate documents or search results")
  ("top_n" :integer :doc "keep only this many, best first")
  ("model" :string :default "" :doc "the rerank model; empty is the configured one"))

(defun tool-rerank (call)
  "The openrouter_rerank tool: the ranked documents, one per line."
  (multiple-value-bind (results model usage)
      (rerank (rerank-call-query call) (rerank-call-documents call)
              :top-n (rerank-call-top-n call)
              :model (rerank-call-model call))
    (format nil "~a ranked ~d document~:p~@[ (~a)~]~%~{~a~^~%~}"
            model (length results) (usage-line usage)
            (loop for result in results
                  for score = (nlk:json-value result :number "relevance_score")
                  for index = (nlk:json-value result :integer "index")
                  collect (format nil "~,4f  document ~a" (or score 0.0d0) (or index "?"))))))

(nlk:define-contract embed-call
  (:doc "Embed one or more texts through OpenRouter's Embeddings API; several texts come back with their similarity matrix.")
  ("input" :array :of :string :required t :doc "the texts to embed, all in one request")
  ("include_vectors" :boolean :default nil :doc "return the full float vectors, not only a preview")
  ("model" :string :default "" :doc "the embeddings model; empty is the configured one"))

(defun tool-embed (call)
  "The openrouter_embed tool: dimensions, and how near each text is to the others."
  (multiple-value-bind (answer model usage)
      (embed (embed-call-input call)
             :include-vectors (embed-call-include-vectors call)
             :model (embed-call-model call))
    (format nil "~a embedded ~d text~:p of ~d dimensions~@[ (~a)~]~@[~%similarity: ~a~]"
            model (getf answer :count) (getf answer :dimensions) (usage-line usage)
            (getf answer :similarity-matrix))))

(nlk:define-contract search-call
  (:doc "Search the web through OpenRouter's web_search server tool: a synthesis plus the urls it read.")
  ("query" :text :required t :doc "what to look for")
  ("limit" :integer :default 5 :doc "how many hits to keep, 1 to 25")
  ("model" :string :default "" :doc "the web-search model; empty is the configured one"))

(defun tool-search (call)
  "The openrouter_search tool: the synthesis, then the urls behind it."
  (multiple-value-bind (hits answer model)
      (search (search-call-query call) :limit (search-call-limit call) :model (search-call-model call))
    (format nil "~a searched~%~a~%~{~a - ~a~^~%~}"
            model answer
            (loop for hit in hits
                  append (list (getf hit :title) (getf hit :url))))))

;;; --- the command ---------------------------------------------------------------

(defun status-text ()
  "What this cell is connected to, without ever printing the key."
  (format nil "openrouter-everything: ~a, key ~a~%models: decision ~a, extraction ~a, rerank ~a, embeddings ~a, web search ~a~%\
verbs: ~{~a~^, ~} -- (help :openrouter-everything) for what each takes"
          (base-url)
          (if (api-key) "set" (format nil "MISSING (make one at ~a)" +key-page+))
          (configured-model :decision) (configured-model :extraction)
          (configured-model :rerank) (configured-model :embeddings)
          (configured-model :web-search)
          +verbs+))

(defun run-command (args session-id)
  "One /openrouter-everything invocation: alone, what the cell is connected
to; ``models`` the same, one capability per line."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond
      ((zerop (length verb)) (status-text))
      ((equal verb "models")
       (format nil "~{~a~^~%~}"
               (loop for capability in '(:decision :extraction :rerank :embeddings :web-search)
                     collect (format nil "~(~a~): ~a" capability (configured-model capability)))))
      (t (format nil "openrouter-everything: ~a is not one of its choices; try /openrouter-everything or /openrouter-everything models"
                 verb)))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell openrouter-everything
  (:section ("openrouter-everything")
    (:guide "make a key at https://openrouter.ai/settings/keys and paste it in api_key, or export OPENROUTER_API_KEY (or name the variable in api_key_env); base_url only for a relay of your own; each *_model is optional (an empty one is the plugin's default); next step: /openrouter-everything shows the connection")
    ("api_key" :secret
     :doc "the OpenRouter API key")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (OPENROUTER_API_KEY is read either way)")
    ("base_url" :string :default ""
     :doc "the OpenRouter v1 root: https://openrouter.ai/api/v1 unless you name a relay")
    ("decision_model" :string :default ""
     :doc "the model decisions use; empty is typesafe/jev-1.14")
    ("extraction_model" :string :default ""
     :doc "the model extractions use; empty is cloudflare/clairic")
    ("rerank_model" :string :default ""
     :doc "the model reranking uses; empty is cohere/rerank-v3.5")
    ("embeddings_model" :string :default ""
     :doc "the model embeddings use; empty is google/gemini-embedding-2")
    ("web_search_model" :string :default ""
     :doc "the model that drives the web_search server tool; empty is the plugin's default")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one call's ceiling"))
  (:help :openrouter-everything
         "openrouter-everything:decide, :extract-fields, :rerank, :embed, :search -- OpenRouter's non-chat APIs on your OpenRouter key (help :openrouter-everything)"
         +primer+)
  (:command "openrouter-everything" 'run-command
            :description "OpenRouter's decisions, extractions, rerank, embeddings and web search: the connection"
            :argument-hint "models")
  (:tool "openrouter_decide" (nlk:contract 'decide-call) #'tool-decide)
  (:tool "openrouter_extract_fields" (nlk:contract 'extract-call) #'tool-extract-fields)
  (:tool "openrouter_rerank" (nlk:contract 'rerank-call) #'tool-rerank)
  (:tool "openrouter_embed" (nlk:contract 'embed-call) #'tool-embed)
  (:tool "openrouter_search" (nlk:contract 'search-call) #'tool-search))
