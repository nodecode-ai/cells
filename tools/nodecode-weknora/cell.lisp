;;;; cell.lisp --- the section, the four tools, /weknora, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What a dsh plugin registered through ctx.tools.register() is here a clause
;;;; of one NLE:DEFINE-CELL: four tools the model calls, a command the
;;;; operator types, a topic on the manual, and the `weknora' section they run
;;;; on. Stop takes every one of them back, because the declaration derives it.
;;;;
;;;; The section is the whole of the setup: base_url says which deployment,
;;;; and everything else has a default that holds. An unanswered section
;;;; still starts - the tools are there, and the first call refuses naming
;;;; base_url - so the way in is the refusal, not a silent nothing.

(in-package #:nodecode-weknora)

;;; --- the words ----------------------------------------------------------------

(defparameter +primer+
  "weknora: your own WeKnora knowledge base as four tools, read-only.
  list_knowledge_bases  what the credential can see, with ids
  search                 the matching passages verbatim (hybrid retrieval), and the
                         documents a title-like query names; every hit carries the
                         knowledge_id read_document needs
  read_document          one document's passages in order, page 1 led by its title
                         and the summary WeKnora generated
  ask                    WeKnora's own composed answer with citations, from its RAG
                         pipeline (or the agent one, when an agent_id is set)
The tool names carry the configured tool_prefix, weknora by default. From eval the
same four are (weknora:search \"...\"), (weknora:read-document \"id\"),
(weknora:ask \"...\") and (weknora:knowledge-bases), each answering the text the
tool answers. Prefer search: it hands you the evidence to reason over, where ask
runs another model server-side and returns a conclusion. Nothing here writes to the
deployment: no ingestion, no edits, no deletion. (help :weknora) has this while
the cell runs; the section is in ~/.nodecode/config.jsonc under weknora."
  "What (help :weknora) answers while the cell runs.")

(defparameter +tool-keys+
  '(:tool-list-knowledge-bases :tool-search :tool-read-document :tool-ask)
  "The settings that switch one tool off, in registration order.")

;;; --- the arguments each tool is held to ------------------------------------------

(nlk:define-contract search-query
  (:doc "One hybrid retrieval call against a WeKnora deployment.")
  ("query" :text :required t
          :doc "Natural-language query; a full question retrieves better than keywords.")
  ("knowledge_base_ids" :array :of :string
          :doc "Restrict the search to these knowledge base ids.")
  ("knowledge_ids" :array :of :string
          :doc "Restrict the search to these document ids.")
  ("max_results" :integer
          :doc "Maximum passages to return; at most the configured maximum."))

(nlk:define-contract document-request
  (:doc "One page of one WeKnora document's stored passages.")
  ("knowledge_id" :text :required t
          :doc "Document id, as returned in a search hit.")
  ("page" :integer
          :doc "Page of passages, starting at 1.")
  ("page_size" :integer
          :doc "Passages per page (max 100)."))

(nlk:define-contract question
  (:doc "One question put to WeKnora's own composed-answer pipeline.")
  ("query" :text :required t
          :doc "The question to answer.")
  ("knowledge_base_ids" :array :of :string
          :doc "Restrict retrieval to these knowledge base ids.")
  ("agent_id" :string
          :doc "Custom agent id; selects the server-side ReAct pipeline.")
  ("session_id" :string
          :doc "Continue an earlier WeKnora session instead of starting one.")
  ("web_search" :boolean
          :doc "Let WeKnora also search the web, when its deployment allows it."))

;;; --- the section, read once ------------------------------------------------------

(defun tool-prefix-p (text)
  "Whether TEXT is a usable tool-name prefix: lowercase letters, digits and _,
a letter first."
  (and (stringp text)
       (plusp (length text))
       (lower-case-p (char text 0))
       (every (lambda (char)
                (or (and (< (char-code char) 128) (alphanumericp char)) (char= char #\_)))
              text)))

(defun tool-toggle (table key)
  "Whether the tools.KEY toggle is on: absent keeps it on, and JSON false is
how one is switched off."
  (let ((toggles (nlk:json-value table :object "tools")))
    (if toggles
        (multiple-value-bind (value present) (nlk:json-value toggles :boolean key)
          (if present value t))
        t)))

(defun read-settings (values table)
  "The section's members, with the address normalized and the four tool toggles
read out of their own object. A value the declaration cannot hold refuses at the
start, not inside the first call."
  (when (getf values :base-url)
    (setf (getf values :base-url) (normalize-base-url (getf values :base-url))))
  (when (and (getf values :tool-prefix) (not (tool-prefix-p (getf values :tool-prefix))))
    (nlk:config-error "weknora.tool_prefix is ~s: lowercase letters, digits and _, a letter first"
                      (getf values :tool-prefix)))
  (setf (getf values :tool-list-knowledge-bases) (tool-toggle table "list_knowledge_bases")
        (getf values :tool-search) (tool-toggle table "search")
        (getf values :tool-read-document) (tool-toggle table "read_document")
        (getf values :tool-ask) (tool-toggle table "ask"))
  (unless (some (lambda (key) (getf values key)) +tool-keys+)
    (nlk:config-error "every weknora tool is switched off under tools: turn one back on, or remove the cell"))
  values)

;;; --- one tool's registration ------------------------------------------------------

(defun tool-schema (contract description)
  "The plist a tool registers: DESCRIPTION for the model, and the declaration's
own projection for the arguments the call is held to."
  (list :description description
        :parameters (nlk:contract-schema (nlk:contract contract))))

(defun empty-schema (description)
  "A tool that takes no arguments."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                     "properties" (nlk:json-object)
                                     "additionalProperties" nil)))

(defun list-tool (arguments)
  "list_knowledge_bases: the deployment's knowledge bases, ids and all."
  (declare (ignore arguments))
  (knowledge-bases))

(defun search-tool (arguments)
  "search: the matching passages, and the documents a title-like query names."
  (let ((request (decode-search-query arguments)))
    (search (search-query-query request)
            :knowledge-base-ids (coerce (search-query-knowledge-base-ids request) 'list)
            :knowledge-ids (coerce (search-query-knowledge-ids request) 'list)
            :max-results (search-query-max-results request))))

(defun read-tool (arguments)
  "read_document: one document's passages, in order, a page at a time."
  (let ((request (decode-document-request arguments)))
    (read-document (document-request-knowledge-id request)
                   :page (document-request-page request)
                   :page-size (document-request-page-size request))))

(defun ask-tool (arguments)
  "ask: WeKnora's own composed answer, with its citations."
  (let ((request (decode-question arguments)))
    (ask (question-query request)
         :knowledge-base-ids (coerce (question-knowledge-base-ids request) 'list)
         :agent-id (question-agent-id request)
         :session-id (question-session-id request)
         :web-search (question-web-search request))))

;;; --- what the section leaves on ----------------------------------------------------

(defun tool-rows ()
  "One row per tool: its name's suffix, the setting that switches it, the schema
it registers against and the function a call runs. The schemas are built at
start, not read from a constant, because a description names the sibling tools
the configured prefix gave them."
  (list (list "list_knowledge_bases" :tool-list-knowledge-bases
              (lambda () (empty-schema (list-description))) 'list-tool)
        (list "search" :tool-search
              (lambda () (tool-schema 'search-query (search-description))) 'search-tool)
        (list "read_document" :tool-read-document
              (lambda () (tool-schema 'document-request (read-description))) 'read-tool)
        (list "ask" :tool-ask
              (lambda () (tool-schema 'question (ask-description))) 'ask-tool)))

(defun install-tools ()
  "Register each tool the section leaves on, its own inverse beside it: the
original's tools.* switches are what keep a disabled tool out of the prompt."
  (dolist (row (tool-rows))
    (destructuring-bind (suffix key schema function) row
      (when (setting key)
        (let ((name (tool-name suffix)))
          (nle:register-tool name (funcall schema) function)
          (nle:on-stop (lambda () (nle:unregister-tool name))))))))

;;; --- /weknora -----------------------------------------------------------------

(defun registered-tools ()
  "The tool names this start registered, in the order the section lists them."
  (loop for (suffix key) in '(("list_knowledge_bases" :tool-list-knowledge-bases)
                              ("search" :tool-search)
                              ("read_document" :tool-read-document)
                              ("ask" :tool-ask))
        when (setting key) collect (tool-name suffix)))

(defun run-command (args session-id)
  "(/weknora) says which deployment this cell talks to and what it can see;
(/weknora kbs) asks the deployment for the knowledge bases it can see."
  (declare (ignore session-id))
  (let ((args (string-downcase (nlk:trimmed (or args "")))))
    (cond
      ((zerop (length args))
       (format nil "weknora: ~a~%scope: ~a~@[~%agent: ~a~]~%resource URLs: ~a, ~ds a call, ~ds an answer~%tools: ~{~a~^, ~}~%(/weknora kbs lists what the credential can see)"
               (api-root)
               (let ((ids (coerce (setting :knowledge-base-ids) 'list)))
                 (if ids (format nil "~{~a~^, ~}" ids)
                     "every knowledge base the credential can see"))
               (setting :agent-id)
               (if (public-mode-p) "public" "handle")
               (setting :request-timeout)
               (setting :chat-timeout)
               (registered-tools)))
      ((member args '("kbs" "bases" "knowledge bases") :test #'string=)
       (knowledge-bases))
      (t (format nil "~s is not one of its choices: kbs" args)))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell weknora
  ;; base_url is the one required member: an unanswered section still starts,
  ;; and the first call names it. Everything else holds a default, so a local
  ;; docker-compose deployment works with the address and nothing else.
  (:section ("weknora")
    (:guide "base_url is the one required step: the API root of your WeKnora, so https://kb.example.com or http://localhost:8080 for a docker-compose deployment; api_key and tenant_id only if the deployment asks for them; knowledge_base_ids narrows what search and ask span, and empty means every knowledge base the credential can see; tools.* switch one tool off")
    ("base_url" :string :required t
                :doc "the deployment's API root; a bare origin gains /api/v1")
    ("api_key" :secret
               :doc "sent as X-API-Key; unset for an unauthenticated deployment")
    ("tenant_id" :string
                 :doc "X-Tenant-ID, for a platform-scoped key that must pick a workspace")
    ("knowledge_base_ids" :list
                          :doc "the default scope; empty means every knowledge base the credential can see")
    ("agent_id" :string
                :doc "a custom agent id: ask then runs its ReAct pipeline, which resolves its own scope")
    ("max_results" :integer :default 8 :min 1
                   :doc "passages one search returns, and the ceiling a call may ask for")
    ("max_chunk_chars" :integer :default 1200 :min 1
                       :doc "per-passage character budget, before a passage is clipped")
    ("request_timeout" :integer :default 30 :min 1
                       :doc "seconds one retrieval or document read may take")
    ("chat_timeout" :integer :default 300 :min 1
                    :doc "seconds an asked answer may take: it waits on WeKnora's own model")
    ("resource_urls" :choice :options '("public" "handle") :default "public"
                     :doc "public returns directly loadable file URLs in citations; handle keeps internal resource:// refs")
    ("tool_prefix" :string :default "weknora"
                   :doc "the prefix the four tools take, so two deployments can sit side by side"))
  ;; The members the declaration cannot carry: the address normalized, and the
  ;; four toggles nested under one `tools' object, as the plugin had them.
  (:settings 'read-settings)
  (:command "weknora" 'run-command
            :description "Which WeKnora this cell talks to, and what it can see")
  (:help :weknora "WeKnora: search, read and cited answers from your own knowledge bases" +primer+)
  ;; The tools are registered here rather than by a (:tool ...) clause because
  ;; the section decides which of them exist: a tool switched off under `tools'
  ;; must never reach the prompt. Each registration records its own inverse in
  ;; this scope, so stop takes back exactly the ones this start installed.
  (:start (lambda ()
            ;; One start is one deployment: the handle-mode fallback and the
            ;; resolved scope belong to the credential this start read, and a
            ;; restart must not inherit either.
            (setf *resource-blocked* nil
                  *visible-scope* :unresolved)
            (install-tools))))
