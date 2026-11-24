;;;; cell.lisp --- the section, the ten tools, /patsnap, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the original declared as a manifest (one Streamable-HTTP MCP server,
;;;; one key) and shipped as a skill is here one clause of one NLE:DEFINE-CELL:
;;;; the `patsnap' section the tools run on, ten tools the model calls, a
;;;; command the operator types, a topic on the manual, and stop deriving from
;;;; them all.
;;;;
;;;; An unanswered section still starts: the tools are there, and the first
;;;; call refuses naming the member to write. That is the way in, rather than a
;;;; cell that silently disappears when the key is missing.

(in-package #:nodecode-patsnap)

;;; --- the words ----------------------------------------------------------------

(defparameter +primer+
  "patsnap: Patsnap's global patent database (WIPO/PCT, EPO, USPTO, CNIPA, JPO, KIPO
and more) and its scientific literature (PubMed, Nature, arXiv and others), through
Patsnap's hosted MCP server. Patents and papers are separate tools: one search call
returns one record type.

  search_patent              semantic, keyword, filter or boolean; each strategy takes
                              only its own parameter (semantic_query, keywords,
                              filters/domain_filters, boolean_query)
  search_literature          semantic, keyword or filter; no boolean, no domains
  build_patent_boolean_query a full intent turned into Patsnap Boolean syntax; pass
                              its boolean_query unchanged to search_patent as boolean
  describe_search_capability  the fields and operators of a specialised domain (ls,
                              material); call it before filtering on one
  get_standard_assignee_name  a company name to the company_id/ans_id a search filters on
  get_corptree_hierarchy      one confirmed company_id's ancestors, children or root
  fetch_patent                one patent by id or publication number: bibliography,
                              claims, description; include takes only relations, media
  fetch_patent_lifecycle      one patent's legal events, annuities, transfers, litigation
  fetch_literature            one paper by its paper_id -- the id a search returned,
                              not a DOI or a URL
The tool names carry the configured prefix, patsnap by default; <prefix>_tools answers
the server's own descriptions and argument schemas. From eval the same calls are
(patsnap:search-patent :semantic-query \"...\"), (patsnap:fetch-patent :patent-id \"...\"),
(patsnap:tools), each answering the text the tool answers.

How to work: pull the concepts, assignees, dates, jurisdictions and classifications out
of the request; resolve a company name with get_standard_assignee_name before filtering
on it; search with only the parameter the strategy needs and a small limit (10-20) to
keep credit use down, and page only when the user needs more; fetch only the records the
answer needs, one call each; then answer with titles, publication numbers or paper ids
and the record URLs the server returned.

Pitfalls: company_id and ans_id are not interchangeable, and a parent or subsidiary from
get_corptree_hierarchy does not widen a search until its ids are added. Do not invent a
domain field, operator or identifier: take them from describe_search_capability and from
earlier results. An authentication error means the key is wrong, expired or out of
credits -- report it and stop rather than retrying in a loop. Not for statistics, trends
or Top-N counts, and never a substitute for a legal opinion or a freedom-to-operate
conclusion: say so when the user asks for one.

Setup: a key from open.patsnap.com in the patsnap section's api_key, or the environment
variable api_key_env names (PATSNAP_API_KEY by default). New accounts get free credits;
every call spends credits on that account. (help :patsnap) has this while the cell runs."
  "What (help :patsnap) answers while the cell runs.")

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

(defun web-address-p (text)
  "Whether TEXT is an http:// or https:// address, the only kind MCP's
Streamable HTTP transport speaks."
  (and (stringp text)
       (plusp (length text))
       (or (uiop:string-prefix-p "https://" text)
           (uiop:string-prefix-p "http://" text))
       (> (length text) 8)))

(defun read-settings (values table)
  "The section's members, with the two the declaration cannot check refused
here: an address that is not one, and a prefix a tool name cannot carry. A
value refused now refuses the start, not the first call."
  (declare (ignore table))
  (unless (web-address-p (getf values :endpoint))
    (nlk:config-error "patsnap.endpoint is ~s: the https:// address of an MCP server"
                      (getf values :endpoint)))
  (unless (tool-prefix-p (getf values :tool-prefix))
    (nlk:config-error "patsnap.tool_prefix is ~s: lowercase letters, digits and _, a letter first"
                      (getf values :tool-prefix)))
  (when (and (getf values :api-key-env)
             (not (plusp (length (getf values :api-key-env)))))
    (nlk:config-error "patsnap.api_key_env is empty: name the variable that holds the key, or remove it"))
  values)

;;; --- the tools, and the one that answers the server's own schemas ------------------

(defun tool-name (server-name)
  "The Nodecode tool name SERVER-NAME takes: the configured prefix and an
underscore, so two cells can list the same server without colliding."
  (format nil "~a_~a" (setting :tool-prefix) server-name))

(defun tools-tool-description ()
  "What <prefix>_tools says it is for, naming the prefix this start configured."
  (format nil "Patsnap's own tool list, or one tool's exact argument schema straight from the server. Call ~a with a tool name before a call whose parameters you are not sure of; without one it lists every tool the server offers."
          (tool-name "tools")))

(defun install-tools ()
  "Register the nine server tools and the schema lookup, each with its own
inverse in this start's scope. A tool's body forwards its arguments whole:
Patsnap validates them, and this cell does not paraphrase the shape."
  (dolist (row (catalog))
    (destructuring-bind (server-name description schema paging) row
      (let ((name (tool-name server-name))
            (remote server-name)
            (default-limit paging))
        (nle:register-tool name (list :description description :parameters schema)
                            (lambda (arguments)
                              (call-tool remote (if default-limit
                                                    (with-default-limit arguments)
                                                    arguments))))
        (nle:on-stop (lambda () (nle:unregister-tool name))))))
  (let ((name (tool-name "tools")))
    (nle:register-tool name
                        (list :description (tools-tool-description)
                              :parameters (tool-schema
                                           "tool" (string-schema "a server tool name, such as search_patent")))
                        (lambda (arguments)
                          (tools-text (and arguments (nlk:json-value arguments :string "tool")))))
    (nle:on-stop (lambda () (nle:unregister-tool name)))))

(defun registered-tools ()
  "The tool names this start registered, in the order the catalog lists them."
  (append (mapcar (lambda (row) (tool-name (first row))) (catalog))
          (list (tool-name "tools"))))

;;; --- /patsnap -----------------------------------------------------------------

(defun run-command (args session-id)
  "(/patsnap) says which server this cell talks to and what this start has
spent; (/patsnap tools) asks the server for its own tools."
  (declare (ignore session-id))
  (let ((args (string-downcase (nlk:trimmed (or args "")))))
    (cond
      ((member args '("tools" "list") :test #'string=)
       (tools-text))
      ((zerop (length args))
       (format nil "patsnap: ~a~%key: ~a~%tools: ~{~a~^, ~}~%calls this start: ~d, each one spent credits~%(/patsnap tools lists the server's own tools and their schemas)"
               (setting :endpoint)
               (if (api-key)
                   "set"
                   (format nil "missing: write api_key in the patsnap section, or export ~a"
                           (setting :api-key-env)))
               (registered-tools)
               *calls*))
      (t (format nil "~s is not one of its choices: tools" args)))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell patsnap
  ;; The endpoint and the key are the whole setup: the original's manifest
  ;; named the same one server, and its README named the same one key. Every
  ;; other member holds a default that saves credits or keeps a name readable.
  (:section ("patsnap")
    (:guide "endpoint is where Patsnap's hosted MCP server answers and is already set; api_key is the step left: create one at open.patsnap.com (new accounts get free credits) and paste it, or export the variable api_key_env names; every tool call spends credits on that account, so limit bounds what a search asks for when the call names none")
    ("endpoint" :string :default "https://connect.patsnap.com/2b0355/logic-mcp"
                :doc "Patsnap's hosted MCP server; another instance answers at another path")
    ("api_key" :secret
               :doc "your Patsnap key, sent in an apikey header and never in the URL")
    ("api_key_env" :string :default "PATSNAP_API_KEY"
                   :doc "the environment variable read when api_key is unset")
    ("tool_prefix" :string :default "patsnap"
                   :doc "the prefix the ten tools take; set another when another cell lists this server too")
    ("language" :choice :options '("en" "zh-CN" "zh-TW" "ja") :default "en"
                :doc "the language the detail tools answer in")
    ("limit" :integer :default 10 :min 1
             :doc "results a search asks for when the call names no limit; a small one keeps credits down")
    ("timeout" :integer :default 60 :min 1
               :doc "seconds one MCP call may take"))
  ;; The members the declaration does not check: an address that is one, and a
  ;; prefix a tool name can carry.
  (:settings 'read-settings)
  (:command "patsnap" 'run-command
            :description "Which Patsnap server this cell talks to, and what it can see")
  (:help :patsnap "Patsnap: patent and literature search through Patsnap's hosted MCP server" +primer+)
  ;; The tools are registered here rather than by (:tool ...) clauses because
  ;; their names carry the configured prefix: one clause per tool would need
  ;; the prefix before the section is read. Each records its own inverse in
  ;; this scope, so a stop takes back exactly what this start installed.
  (:start (lambda ()
            ;; One start is one server and one key: the session and the
            ;; server's tool list belong to them, and a restart inherits
            ;; neither.
            (setf *session* nil
                  *remote-tools* nil
                  *request-id* 0
                  *calls* 0)
            (install-tools))))
