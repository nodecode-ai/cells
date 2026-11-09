;;;; tools.lisp --- the nine Patsnap tools, their schemas, and the verbs behind them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The server's own nine tools, one Nodecode tool each, named
;;;; <prefix>_<server tool> so the model reads the name the skill taught. Every
;;;; call forwards its arguments whole: Patsnap owns the shape of an argument,
;;;; and a paraphrase of it here would drift from the server that validates it.
;;;; The schema each tool registers names the arguments the guide documents and
;;;; admits the rest, and patsnap_tools answers the server's own schema for the
;;;; call whose fields the model is not sure of.

(in-package #:nodecode-patsnap)

;;; --- schema pieces -------------------------------------------------------------

(defun json (&rest pairs)
  "A JSON object from PAIRS: NLK:MAKE-JSON-OBJECT, which a function that
assembles its members with a computed tail can call."
  (apply #'nlk:make-json-object pairs))

(defun string-schema (description &rest values)
  "A string argument, optionally one of VALUES."
  (if values
      (json "type" "string" "description" description
            "enum" (coerce values 'vector))
      (json "type" "string" "description" description)))

(defun strings-schema (description &rest values)
  "An array of strings, optionally each one of VALUES."
  (json "type" "array" "description" description
        "items" (apply #'string-schema description values)))

(defun object-schema (description)
  "A free-form object argument."
  (json "type" "object" "description" description))

(defun integer-schema (description &key minimum maximum)
  "An integer argument, bounded when the server bounds it."
  (apply #'json
         (append (list "type" "integer" "description" description)
                 (when minimum (list "minimum" minimum))
                 (when maximum (list "maximum" maximum)))))

(defparameter +languages+ '("en" "zh-CN" "zh-TW" "ja")
  "The languages the detail tools answer in.")

(defun tool-schema (&rest fields)
  "An argument schema: FIELDS alternate a wire name and that argument's
schema. A member the list does not name is passed through to Patsnap, which
owns the exact rule for it."
  (nlk:make-json-object
   "type" "object"
   "properties" (apply #'nlk:make-json-object
                       (loop for (name schema) on fields by #'cddr
                             append (list name schema)))
   "additionalProperties" t))

(defun language-schema ()
  "The language a detail tool answers in."
  (apply #'string-schema "the language the records answer in" +languages+))

(defun format-schema ()
  "The format a detail tool answers in."
  (string-schema "json, or markdown for a reader" "json" "markdown"))

;;; --- the catalog ---------------------------------------------------------------

(defun catalog ()
  "Every tool this cell offers, one row each:
\(SERVER-TOOL DESCRIPTION SCHEMA PAGING-P). The descriptions carry the
workflow the original shipped as a skill; PAGING-P marks the two searches
that take the configured limit when the call names none."
  (list
   (list "search_patent"
         "Search Patsnap's global patent database (WIPO/PCT, EPO, USPTO, CNIPA, JPO, KIPO and more). Pick one strategy: semantic (needs semantic_query), keyword (needs keywords), filter (needs filters or domain_filters) or boolean (needs boolean_query), and pass only the parameter that strategy uses. One call returns patents only. limit 1-100, offset >= 0, offset + limit <= 1000."
         (tool-schema
          "search_strategy" (strings-schema "usually one of semantic, keyword, filter or boolean"
                                             "semantic" "keyword" "filter" "boolean")
          "semantic_query" (string-schema "the question, for the semantic strategy")
          "keywords" (strings-schema "search terms, for the keyword strategy")
          "boolean_query" (string-schema "a Patsnap Boolean query, for the boolean strategy")
          "filters" (object-schema "field filters, general domain")
          "domain_filters" (object-schema "field filters of a specialised domain")
          "domains" (strings-schema "the specialised domains a filter search spans")
          "sort" (string-schema "the order of the results, as the server names it")
          "limit" (integer-schema "results wanted; at most 100" :minimum 1 :maximum 100)
          "offset" (integer-schema "results to skip; offset + limit <= 1000" :minimum 0)
          "language" (language-schema))
         t)
   (list "search_literature"
         "Search Patsnap's scientific literature (PubMed, Nature, arXiv and other journals and repositories). Strategies semantic, keyword or filter only: no boolean, no domains. One call returns papers only; patents are another tool."
         (tool-schema
          "search_strategy" (strings-schema "usually one of semantic, keyword or filter"
                                             "semantic" "keyword" "filter")
          "semantic_query" (string-schema "the question, for the semantic strategy")
          "keywords" (strings-schema "search terms, for the keyword strategy")
          "filters" (object-schema "field filters: author, institution, dates, citation count")
          "limit" (integer-schema "results wanted; at most 100" :minimum 1 :maximum 100)
          "offset" (integer-schema "results to skip; offset + limit <= 1000" :minimum 0)
          "language" (language-schema))
         t)
   (list "build_patent_boolean_query"
         "Turn a full natural-language patent intent into Patsnap's Boolean syntax. Pass the returned boolean_query unchanged to search_patent with search_strategy [\"boolean\"]."
         (tool-schema "query" (string-schema "the whole intent, in one sentence or more"))
         nil)
   (list "describe_search_capability"
         "Patsnap's fields and operators for a specialised patent domain (ls: life sciences, material). Call it before filtering on a domain's fields, and never invent a field or an operator."
         (tool-schema "domain" (string-schema "the specialised domain to describe" "ls" "material"))
         nil)
   (list "get_standard_assignee_name"
         "Resolve a company or applicant name (a short name, an alias or a translation) to Patsnap's standard assignee: the company_id and ans_id a patent search filters on."
         (tool-schema "name" (string-schema "the company or applicant name, as the user wrote it"))
         nil)
   (list "get_corptree_hierarchy"
         "One confirmed company_id's corporate tree: ancestors, children, descendants or the root group. A parent or subsidiary found here does not widen a search until its ids are added to it."
         (tool-schema
          "company_id" (string-schema "a company_id get_standard_assignee_name returned")
          "direction" (string-schema "which part of the tree" "ancestors" "children" "descendants" "root"))
         nil)
   (list "fetch_patent"
         "One patent by internal id or publication number: bibliography, claims and description by default. include takes only relations and media, which cost extra time and credits; a patent's legal status is fetch_patent_lifecycle."
         (tool-schema
          "patent_id" (string-schema "an internal id, or a publication number such as US-11234567-B2")
          "include" (strings-schema "extra sections to add; both are billed" "relations" "media")
          "format" (format-schema)
          "language" (language-schema))
         nil)
   (list "fetch_patent_lifecycle"
         "One patent's legal lifecycle: events, annuities, transfers, licences and litigation, by internal id or publication number."
         (tool-schema
          "patent_id" (string-schema "an internal id, or a publication number such as US-11234567-B2")
          "format" (format-schema)
          "language" (language-schema))
         nil)
   (list "fetch_literature"
         "One paper by its internal paper_id, the id a literature search returned -- not a DOI or a URL: bibliography and abstract."
         (tool-schema
          "paper_id" (string-schema "the paper_id a literature search returned")
          "format" (format-schema)
          "language" (language-schema))
         nil)))

;;; --- a plist, as the wire wants it ----------------------------------------------

(defun wire-key (key)
  "KEY as Patsnap spells it: :semantic-query is semantic_query."
  (string-downcase (if (stringp key) key (substitute #\_ #\- (string key)))))

(defun plist-object (plist)
  "PLIST as a JSON object, its keywords Patsnap's wire names, its values as
JSON: a keyword is its own lowercase name, a list is an array, a nested plist
an object."
  (labels ((json-ify (value)
             (cond ((keywordp value)
                    (string-downcase (substitute #\_ #\- (symbol-name value))))
                   ((and (consp value) (keywordp (first value))) (plist-object value))
                   ((listp value) (coerce (mapcar #'json-ify value) 'vector))
                   ((and (vectorp value) (not (stringp value)))
                    (coerce (map 'list #'json-ify value) 'vector))
                   (t value))))
    (let ((object (nlk:make-json-object)))
      (loop for (key value) on plist by #'cddr
            do (setf (gethash (wire-key key) object) (json-ify value)))
      object)))

(defun with-default-limit (object)
  "OBJECT with the configured limit, when the call names neither a limit nor
an offset. The skill's advice -- search small, page only when needed -- is
this one line, and it is what keeps a casual search cheap."
  (let ((object (or object (nlk:make-json-object))))
    (when (and (null (gethash "limit" object))
               (null (gethash "offset" object)))
      (setf (gethash "limit" object) (setting :limit)))
    object))

;;; --- the verbs -------------------------------------------------------------------
;;; Each answers the text the tool answers, so an operator at eval and the model
;;; in a turn see one thing.

(define-verb search-patent (&rest arguments)
  "(patsnap:search-patent :semantic-query \"...\" :limit 10) -- patent search."
  (call-tool "search_patent" (with-default-limit (plist-object arguments))))

(define-verb search-literature (&rest arguments)
  "(patsnap:search-literature :keywords '(\"solid electrolyte\")) -- paper search."
  (call-tool "search_literature" (with-default-limit (plist-object arguments))))

(define-verb build-boolean-query (&rest arguments)
  "(patsnap:build-boolean-query :query \"...\") -- a Patsnap Boolean query."
  (call-tool "build_patent_boolean_query" (plist-object arguments)))

(define-verb describe-capability (&rest arguments)
  "(patsnap:describe-capability :domain \"material\") -- a domain's fields."
  (call-tool "describe_search_capability" (plist-object arguments)))

(define-verb standard-assignee (&rest arguments)
  "(patsnap:standard-assignee :name \"...\") -- a company's standard ids."
  (call-tool "get_standard_assignee_name" (plist-object arguments)))

(define-verb corptree (&rest arguments)
  "(patsnap:corptree :company-id \"...\" :direction \"children\") -- one tree."
  (call-tool "get_corptree_hierarchy" (plist-object arguments)))

(define-verb fetch-patent (&rest arguments)
  "(patsnap:fetch-patent :patent-id \"US-11234567-B2\") -- one patent."
  (call-tool "fetch_patent" (plist-object arguments)))

(define-verb patent-lifecycle (&rest arguments)
  "(patsnap:patent-lifecycle :patent-id \"US-11234567-B2\") -- its legal events."
  (call-tool "fetch_patent_lifecycle" (plist-object arguments)))

(define-verb fetch-literature (&rest arguments)
  "(patsnap:fetch-literature :paper-id \"...\") -- one paper."
  (call-tool "fetch_literature" (plist-object arguments)))

(define-verb tools (&optional name)
  "(patsnap:tools) -- the server's own tools; (patsnap:tools \"search_patent\") -- one schema."
  (tools-text name))
