;;;; cell.lisp --- the one tool, the demo catalog, /mcp-router, the manual, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is an Agent Plugins package: one stdio MCP server, one tool,
;;;; mcp_select {query, tools}, that ranks a catalog of MCP tool descriptions
;;;; under a token budget and answers a decision with its reasons. It never
;;;; runs a tool. This cell is the same tool in the image's own vocabulary --
;;;; a registered model tool against a declared contract, its local heuristic
;;;; in select.lisp -- so nothing here needs Node, Python, uv or a key.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "mcp-router": {"max_tools": 8, "budget_tokens": 800,
;;;;                  "min_relevance": 0.15, "omit_rejected": false,
;;;;                  "extra_stopwords": []}
;;;; A present section is enabled; "enabled": false vetoes it.

(in-package #:nodecode-mcp-router)

;;; --- the boundary: what a call carries -------------------------------------------

(nlk:define-contract select-request
  (:doc "Pick the MCP tools that fit a query inside a context budget, without running any
of them. Use it when many MCP tools are available and only a budgeted subset should be
offered to a model: pass the catalog you have and the thing you are about to do. It
ranks the candidates by the words they share with the query, fits them into max_tools
and budget_tokens, explains each choice, names every rejection with its cause, and
abstains when nothing clears the floor. It does not call a tool and does not generate
user-facing text.")
  ("query" :text :required t
   :doc "what you are about to do, in words, e.g. \"read a local file, then search GitHub issues\"")
  ("tools" :array :of :object :required t
   :doc "the candidate tools, one JSON object each: {id, name, description, server, tags, tokens, permissions, enabled, fallback_id, max_calls, scope}. id is required; tokens defaults to 80, max_calls to 8, enabled to true, scope to \"default\".")
  ("max_tools" :integer
   :doc "how many tools the answer may keep; the section's max_tools when absent")
  ("budget_tokens" :integer
   :doc "the context budget the kept tools must fit; the section's budget_tokens when absent")
  ("min_relevance" :number
   :doc "the lexical floor, 0.0 to 1.0; a candidate below it is rejected as low_relevance. The section's min_relevance when absent")
  ("required_permissions" :array :of :string
   :doc "permissions every kept tool must declare, e.g. [\"read_files\"]. Declared by the caller: a query never grants one")
  ("forbidden_permissions" :array :of :string
   :doc "permissions no kept tool may declare")
  ("failed_tool_id" :string
   :doc "the tool that just failed, when you are choosing a replacement: its own fallback_id is tried first, and the answer's decision reads fallback")
  ("no_fallback" :boolean
   :doc "true to refuse the one fallback hop entirely; absent reads as false. (An absent JSON boolean and a false one are one value on the wire, so this member is the inverted spelling of the original's allow_fallback)")
  ("scope" :string
   :doc "the list this call selects from, \"default\" when absent: a candidate is kept only when its own scope is equal to it"))

;;; --- the tool -------------------------------------------------------------------

(defun string-list (value)
  "A JSON array of strings, a list, or NIL, as a list."
  (cond ((null value) '())
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (list (princ-to-string value)))))

(defun select-tools-tool (request)
  "The mcp_select tool: the selector's answer as JSON text."
  (let* ((tools (select-request-tools request))
         (query (select-request-query request)))
    (when (zerop (length tools))
      (fail "mcp_select needs at least one candidate: pass tools as a JSON array of {id, name, description, tokens, tags, permissions}"))
    (nlk:pretty-json
     (select-tools query tools
                    :max-tools (or (select-request-max-tools request) (setting :max-tools))
                    :budget-tokens (or (select-request-budget-tokens request)
                                       (setting :budget-tokens))
                    :min-relevance (float (or (select-request-min-relevance request)
                                              (setting :min-relevance)))
                    :required-permissions (string-list (select-request-required-permissions request))
                    :forbidden-permissions (string-list (select-request-forbidden-permissions request))
                    :failed-tool-id (select-request-failed-tool-id request)
                    :allow-fallback (not (select-request-no-fallback request))
                    :scope (or (select-request-scope request) "default")
                    :include-rejected (not (setting :omit-rejected))))))

;;; --- the demo catalog the command selects from ------------------------------------

(defparameter +demo-tools+
  (list (nlk:json-object
         "id" "filesystem_read" "name" "Filesystem Read"
         "description" "Read local files from the workspace."
         "server" "filesystem" "tags" (vector "files" "read" "local" "workspace")
         "tokens" 80 "permissions" (vector "read_files")
         "fallback_id" "docs_search" "max_calls" 20)
        (nlk:json-object
         "id" "filesystem_write" "name" "Filesystem Write"
         "description" "Write or edit local files."
         "server" "filesystem" "tags" (vector "files" "write" "edit")
         "tokens" 80 "permissions" (vector "read_files" "write_files")
         "fallback_id" "filesystem_read" "max_calls" 10)
        (nlk:json-object
         "id" "github_search" "name" "GitHub Search"
         "description" "Search GitHub repositories and issues."
         "server" "github" "tags" (vector "github" "search" "issues" "code")
         "tokens" 120 "permissions" (vector "read_github")
         "fallback_id" "docs_search" "max_calls" 8)
        (nlk:json-object
         "id" "browser" "name" "Browser"
         "description" "Browse web pages and extract their content."
         "server" "browser" "tags" (vector "web" "browser" "http" "page")
         "tokens" 150 "permissions" (vector "browse_web")
         "fallback_id" "docs_search" "max_calls" 6)
        (nlk:json-object
         "id" "docs_search" "name" "Docs Search"
         "description" "Search project documentation and the API reference."
         "server" "docs" "tags" (vector "docs" "search" "documentation" "reference" "api")
         "tokens" 60 "permissions" (vector "read_docs") "max_calls" 12)
        (nlk:json-object
         "id" "memory_select" "name" "Memory Select"
         "description" "Select relevant memories, under a budget."
         "server" "memory" "tags" (vector "memory" "select" "context" "budget")
         "tokens" 100 "permissions" (vector "read_memory")
         "fallback_id" "docs_search" "max_calls" 4)
        (nlk:json-object
         "id" "shell" "name" "Shell"
         "description" "Run shell commands."
         "server" "shell" "tags" (vector "shell" "exec" "command" "terminal")
         "tokens" 200 "permissions" (vector "read_files" "write_files" "exec_shell")
         "max_calls" 4))
  "The original's demo catalog: the list /mcp-router selects from when it is typed
with a query, so the cell says what it does without a config or a network.")

;;; --- /mcp-router -----------------------------------------------------------------

(defparameter +status+
  "mcp-router: mcp_select picks the MCP tools that fit a query inside a context budget,
locally, and runs none of them. Type a query to try it on the demo catalog."
  "What /mcp-router says typed alone.")

(defun selection-text (query &optional (catalog +demo-tools+))
  "QUERY selected over CATALOG, as the lines the operator reads."
  (let* ((result (select-tools query catalog
                                :max-tools (setting :max-tools)
                                :budget-tokens (setting :budget-tokens)
                                :min-relevance (setting :min-relevance)))
         (selected (nlk:json-array result "selected"))
         (rejected (nlk:json-array result "rejected"))
         (total (or (nlk:json-value result :integer "total_tokens") 0))
         (decision (nlk:json-value result :string "decision")))
    (with-output-to-string (out)
      (format out "mcp-router: ~a ~d of ~d tools for ~s~@[, ~d tokens~]~%"
              decision (length selected) (length catalog) query
              (and (plusp (length selected)) total))
      (loop for item across selected
            do (format out "  ~a, ~d tokens: ~{~a~^ ~}~%"
                       (nlk:json-value item :string "id")
                       (nlk:json-value item :integer "tokens")
                       (coerce (nlk:json-array item "reasons") 'list)))
      (let ((fallback (nlk:json-value result :object "fallback")))
        (when fallback
          (format out "  next if the first fails: ~a~%" (nlk:json-value fallback :string "id"))))
      (let ((abstain (nlk:json-value result :string "abstain_reason")))
        (when (and (zero-p selected) abstain)
          (format out "  nothing was kept: ~a~%" abstain)))
      (loop for item across rejected
            do (format out "  rejected ~a: ~a~%"
                       (nlk:json-value item :string "id")
                       (nlk:json-value item :string "reason"))))))

(defun zero-p (vector)
  (zerop (length vector)))

(defun panel (summary)
  "The dialog bare /mcp-router answers: SUMMARY over one row that asks for a query."
  (nle:list-dialog
   :title "MCP Router" :summary summary
   :rows (list (nle:ask-row "Pick tools for a query" "Query" "/mcp-router"
                             :placeholder "read a local file, then search GitHub issues"
                             :verb "select"))))

(defun run-slash (args session-id)
  "/mcp-router QUERY: the selection that query would get over the demo catalog."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (zerop (length query))
        (values +status+ (panel +status+))
        (selection-text query))))

;;; --- the words the model reads ---------------------------------------------------

(defparameter +manual+
  "MCP Router is available through the nodecode-mcp-router cell: mcp_select ranks a
catalog of MCP tool descriptions under a context budget, and runs none of them.
  mcp_select {query, tools, max_tools, budget_tokens, min_relevance,
              required_permissions, forbidden_permissions, failed_tool_id, no_fallback, scope}
      Call it when many MCP tools are available and only a budgeted subset should be
      offered, or when a trace of why one tool beat another is wanted. It answers JSON:
      decision (select, fallback or abstain), selected (each with its score, tokens and
      reasons), an optional fallback preview, total_tokens, rejected (each with its
      cause: out_of_scope, disabled, already_failed, missing_permissions,
      forbidden_permissions, low_relevance, duplicate_input, max_tools, budget,
      fallback_unavailable, tool_absent), and abstain_reason when it chose nothing.
Doctrine, from the original. Selection is not authorization: a permission is what the
catalog declares, and a query, a tool or a model cannot add one. The local ranking is
lexical and deterministic, scope isolates lists and is not auth, and there is one
fallback hop and no execution. Say why a tool was kept or dropped only from the answer's
own reasons."
  "What (help :mcp-router) answers while the cell runs.")

;;; --- the declaration --------------------------------------------------------------

(defun read-floor (value)
  "VALUE -- the section's min_relevance, written as text -- as the number the selector
uses; anything that is not 0.0 to 1.0 refuses the start."
  (let ((*read-eval* nil))
    (let ((number (and (stringp value) (ignore-errors (read-from-string value)))))
      (if (and (realp number) (<= 0.0 number 1.0))
          (float number)
          (nlk:config-error
           "mcp-router.min_relevance is ~s: give a number between 0.0 and 1.0, the lexical floor a candidate must clear"
           value)))))

(defun read-settings (values table)
  "The section's values, with min_relevance parsed and checked here so a wrong floor
refuses the start rather than every selection."
  (declare (ignore table))
  (setf (getf values :min-relevance) (read-floor (getf values :min-relevance)))
  values)

(nle:define-cell mcp-router
  (:section ("mcp-router")
    (:guide "nothing here is required: the selector is local and needs no key; max_tools and budget_tokens are the ceiling a call that names neither gets, min_relevance is the lexical floor below which a candidate is dropped")
    ("max_tools" :integer :default 8 :min 1
     :doc "how many tools a selection may keep")
    ("budget_tokens" :integer :default 800 :min 1
     :doc "the context budget the kept tools' token costs must fit")
    ("min_relevance" :string :default "0.15"
     :doc "the lexical floor, a number between 0.0 and 1.0: a candidate sharing too few words with the query is dropped as low_relevance")
    ("omit_rejected" :boolean :default nil
     :doc "true to leave the rejected list out of the answer")
    ("extra_stopwords" :list
     :doc "words the lexical match ignores, on top of the built-in French and English list"))
  (:settings #'read-settings)
  (:tool "mcp_select" (nlk:contract 'select-request) 'select-tools-tool)
  (:command "mcp-router" 'run-slash
            :description "Pick MCP tools under a context budget"
            :argument-hint "QUERY")
  (:help :mcp-router
         "mcp-router: pick the MCP tools that fit a query inside a context budget - (mcp_select {query, tools})"
         +manual+))
