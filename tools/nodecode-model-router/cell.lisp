;;;; cell.lisp --- the section, the model_route tool, /route, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the original exposed as an MCP server with one tool, a CLI and an
;;;; HTTP route is here one clause of NLE:DEFINE-CELL: a tool the model calls,
;;;; a command the operator types, a topic on the manual, and the `model-router'
;;;; section they run on. Stop takes every one of them back, because the
;;;; declaration derives it.
;;;;
;;;; Nothing here needs a key, a network or a vendor: the catalog is data, the
;;;; ranking is arithmetic, and a key's whereabouts is one fact about the
;;;; process. A key is named, never read into a result.

(in-package #:nodecode-model-router)

;;; --- the words ----------------------------------------------------------------

(defparameter +primer+
  "model-router: pick the LLM a task goes to, under quality, cost and latency limits.
  model_route(task, ...)  one decision from the catalog: select, fallback or abstain,
                          with the reasons, the runner-up and what was rejected
Constraints: required_capabilities, prefer (balanced|best|cheapest|fastest|named),
min_quality, max_cost, max_latency, min_context, provider, model, failed_model_id,
allow_fallback, max_alternatives (the three numbers are 0.0-1.0, the rest integers).
Pass models=[...] to rank your own rows instead of the catalog. The answer names the
vendor's base_url and the NAME of the environment variable holding its key - this
tool never reads a key, never calls a model and never generates text; the caller
sends the task to the chosen row.
From eval the same decision is (model-router:route \"...\") for the lines and
(model-router:route-json \"...\") for the JSON; (model-router:catalog-text) prints
the catalog. In a shell /route asks the same, /route -- provider NAME shows one
vendor's rows. (help :model-router) has this while the cell runs; the section is
in ~/.nodecode/config.jsonc under model-router."
  "What (help :model-router) answers while the cell runs.")

;;; --- what one call may say --------------------------------------------------------

(nlk:define-contract route-call
  (:doc "One routing decision: which of the declared models can take this task now,
under these constraints, or that none can.")
  ("task" :text :required t
          :doc "What the model will be asked to do, in your own words; it is scored, never sent.")
  ("models" :array
            :doc "The rows to rank instead of the configured catalog: one object a row, with id, provider, model, base_url, api_key_env, quality, cost, latency, capabilities, context_window.")
  ("required_capabilities" :array :of :string
                            :doc "Every one of these must be in the row's capabilities: chat, tools, json, coding, vision.")
  ("prefer" :string :options '("balanced" "best" "cheapest" "fastest" "named")
            :doc "How to rank: the default, the highest quality, the lowest cost, the lowest latency, or one pinned model.")
  ("min_quality" :number :doc "Reject a row whose quality is below this, 0.0 to 1.0.")
  ("max_cost" :number :doc "Reject a row whose cost is above this, 0.0 to 1.0.")
  ("max_latency" :number :doc "Reject a row whose latency is above this, 0.0 to 1.0.")
  ("min_context" :integer :doc "Reject a row whose context window is smaller than this many tokens.")
  ("allow_fallback" :boolean :default t
                     :doc "Answer one runner-up under the choice; false leaves the selection alone.")
  ("failed_model_id" :string
                      :doc "A row that just failed: answer what to use instead, one hop, honouring its fallback_id.")
  ("provider" :string :doc "Only rows of this vendor.")
  ("model" :string :doc "Only this row: an id, a model, or vendor:model. With prefer=named it is the pin.")
  ("scope" :string :doc "Which namespace of rows to consider; default is the default one.")
  ("max_alternatives" :integer :doc "How many runners-up to list after the choice."))

;;; --- a caller's own rows ----------------------------------------------------------

(defun split-ref (id)
  "ID as (values VENDOR MODEL): the first colon, when the part before it is a
vendor name rather than a path."
  (let ((colon (position #\: id)))
    (if (and colon (plusp colon) (not (find #\/ id :end colon)))
        (values (subseq id 0 colon) (subseq id (1+ colon)))
        (values "custom" id))))

(defun clamped-number (value &optional (default 0.5))
  "VALUE as a 0.0-1.0 single float, DEFAULT where it is absent; a row that
says 1.4 is clamped, not refused, so one typo cannot lose the call."
  (coerce (min 1.0 (max 0.0 (coerce (or value default) 'single-float))) 'single-float))

(defun hash-row-entry (row)
  "One caller-supplied JSON row as a catalog entry."
  (let* ((id (nlk:json-value row :string "id"))
         (named-provider (nlk:json-value row :string "provider"))
         (named-model (nlk:json-value row :string "model")))
    (unless (or id named-model)
      (fail "a models row needs an id, or a model (and ideally its provider)"))
    (multiple-value-bind (guessed-provider guessed-model)
        (if id (split-ref id) (values nil named-model))
      (let* ((provider (or named-provider guessed-provider "custom"))
             (model (or named-model guessed-model id))
             (envs (json-strings (nlk:json-value row :array "api_key_envs")))
             (one (nlk:json-value row :string "api_key_env"))
             (caps (json-strings (nlk:json-value row :array "capabilities"))))
        (make-entry :id (or id (format nil "~a:~a" provider model))
                    :name (or (nlk:json-value row :string "name") model)
                    :provider provider
                    :model model
                    :base-url (or (nlk:json-value row :string "base_url") "")
                    :key-envs (or envs (and one (list one)))
                    :quality (clamped-number (nlk:json-value row :number "quality"))
                    :cost (clamped-number (nlk:json-value row :number "cost"))
                    :latency (clamped-number (nlk:json-value row :number "latency"))
                    :capabilities (or caps '("chat"))
                    :context-window (or (nlk:json-value row :integer "context_window") 8192)
                    :enabled (json-boolean row "enabled" t)
                    :fallback-id (nlk:json-value row :string "fallback_id")
                    :scope (or (nlk:json-value row :string "scope") "default")
                    :optional-key (json-boolean row "optional_key"
                                                 (string= provider "ollama")))))))

(defun plist-row-entry (row)
  "One caller-supplied row written as a plist, for a form typed in eval."
  (let* ((id (getf row :id))
         (named-provider (getf row :provider))
         (named-model (getf row :model)))
    (unless (or id named-model)
      (fail "a models row needs an :id, or a :model (and ideally its :provider)"))
    (multiple-value-bind (guessed-provider guessed-model)
        (if id (split-ref id) (values nil named-model))
      (let ((provider (or named-provider guessed-provider "custom"))
            (model (or named-model guessed-model id)))
        (make-entry :id (or id (format nil "~a:~a" provider model))
                    :name (or (getf row :name) model)
                    :provider provider
                    :model model
                    :base-url (or (getf row :base-url) "")
                    :key-envs (let ((one (getf row :api-key-env)))
                                (if (listp one) one (and one (list one))))
                    :quality (clamped-number (getf row :quality))
                    :cost (clamped-number (getf row :cost))
                    :latency (clamped-number (getf row :latency))
                    :capabilities (copy-list (or (getf row :capabilities) '("chat")))
                    :context-window (or (getf row :context-window) 8192)
                    :enabled (if (member :enabled row) (and (getf row :enabled) t) t)
                    :fallback-id (getf row :fallback-id)
                    :scope (or (getf row :scope) "default")
                    :optional-key (or (getf row :optional-key)
                                      (string= provider "ollama")))))))

(defun row-entry (row)
  "A caller's row, as JSON or as a plist, as one catalog entry."
  (cond ((hash-table-p row) (hash-row-entry row))
        ((listp row) (plist-row-entry row))
        (t (fail "a models row is an object, not ~s" (type-of row)))))

;;; --- one decision ----------------------------------------------------------------

(defun request-of (task &key models required-capabilities prefer min-quality max-cost
                              max-latency min-context (allow-fallback t) failed-model-id
                              provider model scope max-alternatives)
  "The request a call or a /route makes, the settings answering what it leaves out.
=> (values REQUEST ROWS), ROWS the caller's own rows or NIL for the catalog's."
  (let* ((strategy (string-downcase (or prefer (setting :default-prefer) "balanced")))
         (pin (or model (and (string= strategy "named") (setting :default-model))))
         (vendor (nlk:trimmed (or provider "")))
         (pinned (nlk:trimmed (or pin ""))))
    (values (make-route-request
             :task task
             :required-capabilities (mapcar #'string-downcase
                                            (coerce (or required-capabilities '()) 'list))
             :prefer strategy
             :min-quality (coerce (or min-quality 0.0) 'single-float)
             :max-cost (coerce (or max-cost 1.0) 'single-float)
             :max-latency (coerce (or max-latency 1.0) 'single-float)
             :min-context (or min-context 0)
             :allow-fallback (and allow-fallback t)
             :failed-model-id failed-model-id
             :provider (and (plusp (length vendor)) (string-downcase vendor))
             :model (and (plusp (length pinned)) pinned)
             :scope (or scope "default")
             :max-alternatives (or max-alternatives (setting :max-alternatives)))
            (and models (mapcar #'row-entry (coerce models 'list))))))

(defun narrow (entries request)
  "ENTRIES cut to the max_candidates a judge is ever shown, best local rows
first: the original scored everything and sent only the top of it."
  (let ((limit (setting :max-candidates)))
    (if (<= (length entries) limit)
        entries
        (multiple-value-bind (candidates) (collect entries request)
          (loop for candidate in (take-n candidates limit)
                collect (candidate-entry candidate))))))

(defun decide (request entries)
  "The one decision: the local ranking, a judge's opinion when one is
configured, and the note that says when it was not there."
  (let* ((entries (narrow entries request))
         (url (setting :judge-url)))
    (multiple-value-bind (scores note)
        (if url
            (judge-verdict url (setting :judge-key-env) (route-request-task request)
                           entries (setting :judge-timeout))
            (values nil nil))
      (setf (route-request-judge-weight request)
            (if scores (coerce (/ (setting :judge-weight-percent) 100.0) 'single-float) 0.0))
      (let ((result (rank entries request :judge-scores (or scores '())
                                       :require-keys (setting :require-keys))))
        (when note
          (setf (route-result-reasons result)
                (append (route-result-reasons result) (list note))))
        result))))

(defun entries-for (rows)
  "The rows a decision ranks: the caller's own, or the catalog's."
  (or rows (catalog-entries (setting :catalog-file))))

;;; --- the verbs -------------------------------------------------------------------

(define-verb route-json (task &rest options)
  "Route TASK and answer the JSON the model_route tool answers. OPTIONS are the
tool's own members as keywords: (model-router:route-json \"...\" :prefer \"cheapest\")."
  (multiple-value-bind (request rows) (apply #'request-of task options)
    (result-json (decide request (entries-for rows))
                 :show-rejected (setting :show-rejected))))

(define-verb route (task &rest options)
  "Route TASK and answer the lines /route prints: what was chosen, where it is
reached, which variable holds its key, why, and what was thrown out."
  (multiple-value-bind (request rows) (apply #'request-of task options)
    (result-text (decide request (entries-for rows)))))

(define-verb catalog-text ()
  "The catalog as the table the original's `jev-model llms` printed: every row,
with the variable that would make it callable, and whether this process holds it."
  (catalog-lines (catalog-entries (setting :catalog-file))))

;;; --- the tool --------------------------------------------------------------------

(defun route-call-tool (call)
  "model_route: one decision from the catalog, as JSON."
  (multiple-value-bind (request rows)
      (request-of (route-call-task call)
                  :models (route-call-models call)
                  :required-capabilities (route-call-required-capabilities call)
                  :prefer (route-call-prefer call)
                  :min-quality (route-call-min-quality call)
                  :max-cost (route-call-max-cost call)
                  :max-latency (route-call-max-latency call)
                  :min-context (route-call-min-context call)
                  :allow-fallback (route-call-allow-fallback call)
                  :failed-model-id (route-call-failed-model-id call)
                  :provider (route-call-provider call)
                  :model (route-call-model call)
                  :scope (route-call-scope call)
                  :max-alternatives (route-call-max-alternatives call))
    (result-json (decide request (entries-for rows))
                 :show-rejected (setting :show-rejected))))

;;; --- /route ---------------------------------------------------------------------

(defun split-first (text)
  "TEXT as (values FIRST REST) at its first blank."
  (let ((space (position-if (lambda (char) (member char '(#\Space #\Tab))) text)))
    (if space
        (values (subseq text 0 space) (nlk:trimmed (subseq text (1+ space))))
        (values text ""))))

(defun chosen-name (result)
  (or (and (route-result-selected result) (selection-id (route-result-selected result)))
      (route-result-abstain-reason result)
      "nothing"))

(defun strategy-rows (task)
  "One row a strategy, so a decision can be asked again the other way."
  (loop for strategy in (remove "named" +strategies+ :test #'string=)
        collect (list :label strategy
                      :detail (format nil "rank this task for ~a" strategy)
                      :run (list :enter (format nil "/route -- ~a ~a" strategy task)))))

(defun routed (task prefer)
  "TASK under PREFER, as /route answers it: the lines, and a row a strategy."
  (multiple-value-bind (request rows) (request-of task :prefer prefer)
    (let ((result (decide request (entries-for rows))))
      (values (result-text result)
              (nle:list-dialog :title "model-router"
                                :context (route-result-decision result)
                                :summary (format nil "~a under ~a"
                                                 (chosen-name result)
                                                 (route-request-prefer request))
                                :rows (strategy-rows task))))))

(defun route-provider (name)
  "One vendor's rows, as /route -- provider NAME prints them."
  (let* ((catalog (model-catalog (setting :catalog-file)))
         (rows (remove name catalog :key (lambda (row) (getf row :provider)) :test-not #'string=)))
    (if rows
        (with-output-to-string (out)
          (format out "~a: ~d model~:p~%" name (length rows))
          (dolist (row rows)
            (format out "  ~a~%    q ~,2f  cost ~,2f  lat ~,2f  ~d ctx  key ~a~@[ (set)~]~%"
                    (getf row :id) (getf row :quality) (getf row :cost) (getf row :latency)
                    (getf row :context-window) (getf row :api-key-env)
                    (and (getf row :has-key) t))))
        (format nil "no vendor ~a in the catalog: ~{~a~^, ~}" name
                (remove-duplicates (mapcar (lambda (row) (getf row :provider)) catalog)
                                   :test #'string=)))))

(defun catalog-panel ()
  "The catalog as lines, and the panel that opens one vendor."
  (let ((catalog (model-catalog (setting :catalog-file))))
    (values (catalog-lines (catalog-entries (setting :catalog-file)))
            (nle:list-dialog
             :title "model-router"
             :context (format nil "~d models" (length catalog))
             :summary "Type a task after /route, or open one vendor's rows."
             :rows (loop for provider in (remove-duplicates
                                           (mapcar (lambda (row) (getf row :provider)) catalog)
                                           :test #'string=)
                         for rows = (remove provider catalog
                                            :key (lambda (row) (getf row :provider))
                                            :test-not #'string=)
                         collect (list :label provider
                                       :detail (format nil "~d model~:p, ~d callable here"
                                                       (length rows)
                                                       (count-if (lambda (row) (getf row :has-key))
                                                                 rows))
                                       :run (list :enter (format nil "/route -- provider ~a"
                                                                 provider))))))))

(defun run-command (args session-id)
  "/route: nothing prints the catalog; TASK routes; \"-- STRATEGY TASK\" routes
it under that strategy; \"-- provider NAME\" opens one vendor. => the answer's
text, and the panel that asks again."
  (declare (ignore session-id))
  (running-settings)
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (catalog-panel)
        (multiple-value-bind (word rest) (split-first text)
          (if (string= word "--")
              (multiple-value-bind (verb argument) (split-first rest)
                (cond ((string= verb "provider") (values (route-provider argument) nil))
                      ((member verb +strategies+ :test #'string=) (routed argument verb))
                      (t (values (format nil "-- takes provider NAME, or a strategy and a task; not ~a"
                                         verb)
                                 nil))))
              (routed text nil))))))

;;; --- the section, once ------------------------------------------------------------

(defun read-settings (values table)
  "The section's members, checked where the declaration cannot hold them: a
catalog file that is not there, a pin that is not vendor:model, a weight
outside its range. A refusal here is the start's, not the first call's."
  (declare (ignore table))
  (let ((file (getf values :catalog-file))
        (pin (nlk:trimmed (or (getf values :default-model) "")))
        (weight (getf values :judge-weight-percent)))
    (when (and file (not (probe-file file)))
      (nlk:config-error "model-router.catalog_file is ~s, which is not a file" file))
    (when (and (plusp (length pin)) (not (find #\: pin)))
      (nlk:config-error "model-router.default_model is ~s: write it as vendor:model" pin))
    (setf (getf values :default-model) (and (plusp (length pin)) pin))
    (when (and (integerp weight) (> weight 100))
      (nlk:config-error "model-router.judge_weight_percent is ~s: 0 to 100" weight))
    values))

(nle:define-cell model-router
  ;; Nothing here is required: an unanswered section ranks the presets, and a
  ;; key is only ever named. catalog_file adds vendors or replaces numbers;
  ;; judge_url asks a second opinion; the rest are defaults of the ranking.
  (:section ("model-router")
    (:guide "nothing here is required: the preset catalog ranks with no key; catalog_file adds vendors or replaces numbers from a JSON file of {\"providers\": {name: {base_url, api_key_env, models}}}; default_prefer is how a call that names none is ranked; default_model is the pin prefer=named uses when a call names none; judge_url asks a decision endpoint for a second opinion, judge_key_env names the variable holding its key; require_keys leaves out rows whose key is not set")
    ("catalog_file" :path
                    :doc "a JSON catalog merged over the presets, the original's JEV_MODELS_FILE")
    ("default_prefer" :choice :options '("balanced" "best" "cheapest" "fastest" "named")
                      :default "balanced"
                      :doc "how a call that names no preference is ranked")
    ("default_model" :string
                     :doc "the row prefer=named pins when a call names none, as vendor:model")
    ("max_alternatives" :integer :default 3 :min 0
                         :doc "how many runners-up a decision lists")
    ("max_candidates" :integer :default 50 :min 1
                       :doc "how many rows are ever shown to a judge")
    ("require_keys" :boolean :default nil
                     :doc "leave out rows whose key variable is not set here")
    ("show_rejected" :boolean :default t
                     :doc "name the rows a decision threw out, and why")
    ("judge_url" :string
                  :doc "a decision endpoint taking {task, models} and answering {scores:[{id,score}]}")
    ("judge_key_env" :env
                      :doc "the environment variable holding the judge's bearer token")
    ("judge_timeout" :integer :default 30 :min 1
                      :doc "seconds one judge call may take")
    ("judge_weight_percent" :integer :default 40 :min 0
                             :doc "what the judge's score is worth against the strategy's own numbers, in percent"))
  (:settings 'read-settings)
  (:tool "model_route" (nlk:contract 'route-call) 'route-call-tool)
  (:command "route" 'run-command
            :description "Which model to send a task to, and why")
  (:help :model-router "model-router: choose the LLM for a task under quality, cost and latency limits"
         +primer+))
