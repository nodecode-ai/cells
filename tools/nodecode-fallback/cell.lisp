;;;; cell.lisp --- the criteria, the ranking, the verbs, and the chain.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin's whole idea, kept: when the model a session runs on is
;;;; gone -- rate-limited, retired, offline -- someone has to name the next one,
;;;; and the model that would have to do it is the one that is down. So the
;;;; choice is made deterministically, from what the catalogs say, with no model
;;;; call: rank every model the operator's own providers offer against a small
;;;; set of criteria (context window, parameter count, recency, price tier, tool
;;;; calls, provider preference), and write the best few into where Nodecode
;;;; reads its fallbacks from -- the shared config's `failover.chain'.
;;;;
;;;; What the original wrote was Hermes' `fallback_providers'; what it read was
;;;; Hermes' provider registry. This reads Nodecode's own model list -- the one
;;;; the /models picker shows -- and writes failover.chain, which the engine
;;;; already walks on a rate limit, an outage, a broken answer or a refusal
;;;; (the engine's FAILOVER-CHAIN). Nothing here is a new failover mechanism:
;;;; the cell only keeps the chain filled.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "fallback": {"sort_by": "largest_parameter_count", "min_context": 128000,
;;;;                "auto_sync": true, "max_candidates": 3, ...}

(in-package #:nodecode-fallback)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Fallback keeps Nodecode's failover chain filled from what your own providers offer: it ranks
candidate models by context window, parameter count, recency, price tier, tool calls and
provider preference, and writes the best few into the shared config's failover.chain. It makes
no model call, so it works while the model you are on is the one that is down.
  (fallback:resolve)                      what it would pick, under the section's criteria
  (fallback:resolve :sort-by \"latest\" :min-context \"128k\" :free-only t)
  (fallback:apply-chain :max-candidates 3)      write the top 3 into failover.chain - say count=3
  (fallback:status)                        the primary, the chain, the providers
  (fallback:providers)                     each provider and how many models it names
  /fallback status | resolve | apply | clear | providers [key=value ...]
Criteria (a tool's argument, or key=value after the slash command): provider (auto, existing,
or one id), providers (a preference order, also the only providers searched), exclude_providers,
sort_by (largest_parameter_count, greatest_context, latest, latest_flash, smallest,
provider_preference), min_context/max_context (128k, 1m, 200000), min_params/max_params (30b),
free_only, flash_only, require_tools, vendor, name_filter (a substring or * glob), max_candidates.
A model's numbers come from its id (70b, 128k, :free, 4o-mini), then from Nodecode's own
models.dev row for it, then from a small table of families whose ids publish none; a number
nobody published stays unknown, and require_tools only drops a model whose row says it cannot
call tools. Nodecode walks failover.chain once a round's own retries are spent, on a 429, an
outage, a broken answer or a refusal; the primary model is never rewritten by this cell."
  "What (help :fallback) answers while the cell runs.")

(defparameter +sorts+ '("largest_parameter_count" "greatest_context" "latest"
                          "latest_flash" "smallest" "provider_preference")
  "The ranking strategies, as the original named them.")

(defparameter +auto-providers+ '("" "auto" "existing" "any")
  "PROVIDER values that mean every provider, not one.")

;;; --- criteria -------------------------------------------------------------------

(defun quantity (value what)
  "VALUE -- a number, or a string like 128k, 1m, 30b, 200000 -- as a number; a
refusal naming WHAT when it is neither."
  (cond ((numberp value) value)
        ((null value) nil)
        ((and (stringp value) (string= "" (nlk:trimmed value))) nil)
        ((and (stringp value) (say-number (nlk:trimmed value))))
        ((stringp value)
         (let* ((text (string-downcase (nlk:trimmed value)))
                (unit (char text (1- (length text))))
                (scale (case unit (#\k 1024) (#\m 1048576) (#\b 1) (t nil)))
                (number (say-number (if scale (subseq text 0 (1- (length text))) text))))
           (if (and scale number)
               (* number scale)
               (fail "~a is a number with an optional k, m or b: ~s is neither" what value))))
        (t (fail "~a is a number with an optional k, m or b: ~s is neither" what value))))

(defun provider-list (value)
  "VALUE -- a list, a vector, or a comma-separated string -- as a list of
lowercased provider ids."
  (let ((items (typecase value
                 (null nil)
                 (string (uiop:split-string value :separator ","))
                 (vector (coerce value 'list))
                 (list value)
                 (t (list (princ-to-string value))))))
    (loop for item in items
          for name = (string-downcase (nlk:trimmed (princ-to-string item)))
          when (plusp (length name)) collect name)))

(defun current-pin ()
  "The operator's primary model as (:provider P :model M), or NIL: the shared
config's general.default_model, an object or an old `provider/model' string."
  (let ((pin (ignore-errors (nle:config-get '("general" "default_model")))))
    (cond ((stringp pin)
           (let ((slash (position #\/ pin)))
             (and slash (list :provider (subseq pin 0 slash) :model (subseq pin (1+ slash))))))
          ((consp pin) (and (getf pin :provider)
                            (list :provider (princ-to-string (getf pin :provider))
                                  :model (princ-to-string (or (getf pin :model) "")))))
          (t nil))))

(defun section-criteria ()
  "The criteria the running cell's section declares."
  (list :sort-by (setting :sort-by)
        :provider (setting :provider)
        :providers (provider-list (setting :providers))
        :exclude-providers (provider-list (setting :exclude-providers))
        :free-only (setting :free-only)
        :flash-only (setting :flash-only)
        :require-tools (setting :require-tools)
        :min-context (setting :min-context)
        :min-params (setting :min-params)
        :max-candidates (setting :max-candidates)
        :vendor (setting :vendor)
        :name-filter (setting :name-filter)
        :prefer-current-provider (setting :prefer-current-provider)
        :allow-provider-switch (setting :allow-provider-switch)
        :current-provider (getf (current-pin) :provider)))

(defun criteria (&rest overrides)
  "The section's criteria with the non-NIL OVERRIDES laid over them."
  (let ((base (section-criteria)))
    (loop for (key value) on overrides by #'cddr
          when value do (setf (getf base key) value))
    base))

;;; --- filtering ------------------------------------------------------------------

(defun pattern (filter)
  "FILTER as a case-insensitive regex: a * is any run of characters, every
other character stands for itself."
  (with-output-to-string (out)
    (loop for character across (string-downcase filter)
          do (case character
               (#\* (write-string ".*" out))
               ((#\. #\+ #\? #\( #\) #\[ #\] #\{ #\} #\^ #\$ #\| #\\)
                (write-char #\\ out) (write-char character out))
               (t (write-char character out))))))

(defun name-matches-p (filter cand)
  "Whether CAND's provider/model line holds FILTER: a substring, or a pattern
when FILTER names one with a *."
  (let ((text (string-downcase (candidate-key cand))))
    (if (find #\* filter)
        (and (ppcre:scan (pattern filter) text) t)
        (and (search (string-downcase filter) text) t))))

(defun target-provider (criteria)
  "The one provider the criteria search, or NIL for every provider."
  (let ((provider (getf criteria :provider)))
    (when (and provider (not (member (string-downcase provider) +auto-providers+ :test #'equal)))
      (string-downcase provider))))

(defun keeps-p (cand criteria)
  "Whether CAND is a candidate the criteria allow."
  (let* ((provider (string-downcase (getf cand :provider)))
         (target (target-provider criteria))
         (wanted (getf criteria :providers))
         (excluded (getf criteria :exclude-providers))
         (current (string-downcase (or (getf criteria :current-provider) "")))
         (vendor (getf criteria :vendor))
         (filter (getf criteria :name-filter))
         (context (getf cand :context))
         (params (getf cand :params))
         (min-context (getf criteria :min-context))
         (max-context (getf criteria :max-context))
         (min-params (getf criteria :min-params))
         (max-params (getf criteria :max-params)))
    (and (or (null target) (string-equal target provider))
         (not (member provider excluded :test #'equal))
         (or (null wanted) (member provider wanted :test #'equal))
         (or (getf criteria :allow-provider-switch)
             (string= current "")
             (string-equal current provider))
         (or (not (getf criteria :free-only)) (getf cand :free))
         (or (not (getf criteria :require-tools)) (not (eql :no (getf cand :tools))))
         (or (not (getf criteria :flash-only)) (getf cand :flash))
         (or (null vendor) (string= vendor "") (string-equal vendor (getf cand :vendor)))
         (or (null filter) (string= filter "") (name-matches-p filter cand))
         (or (not (and min-context (plusp min-context))) (and context (>= context min-context)))
         (or (not (and max-context (plusp max-context))) (and context (<= context max-context)))
         (or (not (and min-params (plusp min-params))) (and params (>= params min-params)))
         (or (not (and max-params (plusp max-params))) (and params (<= params max-params))))))

;;; --- ranking ---------------------------------------------------------------------

(defun provider-rank (provider criteria)
  "How highly the criteria rank PROVIDER: its place in the preference list, best
first, 0 for one the list does not name."
  (let* ((order (getf criteria :providers))
         (position (position (string-downcase provider) order :test #'equal)))
    (if position (- (length order) position) 0)))

(defun sort-key (cand criteria)
  "CAND under the criteria' strategy, as a list of numbers compared
high-to-low: the first that differs decides."
  (let ((params (or (getf cand :params) 1.0))
        (context (or (getf cand :context) 0))
        (date (or (getf cand :date) 0))
        (version (or (getf cand :version) 0))
        (sort-by (or (getf criteria :sort-by) "")))
    (cond ((string-equal sort-by "greatest_context") (list context params date version))
          ((string-equal sort-by "latest") (list date version params context))
          ((string-equal sort-by "latest_flash")
           (list (if (getf cand :flash) 1 0) date version params context))
          ((string-equal sort-by "smallest")
           (list (- (if (and params (plusp params)) params 999999.0)) (- context)))
          ((string-equal sort-by "provider_preference")
           (list (provider-rank (getf cand :provider) criteria) params context date))
          (t (list params context date version)))))

(defun key-first-p (a b)
  "Whether the numbers A sort before B, high to low."
  (loop for x in a for y in b
        when (> x y) return t
        when (< x y) return nil
        finally (return nil)))

(defun better-p (a b criteria)
  "Whether A outranks B under the criteria, ties broken by provider/model so the
order is the same on every run."
  (let ((ka (sort-key a criteria)) (kb (sort-key b criteria)))
    (cond ((key-first-p ka kb) t)
          ((key-first-p kb ka) nil)
          (t (string< (candidate-key a) (candidate-key b))))))

(defun current-first (candidates criteria)
  "CANDIDATES with the current provider's own models first, their order kept,
when the criteria prefer them."
  (let ((current (and (getf criteria :prefer-current-provider)
                      (getf criteria :current-provider))))
    (if current
        (multiple-value-bind (mine others)
            (loop for cand in candidates
                  if (string-equal (getf cand :provider) current)
                    collect cand into mine
                  else collect cand into others
                  finally (return (values mine others)))
          (append mine others))
        candidates)))

(defun rank (candidates criteria)
  "CANDIDATES the criteria allow, best first, cut to their limit."
  (let* ((kept (remove-if-not (lambda (cand) (keeps-p cand criteria)) candidates))
         (sorted (stable-sort kept (lambda (a b) (better-p a b criteria))))
         (ordered (current-first sorted criteria))
         (limit (getf criteria :max-candidates)))
    (if (and limit (plusp limit))
        (subseq ordered 0 (min limit (length ordered)))
        ordered)))

;;; --- what a reader sees ------------------------------------------------------------

(defun short-figure (value unit)
  "VALUE in thousands or millions, rounded to one decimal, as `1M', `1.5M',
`128k': the figure a catalog publishes, not a quotient of it."
  (let ((rounded (/ (round (* value 10)) 10.0)))
    (if (= rounded (round rounded))
        (format nil "~d~a" (round rounded) unit)
        (format nil "~,1f~a" rounded unit))))

(defun window-label (tokens)
  "TOKENS as a short window: 200k, 1M, 128k."
  (cond ((>= tokens 1000000) (short-figure (/ tokens 1000000.0) "M"))
        ((>= tokens 1000) (short-figure (/ tokens 1000.0) "k"))
        (t (format nil "~d" tokens))))

(defun date-label (date)
  "DATE, YYYYMMDD, as YYYY-MM-DD, or NIL."
  (when (and date (plusp date))
    (format nil "~d-~2,'0d-~2,'0d" (floor date 10000) (mod (floor date 100) 100) (mod date 100))))

(defun candidate-line (cand)
  "Everything known about CAND, in one line."
  (format nil "~:[context unknown~;context ~a~]~@[, ~,1fB~]~@[, ~a~]~@[, ~a~]~@[, released ~a~]"
          (getf cand :context) (and (getf cand :context) (window-label (getf cand :context)))
          (getf cand :params)
          (case (getf cand :tools) (:no "no tool calls") (:unknown "tool calls unknown") (t "tools"))
          (cond ((and (getf cand :free) (getf cand :flash)) "free, fast tier")
                ((getf cand :free) "free")
                ((getf cand :flash) "fast tier"))
          (date-label (getf cand :date))))

(defun sort-label (sort-by)
  "SORT-BY in words, as the original described it."
  (or (cdr (assoc (or sort-by "") '(("greatest_context" . "greatest context")
                                       ("latest" . "latest release")
                                       ("latest_flash" . "latest fast tier")
                                       ("smallest" . "smallest first")
                                       ("provider_preference" . "provider preference")
                                       ("largest_parameter_count" . "largest parameter count"))
                       :test #'string-equal))
      "largest parameter count"))

(defun render (ranked criteria considered)
  "RANKED, as the reader sees it: one line a candidate, and what was searched."
  (with-output-to-string (out)
    (format out "~d of ~d candidate~:p, ranked by ~a~%"
            (length ranked) considered (sort-label (getf criteria :sort-by)))
    (if (null ranked)
        (format out "No model survives the criteria: widen min_context or min_params, drop free_only, or add a provider with (fallback:providers).")
        (loop for cand in ranked
              for i from 1
              do (format out "~d. ~a - ~a~%" i (candidate-key cand) (candidate-line cand))))))

;;; --- the chain, and where it is written ------------------------------------------------

(defun chain-entries (candidates)
  "CANDIDATES as the (:provider .. :model ..) plists the shared config holds."
  (loop for cand in candidates
        collect (list :provider (getf cand :provider) :model (getf cand :model))))

(defun current-chain ()
  "The failover chain the shared config carries, as a list of entries; NIL when
it carries none."
  (let ((value (ignore-errors (nle:config-get '("failover" "chain")))))
    (cond ((vectorp value) (coerce value 'list))
          ((and (consp value) (not (keywordp (first value)))) value)
          (t nil))))

(defvar *chain-reader* #'current-chain
  "How the cell reads the chain it may fill: () => the entries in the config now.
A test binds one that already answers entries, proving a hand-written chain is
never overwritten.")

(defvar *chain-writer*
  (lambda (entries)
    "Write ENTRIES as the shared config's failover.chain; NIL takes the key out."
    (if entries
        (nle:config-set '("failover" "chain") entries)
        (nle:config-set '("failover" "chain") :remove)))
  "How the cell writes the chain: (ENTRIES) => the diff's one line. A test binds
its own recorder, so nothing it runs writes the operator's config.")

(defun chain-line (entries)
  "ENTRIES as one line: provider/model, in order."
  (if (null entries)
      "(none: /fallback apply fills it from the section's criteria)"
      (format nil "~{~a~^ then ~}"
              (loop for entry in entries
                    collect (format nil "~a/~a"
                                    (getf entry :provider)
                                    (getf entry :model))))))

;;; --- the verbs ---------------------------------------------------------------------

(define-verb resolve (&key provider providers exclude-providers sort-by min-context
          max-context min-params max-params free-only require-tools flash-only
          vendor name-filter prefer-current-provider allow-provider-switch
          (max-candidates nil) (source nil))
  "Rank the models your providers offer against the criteria and answer them, best
first, without writing anything. PROVIDER is auto (every provider), existing, or
one id; PROVIDERS a preference order that also limits the search;
SORT-BY one of largest_parameter_count, greatest_context, latest, latest_flash,
smallest, provider_preference; MIN-CONTEXT/MAX-CONTEXT a window (128k, 1m);
MIN-PARAMS/MAX-PARAMS billions (30b); FREE-ONLY, FLASH-ONLY, REQUIRE-TOOLS;
VENDOR; NAME-FILTER (a substring or * glob); MAX-CANDIDATES how many to answer
with. Returns a string."
  (let* ((criteria (criteria :provider provider
                             :providers (and providers (provider-list providers))
                             :exclude-providers (and exclude-providers (provider-list exclude-providers))
                             :sort-by (and sort-by (string-downcase (nlk:trimmed sort-by)))
                             :min-context (quantity min-context "min_context")
                             :max-context (quantity max-context "max_context")
                             :min-params (quantity min-params "min_params")
                             :max-params (quantity max-params "max_params")
                             :free-only free-only
                             :flash-only flash-only
                             :require-tools require-tools
                             :vendor vendor
                             :name-filter name-filter
                             :prefer-current-provider prefer-current-provider
                             :allow-provider-switch allow-provider-switch
                             :max-candidates max-candidates))
         (all (candidates :source (or source *candidates*))))
    (render (rank all criteria) criteria (length all))))

(define-verb apply-chain (&key provider providers exclude-providers sort-by min-context
          min-params max-params free-only require-tools flash-only vendor name-filter
          prefer-current-provider allow-provider-switch (max-candidates nil) (source nil))
  "Rank the models your providers offer, then write the best MAX-CANDIDATES of them
into the shared config's failover.chain, as the criteria say; answers the chain and
the diff it made. The criteria are RESOLVE's. Returns a string."
  (let* ((criteria (criteria :provider provider
                             :providers (and providers (provider-list providers))
                             :exclude-providers (and exclude-providers (provider-list exclude-providers))
                             :sort-by (and sort-by (string-downcase (nlk:trimmed sort-by)))
                             :min-context (quantity min-context "min_context")
                             :min-params (quantity min-params "min_params")
                             :max-params (quantity max-params "max_params")
                             :free-only free-only
                             :require-tools require-tools
                             :flash-only flash-only
                             :vendor vendor
                             :name-filter name-filter
                             :prefer-current-provider prefer-current-provider
                             :allow-provider-switch allow-provider-switch
                             :max-candidates max-candidates))
         (all (candidates :source (or source *candidates*)))
         (ranked (rank all criteria)))
    (if (null ranked)
        (fail "no model survives the criteria, so failover.chain is left as it was: ~a"
              (render ranked criteria (length all)))
        (let ((entries (chain-entries ranked)))
          (format nil "~a~%failover.chain: ~a"
                  (funcall *chain-writer* entries)
                  (chain-line entries))))))

(define-verb status ()
  "The primary model, the failover chain the shared config carries, and every
provider this box can name models for. Returns a string."
  (multiple-value-bind (providers refused) (provider-counts)
    (let ((pin (current-pin)))
      (with-output-to-string (out)
        (format out "primary   ~a~%"
                (if pin (format nil "~a/~a" (getf pin :provider) (getf pin :model))
                    "(unset: the shell's own pick)"))
        (format out "chain     ~a~%" (chain-line (current-chain)))
        (format out "providers ~a~@[ (~a)~]"
                (if providers
                    (format nil "~{~a~^, ~}" (loop for (provider . count) in providers
                                                     collect (format nil "~a (~d)" provider count)))
                    "none named")
                refused)))))

(defun provider-counts ()
  "Every provider this box names models for, as (PROVIDER . COUNT), and why
the listing failed when it did, or NIL."
  (handler-case (values (ignore-errors (nle:models)) nil)
    (error (condition) (values nil (princ-to-string condition)))))

(define-verb providers ()
  "Every provider this box can name models for, and how many models each names.
Returns a string."
  (multiple-value-bind (providers refused) (provider-counts)
    (with-output-to-string (out)
      (if (null providers)
          (format out "No provider names a model here~@[ (~a)~]: configure one in /settings, then (restart-cells).~%" refused)
          (loop for (provider . count) in providers
                do (format out "~a - ~d model~:p~%" provider count))))))

;;; --- the slash command --------------------------------------------------------------

(defun boolean-value (text)
  "TEXT a shell typed as a flag's value: true for on, yes, 1, t."
  (member (string-downcase (nlk:trimmed text)) '("true" "yes" "1" "on" "t") :test #'equal))

(defun line-arguments (text)
  "TEXT's key=value words as the keyword arguments a verb takes."
  (let ((arguments '()))
    (dolist (word (uiop:split-string (or text "") :separator " "))
      (let ((equals (position #\= word)))
        (when equals
          (let* ((key (let ((name (string-downcase (subseq word 0 equals))))
                        (if (member name '("count" "limit") :test #'equal) "max_candidates" name)))
                 (value (subseq word (1+ equals)))
                 (flag-p (member key '("free_only" "flash_only" "require_tools"
                                       "prefer_current_provider" "allow_provider_switch")
                                 :test #'equal)))
            (setf arguments
                  (append arguments
                          (list (intern (string-upcase (substitute #\- #\_ key)) :keyword)
                                (cond (flag-p (and (boolean-value value) t))
                                      ((member key '("min_context" "max_context" "min_params" "max_params")
                                               :test #'equal)
                                       (quantity value key))
                                      ((string= key "max_candidates") (quantity value key))
                                      (t value)))))))))
    arguments))

(defun run-command (args session-id)
  "One /fallback invocation: STATUS (the default), RESOLVE, APPLY, CLEAR or
PROVIDERS, each with key=value criteria after it. => the answer's text."
  (declare (ignore session-id))
  (let* ((line (nlk:trimmed (or args "")))
         (space (position #\Space line))
         (verb (if space (subseq line 0 space) line))
         (rest (if space (nlk:trimmed (subseq line (1+ space))) "")))
    (cond ((string= verb "") (status))
          ((string-equal verb "status") (status))
          ((string-equal verb "providers") (providers))
          ((string-equal verb "clear")
           (format nil "~a~%failover.chain: ~a" (funcall *chain-writer* nil) (chain-line nil)))
          ((string-equal verb "resolve") (apply #'resolve (line-arguments rest)))
          ((string-equal verb "apply") (apply #'apply-chain (line-arguments rest)))
          (t (fail "/fallback ~a is not one of status, resolve, apply, clear, providers" verb)))))

;;; --- the tools ----------------------------------------------------------------------

(nlk:define-contract fallback-resolve-query
  (:doc "Rank the models the configured providers offer, best first, without writing
anything: which model to fall back to when the one a session runs on fails.")
  ("provider" :text :default "" :doc "'auto' (every provider), 'existing', or one provider id")
  ("providers" :array :of :string :doc "providers to search, best first")
  ("exclude_providers" :array :of :string :doc "providers to leave out")
  ("sort_by" :text :default "" :options '("largest_parameter_count" "greatest_context" "latest" "latest_flash"
              "smallest" "provider_preference")
   :doc "how to rank the candidates")
  ("min_context" :text :default "" :doc "the smallest context window that may answer: 128k, 1m, 200000")
  ("max_context" :text :default "" :doc "the largest context window that may answer")
  ("min_params" :text :default "" :doc "the smallest model that may answer, in billions: 30b")
  ("max_params" :text :default "" :doc "the largest model that may answer, in billions: 200b")
  ("free_only" :boolean :default nil :doc "only models on a free tier")
  ("flash_only" :boolean :default nil :doc "only fast-tier models")
  ("require_tools" :boolean :default nil :doc "leave out a model whose catalog row says it cannot call tools")
  ("vendor" :text :default "" :doc "a model family: anthropic, google, qwen, deepseek")
  ("name_filter" :text :default "" :doc "a substring or * glob on the provider/model line")
  ("max_candidates" :integer :default 0 :doc "how many to answer with; 0 reads the section"))

(nlk:define-contract fallback-apply-query
  (:doc "Rank the models the configured providers offer, then write the best few into
Nodecode's failover chain (failover.chain in the shared config), which the engine walks
when a round's model fails.")
  ("provider" :text :default "" :doc "'auto' (every provider), 'existing', or one provider id")
  ("providers" :array :of :string :doc "providers to search, best first")
  ("exclude_providers" :array :of :string :doc "providers to leave out")
  ("sort_by" :text :default "" :options '("largest_parameter_count" "greatest_context" "latest" "latest_flash"
              "smallest" "provider_preference")
   :doc "how to rank the candidates")
  ("min_context" :text :default "" :doc "the smallest context window that may answer: 128k, 1m")
  ("min_params" :text :default "" :doc "the smallest model that may answer, in billions: 30b")
  ("free_only" :boolean :default nil :doc "only models on a free tier")
  ("require_tools" :boolean :default nil :doc "leave out a model whose catalog row says it cannot call tools")
  ("max_candidates" :integer :default 0 :doc "how many entries to write; 0 reads the section"))

(defun resolve-tool (query)
  "FALLBACK_RESOLVE: the ranking, as text."
  (resolve :provider (fallback-resolve-query-provider query)
           :providers (fallback-resolve-query-providers query)
           :exclude-providers (fallback-resolve-query-exclude-providers query)
           :sort-by (fallback-resolve-query-sort-by query)
           :min-context (fallback-resolve-query-min-context query)
           :max-context (fallback-resolve-query-max-context query)
           :min-params (fallback-resolve-query-min-params query)
           :max-params (fallback-resolve-query-max-params query)
           :free-only (fallback-resolve-query-free-only query)
           :flash-only (fallback-resolve-query-flash-only query)
           :require-tools (fallback-resolve-query-require-tools query)
           :vendor (fallback-resolve-query-vendor query)
           :name-filter (fallback-resolve-query-name-filter query)
           :max-candidates (fallback-resolve-query-max-candidates query)))

(defun apply-tool (query)
  "FALLBACK_APPLY: the chain it wrote, as text."
  (apply-chain :provider (fallback-apply-query-provider query)
               :providers (fallback-apply-query-providers query)
               :exclude-providers (fallback-apply-query-exclude-providers query)
               :sort-by (fallback-apply-query-sort-by query)
               :min-context (fallback-apply-query-min-context query)
               :min-params (fallback-apply-query-min-params query)
               :free-only (fallback-apply-query-free-only query)
               :require-tools (fallback-apply-query-require-tools query)
               :max-candidates (fallback-apply-query-max-candidates query)))

(defun status-tool (arguments)
  "FALLBACK_STATUS: the primary, the chain and the providers, as text."
  (declare (ignore arguments))
  (status))

(defparameter +status-schema+
  (list :description "The primary model, the failover chain in the shared config, and every provider this box can name models for."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "FALLBACK_STATUS's schema: it takes no argument.")

;;; --- the start ---------------------------------------------------------------------

(defun seed-step (source writer reader)
  "Fill the chain once, when the section syncs and the config carries none, and
answer :STOP so the worker leaves."
  (when (and (setting :auto-sync) (null (funcall reader)))
    (let* ((all (candidates :source source))
           (criteria (section-criteria))
           (entries (chain-entries (rank all criteria))))
      (cond ((null entries)
             (nle:notice "fallback: no model survives the section's criteria, so failover.chain was left empty"
                         :level :warning :key "nodecode-fallback"))
            (t (ignore-errors (funcall writer entries))
               (nle:notice (format nil "fallback: seeded failover.chain with ~d candidate~:p~@[, by ~a~]"
                                   (length entries) (sort-label (getf criteria :sort-by)))
                           :key "nodecode-fallback")))))
  :stop)

(defun start ()
  "Seed the chain once, on a worker, so a cold models.dev cache costs the launch
nobody a second of the boot, and stop with the cell."
  (let ((source *candidates*)
        (writer *chain-writer*)
        (reader *chain-reader*))
    (let ((worker (nlk:worker-start "fallback-seed"
                                    (lambda () (seed-step source writer reader))
                                    :wake 3600 :prime t)))
      (nle:on-stop (lambda () (nlk:worker-stop worker))))))

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell fallback
  (:section ("fallback")
    (:guide "nothing is required; sort_by, min_context and min_params are how models are
ranked and filtered, and auto_sync fills failover.chain once while it is empty")
    ("sort_by" :choice :options '("largest_parameter_count" "greatest_context" "latest" "latest_flash"
                "smallest" "provider_preference")
     :default "largest_parameter_count"
     :doc "how candidates rank: biggest, longest context, newest, newest fast tier, smallest, or your provider order")
    ("provider" :string :default "auto"
     :doc "'auto' (every provider), 'existing', or the one provider to search")
    ("providers" :list
     :doc "a preference order, best first, that also limits the search to those providers")
    ("exclude_providers" :list :doc "providers to leave out")
    ("free_only" :boolean :default nil :doc "only models whose id rides a free tier")
    ("flash_only" :boolean :default nil :doc "only fast-tier models")
    ("require_tools" :boolean :default t
     :doc "leave out a model whose catalog row says it cannot call tools")
    ("min_context" :integer :default 0 :min 0
     :doc "the smallest context window a fallback may have, in tokens: 128000, 1000000")
    ("min_params" :integer :default 0 :min 0
     :doc "the smallest model a fallback may be, in billions of parameters: 30")
    ("vendor" :string :default "" :doc "a model family to keep: anthropic, google, qwen")
    ("name_filter" :string :default "" :doc "a substring or * glob the provider/model line must hold")
    ("prefer_current_provider" :boolean :default nil
     :doc "put the provider the primary model is on first")
    ("allow_provider_switch" :boolean :default t
     :doc "let a fallback sit on another provider; off keeps the primary's own")
    ("max_candidates" :integer :default 3 :min 1
     :doc "how many models the chain holds")
    ("auto_sync" :boolean :default t
     :doc "fill failover.chain at start while it is empty; a chain somebody wrote is never touched"))
  (:tool "fallback_resolve" (nlk:contract 'fallback-resolve-query) #'resolve-tool)
  (:tool "fallback_apply" (nlk:contract 'fallback-apply-query) #'apply-tool)
  (:tool "fallback_status" +status-schema+ #'status-tool)
  (:help :fallback
         "fallback: rank your providers' models by context, size, recency or price, and fill Nodecode's failover chain"
         +primer+)
  (:command "fallback" #'run-command
            :description "Rank the best fallback models, and fill failover.chain from them"
            :argument-hint "STATUS | RESOLVE | APPLY | CLEAR | PROVIDERS [key=value ...]")
  (:start #'start))
