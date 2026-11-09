;;;; catalog.lisp --- the presets, a catalog file, and a model's own facts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A catalog row is what the router ranks: a model id, the vendor that
;;;; serves it, the address it is reached at, the environment variables that
;;;; may hold its key, and the three numbers the ranking is made of - quality,
;;;; cost and latency, each 0.0 to 1.0, where 1.0 is the best quality and the
;;;; highest cost. None of them is measured here: they are declarations, and
;;;; a catalog file or the caller's own rows replace them.
;;;;
;;;; The presets are the original's (catalog.py), same vendors, same model
;;;; ids, same numbers. A key is never stored: a row names the environment
;;;; variables that may hold one, and ENTRY-HAS-KEY asks the process whether
;;;; this start has it.

(in-package #:nodecode-model-router)

;;; --- one row ------------------------------------------------------------------

(defstruct (entry (:constructor make-entry) (:copier nil))
  "One model the router may pick, with what picking it costs."
  (id "" :type string)
  (name "" :type string)
  (provider "" :type string)
  (model "" :type string)
  (base-url "" :type string)
  ;; Not one name but the list of names that may answer for this vendor: the
  ;; preset's own, the operator's JEV_LLM_<VENDOR>_API_KEY, and, for
  ;; OpenRouter, the gateway key the original also accepted there.
  (key-envs '() :type list)
  (quality 0.5 :type real)
  (cost 0.5 :type real)
  (latency 0.5 :type real)
  (capabilities '() :type list)
  (context-window 8192 :type integer)
  (enabled t)
  (fallback-id nil)
  (scope "default" :type string)
  (optional-key nil))

(defun env-set-p (name)
  "Whether this process holds a non-empty NAME."
  (and (stringp name)
       (let ((value (uiop:getenv name)))
         (and value (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))))))

(defun entry-key-env (entry)
  "The environment variable that answers for ENTRY here: the one that is set,
else the first name the catalog gives, so a message can say which to export."
  (or (find-if #'env-set-p (entry-key-envs entry))
      (first (entry-key-envs entry))
      ""))

(defun entry-has-key (entry)
  "Whether this process can call ENTRY now: a key in one of its variables, or
a vendor that needs none (a local Ollama)."
  (or (entry-optional-key entry)
      (string= (entry-provider entry) "ollama")
      (and (find-if #'env-set-p (entry-key-envs entry)) t)))

(defun entry-ref (entry)
  "provider:model, what a caller pins and what a rejection names."
  (format nil "~a:~a" (entry-provider entry) (entry-model entry)))

(defun entry-public (entry)
  "ENTRY as the plist a caller reads: the vendor's address and the NAME of its
key variable, never the key."
  (list :id (entry-id entry)
        :name (entry-name entry)
        :provider (entry-provider entry)
        :model (entry-model entry)
        :base-url (entry-base-url entry)
        :api-key-env (entry-key-env entry)
        :has-key (entry-has-key entry)
        :quality (entry-quality entry)
        :cost (entry-cost entry)
        :latency (entry-latency entry)
        :capabilities (copy-list (entry-capabilities entry))
        :context-window (entry-context-window entry)
        :enabled (entry-enabled entry)
        :scope (entry-scope entry)))

;;; --- the presets ---------------------------------------------------------------

(defparameter *presets*
  '(;; vendor, the address it is reached at, the variables that may hold its
    ;; key, and its models with the three numbers and what each can do.
    ("openrouter"
     :base-url "https://openrouter.ai/api/v1"
     :key-envs ("OPENROUTER_API_KEY" "GATEWAY_API_KEY")
     :models ((:id "anthropic/claude-sonnet-4" :quality 0.95 :cost 0.55 :latency 0.45
              :capabilities ("chat" "tools" "json" "coding" "vision") :context-window 200000)
              (:id "anthropic/claude-3.5-sonnet" :quality 0.90 :cost 0.50 :latency 0.40
              :capabilities ("chat" "tools" "json" "coding" "vision") :context-window 200000)
              (:id "openai/gpt-4o-mini" :quality 0.78 :cost 0.12 :latency 0.22
              :capabilities ("chat" "tools" "json") :context-window 128000)
              (:id "google/gemini-2.0-flash" :quality 0.82 :cost 0.18 :latency 0.20
              :capabilities ("chat" "tools" "json") :context-window 1000000)))
    ("openai"
     :base-url "https://api.openai.com/v1"
     :key-envs ("OPENAI_API_KEY")
     :models ((:id "gpt-4o" :quality 0.92 :cost 0.70 :latency 0.40
              :capabilities ("chat" "tools" "json" "coding" "vision") :context-window 128000)
              (:id "gpt-4o-mini" :quality 0.78 :cost 0.15 :latency 0.20
              :capabilities ("chat" "tools" "json") :context-window 128000)))
    ("groq"
     :base-url "https://api.groq.com/openai/v1"
     :key-envs ("GROQ_API_KEY")
     :models ((:id "llama-3.3-70b-versatile" :quality 0.80 :cost 0.10 :latency 0.12
              :capabilities ("chat" "tools" "json") :context-window 128000)
              (:id "mixtral-8x7b-32768" :quality 0.72 :cost 0.08 :latency 0.10
              :capabilities ("chat" "json") :context-window 32768)))
    ("together"
     :base-url "https://api.together.xyz/v1"
     :key-envs ("TOGETHER_API_KEY")
     :models ((:id "meta-llama/Meta-Llama-3.1-70B-Instruct-Turbo" :quality 0.80 :cost 0.20
              :latency 0.28 :capabilities ("chat" "json") :context-window 128000)))
    ("fireworks"
     :base-url "https://api.fireworks.ai/inference/v1"
     :key-envs ("FIREWORKS_API_KEY")
     :models ((:id "accounts/fireworks/models/llama-v3p1-70b-instruct" :quality 0.79
              :cost 0.20 :latency 0.25 :capabilities ("chat" "json") :context-window 128000)))
    ("mistral"
     :base-url "https://api.mistral.ai/v1"
     :key-envs ("MISTRAL_API_KEY")
     :models ((:id "mistral-large-latest" :quality 0.88 :cost 0.45 :latency 0.35
              :capabilities ("chat" "tools" "json" "coding") :context-window 128000)
              (:id "mistral-small-latest" :quality 0.72 :cost 0.12 :latency 0.20
              :capabilities ("chat" "tools" "json") :context-window 32000)))
    ("ollama"
     :base-url "http://127.0.0.1:11434/v1"
     :key-envs ("OLLAMA_API_KEY")
     :optional-key t
     :models ((:id "llama3.2" :quality 0.58 :cost 0.0 :latency 0.50
              :capabilities ("chat" "json") :context-window 128000)
              (:id "qwen2.5" :quality 0.62 :cost 0.0 :latency 0.48
              :capabilities ("chat" "json") :context-window 128000))))
  "The vendors and models this cell knows without being told, as the original
catalog.py declares them. Data: a layer or a catalog file replaces any of it.")

(defun provider-key-envs (name spec)
  "The variables that may hold NAME's key: the preset's own, then the
per-vendor JEV_LLM_<VENDOR>_API_KEY the original also read."
  (let* ((own (remove nil (getf spec :key-envs)))
         (custom (format nil "JEV_LLM_~:@(~a~)_API_KEY" name)))
    (if (member custom own :test #'string=) own (append own (list custom)))))

(defun entry-of (name spec row)
  "One catalog row from a provider NAME, its SPEC and one model ROW."
  (let ((model (getf row :id)))
    (make-entry :id (format nil "~a:~a" name model)
                :name (or (getf row :name) model)
                :provider name
                :model model
                :base-url (or (getf row :base-url) (getf spec :base-url) "")
                :key-envs (provider-key-envs name spec)
                :quality (coerce (or (getf row :quality) 0.5) 'single-float)
                :cost (coerce (or (getf row :cost) 0.5) 'single-float)
                :latency (coerce (or (getf row :latency) 0.5) 'single-float)
                :capabilities (copy-list (or (getf row :capabilities) '("chat")))
                :context-window (or (getf row :context-window) 8192)
                :enabled (if (member :enabled row) (and (getf row :enabled) t) t)
                :fallback-id (getf row :fallback-id)
                :scope (or (getf row :scope) "default")
                :optional-key (or (getf spec :optional-key)
                                  (string= name "ollama")))))

(defun spec-entries (name spec)
  "Every row provider NAME's SPEC declares, in the order it declares them."
  (loop for row in (getf spec :models)
        when (and (listp row) (stringp (getf row :id)) (plusp (length (getf row :id))))
          collect (entry-of name spec row)))

;;; --- a catalog file -------------------------------------------------------------

(defun json-boolean (table key &optional default)
  "TABLE's KEY as a boolean: an absent member reads DEFAULT, a false one false."
  (multiple-value-bind (value present) (nlk:json-value table :boolean key)
    (if present (and value t) default)))

(defun json-strings (value)
  "VALUE, a JSON array, as a list of its strings."
  (when (vectorp value)
    (loop for item across value
          for text = (nlk:json-value item :string)
          when text collect text)))

(defun json-row (row)
  "One model row of a catalog file: an id, or an object naming its numbers."
  (if (stringp row)
      (list :id row)
      (let ((spec (list :id (nlk:json-value row :string "id")
                        :name (nlk:json-value row :string "name"))))
        (dolist (key '("quality" "cost" "latency"))
          (let ((value (nlk:json-value row :number key)))
            (when value (setf (getf spec (intern (string-upcase key) :keyword)) value))))
        (let ((caps (json-strings (nlk:json-value row :array "capabilities"))))
          (when caps (setf (getf spec :capabilities) caps)))
        (let ((window (nlk:json-value row :integer "context_window")))
          (when window (setf (getf spec :context-window) window)))
        (when (nlk:json-value row :string "fallback_id")
          (setf (getf spec :fallback-id) (nlk:json-value row :string "fallback_id")))
        (when (nlk:json-value row :string "scope")
          (setf (getf spec :scope) (nlk:json-value row :string "scope")))
        (let ((enabled (json-boolean row "enabled" :absent)))
          (unless (eq enabled :absent) (setf (getf spec :enabled) enabled)))
        spec)))

(defun json-spec (table)
  "One provider object of a catalog file as the plist the presets are in."
  (let ((spec '()))
    (dolist (pair '(("base_url" . :base-url) ("base_url_override" . :base-url)))
      (let ((value (nlk:json-value table :string (car pair))))
        (when value (setf (getf spec (cdr pair)) value))))
    (let ((names (append (json-strings (nlk:json-value table :array "api_key_envs"))
                         (let ((one (nlk:json-value table :string "api_key_env")))
                           (and one (list one))))))
      (when names (setf (getf spec :key-envs) names)))
    (let ((optional (json-boolean table "optional_key" :absent)))
      (unless (eq optional :absent) (setf (getf spec :optional-key) optional)))
    (let ((models (nlk:json-value table :array "models")))
      (when (and models (plusp (length models)))
        (setf (getf spec :models) (loop for row across models collect (json-row row)))))
    spec))

(defun read-catalog-file (path)
  "The provider specs a JSON catalog file declares, as (NAME . SPEC) pairs.
The file is the original's JEV_MODELS_FILE: {\"providers\": {name: spec}}."
  (unless (probe-file path)
    (fail "catalog_file ~a does not exist" path))
  (let ((document
          (handler-case (nlk:decode-json (uiop:read-file-string path))
            (error (condition)
              (fail "catalog_file ~a is not JSON: ~a" path condition)))))
    (let ((providers (nlk:json-value document :object "providers")))
      (unless providers
        (fail "catalog_file ~a declares no providers object" path))
      (let ((pairs '()))
        (maphash (lambda (name table)
                   (when (and (stringp name) (hash-table-p table))
                     (push (cons (string-downcase name) (json-spec table)) pairs)))
                 providers)
        (nreverse pairs)))))

;;; --- the catalog ---------------------------------------------------------------

(defun merge-spec (old new)
  "NEW over OLD: a member the file names wins, and its models replace the
preset's rather than mixing with them."
  (let ((merged (copy-list old)))
    (loop for (key value) on new by #'cddr
          when value do (setf (getf merged key) value))
    merged))

(defun catalog-specs (file)
  "The provider specs: the presets, the file's merged over them, a vendor the
file adds appended after them."
  (let ((specs (loop for preset in *presets*
                     collect (cons (first preset) (copy-list (rest preset)))))
        (ollama (and (env-set-p "OLLAMA_BASE_URL") (uiop:getenv "OLLAMA_BASE_URL"))))
    (when (and ollama (assoc "ollama" specs :test #'string=))
      (setf (getf (cdr (assoc "ollama" specs :test #'string=)) :base-url) ollama))
    (when file
      (dolist (pair (read-catalog-file file))
        (let ((known (assoc (car pair) specs :test #'string=)))
          (if known
              (setf (cdr known) (merge-spec (cdr known) (cdr pair)))
              (setf specs (append specs (list (cons (car pair) (cdr pair)))))))))
    specs))

(defun catalog-entries (file)
  "Every row the presets and FILE declare, one list, vendors in order."
  (loop for (name . spec) in (catalog-specs file)
        append (spec-entries name spec)))

(defun model-catalog (&optional file)
  "Both catalogs as the lists a caller reads: the presets, with FILE's
providers merged over them. One plist a row; never a key, only its variable."
  (mapcar #'entry-public (catalog-entries file)))
