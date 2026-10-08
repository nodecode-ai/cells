;;;; provider.lisp --- what Ollama Cloud is: its address, its key, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/ollama-cloud.kdl and providers/ollama-cloud.kdl, catalog/src/
;;;; provider-models/ollama.ts (the base, the discovery), and the bundled rows
;;;; of catalog/src/models.json, which models.json in this folder carries
;;;; (tools/omp-models.py wrote it).
;;;;
;;;; Ollama Cloud is Ollama's hosted runtime at https://ollama.com, with a
;;;; bearer key from https://ollama.com/settings/keys. models.dev lists it on
;;;; the OpenAI-compatible chat wire (https://ollama.com/v1); omp maps that very
;;;; row to Ollama's own wire instead (openai-compat.ts:
;;;; simpleModelsDevDescriptor("ollama-cloud", ..., "ollama-chat",
;;;; "https://ollama.com")) and serves all 52 of its models over POST
;;;; /api/chat. That wire carries what /v1 does not: the native `think' value,
;;;; whose `max' rung the DeepSeek V4 and GLM 5.2 ladders reach and no
;;;; reasoning_effort spelling does; the cache split of the prompt
;;;; (prompt_eval_cached_count); done_reason `load', the answer Ollama gives a
;;;; request that has no user turn; a tool result named by its tool. The cell
;;;; ports that choice: a lane of its own speaking /api/chat (wire.lisp).

(in-package #:nodecode-ollama-cloud)

(defparameter +base+ "https://ollama.com"
  "Where Ollama Cloud is served: the base the lane appends /api/chat to.")

(defparameter +env+ '("OLLAMA_CLOUD_API_KEY")
  "The environment variables an Ollama Cloud key is read from, in order.")

(defparameter +key-page+ "https://ollama.com/settings/keys"
  "Where a key is made.")

(defun normalized-base (base)
  "BASE as omp normalizes it (normalizeOllamaCloudBaseUrl): trimmed, no
trailing slash, no trailing /api, the public host when blank."
  (let ((value (nlk:trimmed (or base ""))))
    (if (zerop (length value))
        +base+
        (let ((trimmed (string-right-trim "/" value)))
          (if (uiop:string-suffix-p trimmed "/api")
              (subseq trimmed 0 (- (length trimmed) 4))
              trimmed)))))

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-ollama-cloud" "models.json")))
  "omp's bundled Ollama Cloud rows, read when this file loads: a vector of objects.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun model-reasoning-p (model-id)
  "Whether MODEL-ID thinks: its bundled row says so; a model the rows do not
carry is taken to, the way omp's discovery reads Ollama's `thinking'
capability, so an effort reaches it as a `think' value."
  (let ((row (model-row model-id)))
    (if row (nlk:json-value row :boolean "reasoning") t)))

(defun model-images-p (model-id)
  "Whether MODEL-ID takes images: its bundled row's input lists them."
  (find "image" (nlk:json-array (model-row model-id) "input") :test #'equal))

(defun catalog-model (row)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (flet ((value (type key) (nlk:json-value row type key)))
    (let ((cost (value :object "cost")))
      (nle::make-catalog-model
       (value :string "name")
       (value :integer "context")
       (value :integer "output")
       (or (value :array "input") #("text"))
       #("text")
       (sort (remove-if-not #'nle::effort-rank (coerce (or (value :array "efforts") #()) 'list))
             #'< :key #'nle::effort-rank)
       (value :boolean "reasoning")
       nil
       t
       ;; CATALOG-PRICE's shape; a row priced at nothing is a plan's, not a price
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "Ollama Cloud as a models.dev provider: this cell's lane package, this
section's base, the key variable, and the bundled models over PRIOR's (the
row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Ollama Cloud"
                     "npm" +npm+
                     "api" (normalized-base (setting :base-url))
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
