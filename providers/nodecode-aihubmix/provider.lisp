;;;; provider.lisp --- what AIHubMix is: its address, its key, its catalog, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of AIHubMix's Hermes provider plugin (see NOTICE). The plugin's
;;;; one idea, kept here: Hermes is an agent, so the /model picker should list
;;;; only the routes a turn can actually run on. AIHubMix serves 400+ models
;;;; over one OpenAI-compatible endpoint but publishes no capability metadata
;;;; on /v1/models, so the plugin reads the capability-annotated catalog at
;;;; /api/v1/models?types=llm and keeps the entries advertising `tools' or
;;;; `function_calling' -- either marker alone qualifies, because AIHubMix
;;;; fronts many upstream backends and their metadata is uneven.
;;;;
;;;; Two tiers, as in the original: the live catalog when it answers, and a
;;;; curated list of tool-capable flagships when it does not, so an offline
;;;; start still fills the picker.

(in-package #:nodecode-aihubmix)

(defparameter +base+ "https://aihubmix.com/v1"
  "Where AIHubMix is served: the base the chat lane appends /chat/completions to.")

(defparameter +catalog-url+ "https://aihubmix.com/api/v1/models?types=llm"
  "The capability-annotated catalog: one `features' string per model.")

(defparameter +api-key-env+ "AIHUBMIX_API_KEY"
  "The environment variable an AIHubMix key is read from; /connect's saved key answers first.")

(defparameter +timeout+ 8
  "Seconds one catalog fetch may take, connect included.")

(defparameter +agentic-features+ '("tools" "function_calling")
  "The catalog's markers for a route that accepts tool calls: either one qualifies.")

(defparameter +fallback-ids+
  '("claude-sonnet-5"
    "claude-opus-5"
    "claude-fable-5"
    "gpt-5.6-sol"
    "gemini-3.7-flash"
    "gemini-3.5-flash-lite"
    "deepseek-v4-pro"
    "glm-5.3"
    "kimi-k3"
    "qwen3.8-max")
  "The picker when the live catalog is unreachable: the original plugin's
curated flagships, each verified there to advertise tool support.")

;;; --- the catalog -------------------------------------------------------------

(defun agentic-p (entry)
  "Whether catalog ENTRY advertises tool-calling support."
  (let ((raw (nlk:json-value entry :string "features")))
    (and (stringp raw)
         (loop for part in (uiop:split-string raw :separator ",")
               thereis (member (string-trim '(#\Space #\Tab) part) +agentic-features+
                               :test #'string-equal)))))

(defun entry-id (entry)
  "ENTRY's model id: the catalog's `model_id', else a plain `id'."
  (or (nlk:json-value entry :string "model_id") (nlk:json-value entry :string "id")))

(defun catalog-rows (payload)
  "The tool-capable rows of the capability catalog PAYLOAD, in catalog order."
  (let ((entries (if (hash-table-p payload)
                     (nlk:json-value payload :array "data")
                     payload))
        (rows '())
        (seen (make-hash-table :test 'equal)))
    (when (vectorp entries)
      (loop for entry across entries
            do (let ((id (and (hash-table-p entry) (agentic-p entry) (entry-id entry))))
                 (when (and id (plusp (length id)) (not (gethash id seen)))
                   (setf (gethash id seen) t)
                   (push (list :id id
                               :name (nlk:json-value entry :string "name")
                               :context (nlk:json-value entry :integer "context_length"))
                         rows)))))
    (nreverse rows)))

(defun fallback-rows ()
  "The curated tool-capable rows: what the picker keeps when the catalog is unreachable."
  (loop for id in +fallback-ids+ collect (list :id id :name id :context nil)))

(defvar *live-rows* nil
  "The rows the live capability catalog answered, or NIL while it is unfetched
or unreachable; NIL is what makes the curated list answer instead.")

(defun fetch-rows (&aux (answer nil) (status nil))
  "One fetch of the capability catalog => its tool-capable rows, or NIL."
  (handler-case
      (multiple-value-setq (answer status)
        (nlk:http :get +catalog-url+
                  :headers (let ((key (nle::credential-env +api-key-env+)))
                             (append (when key
                                       (list (cons "authorization" (format nil "Bearer ~a" key))))
                                     (list (cons "accept" "application/json"))))
                  :timeout +timeout+
                  :connect-timeout (min 5 +timeout+)))
    ((or error sb-sys:deadline-timeout) () (return-from fetch-rows nil)))
  (when (and (integerp status) (< status 300))
    (let ((rows (handler-case (catalog-rows (nlk:decode-json answer))
                  (error () nil))))
      (and rows (plusp (length rows)) rows))))

(defun refresh (base)
  "One step of the catalog worker: fetch the live catalog when BASE is
AIHubMix's own, keep its rows, and stop the worker once they have landed.
Answers NIL to be woken again after the worker's wait, so an unreachable
catalog is retried rather than given up on."
  (when (equal base +base+)
    (when (setf *live-rows* (fetch-rows))
      (forget-catalog)
      (nle:notice (format nil "aihubmix: ~d tool-capable models from the live catalog"
                          (length *live-rows*)))
      :stop)))

;;; --- the provider row --------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
AIHubMix's row in it.")

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base or other models builds a new one."
  (setf *catalog* (cons nil nil)))

(defun catalog-model (row)
  "ROW, a plist from the catalog or the curated list, as a CATALOG-MODEL."
  (destructuring-bind (&key id name context) row
    (declare (ignore id))
    ;; every row here is tool-capable by construction, and a turn answers in
    ;; text: vision in, text out, as AIHubMix relays it
    (nle::make-catalog-model name context nil #("text" "image") #("text")
                              nil nil nil t nil)))

(defun catalog-row (&optional prior)
  "AIHubMix as a models.dev provider: the chat lane's package, this section's
base, the key variable, and the tool-capable models over PRIOR's (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (dolist (row (or *live-rows* (fallback-rows)))
      (setf (gethash (getf row :id) models) (catalog-model row)))
    (nlk:json-object "name" "AIHubMix"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (vector +api-key-env+)
                     "models" models)))

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with AIHubMix's row, made once
per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))
