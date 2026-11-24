;;;; provider.lisp --- what Telnyx is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of team-telnyx's hermes-inference-provider (see NOTICE):
;;;; __init__.py's provider profile and README.md's quirks.
;;;;
;;;; Telnyx serves an OpenAI-compatible inference API at
;;;; https://api.telnyx.com/v2/ai/openai: chat completions (streaming, tool
;;;; calling, usage on the final chunk) plus a standard /models catalog. The
;;;; catalog mixes models hosted on Telnyx's own GPU infrastructure (Kimi, GLM,
;;;; MiniMax) with proxied frontier routes (openai/..., anthropic/...), all of
;;;; which speak the same chat wire through this base. A model is addressed by
;;;; its catalog id, e.g. moonshotai/Kimi-K3.
;;;;
;;;; Three quirks the original documents, carried here:
;;;;
;;;;   - /models requires Bearer auth; an anonymous request gets a 401. The
;;;;     core's listing sends the lane's key, so the live catalog arrives
;;;;     for a configured provider and the rows below are only the fallback.
;;;;   - No output cap is volunteered. A cap combined with function tools
;;;;     trips Telnyx error 10015 on hosted models, and no catalog metadata
;;;;     predicts which models reject it, so the profile never defaults one.
;;;;     Nodecode sends max_tokens only when it knows an output limit for the
;;;;     model or the operator configured one, and these rows name none, so a
;;;;     Telnyx round carries no cap unless the operator asked for one.
;;;;   - Catalog prices are quoted per 1M tokens. Nodecode's listing reads
;;;;     ids, display names, windows and owners, not rates, so the unit never
;;;;     reaches a price here; the rows below carry no cost at all.

(in-package #:nodecode-telnyx)

(defparameter +base+ "https://api.telnyx.com/v2/ai/openai"
  "Where Telnyx inference is served: the base the chat lane appends
/chat/completions to, and the base the catalog is listed from.")

(defparameter +env+ '("TELNYX_API_KEY")
  "The environment variables a Telnyx key is read from, in order.")

(defparameter +key-page+ "https://portal.telnyx.com/#/app/api-keys"
  "Where a key is made: the Telnyx portal's API keys page.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-telnyx" "models.json")))
  "The original's curated fallback list, read when this file loads: a vector
of objects. The live catalog replaces it the moment the listing answers.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

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
       ;; a turn always offers tools and reads text back: these rows are chat models
       t
       ;; CATALOG-PRICE's shape; a row priced at nothing is not priced
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "Telnyx as a models.dev provider: the chat lane's package, this section's
base, the key variable, and the bundled models over PRIOR's (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Telnyx"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
