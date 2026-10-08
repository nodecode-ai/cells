;;;; provider.lisp --- what OpenRouter is: its address, its models, the way a request reaches it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; providers/openrouter.kdl (the env name, wire-model-id-mode "openrouter",
;;;; thinking-format "openrouter"), ai/src/utils/openrouter-headers.ts (the
;;;; attribution headers), ai/src/providers/openai-shared.ts (the routing
;;;; variant suffix, the routing preferences, the reasoning object, the
;;;; output cap OpenRouter is not sent), coding-agent's providers.openrouterVariant
;;;; setting, and the bundled rows of catalog/src/models.json, which models.json
;;;; in this folder carries (tools/omp-models.py wrote it).
;;;;
;;;; OpenRouter speaks the OpenAI chat wire at https://openrouter.ai/api/v1,
;;;; and models.dev already makes it usable on the chat lane. What omp adds:
;;;;
;;;;   - every request names the app it comes from (HTTP-Referer, the title,
;;;;     the category) and asks OpenRouter's cache to keep it an hour
;;;;   - a model is named with a routing variant when one is chosen:
;;;;     `openai/gpt-5.5:nitro', unless the id already carries one
;;;;   - reasoning is OpenRouter's own object, {"effort": ...}, or
;;;;     {"enabled": false} for off, never reasoning_effort
;;;;   - no output cap the operator did not ask for: OpenRouter fans a
;;;;     request out to upstreams with caps of their own, and one above an
;;;;     upstream's cap makes OpenRouter skip that upstream
;;;;   - routing preferences (only, order) ride as the body's `provider'
;;;;
;;;; The attribution names Nodecode, as omp's names omp.

(in-package #:nodecode-openrouter)

(defparameter +base+ "https://openrouter.ai/api/v1"
  "Where OpenRouter is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("OPENROUTER_API_KEY")
  "The environment variables a key is read from, in order.")

(defparameter +key-page+ "https://openrouter.ai/settings/keys"
  "Where a key is made by hand.")

(defparameter +app-url+ "https://nodecode.ai"
  "The app OpenRouter attributes a request to: its HTTP-Referer.")

(defparameter +app-name+ "Nodecode"
  "The app's title on OpenRouter's rankings: X-OpenRouter-Title.")

(defparameter +variants+ '("default" "nitro" "floor" "online" "exacto")
  "omp's routing variants; default appends nothing.")

(defun attribution-headers ()
  "The headers every OpenRouter request carries: omp's getOpenRouterHeaders,
naming this app. The core's own User-Agent rides beside them."
  `(("HTTP-Referer" . ,+app-url+)
    ("X-OpenRouter-Title" . ,+app-name+)
    ("X-OpenRouter-Categories" . "cli-agent")
    ("X-OpenRouter-Cache" . "true")
    ("X-OpenRouter-Cache-TTL" . "3600")))

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-openrouter" "models.json")))
  "omp's bundled OpenRouter rows, read when this file loads: a vector of objects.")

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
       ;; CATALOG-PRICE's shape; a row priced at nothing is free, not a price
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "OpenRouter as a models.dev provider: models.dev's own row (the chat lane's
package, its key variable) with this section's base and omp's bundled
models over models.dev's."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" (or (nlk:json-value prior :text "name") "OpenRouter")
                     "npm" (or (nlk:json-value prior :text "npm") "@openrouter/ai-sdk-provider")
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

;;; --- the key -------------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for openrouter: OPENROUTER_API_KEY, else none. A
key the sign-in or /connect saved in auth.json answers before this point."
  ;; Never NEXT for openrouter: the ladder behind this point falls back to
  ;; the chat family's default variable, and would send OPENAI_API_KEY to
  ;; OpenRouter. The key never expires, so a probe costs nothing.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the request --------------------------------------------------------------

(defun wire-id (model-id variant)
  "The id OpenRouter is asked for: MODEL-ID with VARIANT appended as
`:VARIANT', unless VARIANT is default or the id already names one after its
last slash. omp's applyOpenRouterRoutingVariant."
  (let ((slash (or (position #\/ model-id :from-end t) -1))
        (colon (or (position #\: model-id :from-end t) -1)))
    (if (or (null variant) (equal variant "default") (> colon slash))
        model-id
        (format nil "~a:~a" model-id variant))))

(defun reasons-p (model)
  "Whether the catalog says MODEL on OpenRouter reasons."
  (let ((capability (ignore-errors (nle::resolve-model-capability model +provider+))))
    (and capability (nle::model-capability-reasoning-p capability))))

(defun shape-body (body config)
  "BODY, the chat request the core built for the OpenRouter round CONFIG,
as omp sends it: the routed wire id, OpenRouter's reasoning object, no
output cap the operator did not set, the routing preferences."
  (let ((model (nle::effective-provider-config-model config))
        (effort (nle::effective-provider-config-reasoning-effort config))
        (asked (gethash "reasoning_effort" body)))
    (setf (gethash "model" body) (wire-id model (setting :variant)))
    (remhash "reasoning_effort" body)
    (cond (asked
           (setf (gethash "reasoning" body) (nlk:json-object "effort" asked)))
          ((and (stringp effort) (string-equal effort "off") (reasons-p model))
           (setf (gethash "reasoning" body) (nlk:json-object "enabled" nil))))
    (unless (nle::effective-provider-config-max-completion-tokens config)
      (remhash "max_tokens" body))
    (let ((only (coerce (or (setting :only) #()) 'vector))
          (order (coerce (or (setting :order) #()) 'vector)))
      (when (or (plusp (length only)) (plusp (length order)))
        (setf (gethash "provider" body)
              (nlk:json-object :when (plusp (length only)) "only" only
                               :when (plusp (length order)) "order" order))))
    body))
