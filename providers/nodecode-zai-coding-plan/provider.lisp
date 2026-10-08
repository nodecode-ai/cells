;;;; provider.lisp --- what the GLM Coding Plan is: its two addresses, its key, its models, its wires.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; providers/zai.kdl (the env name, the thinking modes), runtime/
;;;; behavior.kdl's `api-routes provider="zai"' (which wire a model rides),
;;;; ai/src/providers/anthropic.ts (the bearer a non-Anthropic Messages host
;;;; is sent, the budget thinking block), and the bundled zai rows of
;;;; catalog/src/models.json, which models.json in this folder carries
;;;; (tools/omp-models.py wrote it from the `zai' provider: omp keeps no rows
;;;; under `zai-coding-plan', whose sign-in stores the key it mints as zai's).
;;;;
;;;; omp serves the plan on two of Z.AI's endpoints with the one key: every
;;;; GLM model on the Anthropic Messages endpoint at
;;;; https://api.z.ai/api/anthropic, and GLM-5.3-Flash, which that endpoint
;;;; does not carry, on the native chat endpoint at
;;;; https://api.z.ai/api/coding/paas/v4. models.dev knows the plan too, as
;;;; `zai-coding-plan' on the chat endpoint alone; its models omp has no row
;;;; for keep riding there, as they did before this cell.

(in-package #:nodecode-zai-coding-plan)

(defparameter +base+ "https://api.z.ai/api/coding/paas/v4"
  "The native chat endpoint: the base the chat lane appends /chat/completions to.")

(defparameter +anthropic-base+ "https://api.z.ai/api/anthropic/v1"
  "The Anthropic Messages endpoint, as the Nodecode lane composes it: omp's
base is https://api.z.ai/api/anthropic and its SDK appends /v1/messages; the
lane appends /messages, so the base here carries the /v1.")

(defparameter +env+ '("ZAI_API_KEY")
  "The environment variables omp reads a Z.AI key from, in order. The core's
own ladder reads ZAI_CODING_PLAN_API_KEY.")

(defparameter +budget-effort-models+ '("glm-5.2" "glm-5.3" "glm-5.3-flash")
  "The models omp's zai rules give thinking-mode anthropic-budget-effort: a
budget thinking block AND the effort named in output_config. Every other
reasoning row is thinking-mode budget: the block alone.")

(defparameter +thinking-budgets+
  '(("minimal" . 1024) ("low" . 4096) ("medium" . 8192) ("high" . 16384)
    ("xhigh" . 32768) ("max" . 32768))
  "omp's ANTHROPIC_THINKING: budget_tokens per effort, for the Messages
endpoint, which thinks on a budget and has no adaptive thinking.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-zai-coding-plan" "models.json")))
  "omp's bundled zai rows, read when this file loads: a vector of objects.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun model-lane (model-id)
  "The lane omp sends MODEL-ID on: anthropic for a Messages row, the chat
lane for a chat row, NIL for a model omp has no row for."
  (let ((api (nlk:json-value (model-row model-id) :string "api")))
    (cond ((equal api "anthropic-messages") "anthropic")
          ((equal api "openai-completions") "openai-completions"))))

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
  "The plan as a models.dev provider: the chat lane's package and this
section's chat base (models.dev's own), the key variables, and the bundled
models over PRIOR's (the row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Z.AI (GLM Coding Plan, Sign in)"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce (remove-duplicates
                                    (append +env+ (coerce (or (nlk:json-value prior :array "env") #()) 'list))
                                    :test #'equal :from-end t)
                                   'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- what the operator's own config says ------------------------------------------

(defun operator-lane-p (model)
  "Whether providers.zai-coding-plan in the shared config names a wire for
MODEL, the provider's sdk or the model's own: the operator's declaration
outranks omp's route."
  (or (nle::trimmed-config-string (nle::configured-provider-entry +provider+) "sdk")
      (nle::trimmed-config-string (nle::configured-model-entry +provider+ model) "sdk")))

(defun operator-base-p ()
  "Whether providers.zai-coding-plan in the shared config names a base_url:
then every lane composes from it, as for any configured provider."
  (nle::trimmed-config-string (nle::configured-provider-entry +provider+) "base_url"))

;;; --- the wire ---------------------------------------------------------------------

(defun bearer-headers (headers key)
  "HEADERS, the Messages lane's, sending KEY as a bearer token instead of
x-api-key: what omp's Anthropic client sends a host that is not Anthropic's."
  (cons (cons "authorization" (format nil "Bearer ~a" key))
        (remove-if (lambda (pair) (member (car pair) '("x-api-key" "authorization") :test #'string-equal))
                   headers)))

(defun budget-thinking (body model)
  "BODY, a Messages request for MODEL, thinking on a budget: the Messages
endpoint takes omp's budget block (budget_tokens, display summarized) and,
for the budget-effort models, the effort beside it; adaptive thinking, which
the core asks of a model whose ladder reaches past high, it does not take."
  ;; The budget sits under max_tokens with 1024 tokens of answer headroom and
  ;; is dropped below the wire's 1024 minimum, the core's own clamp.
  (let* ((thinking (nlk:json-value body :object "thinking"))
         (type (nlk:json-value thinking :string "type"))
         (effort (nlk:json-value body :string "output_config" "effort"))
         (max-tokens (or (nlk:json-value body :integer "max_tokens") 32000)))
    (when (equal type "adaptive")
      (let* ((tier (cdr (assoc effort +thinking-budgets+ :test #'string-equal)))
             (budget (and tier (min tier (max 0 (- max-tokens 1024))))))
        (if (and budget (>= budget 1024))
            (setf (gethash "thinking" body) (nlk:json-object "type" "enabled" "budget_tokens" budget))
            (remhash "thinking" body))))
    (when (equal "enabled" (nlk:json-value body :string "thinking" "type"))
      (setf (gethash "display" (gethash "thinking" body)) "summarized"))
    (unless (and (member model +budget-effort-models+ :test #'equal)
                 (equal "enabled" (nlk:json-value body :string "thinking" "type")))
      (remhash "output_config" body))
    body))

(defun chat-thinking (body effort)
  "BODY, a chat request at reasoning EFFORT, with Z.AI's thinking switch:
omp's zai thinking format turns thinking on beside reasoning_effort and off
when the effort is off."
  (cond ((and (stringp effort) (string-equal effort "off"))
         (setf (gethash "thinking" body) (nlk:json-object "type" "disabled")))
        ((stringp effort)
         (setf (gethash "thinking" body) (nlk:json-object "type" "enabled"))))
  body)
