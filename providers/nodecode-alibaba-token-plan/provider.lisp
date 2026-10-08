;;;; provider.lisp --- what the QwenCloud Token Plan is: its regions, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/alibaba-token-plan.kdl and providers/alibaba-token-plan.kdl (the
;;;; model seed, the env names, the thinking dialects), catalog/src/wire/
;;;; alibaba-token-plan.ts (the regional bases), ai/src/registry/oauth/
;;;; alibaba-token-plan.ts (the region choice and the key pages), ai/src/
;;;; providers/openai-shared.ts (the dialects on the wire), and the bundled
;;;; rows of catalog/src/models.json, which models.json in this folder
;;;; carries (tools/omp-models.py wrote it).
;;;;
;;;; The Token Plan is Alibaba's token subscription, sold as two regional
;;;; products whose keys do not cross: International (Singapore) and China
;;;; (Beijing). Each region is its own OpenAI chat endpoint, a bearer key
;;;; from its own console. omp's login asks for the region (or a custom base)
;;;; before it asks for the key, and keeps a region other than the default in
;;;; the credential, so inference and discovery both go there. Here the region
;;;; is the section's: `region', and `base_url' for a custom endpoint.
;;;;
;;;; Its models think in the Qwen dialect, an enable_thinking switch in place
;;;; of reasoning_effort, except Qwen3.8 Max and Flash: thinking, they take
;;;; reasoning_effort for its depth and enable_thinking to turn it on.

(in-package #:nodecode-alibaba-token-plan)

(defparameter +regions+
  '(("international" "https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1"
     "https://home.qwencloud.com/billing/subscription/token-plan-individual")
    ("china" "https://token-plan.cn-beijing.maas.aliyuncs.com/compatible-mode/v1"
     "https://www.aliyun.com/benefit/scene/tokenplan"))
  "Each region omp's login offers: (NAME BASE KEY-PAGE).")

(defparameter +region-names+ (mapcar #'first +regions+)
  "What the section's region may name.")

(defparameter +env+ '("ALIBABA_TOKEN_PLAN_API_KEY" "BAILIAN_TOKEN_PLAN_API_KEY")
  "The environment variables a Token Plan key is read from, in order.")

(defun region-base (region)
  "The base REGION is served at."
  (second (assoc region +regions+ :test #'string=)))

(defun base ()
  "Where this section sends a round: its base_url, else its region's base."
  (or (setting :base-url) (region-base (setting :region))))

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-alibaba-token-plan" "models.json")))
  "omp's bundled Token Plan rows, read when this file loads: a vector of
objects. They are the provider rule's seed, the plan's documented Individual
text models.")

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
       t
       ;; CATALOG-PRICE's shape; the seed prices every model at nothing: the plan's
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "The Token Plan as a models.dev provider: the chat lane's package, this
section's base, the key variables, and the bundled models over PRIOR's (the
row models.dev itself published, when it did)."
  ;; The seed outranks a same-id row, as omp's precedence "seed" has it, so
  ;; incomplete upstream metadata cannot replace its capabilities.
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "QwenCloud Token Plan"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- the thinking dialects -------------------------------------------------------
;;; The provider's rule is thinking-format "qwen": a top-level enable_thinking
;;; says whether a reasoning model thinks, and reasoning_effort is never sent.
;;; Qwen3.8 Max and Flash combine the switch with OpenAI's reasoning_effort:
;;; their rule turns, when thinking, to the openai format with
;;; enable_thinking: true as extra body, so the effort steers the depth; not
;;; thinking, they stay on the Qwen dialect, which says enable_thinking: false.

(defparameter +effort-models+ '("qwen3.8-max" "qwen3.8-flash")
  "Models that take reasoning_effort beside enable_thinking while they think.")

(defun shape-thinking (body model effort)
  "BODY, the chat request for MODEL at EFFORT, in MODEL's thinking dialect."
  ;; Only a reasoning model is told anything, as in omp. An unset effort is
  ;; the provider's own default, as the chat lane leaves it.
  (when (and effort (nlk:json-value (model-row model) :boolean "reasoning"))
    (let ((thinking (not (string-equal effort "off"))))
      (unless (and thinking (member model +effort-models+ :test #'string=))
        (remhash "reasoning_effort" body))
      (setf (gethash "enable_thinking" body) (if thinking t :false))))
  body)
