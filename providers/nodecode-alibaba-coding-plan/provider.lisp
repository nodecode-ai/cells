;;;; provider.lisp --- what Alibaba Coding Plan is: its regions, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/alibaba-coding-plan.kdl and providers/alibaba-coding-plan.kdl,
;;;; ai/src/registry/oauth/alibaba-coding-plan.ts (the endpoint choice and
;;;; the key pages), ai/src/providers/openai-shared.ts (the Qwen thinking
;;;; dialect), and the bundled rows of catalog/src/models.json, which
;;;; models.json in this folder carries (tools/omp-models.py wrote it).
;;;;
;;;; The Coding Plan is Alibaba Cloud Model Studio's coding subscription. It
;;;; speaks the OpenAI chat wire with a bearer key from the Model Studio
;;;; console, and it is sold in two regions whose keys do not cross: the
;;;; international one at coding-intl.dashscope.aliyuncs.com and the China
;;;; one at coding.dashscope.aliyuncs.com. omp's login asks which (or for a
;;;; custom base, a proxy) before it asks for the key, and keeps the answer in
;;;; the credential. Here the answer is the section's: `region', and
;;;; `base_url' for a custom endpoint. Every model it serves speaks the Qwen
;;;; thinking dialect: enable_thinking in place of reasoning_effort.

(in-package #:nodecode-alibaba-coding-plan)

(defparameter +regions+
  '(("international" "https://coding-intl.dashscope.aliyuncs.com/v1"
     "https://modelstudio.console.alibabacloud.com/")
    ("china" "https://coding.dashscope.aliyuncs.com/v1"
     "https://bailian.console.aliyun.com/?tab=model#/api-key"))
  "Each region omp's login offers: (NAME BASE KEY-PAGE).")

(defparameter +region-names+ (mapcar #'first +regions+)
  "What the section's region may name.")

(defparameter +env+ '("ALIBABA_CODING_PLAN_API_KEY")
  "The environment variables a Coding Plan key is read from, in order.")

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
    (asdf:system-relative-pathname "nodecode-alibaba-coding-plan" "models.json")))
  "omp's bundled Coding Plan rows, read when this file loads: a vector of objects.")

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
       ;; CATALOG-PRICE's shape; a row priced at nothing is the plan's, not a price
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "The Coding Plan as a models.dev provider: the chat lane's package, this
section's base, the key variable, and the bundled models over PRIOR's (the
row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Alibaba Coding Plan"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- the Qwen thinking dialect --------------------------------------------------
;;; The provider's rule is thinking-format "qwen" for every model: a
;;; top-level enable_thinking says whether a reasoning model thinks, and
;;; reasoning_effort is never sent (omp's qwenTemplateReasoningEffort is off).

(defun shape-thinking (body model effort)
  "BODY, the chat request for MODEL at EFFORT, in the Qwen dialect."
  ;; Only a reasoning model is told anything, as in omp. An unset effort is
  ;; the provider's own default, as the chat lane leaves it; "off" turns the
  ;; switch off, any rung turns it on.
  (when (and effort (nlk:json-value (model-row model) :boolean "reasoning"))
    (remhash "reasoning_effort" body)
    (setf (gethash "enable_thinking" body) (if (string-equal effort "off") :false t)))
  body)
