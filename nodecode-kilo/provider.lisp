;;;; provider.lisp --- what Kilo Gateway is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/kilo.kdl and providers/kilo.kdl, catalog/src/provider-models/
;;;; openai-compat.ts (the gateway's base), ai/src/providers/openai-shared.ts
;;;; (the Qwen thinking dialect), and the bundled rows of catalog/src/
;;;; models.json, which models.json in this folder carries (tools/omp-models.py
;;;; wrote it).
;;;;
;;;; Kilo Gateway speaks the OpenAI chat wire at https://api.kilo.ai/api/gateway
;;;; with a bearer: the token a device sign-in mints (signin.lisp), or a key
;;;; from KILO_API_KEY. It routes some six hundred models under their vendors'
;;;; ids (anthropic/..., qwen/...). One thing sets it apart from a plain
;;;; OpenAI-compatible endpoint: omp speaks the Qwen thinking dialect to the
;;;; Qwen models it routes, an enable_thinking switch in place of
;;;; reasoning_effort.

(in-package #:nodecode-kilo)

(defparameter +base+ "https://api.kilo.ai/api/gateway"
  "Where Kilo Gateway is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("KILO_API_KEY")
  "The environment variables a Kilo key is read from, in order.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-kilo" "models.json")))
  "omp's bundled Kilo rows, read when this file loads: a vector of objects.")

(defparameter +rows+
  (let ((table (make-hash-table :test 'equal)))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") table) row))
    table)
  "+MODELS+ by id: a round looks its model up once, among six hundred.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (and (stringp model-id) (gethash model-id +rows+)))

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
       ;; CATALOG-PRICE's shape; a row priced at nothing is a free model's
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "Kilo Gateway as a models.dev provider: the chat lane's package, this
section's base, the key variable, and the bundled models over PRIOR's (the
row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Kilo Gateway"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- the Qwen thinking dialect --------------------------------------------------
;;; omp resolves a model's thinking format from its identity class: the Qwen
;;; class speaks `qwen' (a top-level enable_thinking, reasoning_effort never
;;; sent, since its qwenTemplateReasoningEffort is off), everything else Kilo
;;; routes speaks `openai' (reasoning_effort), which is the chat lane's own.

(defparameter +qwen-class+ '("prism-ml/ternary-bonsai-2-27b")
  "Bundled models omp's identity rules put in the Qwen class although their id
names neither qwen nor qwq.")

(defun qwen-dialect-p (model-id)
  "Whether omp speaks the Qwen thinking dialect to MODEL-ID on Kilo."
  (let ((id (string-downcase (or model-id ""))))
    (or (search "qwen" id)
        (search "qwq" id)
        (member id +qwen-class+ :test #'string=))))

(defun shape-thinking (body model effort)
  "BODY, the chat request for MODEL at EFFORT, in MODEL's thinking dialect."
  ;; Only a reasoning model is told anything, as in omp. An unset effort is
  ;; the provider's own default, as the chat lane leaves it; "off" turns the
  ;; switch off, any rung turns it on.
  (when (and effort
             (qwen-dialect-p model)
             (nlk:json-value (model-row model) :boolean "reasoning"))
    (remhash "reasoning_effort" body)
    (setf (gethash "enable_thinking" body) (if (string-equal effort "off") :false t)))
  body)
