;;;; provider.lisp --- what Cloudflare Workers AI is: its address, its key, its models, its effort.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the model table, the reasoning rule and the free-plan
;;;; reading of AhmetArif0's Hermes plugin (see NOTICE). Cloudflare serves
;;;; Workers AI's models over an OpenAI-compatible chat endpoint under the
;;;; account, https://api.cloudflare.com/client/v4/accounts/<account>/ai/v1,
;;;; with a Workers AI API token as its bearer. Its OpenAI-compatible /models
;;;; answers HTTP 405, so the listing is built into this file: the models the
;;;; Hermes plugin kept on 2026-09-29 -- function calling and at least 64K of
;;;; context -- with Cloudflare's own context windows, vision flags and the
;;;; reasoning_effort values each takes.
;;;;
;;;; Reasoning effort. Cloudflare refuses a rung a model does not take (qwen3.8
;;;; answers 400 to `high'), so a pick is mapped the way the plugin maps it: a
;;;; level the model takes goes as it is, one it does not becomes the nearest
;;;; weaker level it does, a level below its lowest becomes that lowest, and
;;;; turning reasoning off sends `none' where the model takes it and its lowest
;;;; level otherwise (gpt-oss always reasons; glm-5.3 turns `none' into max).
;;;; A model whose catalog entry lists no levels -- it reasons without being
;;;; asked -- gets no reasoning_effort field at all, and neither does a model
;;;; that does not reason.

(in-package #:nodecode-cloudflare-workers-ai)

(defparameter +base-template+
  "https://api.cloudflare.com/client/v4/accounts/${CLOUDFLARE_ACCOUNT_ID}/ai/v1"
  "Workers AI's OpenAI-compatible base, with the account as the ${NAME} blank
the core fills from the environment when the section names no account.")

(defparameter +account-env+ "CLOUDFLARE_ACCOUNT_ID"
  "Where the account id is read from when the section names none.")

(defparameter +base-env+ "CLOUDFLARE_WORKERS_AI_BASE_URL"
  "The variable the Hermes plugin read its base URL from; honoured here too.")

(defparameter +env+ '("CLOUDFLARE_WORKERS_AI_API_TOKEN")
  "The environment variables a Workers AI token is read from, in order. The
plugin's own name, deliberately not Cloudflare's CLOUDFLARE_API_TOKEN: a
Pages or DNS token in that one must never ride a Workers AI turn.")

(defparameter +token-page+
  "https://dash.cloudflare.com/?to=/:account/ai/workers-ai"
  "Where the dashboard shows the account id and makes a Workers AI token.")

(defparameter +free-plan-refusal+ "is not available on the workers free plan"
  "Cloudflare's HTTP 403 words when a Workers Free account calls a paid-plan
model; the README says so, and the paid models' names carry `(paid plan)'.")

;;; --- the models ----------------------------------------------------------------

(defstruct (workers-model (:constructor make-workers-model (id label context vision reasoning efforts paid)))
  "One Workers AI model as Cloudflare's catalog lists it: its id and label,
its context window, whether it takes images, whether it reasons, the
reasoning_effort values it accepts (NIL for a model that takes no field),
and whether only the Workers Paid plan serves it."
  (id "" :read-only t)
  (label "" :read-only t)
  (context 0 :read-only t)
  (vision nil :read-only t)
  (reasoning nil :read-only t)
  (efforts nil :read-only t)
  (paid nil :read-only t))

(defparameter +models+
  (list
   ;; free-plan models first, as the plugin lists them
   (make-workers-model "@cf/openai/gpt-oss-120b" "GPT-OSS 120B" 128000 nil t '("low" "medium" "high") nil)
   (make-workers-model "@cf/openai/gpt-oss-20b" "GPT-OSS 20B" 128000 nil t '("low" "medium" "high") nil)
   (make-workers-model "@cf/qwen/qwen3.8-27b" "Qwen3.8 27B" 262144 t t '("low" "medium" "xhigh") nil)
   (make-workers-model "@cf/zai-org/glm-4.7-flash" "GLM 4.7 Flash" 131072 nil t nil nil)
   (make-workers-model "@cf/google/gemma-4-26b-a4b-it" "Gemma 4 26B A4B" 256000 t t nil nil)
   (make-workers-model "@cf/nvidia/nemotron-3-120b-a12b" "Nemotron 3 120B A12B" 256000 nil t nil nil)
   (make-workers-model "@cf/meta/llama-4-scout-17b-16e-instruct" "Llama 4 Scout 17B 16E" 131000 t nil nil nil)
   (make-workers-model "@cf/mistralai/mistral-small-3.1-24b-instruct" "Mistral Small 3.1 24B" 128000 nil nil nil nil)
   (make-workers-model "@cf/ibm-granite/granite-4.0-h-micro" "Granite 4.0 H Micro" 131000 nil nil nil nil)
   ;; the Workers Paid plan's
   (make-workers-model "@cf/deepseek-ai/deepseek-v4-flash-0731" "DeepSeek V4 Flash" 1310720 nil t '("none" "low" "high" "max") t)
   (make-workers-model "@cf/deepseek-ai/deepseek-v4-pro-0813" "DeepSeek V4 Pro" 1048576 nil t '("none" "low" "high" "max") t)
   (make-workers-model "@cf/moonshotai/kimi-k2.6" "Kimi K2.6" 262144 t t '("none" "high") t)
   (make-workers-model "@cf/moonshotai/kimi-k2.7-code" "Kimi K2.7 Code" 262144 t t nil t)
   (make-workers-model "@cf/zai-org/glm-5.2" "GLM 5.2" 262144 nil t '("none" "high" "max") t)
   (make-workers-model "@cf/zai-org/glm-5.3" "GLM 5.3" 1310720 nil t '("low" "high" "max") t)
   (make-workers-model "@cf/zai-org/glm-5.3-flash" "GLM 5.3 Flash" 1310720 t t '("low" "high" "max") t))
  "Cloudflare's text-generation catalog on 2026-09-29, limited to the models
with function calling and at least 64K of context: the plugin's own table.")

(defun model (model-id)
  "The row of MODEL-ID, or NIL."
  (find model-id +models+ :key #'workers-model-id :test #'equal))

(defun model-label (row)
  "ROW's label, naming the plan that serves it."
  (if (workers-model-paid row)
      (format nil "~a (paid plan)" (workers-model-label row))
      (workers-model-label row)))

;;; --- the address and the key ----------------------------------------------------

(defun section-string (key)
  "The section's KEY as a trimmed string, or NIL when it is unset or blank."
  (let ((value (setting key)))
    (and (stringp value)
         (plusp (length (string-trim " " value)))
         (string-trim " " value))))

(defun api-base ()
  "Workers AI's base: the section's base_url, else the plugin's own variable,
else the account the section or CLOUDFLARE_ACCOUNT_ID names, else the
template whose blank the core fills from that variable on use."
  (or (section-string :base-url)
      (nle::credential-env +base-env+)
      (let ((account (or (section-string :account-id) (nle::credential-env +account-env+))))
        (and account
             (format nil "https://api.cloudflare.com/client/v4/accounts/~a/ai/v1" account)))
      +base-template+))

(defun env-key ()
  "The first token one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- the catalog ------------------------------------------------------------------

(defun catalog-efforts (row)
  "The enabled rungs ROW takes, weakest first: `none' is the core's `off',
which the effort ladder offers once by itself."
  (sort (remove-if (lambda (level) (string-equal level "none"))
                    (copy-list (workers-model-efforts row)))
        #'< :key #'nle::effort-rank))

(defun catalog-model (row)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (nle::make-catalog-model
   (model-label row)
   (workers-model-context row)
   nil                                  ; no published output ceiling
   (if (workers-model-vision row) #("text" "image") #("text"))
   #("text")
   (catalog-efforts row)
   (workers-model-reasoning row)
   nil
   t
   nil))

(defun catalog-row (&optional prior)
  "Cloudflare Workers AI as a models.dev provider: the chat lane's package,
this section's base, the token variable, and the built-in models over
PRIOR's (the row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (dolist (row +models+)
      (setf (gethash (workers-model-id row) models) (catalog-model row)))
    (nlk:json-object "name" "Cloudflare Workers AI"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (api-base)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun listing-rows ()
  "The roster as a provider listing answers it: Workers AI's OpenAI-compatible
/models answers HTTP 405, so the picker is answered from the roster itself."
  (loop for row in +models+
        collect (list :id (workers-model-id row)
                      :display (model-label row)
                      :context-window (workers-model-context row))))

;;; --- reasoning effort ---------------------------------------------------------------

(defun lowest-effort (accepted)
  "ACCEPTED's weakest enabled rung: its `low' where it has one, else what it has."
  (or (first (sort (remove-if (lambda (level) (string-equal level "none"))
                              (copy-list accepted))
                   #'< :key #'nle::effort-rank))
      (first accepted)))

(defun wire-effort (accepted effort)
  "The reasoning_effort value to send for a model that ACCEPTS these rungs at
EFFORT, the pick, or NIL to leave the field out."
  ;; The plugin's rule, in the core's vocabulary (`off' for its `none'): a rung
  ;; the model takes goes as it is, one it does not becomes the nearest weaker
  ;; rung it does, one below its lowest becomes that lowest, and off is `none'
  ;; where it is taken and the lowest rung otherwise.
  (let ((level (string-downcase (string-trim " " (or effort "")))))
    (cond
      ((zerop (length level)) nil)     ; no pick: the provider's own default
      ((member level accepted :test #'string-equal) level)
      ((or (string-equal level "off") (string-equal level "none"))
       (or (find "none" accepted :test #'string-equal) (lowest-effort accepted)))
      ((nle::effort-rank level)
       (let ((rank (nle::effort-rank level)))
         (or (first (sort (remove-if-not (lambda (rung)
                                            (and (not (string-equal rung "none"))
                                                 (< (nle::effort-rank rung) rank)))
                                          (copy-list accepted))
                          #'> :key #'nle::effort-rank))
             (lowest-effort accepted))))
      (t nil))))                        ; a rung this vocabulary does not know

(defun shape-effort (body model-id effort)
  "BODY, the chat request for MODEL-ID at EFFORT, carrying the one
reasoning_effort value that model takes."
  (let ((row (model model-id)))
    (when row
      (remhash "reasoning_effort" body)
      (let ((value (and (workers-model-efforts row)
                        (wire-effort (workers-model-efforts row) effort))))
        (when value
          (setf (gethash "reasoning_effort" body) value)))))
  body)
