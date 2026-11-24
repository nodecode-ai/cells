;;;; provider.lisp --- what Experiential Labs is: its address, its key, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of kyoto-aircon's hermes-experientiallabs (see NOTICE): its
;;;; provider profile (__init__.py) names one OpenAI-compatible gateway at
;;;; https://api.experientiallabs.ai/v1, which lists ~900 models at GET
;;;; /v1/models, and a curated fallback list the /model picker offers while
;;;; that listing has not been read. Here that fallback list is the row: the
;;;; picker never dials the gateway to draw itself.

(in-package #:nodecode-experientiallabs)

(defparameter +base+ "https://api.experientiallabs.ai/v1"
  "Where the gateway is served: the base the chat lane appends /chat/completions to.")

(defparameter +cost-base+ "https://api.experientiallabs.ai/api/v1"
  "Where the Cost API is served: the base /explabs reads GET /credits and
GET /key from (plugin.yaml's cost_api_base default).")

(defparameter +env+ '("EXPLABS_API_KEY" "EXPERIENTIALLABS_API_KEY")
  "The environment variables an Experiential Labs key is read from, in the
order the original declares them (__init__.py:117): the first is primary.")

(defparameter +key-page+ "https://platform.experientiallabs.ai/settings/api-keys"
  "Where a key is made (the profile's signup_url).")

(defparameter +fallback-models+
  '("qwen3.8-27b" "deepseek-v4-flash" "gpt-6-luna" "claude-haiku-4.5" "gpt-6-astra"
    ;; the auxiliary model (__init__.py:40): titles, compression, vision
    "glm-5.3-flash")
  "The curated fallback the picker offers: every id the original's
_FALLBACK_MODELS names, then its _AUX_MODEL. All are tool-calling capable.")

(defun catalog-model (id)
  "ID as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields). The
gateway publishes no context or price here, so neither is claimed; a text-in,
text-out, tool-calling turn is."
  (nle::make-catalog-model id nil nil #("text") #("text") nil nil nil t nil))

(defun catalog-row (&optional prior)
  "Experiential Labs as a models.dev provider: the chat lane's package, this
section's base, the key variables, and the fallback models over PRIOR's (the
row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for id in +fallback-models+
          do (setf (gethash id models) (catalog-model id)))
    (nlk:json-object "name" "Experiential Labs"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +ENV+))
