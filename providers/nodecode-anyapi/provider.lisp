;;;; provider.lisp --- what AnyAPI.ai is: its address, its key, its routes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of es697/hermes-provider-anyapi's anyapi/__init__.py (see
;;;; NOTICE): the provider id, the base and its key variable, the tags that
;;;; mark a route tool-capable, and the fallback roster it showed when its
;;;; catalog could not be read.
;;;;
;;;; AnyAPI fronts many vendors' models behind one OpenAI-compatible gateway
;;;; at https://api.anyapi.ai/v1 with a bearer key. Its /v1/models answer is
;;;; no plain listing: every route carries the request surfaces it accepts in
;;;; `tags', and the plugin kept only the routes that advertise tool calling,
;;;; because an agent turn always offers tools. The same reading lives in
;;;; cell.lisp's LIST-PROVIDER-MODELS advice; the facts it reads are here.

(in-package #:nodecode-anyapi)

(defparameter +base+ "https://api.anyapi.ai/v1"
  "Where AnyAPI is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("ANYAPI_API_KEY")
  "The environment variable an AnyAPI key is read from.")

(defparameter +key-page+ "https://anyapi.ai"
  "Where a key is made: the plugin's signup_url.")

(defparameter +tool-tags+ '("function_calling" "chat_completions:function_calling")
  "The tags an AnyAPI route carries when it accepts tool calls: the plugin's
_TOOL_TAGS, as they are.")

(defparameter +fallback-models+
  '("anthropic/claude-sonnet-4.6"
    "anthropic/claude-opus-4.8"
    "openai/gpt-5.4"
    "google/gemini-3-flash-preview"
    "deepseek/deepseek-v4.1-flash"
    "qwen/qwen3.8-max")
  "The plugin's fallback_models: flagships across the vendors AnyAPI fronts,
on offer while the live catalog is unreachable.")

(defun fallback-model (id)
  "ID as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields): a
tool-capable chat route, images in because the gateway relays multimodal
content, and no price, which AnyAPI bills at its own rate."
  (nle::make-catalog-model id nil nil #("text" "image") #("text") nil nil nil t nil))

(defun catalog-row (&optional prior)
  "AnyAPI as a models.dev provider: the chat lane's package, this section's
base, the key variable, and the fallback roster over PRIOR's models (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (dolist (id +fallback-models+)
      (setf (gethash id models) (fallback-model id)))
    (nlk:json-object "name" "AnyAPI.ai"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The key ANYAPI_API_KEY holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for anyapi: ANYAPI_API_KEY, else none. A key
/connect saved in auth.json answers before this point does."
  ;; Never NEXT for anyapi: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to AnyAPI.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))
