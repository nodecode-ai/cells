;;;; provider.lisp --- what Impossibl is: its address, its key, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of impossiblco/hermes-impossibl's __init__.py (see NOTICE): the
;;;; provider id, the base and its key variable, the rule that keeps only
;;;; chat models an agent turn can drive, and the fallback roster the plugin
;;;; showed when its live catalogue could not be read.
;;;;
;;;; Impossibl fronts 100+ models from many labs behind one OpenAI-compatible
;;;; gateway at https://api.impossibl.com/v1 with a bearer key. Its /v1/models
;;;; answer is no plain listing: every entry names the request surfaces it
;;;; serves in `endpoints', and whether it takes tool calls in
;;;; `supports_tools'. The plugin kept the entries serving
;;;; /v1/chat/completions that do not declare supports_tools false, because an
;;;; agent turn always offers tools. The same reading lives in cell.lisp's
;;;; LIST-PROVIDER-MODELS advice; the facts it reads are here.

(in-package #:nodecode-impossibl)

(defparameter +base+ "https://api.impossibl.com/v1"
  "Where Impossibl is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("IMPOSSIBL_API_KEY")
  "The environment variable an Impossibl key is read from.")

(defparameter +key-page+ "https://impossibl.com"
  "Where a key is made: the plugin's signup_url.")

(defparameter +chat-endpoint+ "/v1/chat/completions"
  "The endpoint a catalogue entry lists in `endpoints' when a chat turn can
drive it: the plugin's CHAT_ENDPOINT.")

(defparameter +fallback-models+
  '("anthropic/claude-sonnet-5"
    "anthropic/claude-opus-5-5"
    "openai/gpt-5.5"
    "openai/gpt-5.4-mini"
    "google/gemini-3.8-flash"
    "google/gemini-3.1-flash-lite"
    "xai/grok-4.7"
    "deepseek/deepseek-v4-pro"
    "moonshotai/kimi-k3"
    "zai/glm-5.3"
    "minimaxai/minimax-m3")
  "The plugin's fallback_models, as they are: flagships across the labs
Impossibl fronts, on offer while the live catalogue is unreachable.")

(defun fallback-model (id)
  "ID as the catalogue keeps a model (NLE::MAKE-CATALOG-MODEL's fields): a
tool-calling chat model, images in because the gateway relays multimodal
content, and no price, which Impossibl bills at its own rate."
  (nle::make-catalog-model id nil nil #("text" "image") #("text") nil nil nil t nil))

(defun catalog-row (&optional prior)
  "Impossibl as a models.dev provider: the chat lane's package, this
section's base, the key variable, and the fallback roster over PRIOR's models
(the row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (dolist (id +fallback-models+)
      (setf (gethash id models) (fallback-model id)))
    (nlk:json-object "name" "Impossibl"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The key IMPOSSIBL_API_KEY holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for impossibl: IMPOSSIBL_API_KEY, else none. A key
/connect saved in auth.json answers before this point does."
  ;; Never NEXT for impossibl: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to Impossibl.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))
