;;;; provider.lisp --- what Opper is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from opper-ai's hermes-opper (see NOTICE): the one
;;;; ProviderProfile in __init__.py -- the base URL, the OPPER_API_KEY
;;;; variable and its OPPER_BASE_URL override, the fallback roster, the
;;;; auxiliary model -- and what the README says the gateway serves.
;;;;
;;;; Opper serves the OpenAI chat-completions wire at
;;;; https://api.opper.ai/v3/compat with a bearer key, so this cell rides the
;;;; core's openai-completions lane through its hooks rather than carrying a
;;;; lane of its own: the catalog row's package names that lane, and the base
;;;; plus "/chat/completions" is the round's address.
;;;;
;;;; Model ids are bare pool names ("claude-sonnet-4-6"); a vendor prefix
;;;; pins one route ("anthropic/claude-sonnet-4-6") and is sent as it is.
;;;; The gateway publishes its own catalogue at <base>/models, which is the
;;;; address the core's listing asks first, so nothing here overrides the
;;;; listing: the roster below is what the picker shows when that request
;;;; cannot be read, the role __init__.py's fallback_models plays.

(in-package #:nodecode-opper)

(defparameter +base+ "https://api.opper.ai/v3/compat"
  "Where the gateway is served: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("OPPER_API_KEY")
  "The environment variables an Opper key is read from, in order.")

(defparameter +base-env+ "OPPER_BASE_URL"
  "The second variable the plugin's env_vars names: a base set there stands
in for the section's, which is how it overrode the profile's own.")

(defparameter +key-page+ "https://platform.opper.ai"
  "Where a key is made: the page the profile's signup_url names.")

(defparameter +aux-model+ "gpt-5.4-mini"
  "The model the profile named default_aux_model for compression, titles and
vision. Nodecode picks its own auxiliary model, so this is the one to choose
by hand.")

(defun pool (id display)
  "The catalog row for pool ID under DISPLAY: text in, text out, tools taken.
Nothing the plugin does not publish is invented here: a NIL window is learned
from the gateway's own overflow reply, and a NIL price is simply unsaid."
  (cons id (nle::make-catalog-model display nil nil #("text") #("text")
                                     '() nil nil t nil)))

(defparameter +roster+
  (let ((rows '()))
    (dolist (pair '(("claude-sonnet-4-6"  . "Claude Sonnet 4.6")
                     ("claude-opus-5"      . "Claude Opus 5")
                     ("gpt-5.5"            . "GPT-5.5")
                     ("gpt-5.4-mini"       . "GPT-5.4 mini")
                     ("gemini-3.8-flash"   . "Gemini 3.8 Flash")
                     ("deepseek-v4-pro"    . "DeepSeek V4 Pro")
                     ("kimi-k3"            . "Kimi K3")
                     ("mistral-large-2512" . "Mistral Large 2512")))
      (push (pool (car pair) (cdr pair)) rows))
    (nreverse rows))
  "The fallback_models the plugin ships, in its order: the pool names the
picker offers when <base>/models cannot be read. Bare pools only -- a vendor
prefix pins one route and is typed, not listed.")

(defun env-base ()
  "OPPER_BASE_URL, without a trailing slash, or NIL when it is unset or
blank."
  (let* ((value (nle::credential-env +base-env+))
         (trimmed (and value (string-right-trim "/" value))))
    (and trimmed (plusp (length trimmed)) trimmed)))

(defun read-settings (values table)
  "The settings the cell runs on: the base the declaration derived, unless
OPPER_BASE_URL names one, as the profile's env_vars list allows."
  (declare (ignore table))
  (list :base-url (or (env-base) (getf values :base-url) +base+)))

(defun base-url ()
  "The base a round is sent to: this section's, without a trailing slash."
  (string-right-trim "/" (setting :base-url)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun key-present-p (key)
  "Whether KEY is one a person gave, rather than the keyless placeholder."
  (and (stringp key) (plusp (length key)) (not (equal key "public"))))

(defun require-key (key)
  "KEY, or a refusal naming what to set: a round without one would only earn
a 401 from the gateway."
  (unless (key-present-p key)
    (error 'nle::provider-config-error
           :detail "Opper API key is required. Save one with /connect or set OPPER_API_KEY."))
  key)

(defun catalog-row (&optional prior)
  "Opper as a models.dev provider: the chat lane's package, this section's
base, the key variable, and the roster laid over PRIOR's models (a row the
catalogue published for an id the roster does not name survives), so /connect
offers it, /models lists its models and a turn resolves its lane and address
the way it does any catalog provider's."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for (id . model) in +roster+
          do (setf (gethash id models) model))
    (nlk:json-object "name" "Opper"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))