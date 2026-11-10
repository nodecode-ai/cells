;;;; provider.lisp --- what Otari is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of mozilla-ai's hermes-provider-otari (see NOTICE): otari/
;;;; __init__.py registers one provider whose base is the hosted gateway and
;;;; whose key is OTARI_API_KEY, and its README names the EU host and a
;;;; self-hosted one as the alternatives.
;;;;
;;;; Otari is an OpenAI-compatible LLM gateway: one key reaches every model
;;;; the gateway routes, with its budgets, routing and usage tracking applied
;;;; to each call. The chat lane appends /chat/completions to the base below,
;;;; so a turn needs nothing but the address and the key. Which models exist
;;;; is the gateway's own answer, GET <base>/models, which is exactly where
;;;; the core's listing looks: the cell carries no model rows of its own.
;;;;
;;;; One wire quirk: Otari accepts reasoning_effort minimal, low, medium,
;;;; high and xhigh, and refuses `none'. The core already leaves the field
;;;; out at effort `off'; the cell spells `max' the way Otari knows it (xhigh)
;;;; and drops any rung the gateway does not name, as the plugin does.

(in-package #:nodecode-otari)

(defparameter +base+ "https://api.otari.ai/api/v1"
  "The hosted gateway: the base the chat lane appends /chat/completions to.
The EU region answers at https://eu.api.otari.ai/api/v1 and a gateway of
one's own at its own address plus /api/v1, both through the section.")

(defparameter +env+ '("OTARI_API_KEY")
  "The environment variables an Otari key is read from, in order.")

(defparameter +key-page+ "https://otari.ai"
  "Where a key is made (the plugin's signup_url).")

(defparameter +wire-efforts+ '("minimal" "low" "medium" "high" "xhigh")
  "The reasoning_effort values Otari accepts (the plugin's _WIRE_EFFORTS).")

(defparameter +effort-aliases+ '(("max" . "xhigh"))
  "The ladder rung Otari spells otherwise (the plugin's _EFFORT_ALIASES).")

(defparameter +npm+ "@ai-sdk/openai-compatible"
  "The package of the lane that drives Otari: the chat-completions wire.")

(defun wire-effort (effort)
  "The reasoning_effort value to send for EFFORT, or NIL to leave the
route's own default, which is how Otari hears `reasoning off'."
  (let* ((spelled (and (stringp effort) (string-downcase (nlk:trimmed effort))))
         (mapped (or (cdr (assoc spelled +effort-aliases+ :test #'equal)) spelled)))
    (and (member mapped +wire-efforts+ :test #'equal) mapped)))

;;; --- the catalog ----------------------------------------------------------------

(defun catalog-row (&optional prior)
  "Otari as a models.dev provider: the chat lane's package, this section's
base, the key variable, and whatever models PRIOR (the row models.dev itself
published, when it did) carried; the gateway's live listing fills the rest."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (nlk:json-object "name" "Otari"
                     "npm" +npm+
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
