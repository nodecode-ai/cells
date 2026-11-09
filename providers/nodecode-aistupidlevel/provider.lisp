;;;; provider.lisp --- what AI Stupid Level is: its address, its key, its modes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from StudioPlatforms/aistupidlevel-sdk's Hermes plugin (see
;;;; NOTICE): integrations/hermes/aistupidlevel/__init__.py, plugin.yaml and
;;;; its README.
;;;;
;;;; AI Stupid Level is a router, not a model host. It speaks the OpenAI chat
;;;; wire at https://aistupidlevel.info/v1, and the id a turn names is either
;;;; a routing mode -- `auto-tooling', `auto-coding', and so on -- or a model
;;;; pinned by name, `claude-opus-5-5'. A routing mode sends each request to
;;;; the model scoring best right now on the benchmark it names, called with
;;;; the provider keys saved on the account; a pinned model always asks that
;;;; model. The live catalog is the authenticated GET /v1/models, which
;;;; Nodecode's own listing reads for any provider with a credential, so this
;;;; cell carries no lister of its own: the modes below stand in until that
;;;; listing answers.
;;;;
;;;; The router takes text and tool calls only: it refuses image input.

(in-package #:nodecode-aistupidlevel)

(defparameter +base+ "https://aistupidlevel.info/v1"
  "Where AI Stupid Level is served: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("ASL_ROUTER_KEY")
  "The environment variables a router key is read from, in order.")

(defparameter +key-page+ "https://aistupidlevel.info/router/keys"
  "Where a router key is made; the account's provider keys are saved beside it.")

(defparameter +models+
  '(("auto-tooling" . "Auto (tooling): best right now on the tool-use benchmark")
    ("auto-coding" . "Auto (coding): best right now on the coding benchmark")
    ("auto-reasoning" . "Auto (reasoning): best on the multi-turn reasoning benchmark")
    ("auto-best" . "Auto (best): best combined score")
    ("auto-task" . "Auto (task): one of the above, chosen from the request itself")
    ("auto-value" . "Auto (value): best score per dollar among near-top models")
    ("auto-fastest-quality" . "Auto (fastest quality): a fast near-top model"))
  "The routing modes the original's profile lists as its fallback, the aux
model it names for titles and compression, and what each routes on:
(ID . NAME). The live catalog replaces them when it answers.")

;;; --- the models ----------------------------------------------------------------

(defun row-model (name)
  "NAME as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields).
A routing mode names no model, so it publishes no context window, no price
and no effort ladder: the model it routes to is decided per request."
  (nle::make-catalog-model name nil nil #("text") #("text") nil nil nil t nil))

(defun catalog-row (&optional prior)
  "AI Stupid Level as a models.dev provider: its name, the chat lane's
package, this section's base, the key variable, and the routing modes over
PRIOR's models (when models.dev published a row of its own)."
  (let ((models (make-hash-table :test 'equal)))
    (when (hash-table-p prior)
      (let ((published (nlk:json-value prior :object "models")))
        (when (hash-table-p published)
          (maphash (lambda (id model) (setf (gethash id models) model)) published))))
    (dolist (row +models+)
      (setf (gethash (car row) models) (row-model (cdr row))))
    (nlk:json-object "name" "AI Stupid Level"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

;;; --- the key -------------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
