;;;; provider.lisp --- what Pareto Inference is: its address, its key, its
;;;; models, and the reasoning_effort its models take.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of pareto-inference's hermes-pareto-provider (see NOTICE): its
;;;; default address, its key variable, its three models and the capabilities
;;;; it declared for them, its reasoning_effort ladder, and the bare-name to
;;;; `vendor/model' mapping it applies before a request leaves.
;;;;
;;;; Pareto speaks the OpenAI chat wire at https://api.paretoinference.com/v1
;;;; with a bearer key made at paretoinference.com, and bills per token. Three
;;;; things set it apart from a plain OpenAI-compatible endpoint, and they are
;;;; why this folder exists:
;;;;
;;;;   * every request must stream -- a plain call is answered HTTP 400
;;;;     `streaming_only'. Nodecode's chat lane only ever streams, so a turn
;;;;     is Pareto's own shape already;
;;;;   * the model is named `vendor/model' on the wire, and the picker's
;;;;     bare `glm-5.3-flash' is not that name;
;;;;   * reasoning rides in the top-level reasoning_effort field, whose
;;;;     levels run from `none' to `max' -- a rung Pareto does not list has
;;;;     to be left out, not sent.
;;;;
;;;; models.dev lists only some of the three under `pareto', so the rows here
;;;; are the original's declared capabilities; a price models.dev publishes
;;;; for one of the ids is kept.

(in-package #:nodecode-pareto)

(defparameter +base+ "https://api.paretoinference.com/v1"
  "Where Pareto Inference is served: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("PARETO_API_KEY")
  "The environment variable a Pareto key is read from.")

(defparameter +key-page+ "https://paretoinference.com/dashboard"
  "Where a key is made.")

(defparameter +docs-url+ "https://docs.paretoinference.com/pricing"
  "Pareto's own pricing page: what the picker's rows describe.")

(defparameter +models+ '("z-ai/glm-5.3-flash" "deepseek/deepseek-v4.1-flash"
                         "xiaomi/mimo-v2.6-flash")
  "The three models the original registers, as the public catalog names them:
GLM 5.3 Flash, DeepSeek V4.1 Flash and MiMo V2.6 Flash, pay per token.")

(defparameter +bare-ids+
  '(("glm-5.3-flash" . "z-ai/glm-5.3-flash")
    ("deepseek-v4.1-flash" . "deepseek/deepseek-v4.1-flash")
    ("mimo-v2.6-flash" . "xiaomi/mimo-v2.6-flash"))
  "A bare model name as the namespaced id the API requires, which is the
mapping the original's normalize-model-id makes.")

(defparameter +context-window+ 1000000
  "The context window the original declares for every one of the three
models, rather than leaving Nodecode's unknown-model default in its place.")

(defparameter +efforts+ '("none" "minimal" "low" "medium" "high" "xhigh" "max")
  "The reasoning_effort levels Pareto's API reference lists, weakest first.")

(defparameter +catalog-efforts+ '("off" "minimal" "low" "medium" "high" "xhigh"
                                  "max")
  "The same ladder as Nodecode spells it: the wire's `none' is the picker's
`off', and it is the only rung the two name differently.")

;;; --- the wire -------------------------------------------------------------------

(defun wire-model (model)
  "MODEL as the API wants it: a bare `glm-5.3-flash' becomes
`z-ai/glm-5.3-flash', and a namespaced id (or one this table does not name)
rides as it is."
  (let ((bare (string-downcase (string-trim " " (or model "")))))
    (or (cdr (assoc bare +bare-ids+ :test #'equal)) model)))

(defun wire-effort (effort)
  "EFFORT as Pareto's top-level reasoning_effort, or NIL to leave the field
out: Nodecode's `off' is the wire's `none', `ultra' its `max' rung, and a
level the API does not list is dropped so Pareto's per-model default stands."
  (let ((level (and (stringp effort) (string-downcase (string-trim " " effort)))))
    (cond ((null level) nil)
          ((string-equal level "off") "none")
          ((string-equal level "ultra") "max")
          (t (find level +efforts+ :test #'string-equal)))))

;;; --- the catalog row -------------------------------------------------------------

(defun catalog-model (model-id &optional prior)
  "MODEL-ID as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields):
the original's declared capabilities -- a 1M window, tool calls, reasoning at
every rung -- with PRIOR's price when models.dev published one."
  (nle::make-catalog-model model-id +context-window+ nil #("text") #("text")
                            (copy-list +catalog-efforts+) t nil t
                            (and prior (nle::catalog-model-price prior))))

(defun catalog-row (&optional prior)
  "Pareto Inference as a models.dev provider: the chat lane's package, this
section's base, the key variable, and the three models over PRIOR's (the row
models.dev itself published, when it did)."
  (let ((published (nlk:json-value prior :object "models"))
        (models (make-hash-table :test 'equal)))
    (loop for id in +models+
          do (setf (gethash id models)
                   (catalog-model id (and (hash-table-p published)
                                          (gethash id published)))))
    (nlk:json-object "name" "Pareto Inference"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "doc" +docs-url+
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
