;;;; provider.lisp --- what Scaleway is: its address, its key, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from the Hermes plugin scaleway-provider (see NOTICE):
;;;; __init__.py's ProviderProfile (the base, the alias, the ambient
;;;; SCW_SECRET_KEY, the curated tool-calling list and the auxiliary model)
;;;; and the recorded models.dev row it keys through models_dev_id, which
;;;; models.json in this folder carries for the curated ids.
;;;;
;;;; Scaleway Generative APIs speak the OpenAI chat wire at
;;;; https://api.scaleway.ai/v1 with a bearer secret key from a Scaleway IAM
;;;; API key -- the same SCW_SECRET_KEY the Scaleway CLI and SDKs export.
;;;; Two things set the provider apart from a plain OpenAI-compatible
;;;; endpoint, and both are metadata: the real context window and price of
;;;; every model (a generic endpoint assumes one window for all of them, so
;;;; compression runs too late on the small ones), and a curated list of the
;;;; serverless models that drive an agent over chat-completions --
;;;; gpt-oss-120b is left out because Scaleway serves its tool-calling on the
;;;; beta Responses API only.

(in-package #:nodecode-scaleway)

(defparameter +base+ "https://api.scaleway.ai/v1"
  "Where Scaleway Generative APIs are served: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("SCW_SECRET_KEY" "SCALEWAY_API_KEY")
  "The environment variables a Scaleway key is read from, in order: the
plugin's ambient SCW_SECRET_KEY, then the variable models.dev names.")

(defparameter +key-page+ "https://console.scaleway.com/iam/api-keys"
  "Where an IAM API key is made; its secret is the SCW_SECRET_KEY.")

(defparameter +docs+ "https://www.scaleway.com/en/docs/generative-apis/"
  "Scaleway's own documentation for Generative APIs.")

(defparameter +default-model+ "deepseek-v4.1-flash-1017"
  "The model the plugin's setup picks first: a fast reasoning model that
tool-calls on the chat wire.")

(defparameter +aux-model+ "mistral-small-3.2-24b-instruct-2506"
  "The plugin's auxiliary model: the cheapest tool-calling vision-capable
one, for compression and summaries.")

(defun trimmed (value)
  "VALUE without surrounding whitespace, or NIL when it is blank."
  (let ((text (and (stringp value) (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (and text (plusp (length text)) text)))

(defun env-base ()
  "The base SCALEWAY_BASE_URL names, as the plugin's base_url_env_var does,
or NIL."
  (trimmed (uiop:getenv "SCALEWAY_BASE_URL")))

(defun base-url ()
  "The base this cell reaches the API at: the section's, which a relay may
set, and where the section still says Scaleway's own, SCALEWAY_BASE_URL when
it names one -- the plugin's rule, where the variable overrides the default
endpoint and nothing else."
  (let ((set (setting :base-url)))
    (if (equal set +base+) (or (env-base) set) set)))

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-scaleway" "models.json")))
  "The curated tool-calling rows, read when this file loads: a vector of
objects. The plugin carries the same list as fallback_models; the numbers
are the models.dev row it keys, recorded.")

(defun model-ids ()
  "The ids the curated list offers, in its order: the setup default first."
  (loop for row across +models+ collect (nlk:json-value row :string "id")))

(defun model-row (model-id)
  "The curated row of MODEL-ID, or NIL."
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
       ;; every curated row is a serverless model that tool-calls on the chat wire
       t
       ;; CATALOG-PRICE's shape: US dollars per million tokens
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "Scaleway as a models.dev provider: the chat lane's package, this
section's base, the key variables, and the curated rows -- each taking
PRIOR's (the row models.dev itself published, when it did) numbers, which
are the live ones, over the recorded row this folder carries."
  (let ((published (nlk:json-value prior :object "models"))
        (models (make-hash-table :test 'equal)))
    (dolist (id (model-ids))
      (setf (gethash id models)
            (or (and published (gethash id published))
                (catalog-model (model-row id)))))
    (nlk:json-object "name" "Scaleway"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base-url)
                     "doc" +docs+
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
