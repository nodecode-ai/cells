;;;; provider.lisp --- what Abstract is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from abstractpbc's hermes-abstract-provider (see NOTICE):
;;;; __init__.py's one ProviderProfile -- the base URL, the key variable, the
;;;; three model ids, the 65,536-token output cap and the static catalog --
;;;; and the README's price table.
;;;;
;;;; Abstract serves the OpenAI Responses API at https://api.abstract.inc/v1
;;;; with a bearer key, so this cell rides the core's openai-responses lane
;;;; through its hooks rather than carrying a lane of its own: the catalog
;;;; row's package names that lane, and the base plus "/responses" is the
;;;; round's address.
;;;;
;;;; The plugin publishes no context window, and it disables live model
;;;; listing because Abstract's /models also names background lanes it does
;;;; not offer, so the roster below is the three ids the plugin names and each
;;;; window is unknown: the core learns a real window from the provider's own
;;;; overflow reply instead of a number invented here.

(in-package #:nodecode-abstract)

(defparameter +base+ "https://api.abstract.inc/v1"
  "Where Abstract is served: the base the Responses lane appends /responses to.")

(defparameter +env+ '("ABSTRACT_API_KEY")
  "The environment variables an Abstract key is read from, in order.")

(defparameter +key-page+ "https://platform.abstract.inc/"
  "Where a key is made: the platform's own page.")

(defparameter +output-cap+ 65536
  "The output cap the API documents for one response (default_max_tokens).")

(defparameter +models+
  (list (cons "glm-5.3:superfast"
              (nle::make-catalog-model "GLM-5.3" nil +output-cap+
                                        #("text") #("text")
                                        '() nil nil t
                                        '(1.19 4.40 0.26 nil)))
        (cons "deepseek-v4-flash:superfast"
              (nle::make-catalog-model "DeepSeek-V4-Flash" nil +output-cap+
                                        #("text") #("text")
                                        '() nil nil t
                                        '(0.10 0.25 0.05 nil)))
        (cons "kimi-k3:superfast"
              (nle::make-catalog-model "Kimi-K3" nil +output-cap+
                                        #("text") #("text")
                                        '() nil nil t
                                        '(3.00 12.75 0.30 nil))))
  "The roster: the three ids the plugin names, as (ID . CATALOG-MODEL).
Text in, text out, no vision, tools taken; a price is (INPUT OUTPUT
CACHE-READ CACHE-WRITE) in US dollars per million tokens, and a NIL context
window is unknown on purpose.")

(defparameter +aux-model+ "deepseek-v4-flash:superfast"
  "The model the plugin named for compression and titles. Nodecode picks its
own auxiliary model, so this is what to choose by hand.")

(defun base-url ()
  "The Responses base a round is sent to: this section's, without a trailing
slash."
  (string-right-trim "/" (setting :base-url)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun key-present-p (key)
  "Whether KEY is one a person gave, rather than the keyless placeholder."
  (and (stringp key) (plusp (length key)) (not (equal key "public"))))

(defun require-key (key)
  "KEY, or a refusal naming what to set: a round without one would only earn
a 401 from the far end."
  (unless (key-present-p key)
    (error 'nle::provider-config-error
           :detail "Abstract API key is required. Save one with /connect or set ABSTRACT_API_KEY."))
  key)

(defun catalog-row (&optional prior)
  "Abstract as a models.dev provider: the Responses lane's package, this
section's base, the key variable, and the three published models over
PRIOR's (the row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for (id . model) in +models+
          do (setf (gethash id models) model))
    (nlk:json-object "name" "Abstract"
                     "npm" "@ai-sdk/openai"
                     "api" (base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun listing-rows ()
  "The roster as a provider listing answers it: the three static ids, so the
picker never offers the background lanes Abstract's own /models also names."
  (loop for (id . model) in +models+
        collect (list :id id
                      :display (nle::catalog-model-name model)
                      :context-window (nle::catalog-model-context model))))
