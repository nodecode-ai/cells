;;;; provider.lisp --- what xAI Grok OAuth is: its address, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; providers/xai-oauth.kdl (the base, the token variable, the Grok
;;;; reasoning rules, the conversation header, the tool-schema rule),
;;;; packages/ai/src/providers/openai-shared.ts and openai-responses.ts (how
;;;; those rules reach a Responses request), packages/ai/src/utils/schema/
;;;; wire.ts (flattenExclusiveRequiredRootUnion), and the bundled rows of
;;;; catalog/src/models.json, which models.json in this folder carries
;;;; (tools/omp-models.py wrote it).
;;;;
;;;; A SuperGrok or X Premium+ subscription signs in to xAI with a device
;;;; code (signin.lisp) and spends the access token as a bearer on the paid
;;;; Responses API at https://api.x.ai/v1, the same wire an xAI API key
;;;; rides. What sets the Grok dialect apart from OpenAI's:
;;;;
;;;;   - reasoning.effort is refused (HTTP 400) by the models that reason on
;;;;     their own (grok-build, the -reasoning SKUs), so it is left out there;
;;;;     elsewhere minimal is low, and xhigh and max are high except on the
;;;;     SKUs that take xhigh
;;;;   - reasoning.summary is refused, so it is never sent
;;;;   - encrypted reasoning is asked for on every reasoning model, so the
;;;;     next round can replay it
;;;;   - a tool schema whose root is a union of bare required-key fragments
;;;;     is refused, so that union is dropped (its object stays)
;;;;   - the conversation rides the x-grok-conv-id header, which is what
;;;;     xAI's prompt cache routes on

(in-package #:nodecode-xai-oauth)

(defparameter +base+ "https://api.x.ai/v1"
  "Where xAI is served: the base the Responses lane appends /responses to.")

(defparameter +env+ '("XAI_OAUTH_TOKEN")
  "The environment variables an xAI OAuth access token is read from, in order.
omp's dedicated mode: only the provider's own variable counts, not XAI_API_KEY.")

(defparameter +session-header+ "x-grok-conv-id"
  "The header xAI's prompt cache routes a conversation by.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-xai-oauth" "models.json")))
  "omp's bundled xai-oauth rows, read when this file loads: a vector of objects.")

(defparameter +xhigh-prefixes+ '("grok-4.6" "grok-4.7" "grok-4.20-multi-agent")
  "The model families that take xhigh as it is; the others hear it as high.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
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
       t
       ;; CATALOG-PRICE's shape; a row priced at nothing is a plan's, not a price
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "xAI Grok OAuth as a models.dev provider: the Responses lane's package,
this section's base, the token variable, and the bundled models over PRIOR's
(the row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "xAI Grok OAuth (SuperGrok or X Premium+)"
                     "npm" "@ai-sdk/openai"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first token one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- the Responses body --------------------------------------------------------

(defun wire-effort (model effort)
  "EFFORT as Grok MODEL hears it."
  (cond ((string-equal effort "minimal") "low")
        ((and (member effort '("xhigh" "max") :test #'string-equal)
              (notany (lambda (prefix) (uiop:string-prefix-p prefix model)) +xhigh-prefixes+))
         "high")
        (t effort)))

(defun bare-required-p (branch)
  "Whether the union BRANCH only says which keys are required: no type, a
non-empty list of names, nothing but a description or a title beside it."
  (and (hash-table-p branch)
       (null (nth-value 1 (gethash "type" branch)))
       (let ((required (nlk:json-value branch :array "required")))
         (and required (plusp (length required))
              (every (lambda (name) (and (stringp name) (plusp (length name)))) required)))
       (loop for key being the hash-keys of branch
             always (member key '("required" "description" "title") :test #'equal))))

(defun flattened-parameters (schema)
  "SCHEMA, a tool's parameters, without a root anyOf or oneOf made only of
bare required-key fragments: xAI refuses such a union at the root, and the
object it constrains says the same thing to the model without it."
  (let ((key (cond ((vectorp (nlk:json-value schema :array "anyOf")) "anyOf")
                   ((vectorp (nlk:json-value schema :array "oneOf")) "oneOf"))))
    (if (and key
             (plusp (length (gethash key schema)))
             (or (equal "object" (nlk:json-value schema :string "type"))
                 (find "object" (nlk:json-value schema :array "type") :test #'equal)
                 (nlk:json-value schema :object "properties"))
             (every #'bare-required-p (gethash key schema)))
        (let ((copy (nlk:copy-json-object schema)))
          (remhash key copy)
          copy)
        schema)))

(defun grok-body (body model)
  "BODY, the Responses lane's request for Grok MODEL, set as xAI takes it."
  (let ((row (model-row model)))
    (when row
      (cond ((not (nlk:json-value row :boolean "reasoning"))
             ;; a model that does not reason is asked nothing about reasoning
             (remhash "reasoning" body)
             (remhash "include" body))
            (t
             (let ((effort (nlk:json-value body :string "reasoning" "effort")))
               (if (and effort (nlk:json-value row :array "efforts"))
                   (setf (gethash "reasoning" body)
                         (nlk:json-object "effort" (wire-effort model effort)))
                   ;; no rung to say: the model reasons as it does, unasked
                   (remhash "reasoning" body)))
             (setf (gethash "include" body) (vector "reasoning.encrypted_content"))))))
  (alexandria:when-let (tools (nlk:json-value body :array "tools"))
    (setf (gethash "tools" body)
          (map 'vector (lambda (tool)
                         (let ((parameters (nlk:json-value tool :object "parameters")))
                           (if parameters
                               (nlk:copy-json-object tool "parameters" (flattened-parameters parameters))
                               tool)))
               tools)))
  body)
