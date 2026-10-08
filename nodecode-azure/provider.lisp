;;;; provider.lisp --- what Azure OpenAI is: its resource address, its key, its deployments.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/azure.kdl and providers/azure.kdl (AZURE_OPENAI_API_KEY, the
;;;; default model, the Astra rule), ai/src/providers/azure-openai-
;;;; responses.ts (the address, the api-version, the api-key header, the
;;;; deployment name, the tools), openai-shared.ts (parseAzureDeploymentNameMap
;;;; and the reasoning policy), and the bundled rows of catalog/src/models.json,
;;;; which models.json in this folder carries (tools/omp-models.py wrote it).
;;;;
;;;; Azure OpenAI serves the Responses API on a resource of the operator's,
;;;; https://<resource>.openai.azure.com/openai/v1, as the AzureOpenAI SDK
;;;; client sends it:
;;;;
;;;;   - the request goes to <base>/responses?api-version=<version>, the
;;;;     version a query parameter (v1 unless set otherwise)
;;;;   - the key rides as a single `api-key' header, never as a bearer
;;;;   - the body names the deployment, which is the model id unless a
;;;;     deployment map says otherwise
;;;;   - every function tool says strict: false
;;;;
;;;; That is the core's Responses wire with another address, header and
;;;; model name, so this cell rides the core's openai-responses lane through
;;;; its hooks rather than carrying a lane of its own.

(in-package #:nodecode-azure)

(defparameter +env+ '("AZURE_OPENAI_API_KEY")
  "The environment variables omp reads an Azure OpenAI key from, in order.")

(defparameter +default-api-version+ "v1"
  "The api-version a request names when nothing sets one.")

(defparameter +placeholder-base+ "https://<resource>.openai.azure.com/openai/v1"
  "The catalog's base while no resource is named: a round refuses it with
what to set.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-azure" "models.json")))
  "omp's bundled Azure rows, read when this file loads: a vector of objects.")

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
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

;;; --- the resource's address ------------------------------------------------------

(defun present (value)
  "VALUE trimmed when it is a non-empty string, else NIL."
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun base-url ()
  "The resource's Responses base, or NIL when nothing names one: the
section's base_url, else AZURE_OPENAI_BASE_URL, else the section's
resource_name or AZURE_OPENAI_RESOURCE_NAME as
https://<resource>.openai.azure.com/openai/v1 (omp's resolveAzureConfig)."
  (let ((base (or (present (setting :base-url)) (nle::credential-env "AZURE_OPENAI_BASE_URL")))
        (resource (or (present (setting :resource-name)) (nle::credential-env "AZURE_OPENAI_RESOURCE_NAME"))))
    (cond (base (string-right-trim "/" base))
          (resource (format nil "https://~a.openai.azure.com/openai/v1" resource)))))

(defun api-version ()
  "The api-version every request names: the section's, else
AZURE_OPENAI_API_VERSION, else v1."
  (or (present (setting :api-version)) (nle::credential-env "AZURE_OPENAI_API_VERSION")
      +default-api-version+))

(defun parse-deployment-map (text)
  "TEXT, `model=deployment' entries joined by commas, as an alist (omp's
parseAzureDeploymentNameMap): an entry missing either side is skipped."
  (loop for entry in (uiop:split-string (or text "") :separator ",")
        for trimmed = (string-trim " " entry)
        for equals = (position #\= trimmed)
        for model = (and equals (string-trim " " (subseq trimmed 0 equals)))
        for deployment = (and equals (string-trim " " (subseq trimmed (1+ equals))))
        when (and model deployment (plusp (length model)) (plusp (length deployment)))
          collect (cons model deployment)))

(defun deployment (model-id)
  "The deployment a round of MODEL-ID names: the section's map's, else
AZURE_OPENAI_DEPLOYMENT_NAME_MAP's, else the model id itself."
  (or (cdr (assoc model-id (parse-deployment-map (setting :deployment-map)) :test #'equal))
      (cdr (assoc model-id (parse-deployment-map (nle::credential-env "AZURE_OPENAI_DEPLOYMENT_NAME_MAP"))
                  :test #'equal))
      model-id))

(defun round-endpoint ()
  "The address a round posts to: the resource's /responses with the
api-version as a query parameter. A round with no resource is refused in
omp's words."
  (let ((base (or (base-url)
                  (error 'nle::provider-config-error
                         :detail "Azure OpenAI base URL is required. Set azure.base_url or azure.resource_name, ~
                                  or AZURE_OPENAI_BASE_URL or AZURE_OPENAI_RESOURCE_NAME."))))
    (format nil "~a/responses?api-version=~a" base (quri:url-encode (api-version)))))

(defun round-headers (headers key)
  "HEADERS, the lane's own, with the bearer taken out and KEY as `api-key'."
  (when (equal key "public")
    (error 'nle::provider-config-error
           :detail "Azure OpenAI API key is required. Save one with /connect or set AZURE_OPENAI_API_KEY."))
  (append (remove-if (lambda (name) (string-equal name "authorization")) headers :key #'car)
          `(("api-key" . ,key))))

(defun reasoning-off-with-tools-p (model-id)
  "Whether MODEL-ID refuses reasoning beside function tools on Azure, so a
round with tools says effort none (the providers rule for gpt-6-astra*)."
  (uiop:string-prefix-p "gpt-6-astra" model-id))

(defun shape-body (body model-id)
  "BODY, the Responses request the core built, as Azure takes it."
  (setf (gethash "model" body) (deployment model-id))
  (let ((tools (nlk:json-value body :array "tools")))
    (when (and tools (plusp (length tools)))
      ;; omp declares every function tool non-strict
      (setf (gethash "tools" body)
            (map 'vector (lambda (tool) (nlk:copy-json-object tool "strict" :false)) tools))
      (when (reasoning-off-with-tools-p model-id)
        (setf (gethash "reasoning" body) (nlk:json-object "effort" "none"))
        (remhash "include" body))))
  body)

(defun catalog-row (&optional prior)
  "Azure as a models.dev provider: the Responses lane's package, the
resource's base, the key variables, and the bundled models over PRIOR's."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Azure OpenAI"
                     "npm" "@ai-sdk/openai"
                     "api" (or (base-url) +placeholder-base+)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun listing-rows ()
  "The roster as a provider listing answers it: the core's listing would send
the key as a bearer, which Azure refuses, so the picker is answered from the
roster without a request."
  (loop for row across +models+
        collect (list :id (nlk:json-value row :string "id")
                      :display (nlk:json-value row :string "name")
                      :context-window (nlk:json-value row :integer "context"))))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
