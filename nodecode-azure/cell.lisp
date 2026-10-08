;;;; cell.lisp --- the cell: Azure OpenAI among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Five hooks, each declining for every provider but azure:
;;;;
;;;;   MODELS-CATALOG-TABLE     the catalog carries Azure's row: the Responses
;;;;                            lane's package, the resource's base, and omp's
;;;;                            bundled models over whatever models.dev
;;;;                            published (whose own package, @ai-sdk/azure,
;;;;                            no lane speaks)
;;;;   LIST-PROVIDER-MODELS     the listing is the roster: the core's would
;;;;                            send the key as a bearer
;;;;   :CREDENTIAL              a key from AZURE_OPENAI_API_KEY, then the core's
;;;;                            own AZURE_API_KEY, and no other variable; a key
;;;;                            /connect saved in auth.json answers before this
;;;;                            point does
;;;;   RESPONSES-REQUEST-BODY   the body names the deployment, and every tool
;;;;                            is non-strict
;;;;   WALK-PROVIDER-STREAM     the round goes to <base>/responses with its
;;;;                            api-version, the key as `api-key'
;;;;
;;;; Config, a sibling top-level key:
;;;;   "azure": {"resource_name": "my-resource", "api_version": "v1",
;;;;             "deployment_map": "gpt-5.5=prod-gpt55"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-azure)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an azure round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Azure's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Azure's row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun listing (next provider &rest keys &key key &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Azure's listing is its roster. Asked with
KEY, which only /connect's key check does, it says why the key was not
checked."
  ;; The roster asks Azure nothing, and the check reads a NIL second value as
  ;; a key Azure took: any key read `works'.
  (if (equal provider +provider+)
      (values (listing-rows)
              (and key "Azure is asked nothing before a turn: its models come from the bundled roster, so the first turn tries the key"))
      (apply next provider keys)))

(defun credential (op next)
  "The :CREDENTIAL answer for azure: the key AZURE_OPENAI_API_KEY holds,
else the core's own AZURE_API_KEY, else none."
  ;; Never NEXT for azure: the ladder behind this point falls back to the
  ;; OpenAI family's default variable, and would send OPENAI_API_KEY to Azure.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (env-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "RESPONSES-REQUEST-BODY advice: an azure round as Azure takes it."
  (when (and (ours-p config) (hash-table-p body))
    (shape-body body (nle::effective-provider-config-model config)))
  body)

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: an azure round goes to the resource with its
api-version, the key as `api-key'."
  (if (ours-p config)
      (apply next fold
             :headers (round-headers headers (nle::effective-provider-config-api-key config))
             :endpoint (round-endpoint)
             (alexandria:remove-from-plist keys :headers :endpoint))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with other settings builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell azure
  (:section ("azure")
    (:guide "save an Azure OpenAI key with /connect or set AZURE_OPENAI_API_KEY; name the resource (resource_name, or base_url for any other address); api_version is the api-version every request names; deployment_map maps model ids to deployment names, model=deployment joined by commas")
    ("resource_name" :string
     :doc "the Azure OpenAI resource: https://<resource_name>.openai.azure.com/openai/v1 (else AZURE_OPENAI_RESOURCE_NAME)")
    ("base_url" :string
     :doc "the Responses base when it is not the resource's own; outranks resource_name (else AZURE_OPENAI_BASE_URL)")
    ("api_version" :string
     :doc "the api-version query parameter (else AZURE_OPENAI_API_VERSION, else v1)")
    ("deployment_map" :string
     :doc "model=deployment pairs joined by commas; a model not named is its own deployment (else AZURE_OPENAI_DEPLOYMENT_NAME_MAP)"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::list-provider-models #'listing)
  (:hook :credential #'credential)
  (:hook 'nle::responses-request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk))
