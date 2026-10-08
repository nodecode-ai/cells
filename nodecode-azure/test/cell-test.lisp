;;;; cell-test.lisp --- the azure cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "azure" "AZURE-CELL-" :start nodecode-azure:start-cell)

(define-cell-lifecycle-tests "azure"
  (:hooks 'nle::models-catalog-table 'nle::list-provider-models :credential
          'nle::responses-request-body 'nle::walk-provider-stream)
  (:refused ("resource_name" 5) ("api_version" 7)))

(defun azure-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun azure-stream ()
  "One short answer as the Responses wire streams it."
  (make-truncated-sse-stream
   "{\"type\":\"response.created\",\"response\":{\"id\":\"r1\",\"model\":\"m\"}}"
   "{\"type\":\"response.output_text.delta\",\"item_id\":\"m1\",\"delta\":\"ok\"}"
   "{\"type\":\"response.completed\",\"response\":{\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}}"))

(defmacro with-azure-round ((url headers body &key (section ''("resource_name" "my-res")) (env ''())
                                               (key "az-key") effort)
                            model &body forms)
  "FORMS with the cell started on SECTION and one round of MODEL captured:
URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-stubbed-fdefinition (nle::credential-env (name) (cdr (assoc name ,env :test #'equal)))
     (with-cell-stop ((apply #'azure-start ,section))
       (let ((nle::*provider* "azure") (nle::*model* ,model) (nle::*api-key* ,key)
             (nle::*reasoning-effort* ,effort) (nle::*endpoint* nil)
             (,url nil) (,headers nil) (,body nil))
         (declare (ignorable ,url ,headers ,body))
         (with-stubbed-fdefinition
             (dex:post (asked &rest args)
              (setf ,url asked ,headers (getf args :headers)
                    ,body (nlk:decode-json (getf args :content)))
              (values (azure-stream) 200))
           (nle::call-provider (user-context)))
         ,@forms))))

(deftest azure-cell-puts-its-row-in-the-catalog ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((azure-start "resource_name" "my-res"))
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "azure")))
        (is (equal "Azure OpenAI" (nlk:json-value row :string "name")))
        (is (equal "https://my-res.openai.azure.com/openai/v1" (nlk:json-value row :string "api")))
        (is (gethash "gpt-5.5" (nlk:json-value row :object "models")))
        (is (equal "openai-responses" (nle::configured-provider-lane "azure")) "the Responses lane drives it")
        (is (find "gpt-6-sol" (nle::list-provider-models "azure") :key (lambda (row) (getf row :id)) :test #'equal)
            "the listing is the roster, asked of no endpoint"))
      (funcall stop)
      (setf stop nil)
      (is (null (nlk:json-value (nle::models-catalog-table) :object "azure"))))))

(deftest azure-cell-says-a-connect-key-was-not-checked ()
  ;; The roster answered /connect's key check with no reason, which the core
  ;; reads as a key Azure took: any key read `works'. Azure is asked nothing,
  ;; so the verdict is unchecked, even where an asked endpoint would have
  ;; refused the key.
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((azure-start "resource_name" "my-res"))
      (with-temp-file (nle::*provider-models-cache-path*)
        (let ((asked '()))
          (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                     (push url asked)
                                     (values "{\"error\":{\"code\":\"401\"}}" 401))
            (multiple-value-bind (verdict words) (nle::provider-key-check "azure" "az-wrong")
              (is (eq :unchecked verdict))
              (is (search "first turn tries the key" words)))
            (is (null asked) "nothing was asked")
            (multiple-value-bind (rows reason) (nle::list-provider-models "azure")
              (is rows)
              (is (null reason) "the picker's listing, with no key, is the roster as before"))))))))

(deftest azure-cell-base-follows-omps-order ()
  (with-stubbed-fdefinition (nle::credential-env (name)
                             (cdr (assoc name '(("AZURE_OPENAI_RESOURCE_NAME" . "env-res")) :test #'equal)))
    (with-cell-stop ((azure-start))
      (is (equal "https://env-res.openai.azure.com/openai/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "azure" "api"))
          "the resource variable names the base")))
  (with-stubbed-fdefinition (nle::credential-env (name)
                             (cdr (assoc name '(("AZURE_OPENAI_BASE_URL" . "https://gw.example/openai/v1/"))
                                         :test #'equal)))
    (with-cell-stop ((azure-start "resource_name" "my-res"))
      (is (equal "https://gw.example/openai/v1" (nlk:json-value (nle::models-catalog-table) :string "azure" "api"))
          "a base URL outranks a resource name, the variable's included")))
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((azure-start "base_url" "https://proxy.example/azure/v1" "resource_name" "my-res"))
      (is (equal "https://proxy.example/azure/v1" (nlk:json-value (nle::models-catalog-table) :string "azure" "api"))))))

(deftest azure-cell-reads-its-key-variables ()
  (with-cell-stop ((azure-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (cdr (assoc name '(("AZURE_OPENAI_API_KEY" . "az-env") ("AZURE_API_KEY" . "az-core")
                                                      ("OPENAI_API_KEY" . "sk-openai"))
                                               :test #'equal)))
          (let ((credential (nle::resolve-provider-credential "azure" :auth-path auth :probe t)))
            (is (equal "az-env" (nle:credential-key credential)) "omp's variable first")
            (is (eq :env (nle:credential-source credential)))))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (cdr (assoc name '(("AZURE_API_KEY" . "az-core") ("OPENAI_API_KEY" . "sk-openai"))
                                               :test #'equal)))
          (is (equal "az-core" (nle:credential-key (nle::resolve-provider-credential "azure" :auth-path auth :probe t)))
              "then the core's own"))
        (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (is (eq :public (nle:credential-source (nle::resolve-provider-credential "azure" :auth-path auth :probe t)))
              "the OpenAI family's variable never reaches Azure"))))))

(deftest azure-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((azure-start))
    (with-temp-auth (auth "{\"api_keys\":{\"azure\":{\"provider\":\"azure\",\"key\":\"az-saved\"}}}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "AZURE_OPENAI_API_KEY") "az-env"))
          (is (equal "az-saved" (nle:credential-key (nle::resolve-provider-credential "azure" :auth-path auth :probe t)))))))))

(deftest azure-cell-sends-a-round-the-azure-way ()
  (with-azure-round (url headers body) "gpt-5.5"
    (is (equal "https://my-res.openai.azure.com/openai/v1/responses?api-version=v1" url))
    (is (equal "az-key" (azure-header headers "api-key")))
    (is (null (azure-header headers "authorization")) "never a bearer")
    (is (equal "gpt-5.5" (nlk:json-value body :string "model")) "a model not mapped is its own deployment")
    (is (eq nil (nlk:json-value body :boolean "store")))
    (is-present (tools (nlk:json-value body :array "tools")) "the round carries the core's tools"
      (is (every (lambda (tool) (multiple-value-bind (strict present) (gethash "strict" tool)
                                  (and present (null strict))))
                 tools)
          "every tool non-strict"))))

(deftest azure-cell-names-the-deployment-and-the-api-version ()
  (with-azure-round (url headers body :section '("resource_name" "my-res" "api_version" "2025-04-01-preview"
                                                 "deployment_map" "gpt-5.5=prod-55, gpt-5.4=prod-54"))
      "gpt-5.5"
    (is (equal "https://my-res.openai.azure.com/openai/v1/responses?api-version=2025-04-01-preview" url))
    (is (equal "prod-55" (nlk:json-value body :string "model"))))
  (with-azure-round (url headers body :env '(("AZURE_OPENAI_DEPLOYMENT_NAME_MAP" . "gpt-6-sol=sol-east,broken")
                                             ("AZURE_OPENAI_API_VERSION" . "preview")))
      "gpt-6-sol"
    (is (equal "https://my-res.openai.azure.com/openai/v1/responses?api-version=preview" url))
    (is (equal "sol-east" (nlk:json-value body :string "model")) "the variable's map, its broken entry skipped")))

(deftest azure-cell-turns-astra-reasoning-off-beside-tools ()
  (with-azure-round (url headers body :effort "high") "gpt-6-astra"
    (is (equal "none" (nlk:json-value body :string "reasoning" "effort")))
    (is (null (nlk:json-value body :any "include"))))
  (with-azure-round (url headers body :effort "high") "gpt-6-sol"
    (is (equal "high" (nlk:json-value body :string "reasoning" "effort")) "only Astra")))

(deftest azure-cell-refuses-a-round-without-a-resource ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((azure-start))
      (let ((nle::*provider* "azure") (nle::*model* "gpt-5.5") (nle::*api-key* "az-key")
            (nle::*endpoint* nil) (posted nil))
        (is (equal "https://<resource>.openai.azure.com/openai/v1"
                   (nlk:json-value (nle::models-catalog-table) :string "azure" "api")))
        (with-stubbed-fdefinition (dex:post (&rest args) (setf posted t) (values nil 500))
          (let ((condition (handler-case (progn (nle::call-responses-streaming (user-context)) nil)
                             (nle::provider-error (condition) condition))))
            (is (typep condition 'nle::provider-config-error))
            (is (search "base URL is required" (nle::provider-error-detail condition)))
            (is (not posted))))))))

(deftest azure-cell-leaves-openai-alone ()
  (with-cell-stop ((azure-start "resource_name" "my-res"))
    (let ((nle::*provider* "openai-responses") (nle::*model* "gpt-5.5") (nle::*api-key* "sk-openai")
          (nle::*endpoint* nil) (url nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (azure-stream) 200))
        (nle::call-provider (user-context)))
      (is (equal "https://api.openai.com/v1/responses" url))
      (is (equal "Bearer sk-openai" (azure-header headers "authorization")))
      (is (null (azure-header headers "api-key")))
      (is (equal "gpt-5.5" (nlk:json-value body :string "model"))))))
