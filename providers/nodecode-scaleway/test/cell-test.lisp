;;;; cell-test.lisp --- the scaleway cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, the one environment override a
;;;; stubbed UIOP:GETENV: nothing touches the network, the environment or the
;;;; operator's files.

(in-package #:nodecode.test)

(define-test-slice "scaleway" "SCALEWAY-CELL-" :start nodecode-scaleway:start-cell)

(define-cell-lifecycle-tests "scaleway"
  (:hooks 'nle::models-catalog-table :credential)
  (:refused ("base_url" 5)))

(defun scaleway-row ()
  "The catalog row this cell puts up."
  (nlk:json-value (nle::models-catalog-table) :object "scaleway"))

(defun scaleway-api ()
  "The address the catalog row carries."
  (nlk:json-value (scaleway-row) :string "api"))

;;; --- the catalog -----------------------------------------------------------------

(deftest scaleway-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((scaleway-start))
    (let ((row (scaleway-row)))
      (is (equal "Scaleway" (nlk:json-value row :string "name")))
      (is (equal "https://api.scaleway.ai/v1" (nlk:json-value row :string "api")))
      (is (gethash "deepseek-v4.1-flash-1017" (nlk:json-value row :object "models")) "the curated models")
      (is (equal "openai-completions" (nle::configured-provider-lane "scaleway")) "the chat lane drives it")
      (is (equal "https://api.scaleway.ai/v1/chat/completions"
                 (nle::lane-endpoint "scaleway" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (scaleway-row)) "a stop takes the row back off")))

(deftest scaleway-cell-base-follows-the-section ()
  (with-cell-stop ((scaleway-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1" (scaleway-api)) "a relay's base is the row's")
    (is (equal "https://relay.example/v1/chat/completions"
               (nle::lane-endpoint "scaleway" "openai-completions")))))

(deftest scaleway-cell-base-default-follows-the-environment ()
  ;; The plugin's base_url_env_var: SCALEWAY_BASE_URL overrides the default
  ;; endpoint, and a section that names another base still outranks it.
  (with-stubbed-fdefinition (uiop:getenv (name)
                               (and (equal name "SCALEWAY_BASE_URL") " https://eu.example/v1 "))
    (with-cell-stop ((scaleway-start))
      (is (equal "https://eu.example/v1" (scaleway-api)) "the variable sets the default, trimmed"))
    (with-cell-stop ((scaleway-start "base_url" "https://relay.example/v1"))
      (is (equal "https://relay.example/v1" (scaleway-api)) "a section base outranks it")))
  (with-stubbed-fdefinition (uiop:getenv (name) (and (equal name "SCALEWAY_BASE_URL") "  "))
    (with-cell-stop ((scaleway-start))
      (is (equal "https://api.scaleway.ai/v1" (scaleway-api)) "a blank variable leaves Scaleway's own"))))

(deftest scaleway-cell-carries-real-windows-and-modalities ()
  ;; The point of the provider: a small model's real window, not one assumed
  ;; for the whole endpoint, and its modalities as published.
  (with-cell-stop ((scaleway-start))
    (is (eql 100000 (nle::model-context-tokens "llama-3.3-70b-instruct" "scaleway")) "the real 100K")
    (is (eql 128000 (nle::model-context-tokens "mistral-small-3.2-24b-instruct-2506" "scaleway")))
    (is (eq t (nle::model-reads-images-p "mistral-small-3.2-24b-instruct-2506" "scaleway")) "vision")
    (is (null (nle::model-reads-images-p "llama-3.3-70b-instruct" "scaleway")) "text only")))

(deftest scaleway-cell-offers-only-the-curated-tool-calling-list ()
  (with-cell-stop ((scaleway-start))
    (let ((models (nlk:json-value (scaleway-row) :object "models")))
      (is (gethash "qwen3.8-27b" models) "a curated model is offered")
      (is (equal nodecode-scaleway::+default-model+
                 (first (nodecode-scaleway::model-ids))) "the plugin's setup default leads the list")
      (is (not (gethash "gpt-oss-120b" models)) "its tool-calling is on the beta Responses API")
      (is (not (gethash "whisper-large-v3" models)) "a speech model is no turn's model"))))

;;; --- the key ------------------------------------------------------------------------

(deftest scaleway-cell-reads-scw-secret-key ()
  (with-cell-stop ((scaleway-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "SCW_SECRET_KEY") "scw-test"))
        (let ((credential (nle::resolve-provider-credential "scaleway" :auth-path auth :probe t)))
          (is (equal "scw-test" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest scaleway-cell-reads-the-catalog-variable-too ()
  (with-cell-stop ((scaleway-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "SCALEWAY_API_KEY") "scw-other"))
        (is (equal "scw-other"
                   (nle:credential-key (nle::resolve-provider-credential "scaleway" :auth-path auth :probe t))))))))

(deftest scaleway-cell-never-sends-another-familys-key ()
  (with-cell-stop ((scaleway-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "scaleway" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))) "no OpenAI key to Scaleway")
          (is (eq :public (nle:credential-source credential))))))))

(deftest scaleway-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((scaleway-start))
    (with-temp-auth (auth "{\"api_keys\":{\"scaleway\":{\"provider\":\"scaleway\",\"key\":\"scw-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "SCW_SECRET_KEY") "scw-env"))
        (let ((key (nle:credential-key (nle::resolve-provider-credential "scaleway" :auth-path auth :probe t))))
          (is (equal "scw-saved" key) "a key /connect saved wins over the variable"))))))
