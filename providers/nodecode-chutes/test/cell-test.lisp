;;;; cell-test.lisp --- the chutes cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed NLE:HTTP:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "chutes" "CHUTES-CELL-" :start nodecode-chutes:start-cell)

(define-cell-lifecycle-tests "chutes"
  (:hooks 'nle::models-catalog-table :credential)
  (:command "chutes")
  (:refused ("base_url" 5)))

(defparameter *chutes-catalog*
  "{\"data\":[{\"id\":\"deepseek-ai/DeepSeek-V3\",\"supported_features\":[\"tools\"],\"max_model_len\":163840},{\"id\":\"chutes/flux\",\"supported_features\":[\"vision\"]},{\"id\":\"openai/gpt-oss-120b\",\"supported_features\":[\"Tools\",\"Reasoning\"]}]}"
  "A catalog as GET /v1/models answers it: two models a turn can take, one it
cannot.")

(defun chutes-row ()
  "Chutes' row in the catalog the core answers."
  (nlk:json-value (nle::models-catalog-table) :object "chutes"))

;;; --- the catalog, the lane, the address ----------------------------------------------

(deftest chutes-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((chutes-start))
    (let ((row (chutes-row)))
      (is (equal "Chutes" (nlk:json-value row :string "name")))
      (is (equal "https://llm.chutes.ai/v1" (nlk:json-value row :string "api")))
      (is (gethash "default:latency" (nlk:json-value row :object "models")) "the original's meta-models are listed")
      (is (gethash "default:throughput" (nlk:json-value row :object "models")))
      (is (equal "openai-completions" (nle::configured-provider-lane "chutes")) "the chat lane drives it")
      (is (equal "https://llm.chutes.ai/v1/chat/completions"
                 (nle::lane-endpoint "chutes" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "chutes")))))

(deftest chutes-cell-base-follows-the-section ()
  (with-cell-stop ((chutes-start "base_url" "https://chutes.example/v1"))
    (is (equal "https://chutes.example/v1" (nlk:json-value (chutes-row) :string "api")))))

(deftest chutes-cell-base-follows-the-variable ()
  (with-cell-stop ((chutes-start))
    (with-stubbed-fdefinition (nle::credential-env (name)
                                (and (equal name "CHUTES_BASE_URL") "https://relay.example/v1"))
      (is (equal "https://relay.example/v1" (nlk:json-value (chutes-row) :string "api"))))))

;;; --- the key -------------------------------------------------------------------------

(deftest chutes-cell-reads-chutes-api-key ()
  (with-cell-stop ((chutes-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "CHUTES_API_KEY") "cpk-env"))
        (let ((credential (nle::resolve-provider-credential "chutes" :auth-path auth :probe t)))
          (is (equal "cpk-env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest chutes-cell-never-sends-another-familys-key ()
  (with-cell-stop ((chutes-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "chutes" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest chutes-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((chutes-start))
    (with-temp-auth (auth "{\"api_keys\":{\"chutes\":{\"provider\":\"chutes\",\"key\":\"cpk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "CHUTES_API_KEY") "cpk-env"))
        (is (equal "cpk-saved"
                   (nle:credential-key (nle::resolve-provider-credential "chutes" :auth-path auth :probe t))))))))

;;; --- the catalog command --------------------------------------------------------------------

(deftest chutes-cell-command-lists-the-tool-capable-models ()
  (with-cell-stop ((chutes-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore options))
                                (is (equal :get method))
                                (is (search "llm.chutes.ai/v1/models" url))
                                (values *chutes-catalog* 200 nil))
      (let ((answer (nodecode-chutes::run-command "" "s1")))
        (is (search "deepseek-ai/DeepSeek-V3" answer))
        (is (search "openai/gpt-oss-120b" answer) "a `Tools' marker counts as `tools'")
        (is (null (search "chutes/flux" answer)) "a model that takes no tool calls is left out")
        (is (search "163840 tokens" answer) "the catalog's window is shown")
        (let* ((models (nlk:json-value (chutes-row) :object "models"))
               (model (gethash "deepseek-ai/DeepSeek-V3" models)))
          (is model "/models offers it")
          (is (= 163840 (nle::catalog-model-context model)) "its window rides in the catalog")
          (is (nle::catalog-model-tool-call-p model))
          (is (null (gethash "chutes/flux" models)) "a model that takes no tool calls is not listed")
          (is (gethash "default:latency" models) "the original's meta-models stay"))))))

(deftest chutes-cell-command-sends-the-key ()
  (with-cell-stop ((chutes-start))
    (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "CHUTES_API_KEY") "cpk-env"))
      (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                  (declare (ignore method url))
                                  (is (equal "Bearer cpk-env"
                                             (cdr (assoc "authorization" (getf options :headers)
                                                          :test #'string-equal))))
                                  (values *chutes-catalog* 200 nil))
        (is (search "deepseek-ai/DeepSeek-V3" (nodecode-chutes::run-command "" "s1")))))))

(deftest chutes-cell-command-narrows-by-text ()
  (with-cell-stop ((chutes-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (values *chutes-catalog* 200 nil))
      (let ((answer (nodecode-chutes::run-command "oss" "s1")))
        (is (search "openai/gpt-oss-120b" answer))
        (is (null (search "deepseek-ai/DeepSeek-V3" answer)))))))

(deftest chutes-cell-command-says-why-the-catalog-did-not-answer ()
  (with-cell-stop ((chutes-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (values "nope" 503 nil))
      (is (search "HTTP 503" (nodecode-chutes::run-command "" "s1"))))))

(deftest chutes-cell-command-says-when-nothing-takes-tools ()
  (with-cell-stop ((chutes-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (values "{\"data\":[{\"id\":\"chutes/flux\",\"supported_features\":[\"vision\"]}]}" 200 nil))
      (let ((answer (nodecode-chutes::run-command "" "s1")))
        (is (search "none of them take tool calls" answer))
        (is (gethash "default:latency" (nlk:json-value (chutes-row) :object "models")) "the meta-models still stand in")))))
