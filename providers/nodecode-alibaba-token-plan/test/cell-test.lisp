;;;; cell-test.lisp --- the alibaba-token-plan cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "alibaba-token-plan" "ALIBABA-TOKEN-PLAN-CELL-"
  :start nodecode-alibaba-token-plan:start-cell)

(define-cell-lifecycle-tests "alibaba-token-plan"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body)
  (:refused ("region" "mars") ("base_url" 5)))

(defun atp-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-atp-round ((url headers body) (model &key effort config) &body forms)
  "FORMS with the cell started on CONFIG (section pairs) and one chat round
for alibaba-token-plan MODEL at EFFORT captured: URL, HEADERS and BODY (the
decoded request) as dex:post saw them."
  `(with-cell-stop ((alibaba-token-plan-start ,@config))
     (let ((nle::*provider* "alibaba-token-plan") (nle::*model* ,model) (nle::*api-key* "sk-sp-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* ,effort)
           (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (make-truncated-sse-stream
                     "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                     "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                     "[DONE]")
                    200))
         (nle::call-provider-streaming (user-context)))
       ,@forms)))

;;; --- the catalog and the region ---------------------------------------------------

(deftest alibaba-token-plan-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((alibaba-token-plan-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "alibaba-token-plan")))
      (is (equal "QwenCloud Token Plan" (nlk:json-value row :string "name")))
      (is (equal "https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1"
                 (nlk:json-value row :string "api"))
          "the international region unless the section says otherwise")
      (is (gethash "qwen3.8-max" (nlk:json-value row :object "models")) "the seed models are listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "alibaba-token-plan"))
          "the chat lane drives it")
      (is (equal "https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1/chat/completions"
                 (nle::lane-endpoint "alibaba-token-plan" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "alibaba-token-plan"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest alibaba-token-plan-cell-serves-the-region-the-section-names ()
  (with-cell-stop ((alibaba-token-plan-start "region" "china"))
    (is (equal "https://token-plan.cn-beijing.maas.aliyuncs.com/compatible-mode/v1"
               (nlk:json-value (nle::models-catalog-table) :string "alibaba-token-plan" "api"))
        "omp's option 2, China (Beijing)"))
  (with-cell-stop ((alibaba-token-plan-start "base_url"
                                             "https://token-plan.us-east-1.maas.aliyuncs.com/compatible-mode/v1"))
    (is (equal "https://token-plan.us-east-1.maas.aliyuncs.com/compatible-mode/v1"
               (nlk:json-value (nle::models-catalog-table) :string "alibaba-token-plan" "api"))
        "omp's option 3, a custom base, wins over the region")))

;;; --- the key ------------------------------------------------------------------------

(deftest alibaba-token-plan-cell-reads-both-key-variables ()
  (with-cell-stop ((alibaba-token-plan-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "BAILIAN_TOKEN_PLAN_API_KEY") "sk-sp-bailian"))
        (let ((credential (nle::resolve-provider-credential "alibaba-token-plan" :auth-path auth :probe t)))
          (is (equal "sk-sp-bailian" (nle:credential-key credential)) "the second name, alone")
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (cond ((equal name "ALIBABA_TOKEN_PLAN_API_KEY") "sk-sp-first")
                                       ((equal name "BAILIAN_TOKEN_PLAN_API_KEY") "sk-sp-bailian")))
        (is (equal "sk-sp-first"
                   (nle:credential-key (nle::resolve-provider-credential "alibaba-token-plan"
                                                                         :auth-path auth :probe t)))
            "the first name wins")))))

(deftest alibaba-token-plan-cell-never-sends-another-familys-key ()
  (with-cell-stop ((alibaba-token-plan-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "alibaba-token-plan" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential)))
              "OPENAI_API_KEY never reaches QwenCloud, as omp refuses it")
          (is (eq :public (nle:credential-source credential))))))))

(deftest alibaba-token-plan-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((alibaba-token-plan-start))
    (with-temp-auth (auth "{\"api_keys\":{\"alibaba-token-plan\":{\"provider\":\"alibaba-token-plan\",\"key\":\"sk-sp-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "ALIBABA_TOKEN_PLAN_API_KEY") "sk-sp-env"))
        (is (equal "sk-sp-saved"
                   (nle:credential-key (nle::resolve-provider-credential "alibaba-token-plan"
                                                                         :auth-path auth :probe t))))))))

;;; --- one round -------------------------------------------------------------------

(deftest alibaba-token-plan-cell-sends-a-round-to-its-region ()
  (with-atp-round (url headers body) ("qwen3.7-plus" :config ("region" "china"))
    (is (equal "https://token-plan.cn-beijing.maas.aliyuncs.com/compatible-mode/v1/chat/completions" url))
    (is (equal "Bearer sk-sp-test" (atp-header headers "authorization")))
    (is (equal "qwen3.7-plus" (nlk:json-value body :string "model")))))

(deftest alibaba-token-plan-cell-asks-for-thinking-the-qwen-way ()
  (with-atp-round (url headers body) ("qwen3.7-plus" :effort "high")
    (is (eq t (gethash "enable_thinking" body)))
    (is (null (nth-value 1 (gethash "reasoning_effort" body))) "the dialect sends no reasoning_effort"))
  (with-atp-round (url headers body) ("qwen3.8-max-preview" :effort "high")
    (is (eq t (gethash "enable_thinking" body)) "Max Preview stays on the switch alone")
    (is (null (nth-value 1 (gethash "reasoning_effort" body)))))
  (with-atp-round (url headers body) ("glm-5.2" :effort "off")
    (is (nth-value 1 (gethash "enable_thinking" body)) "off is said, not left out")
    (is (member (gethash "enable_thinking" body) '(nil :false))))
  (with-atp-round (url headers body) ("qwen3.7-plus")
    (is (null (nth-value 1 (gethash "enable_thinking" body))) "no effort: the provider's own default")))

(deftest alibaba-token-plan-cell-lets-qwen-3-8-max-steer-its-depth ()
  (with-atp-round (url headers body) ("qwen3.8-max" :effort "medium")
    (is (eq t (gethash "enable_thinking" body)) "thinking turned on")
    (is (equal "medium" (nlk:json-value body :string "reasoning_effort")) "and its depth said"))
  (with-atp-round (url headers body) ("qwen3.8-flash" :effort "off")
    (is (member (gethash "enable_thinking" body) '(nil :false)) "not thinking: the Qwen switch, off")
    (is (null (nth-value 1 (gethash "reasoning_effort" body))))))

(deftest alibaba-token-plan-cell-leaves-other-providers-alone ()
  (with-cell-stop ((alibaba-token-plan-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "qwen3.7-plus") (nle::*api-key* "k")
          (nle::*endpoint* nil) (nle::*reasoning-effort* "high") (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (null (nth-value 1 (gethash "enable_thinking" body)))))))
