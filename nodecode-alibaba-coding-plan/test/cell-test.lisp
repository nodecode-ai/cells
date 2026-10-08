;;;; cell-test.lisp --- the alibaba-coding-plan cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "alibaba-coding-plan" "ALIBABA-CODING-PLAN-CELL-"
  :start nodecode-alibaba-coding-plan:start-cell)

(define-cell-lifecycle-tests "alibaba-coding-plan"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body)
  (:refused ("region" "mars") ("base_url" 5)))

(defun acp-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-acp-round ((url headers body) (model &key effort config) &body forms)
  "FORMS with the cell started on CONFIG (section pairs) and one chat round
for alibaba-coding-plan MODEL at EFFORT captured: URL, HEADERS and BODY (the
decoded request) as dex:post saw them."
  `(with-cell-stop ((alibaba-coding-plan-start ,@config))
     (let ((nle::*provider* "alibaba-coding-plan") (nle::*model* ,model) (nle::*api-key* "sk-test")
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

(deftest alibaba-coding-plan-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((alibaba-coding-plan-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "alibaba-coding-plan")))
      (is (equal "Alibaba Coding Plan" (nlk:json-value row :string "name")))
      (is (equal "https://coding-intl.dashscope.aliyuncs.com/v1" (nlk:json-value row :string "api"))
          "the international region unless the section says otherwise")
      (is (gethash "qwen3.7-plus" (nlk:json-value row :object "models")) "the bundled models are listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "alibaba-coding-plan"))
          "the chat lane drives it")
      (is (equal "https://coding-intl.dashscope.aliyuncs.com/v1/chat/completions"
                 (nle::lane-endpoint "alibaba-coding-plan" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "alibaba-coding-plan"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest alibaba-coding-plan-cell-serves-the-region-the-section-names ()
  (with-cell-stop ((alibaba-coding-plan-start "region" "china"))
    (is (equal "https://coding.dashscope.aliyuncs.com/v1"
               (nlk:json-value (nle::models-catalog-table) :string "alibaba-coding-plan" "api"))
        "omp's option 2, the China endpoint"))
  (with-cell-stop ((alibaba-coding-plan-start "region" "china" "base_url" "https://proxy.example/v1"))
    (is (equal "https://proxy.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "alibaba-coding-plan" "api"))
        "omp's option 3, a custom base, wins over the region")))

;;; --- the key ------------------------------------------------------------------------

(deftest alibaba-coding-plan-cell-reads-its-key-variable ()
  (with-cell-stop ((alibaba-coding-plan-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "ALIBABA_CODING_PLAN_API_KEY") "sk-plan"))
        (let ((credential (nle::resolve-provider-credential "alibaba-coding-plan" :auth-path auth :probe t)))
          (is (equal "sk-plan" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest alibaba-coding-plan-cell-never-sends-another-familys-key ()
  (with-cell-stop ((alibaba-coding-plan-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "alibaba-coding-plan" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential)))
              "the chat family's default variable never reaches Alibaba")
          (is (eq :public (nle:credential-source credential))))))))

(deftest alibaba-coding-plan-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((alibaba-coding-plan-start))
    (with-temp-auth (auth "{\"api_keys\":{\"alibaba-coding-plan\":{\"provider\":\"alibaba-coding-plan\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "ALIBABA_CODING_PLAN_API_KEY") "sk-plan"))
        (is (equal "sk-saved"
                   (nle:credential-key (nle::resolve-provider-credential "alibaba-coding-plan"
                                                                         :auth-path auth :probe t))))))))

;;; --- one round -------------------------------------------------------------------

(deftest alibaba-coding-plan-cell-sends-a-round-to-its-region ()
  (with-acp-round (url headers body) ("qwen3.7-plus" :config ("region" "china"))
    (is (equal "https://coding.dashscope.aliyuncs.com/v1/chat/completions" url))
    (is (equal "Bearer sk-test" (acp-header headers "authorization")))
    (is (equal "qwen3.7-plus" (nlk:json-value body :string "model")))))

(deftest alibaba-coding-plan-cell-asks-for-thinking-the-qwen-way ()
  (with-acp-round (url headers body) ("qwen3.7-plus" :effort "high")
    (is (eq t (gethash "enable_thinking" body)))
    (is (null (nth-value 1 (gethash "reasoning_effort" body))) "the dialect sends no reasoning_effort"))
  (with-acp-round (url headers body) ("glm-5" :effort "off")
    (is (nth-value 1 (gethash "enable_thinking" body)) "off is said, not left out")
    (is (member (gethash "enable_thinking" body) '(nil :false))))
  (with-acp-round (url headers body) ("qwen3.7-plus")
    (is (null (nth-value 1 (gethash "enable_thinking" body))) "no effort: the provider's own default"))
  (with-acp-round (url headers body) ("qwen3-coder-plus" :effort "high")
    (is (null (nth-value 1 (gethash "enable_thinking" body))) "a model that does not reason is told nothing")))

(deftest alibaba-coding-plan-cell-leaves-other-providers-alone ()
  (with-cell-stop ((alibaba-coding-plan-start))
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
