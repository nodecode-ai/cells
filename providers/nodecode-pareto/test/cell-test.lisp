;;;; cell-test.lisp --- the pareto cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every chat round a stubbed dex:post: nothing touches
;;;; the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "pareto" "PARETO-CELL-" :start nodecode-pareto:start-cell)

(define-cell-lifecycle-tests "pareto"
  (:hooks 'nle::models-catalog-table 'nle::request-body)
  (:refused ("base_url" 5)))

(defvar *round-effort* nil
  "The reasoning effort a round binds, as a session pick or the default would.")

(defmacro with-pareto-round ((url body) model &body forms)
  "FORMS with the cell started and one chat round for pareto MODEL captured:
URL and BODY (the decoded request) as dex:post saw them, with *ROUND-EFFORT*
what the round binds as its reasoning effort."
  `(with-cell-stop ((pareto-start))
     (let ((nle::*provider* "pareto") (nle::*model* ,model) (nle::*api-key* "sk-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* *round-effort*)
           (,url nil) (,body nil))
       (declare (ignorable ,url ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,body (nlk:decode-json (getf args :content)))
            (values (make-truncated-sse-stream
                     "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                     "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                     "[DONE]")
                    200))
         (nle::call-provider-streaming (user-context)))
       ,@forms)))

(deftest pareto-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((pareto-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "pareto"))
           (models (nlk:json-value row :object "models")))
      (is (equal "Pareto Inference" (nlk:json-value row :string "name")))
      (is (equal "https://api.paretoinference.com/v1" (nlk:json-value row :string "api")))
      (is (equal "@ai-sdk/openai-compatible" (nlk:json-value row :string "npm")) "the chat lane's package")
      (is (= 3 (hash-table-count models)) "the original's three models, and no others")
      (is (equal 1000000 (nle::catalog-model-context (gethash "z-ai/glm-5.3-flash" models))) "the window the original declares")
      (is (nle::catalog-model-tool-call-p (gethash "xiaomi/mimo-v2.6-flash" models)) "a turn can run on it")
      (is (equal "openai-completions" (nle::configured-provider-lane "pareto")) "the chat lane drives it")
      (is (equal "https://api.paretoinference.com/v1/chat/completions"
                 (nle::lane-endpoint "pareto" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "pareto")))))

(deftest pareto-cell-lists-every-rung-the-api-takes ()
  (with-cell-stop ((pareto-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "pareto"))
           (model (gethash "deepseek/deepseek-v4.1-flash"
                            (nlk:json-value row :object "models"))))
      (is (equal '("off" "minimal" "low" "medium" "high" "xhigh" "max")
                 (nle::catalog-model-efforts model)) "Nodecode's ladder, with `none' spelled `off'"))))

(deftest pareto-cell-base-follows-the-section ()
  (with-cell-stop ((pareto-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "pareto" "api")))))

(deftest pareto-cell-sends-the-namespaced-model-id ()
  (let ((*round-effort* nil))
    (with-pareto-round (url body) "glm-5.3-flash"
      (is (equal "https://api.paretoinference.com/v1/chat/completions" url))
      (is (equal "z-ai/glm-5.3-flash" (nlk:json-value body :string "model")) "a bare name is sent as the vendor/model id")
      (is (null (gethash "reasoning_effort" body)) "no level set, so Pareto's default stands"))
    (with-pareto-round (url body) "z-ai/glm-5.3-flash"
      (is (equal "z-ai/glm-5.3-flash" (nlk:json-value body :string "model")) "a namespaced id rides as it is"))))

(deftest pareto-cell-sends-a-level-the-api-takes-as-it-is ()
  (let ((*round-effort* "high"))
    (with-pareto-round (url body) "xiaomi/mimo-v2.6-flash"
      (is (equal "high" (nlk:json-value body :string "reasoning_effort")))))
  (let ((*round-effort* "ultra"))
    (with-pareto-round (url body) "xiaomi/mimo-v2.6-flash"
      (is (equal "max" (nlk:json-value body :string "reasoning_effort")) "ultra is Pareto's max"))))

(deftest pareto-cell-turns-reasoning-off-in-the-apis-own-spelling ()
  (let ((*round-effort* "off"))
    (with-pareto-round (url body) "deepseek/deepseek-v4.1-flash"
      (is (equal "none" (nlk:json-value body :string "reasoning_effort")) "the lane omits `off'; Pareto is told `none' instead"))))

(deftest pareto-cell-leaves-other-providers-alone ()
  (let ((nle::*provider* "openai-completions") (nle::*model* "glm-5.3-flash")
        (nle::*api-key* "k") (nle::*endpoint* nil) (nle::*reasoning-effort* "high")
        (body nil))
    (with-cell-stop ((pareto-start))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (declare (ignore asked))
           (setf body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context))))
    (is (equal "glm-5.3-flash" (nlk:json-value body :string "model")) "the id is left bare")
    (is (equal "high" (nlk:json-value body :string "reasoning_effort")) "and the lane's own field stands")))

(deftest pareto-cell-reads-pareto-api-key ()
  (with-cell-stop ((pareto-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "PARETO_API_KEY") "sk-pareto"))
        (let ((credential (nle::resolve-provider-credential "pareto" :auth-path auth :probe t)))
          (is (equal "sk-pareto" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "pareto" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))) "another family's key never rides here")
          (is (eq :public (nle:credential-source credential))))))))
