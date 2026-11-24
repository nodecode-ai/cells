;;;; cell-test.lisp --- the ngrok AI Gateway cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "ngrok-ai-gateway" "NGROK-AI-GATEWAY-CELL-" :start nodecode-ngrok-ai-gateway:start-cell)

(define-cell-lifecycle-tests "ngrok-ai-gateway"
  (:hooks 'nle::models-catalog-table :credential 'nle::walk-provider-stream 'nle::provider-failure-reason)
  (:refused ("base_url" 5)))

(defun header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-ngrok-round ((url headers body) model &body forms)
  "FORMS with the cell started and one chat round for ngrok MODEL captured:
URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-cell-stop ((ngrok-ai-gateway-start))
     (let ((nle::*provider* "ngrok") (nle::*model* ,model) (nle::*api-key* "ng-key")
           (nle::*endpoint* nil) (,url nil) (,headers nil) (,body nil))
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

(deftest ngrok-ai-gateway-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((ngrok-ai-gateway-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "ngrok")))
      (is (equal "ngrok AI Gateway" (nlk:json-value row :string "name")))
      (is (equal "https://gateway.ngrok.ai/v1" (nlk:json-value row :string "api")))
      (is (equal '("NGROK_AI_GATEWAY_KEY") (coerce (nlk:json-value row :array "env") 'list)))
      (is (zerop (hash-table-count (nlk:json-value row :object "models"))) "no bundled rows: the list is the gateway's")
      (is (equal "openai-completions" (nle::configured-provider-lane "ngrok")) "the chat lane drives it")
      (is (equal "https://gateway.ngrok.ai/v1/chat/completions"
                 (nle::lane-endpoint "ngrok" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "ngrok")))))

(deftest ngrok-ai-gateway-cell-base-follows-the-section ()
  (with-cell-stop ((ngrok-ai-gateway-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "ngrok" "api")))))

(deftest ngrok-ai-gateway-cell-reads-its-own-key ()
  (with-cell-stop ((ngrok-ai-gateway-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "NGROK_AI_GATEWAY_KEY") "ng-key"))
        (let ((credential (nle::resolve-provider-credential "ngrok" :auth-path auth :probe t)))
          (is (equal "ng-key" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "NGROK_AI_GATEWAY_KEY") "ng-key"))
        (is (not (equal "ng-key"
                        (nle:credential-key (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t)))))))))

(deftest ngrok-ai-gateway-cell-never-sends-another-familys-key ()
  (with-cell-stop ((ngrok-ai-gateway-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "ngrok" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest ngrok-ai-gateway-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((ngrok-ai-gateway-start))
    (with-temp-auth (auth "{\"api_keys\":{\"ngrok\":{\"provider\":\"ngrok\",\"key\":\"ng-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "NGROK_AI_GATEWAY_KEY") "ng-key"))
        (is (equal "ng-saved"
                   (nle:credential-key (nle::resolve-provider-credential "ngrok" :auth-path auth :probe t))))))))

(deftest ngrok-ai-gateway-cell-sends-the-model-as-it-is-and-names-this-client ()
  (with-ngrok-round (url headers body) "openai:gpt-4o"
    (is (equal "https://gateway.ngrok.ai/v1/chat/completions" url))
    (is (equal "openai:gpt-4o" (nlk:json-value body :string "model")) "the gateway's own addressing goes out whole")
    (is (search "(ngrok-ai-gateway)" (header headers "User-Agent")))
    (is (uiop:string-prefix-p "nodecode_cli/" (header headers "User-Agent")))
    (is (equal "Bearer ng-key" (header headers "authorization")))))

(deftest ngrok-ai-gateway-cell-refuses-a-round-without-a-key ()
  (with-cell-stop ((ngrok-ai-gateway-start))
    (let ((nle::*provider* "ngrok") (nle::*model* "openai:gpt-4o") (nle::*api-key* "public")
          (nle::*endpoint* nil))
      (with-stubbed-fdefinition (dex:post (asked &rest args)
                                 (declare (ignore asked args))
                                 (error "the round dialled with no key"))
        (handler-case
            (progn (nle::call-provider-streaming (user-context))
                   (is nil "a round with no key refused"))
          (nle::provider-config-error (condition)
            (is (search "NGROK_AI_GATEWAY_KEY" (nle::provider-error-detail condition)))
            (is (search "https://app.ngrok.ai/keys" (nle::provider-error-detail condition)))))))))

(deftest ngrok-ai-gateway-cell-leaves-other-providers-alone ()
  (with-cell-stop ((ngrok-ai-gateway-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "gpt-4o") (nle::*api-key* "k")
          (nle::*endpoint* nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (is (stringp asked))
           (setf headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (equal "gpt-4o" (nlk:json-value body :string "model")))
      (is (null (search "(ngrok-ai-gateway)" (or (header headers "User-Agent") "")))))))

(deftest ngrok-ai-gateway-cell-reads-the-gateways-verdict ()
  (with-cell-stop ((ngrok-ai-gateway-start))
    (is (functionp (cell-hook "nodecode-ngrok-ai-gateway" 'nle::provider-failure-reason)) "the verdict hook is on")
    (is (equal "not_found" (nodecode-ngrok-ai-gateway::gateway-label 400 "[ERR_NGROK_27224] x")) "the table reads a code")
    (flet ((label (status code &optional (provider "ngrok"))
             (nle::provider-failure-reason
              (make-condition 'nle::provider-error :provider provider :status status
                              :detail (format nil "[~a] the gateway said so" code)))))
      (is (equal "not_found" (label 400 "ERR_NGROK_27224")) "an unrouted model is the model's fault")
      (is (equal "forbidden" (label 403 "ERR_NGROK_27231")) "an access policy is not a refused key")
      (is (equal "forbidden" (label 401 "ERR_NGROK_27211")) "a rejected upstream key is not the operator's key")
      (is (equal "billing" (label 403 "ERR_NGROK_4026")))
      (is (equal "billing" (label 402 "ERR_NGROK_27206")))
      (is (equal "http_400" (label 400 "ERR_NGROK_272242")) "a longer code is not the row it starts with")
      (is (equal "http_400" (label 400 "ERR_NGROK_27224" "openai-completions")) "another provider's is the core's"))))
