;;;; cell-test.lisp --- the cloudflare-ai-gateway cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "cloudflare-ai-gateway" "CLOUDFLARE-AI-GATEWAY-CELL-"
  :start nodecode-cloudflare-ai-gateway:start-cell)

(define-cell-lifecycle-tests "cloudflare-ai-gateway"
  (:hooks 'nle::models-catalog-table 'nle::list-provider-models 'nle::resolve-model-lane
          :credential 'nle::request-body 'nle::anthropic-request-body 'nle::walk-provider-stream)
  (:refused ("base_url" 5) ("account_id" 7)))

(defun cloudflare-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun cloudflare-stream (lane)
  "One short answer as LANE's wire streams it."
  (if (equal lane "anthropic")
      (make-truncated-sse-stream
       "{\"type\":\"message_start\",\"message\":{\"id\":\"m1\",\"model\":\"k\",\"usage\":{\"input_tokens\":1}}}"
       "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}"
       "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}"
       "{\"type\":\"message_stop\"}")
      (make-truncated-sse-stream
       "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
       "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
       "[DONE]")))

(defmacro with-cloudflare-round ((url headers body &key (key "cfut-test")
                                                        (section ''("account_id" "acct" "gateway_id" "gw")))
                                 model &body forms)
  "FORMS with the cell started on SECTION and one round of MODEL sent with
KEY captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-cell-stop ((apply #'cloudflare-ai-gateway-start ,section))
     (let ((nle::*provider* "cloudflare-ai-gateway") (nle::*model* ,model) (nle::*api-key* ,key)
           (nle::*endpoint* nil) (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (cloudflare-stream (nle::resolve-model-lane "cloudflare-ai-gateway" ,model)) 200))
         (nle::call-provider (user-context)))
       ,@forms)))

(deftest cloudflare-ai-gateway-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((cloudflare-ai-gateway-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "cloudflare-ai-gateway")))
      (is (equal "Cloudflare AI Gateway" (nlk:json-value row :string "name")))
      (is (equal "https://gateway.ai.cloudflare.com/v1/<account>/<gateway>/anthropic/v1"
                 (nlk:json-value row :string "api"))
          "with no account or gateway named, the base keeps omp's placeholders")
      (is (gethash "anthropic/claude-opus-5" (nlk:json-value row :object "models")) "the bundled models are listed")
      (loop for (model lane) in '(("anthropic/claude-opus-5" "anthropic")
                                  ("openai/gpt-5.1" "openai-completions")
                                  ("workers-ai/@cf/moonshotai/kimi-k2.6" "openai-completions")
                                  ("xai/grok-4.7" "anthropic"))
            do (is (equal lane (nle::resolve-model-lane "cloudflare-ai-gateway" model)) model))
      (is (find "openai/gpt-5.1" (nle::list-provider-models "cloudflare-ai-gateway")
                :key (lambda (row) (getf row :id)) :test #'equal)
          "the listing is the roster, asked of no endpoint"))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "cloudflare-ai-gateway"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest cloudflare-ai-gateway-cell-says-a-connect-token-was-not-checked ()
  ;; The roster answered /connect's key check with no reason, which the core
  ;; reads as a token the gateway took: any token read `works'. The gateway
  ;; is asked nothing, so the verdict is unchecked, even where an asked
  ;; endpoint would have refused the token.
  (with-cell-stop ((cloudflare-ai-gateway-start "account_id" "acct" "gateway_id" "gw"))
    (with-temp-file (nle::*provider-models-cache-path*)
      (let ((asked '()))
        (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                   (push url asked)
                                   (values "{\"error\":{\"type\":\"authentication_error\"}}" 401))
          (multiple-value-bind (verdict words) (nle::provider-key-check "cloudflare-ai-gateway" "cfut-wrong")
            (is (eq :unchecked verdict))
            (is (search "first turn tries the token" words)))
          (is (null asked) "nothing was asked")
          (multiple-value-bind (rows reason) (nle::list-provider-models "cloudflare-ai-gateway")
            (is rows)
            (is (null reason) "the picker's listing, with no key, is the roster as before")))))))

(deftest cloudflare-ai-gateway-cell-base-follows-the-section ()
  (with-cell-stop ((cloudflare-ai-gateway-start "account_id" "acct" "gateway_id" "gw"))
    (is (equal "https://gateway.ai.cloudflare.com/v1/acct/gw/anthropic/v1"
               (nlk:json-value (nle::models-catalog-table) :string "cloudflare-ai-gateway" "api")))))

(deftest cloudflare-ai-gateway-cell-reads-its-token-variable ()
  (with-cell-stop ((cloudflare-ai-gateway-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (cdr (assoc name '(("CLOUDFLARE_AI_GATEWAY_API_KEY" . "cfut-env")
                                                      ("ANTHROPIC_API_KEY" . "sk-ant")
                                                      ("OPENAI_API_KEY" . "sk-openai"))
                                               :test #'equal)))
          (let ((credential (nle::resolve-provider-credential "cloudflare-ai-gateway" :auth-path auth :probe t)))
            (is (equal "cfut-env" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))))))

(deftest cloudflare-ai-gateway-cell-never-sends-another-familys-key ()
  (with-cell-stop ((cloudflare-ai-gateway-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (cdr (assoc name '(("ANTHROPIC_API_KEY" . "sk-ant")
                                                      ("OPENAI_API_KEY" . "sk-openai"))
                                               :test #'equal)))
          (let ((credential (nle::resolve-provider-credential "cloudflare-ai-gateway" :auth-path auth :probe t)))
            (is (eq :public (nle:credential-source credential))
                "neither the Messages family's variable nor the chat family's reaches the gateway")))))))

(deftest cloudflare-ai-gateway-cell-saved-token-outranks-the-variable ()
  (with-cell-stop ((cloudflare-ai-gateway-start))
    (with-temp-auth (auth "{\"api_keys\":{\"cloudflare-ai-gateway\":{\"provider\":\"cloudflare-ai-gateway\",\"key\":\"cfut-saved\"}}}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "CLOUDFLARE_AI_GATEWAY_API_KEY") "cfut-env"))
          (is (equal "cfut-saved"
                     (nle:credential-key (nle::resolve-provider-credential "cloudflare-ai-gateway"
                                                                           :auth-path auth :probe t)))))))))

(deftest cloudflare-ai-gateway-cell-sends-an-anthropic-model-to-the-messages-route ()
  (with-cloudflare-round (url headers body) "anthropic/claude-sonnet-4.5"
    (is (equal "https://gateway.ai.cloudflare.com/v1/acct/gw/anthropic/v1/messages" url))
    (is (equal "claude-sonnet-4-5" (nlk:json-value body :string "model"))
        "the namespace goes, and the dots become dashes")
    (is (equal "Bearer cfut-test" (cloudflare-header headers "cf-aig-authorization")))
    (is (null (cloudflare-header headers "x-api-key")) "no key leaves for Anthropic")
    (is (null (cloudflare-header headers "authorization")))
    (is (equal "2023-06-01" (cloudflare-header headers "anthropic-version")))))

(deftest cloudflare-ai-gateway-cell-sends-an-openai-model-to-the-openai-route ()
  (with-cloudflare-round (url headers body) "openai/gpt-5.1"
    (is (equal "https://gateway.ai.cloudflare.com/v1/acct/gw/openai/chat/completions" url))
    (is (equal "gpt-5.1" (nlk:json-value body :string "model")))
    (is (equal "Bearer cfut-test" (cloudflare-header headers "cf-aig-authorization")))
    (is (null (cloudflare-header headers "authorization")) "no bearer leaves for OpenAI")
    (is (null (nlk:json-value body :any "max_tokens")) "the cap is never max_tokens")
    (is (null (nlk:json-value body :any "prompt_cache_key")))))

(deftest cloudflare-ai-gateway-cell-sends-a-workers-ai-model-whole-to-compat ()
  (with-cloudflare-round (url headers body) "workers-ai/@cf/moonshotai/kimi-k2.6"
    (is (equal "https://gateway.ai.cloudflare.com/v1/acct/gw/compat/chat/completions" url))
    (is (equal "workers-ai/@cf/moonshotai/kimi-k2.6" (nlk:json-value body :string "model")))))

(deftest cloudflare-ai-gateway-cell-sends-an-unrouted-model-as-its-row-says ()
  (with-cloudflare-round (url headers body) "xai/grok-4.7"
    (is (equal "https://gateway.ai.cloudflare.com/v1/acct/gw/anthropic/v1/messages" url))
    (is (equal "xai/grok-4.7" (nlk:json-value body :string "model")) "no route: the id goes whole")))

(deftest cloudflare-ai-gateway-cell-reads-omps-stored-credential ()
  (with-cloudflare-round (url headers body
                              :key "{\"token\":\"cfut-json\",\"accountId\":\"a2\",\"gatewayId\":\"g2\"}")
      "anthropic/claude-opus-5"
    (is (equal "https://gateway.ai.cloudflare.com/v1/a2/g2/anthropic/v1/messages" url)
        "the credential's ids outrank the section's")
    (is (equal "Bearer cfut-json" (cloudflare-header headers "cf-aig-authorization")))))

(deftest cloudflare-ai-gateway-cell-falls-back-to-the-id-variables ()
  (with-stubbed-fdefinition (nle::credential-env (name)
                             (cdr (assoc name '(("CLOUDFLARE_ACCOUNT_ID" . "a3") ("CLOUDFLARE_GATEWAY_ID" . "g3"))
                                         :test #'equal)))
    (with-cloudflare-round (url headers body :section '()) "openai/gpt-5.1"
      (is (equal "https://gateway.ai.cloudflare.com/v1/a3/g3/openai/chat/completions" url)))))

(deftest cloudflare-ai-gateway-cell-refuses-a-round-without-an-account ()
  (with-cell-stop ((cloudflare-ai-gateway-start "gateway_id" "gw"))
    (with-stubbed-fdefinition (nle::credential-env (name) nil)
      (let ((nle::*provider* "cloudflare-ai-gateway") (nle::*model* "openai/gpt-5.1")
            (nle::*api-key* "cfut-test") (nle::*endpoint* nil) (posted nil))
        (with-stubbed-fdefinition (dex:post (&rest args) (setf posted t) (values nil 500))
          (let ((condition (handler-case (progn (nle::call-provider-streaming (user-context)) nil)
                             (nle::provider-error (condition) condition))))
            (is (typep condition 'nle::provider-config-error))
            (is (search "account ID is required" (nle::provider-error-detail condition)))
            (is (not posted) "nothing is sent to a placeholder address")))))))

(deftest cloudflare-ai-gateway-cell-leaves-other-providers-alone ()
  (with-cell-stop ((cloudflare-ai-gateway-start "account_id" "acct" "gateway_id" "gw"))
    (let ((nle::*provider* "anthropic") (nle::*model* "claude-opus-5") (nle::*api-key* "sk-ant")
          (nle::*endpoint* nil) (url nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (cloudflare-stream "anthropic") 200))
        (nle::call-provider (user-context)))
      (is (equal "https://api.anthropic.com/v1/messages" url))
      (is (equal "sk-ant" (cloudflare-header headers "x-api-key")))
      (is (null (cloudflare-header headers "cf-aig-authorization")))
      (is (equal "claude-opus-5" (nlk:json-value body :string "model"))))))
