;;;; cell-test.lisp --- the Cloudflare Workers AI cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every round a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "cloudflare-workers-ai" "CLOUDFLARE-WORKERS-AI-CELL-" :start nodecode-cloudflare-workers-ai:start-cell)

(define-cell-lifecycle-tests "cloudflare-workers-ai"
  (:config "account_id" "acc123")
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body 'nle::list-provider-models)
  (:refused ("account_id" 5) ("base_url" 5)))

(defun workers-ai-api ()
  "The api base the cell's catalog row carries."
  (nlk:json-value (nlk:json-value (nle::models-catalog-table) :object "cloudflare-workers-ai")
                  :string "api"))

(defun workers-ai-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun workers-ai-round (model &key (key "cf-test") effort)
  "One chat round for Workers AI MODEL on KEY at EFFORT, with dex:post
stubbed: (values URL HEADERS BODY) as dex:post saw them."
  (let ((nle::*provider* "cloudflare-workers-ai") (nle::*model* model) (nle::*api-key* key)
        (nle::*endpoint* nil) (nle::*reasoning-effort* effort)
        (url nil) (headers nil) (body nil))
    (with-stubbed-fdefinition
        (dex:post (asked &rest args)
         (setf url asked headers (getf args :headers) body (nlk:decode-json (getf args :content)))
         (values (make-truncated-sse-stream
                  "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                  "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                  "[DONE]")
                 200))
      (nle::call-provider-streaming (user-context)))
    (values url headers body)))

(defun workers-ai-effort-sent (model effort)
  "The reasoning_effort WORKERS-AI-ROUND sends for MODEL at EFFORT, or NIL
when the round carries no such field."
  (let ((body (nth-value 2 (workers-ai-round model :effort effort))))
    (gethash "reasoning_effort" body)))

;;; --- the catalog -----------------------------------------------------------------

(deftest cloudflare-workers-ai-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((cloudflare-workers-ai-start "account_id" "acc123"))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "cloudflare-workers-ai")))
      (is (equal "Cloudflare Workers AI" (nlk:json-value row :string "name")))
      (is (equal "https://api.cloudflare.com/client/v4/accounts/acc123/ai/v1"
                 (nlk:json-value row :string "api")) "the account names the base")
      (is (equal "openai-completions" (nle::configured-provider-lane "cloudflare-workers-ai")) "the chat lane drives it")
      (is (equal "https://api.cloudflare.com/client/v4/accounts/acc123/ai/v1/chat/completions"
                 (nle::lane-endpoint "cloudflare-workers-ai" "openai-completions")))
      (let* ((models (nlk:json-value row :object "models"))
             (gpt-oss (gethash "@cf/openai/gpt-oss-120b" models)))
        (is (= 16 (hash-table-count models)) "the plugin's 16 models")
        (is (equal "GPT-OSS 120B" (nle::catalog-model-name gpt-oss)))
        (is (equal 128000 (nle::catalog-model-context gpt-oss)) "Cloudflare's own window, not a 256K guess")
        (is (equal '("low" "medium" "high") (nle::catalog-model-efforts gpt-oss)) "the rungs gpt-oss takes")
        (is (search "(paid plan)" (nle::catalog-model-name (gethash "@cf/zai-org/glm-5.3" models))) "a paid-plan model says so in the picker")))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "cloudflare-workers-ai")))))

(deftest cloudflare-workers-ai-cell-leaves-the-account-blank-to-the-variable ()
  (with-cell-stop ((cloudflare-workers-ai-start))
    (with-stubbed-fdefinition (nle::credential-env (name)
                               (and (equal name "CLOUDFLARE_ACCOUNT_ID") "acc-env"))
      (is (equal "https://api.cloudflare.com/client/v4/accounts/acc-env/ai/v1" (workers-ai-api)))))
  (with-cell-stop ((cloudflare-workers-ai-start))
    (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
      (is (equal "https://api.cloudflare.com/client/v4/accounts/${CLOUDFLARE_ACCOUNT_ID}/ai/v1"
                 (workers-ai-api)) "with no account anywhere, the core's own blank stands"))))

(deftest cloudflare-workers-ai-cell-takes-the-plugins-base-variable ()
  (with-cell-stop ((cloudflare-workers-ai-start))
    (with-stubbed-fdefinition (nle::credential-env (name)
                               (and (equal name "CLOUDFLARE_WORKERS_AI_BASE_URL")
                                    "https://example.test/accounts/acc9/ai/v1"))
      (is (equal "https://example.test/accounts/acc9/ai/v1" (workers-ai-api))))))

;;; --- the key ------------------------------------------------------------------------

(deftest cloudflare-workers-ai-cell-reads-its-own-token ()
  (with-cell-stop ((cloudflare-workers-ai-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "CLOUDFLARE_WORKERS_AI_API_TOKEN") "cf-token"))
        (let ((credential (nle::resolve-provider-credential "cloudflare-workers-ai" :auth-path auth :probe t)))
          (is (equal "cf-token" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest cloudflare-workers-ai-cell-never-sends-cloudflares-other-token ()
  (with-cell-stop ((cloudflare-workers-ai-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "CLOUDFLARE_API_TOKEN") "pages-token"))
        (let ((credential (nle::resolve-provider-credential "cloudflare-workers-ai" :auth-path auth :probe t)))
          (is (not (equal "pages-token" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

;;; --- one round ----------------------------------------------------------------------

(deftest cloudflare-workers-ai-cell-sends-a-round-to-the-account ()
  (with-cell-stop ((cloudflare-workers-ai-start "account_id" "acc123"))
    (multiple-value-bind (url headers body) (workers-ai-round "@cf/openai/gpt-oss-120b" :effort "medium")
      (is (equal "https://api.cloudflare.com/client/v4/accounts/acc123/ai/v1/chat/completions" url))
      (is (equal "Bearer cf-test" (workers-ai-header headers "authorization")))
      (is (equal "@cf/openai/gpt-oss-120b" (nlk:json-value body :string "model")))
      (is (equal "medium" (nlk:json-value body :string "reasoning_effort"))))))

(deftest cloudflare-workers-ai-cell-sends-the-rung-each-model-takes ()
  (with-cell-stop ((cloudflare-workers-ai-start "account_id" "acc123"))
    (is (equal "high" (workers-ai-effort-sent "@cf/openai/gpt-oss-120b" "xhigh")) "one above the top becomes the top it takes")
    (is (equal "medium" (workers-ai-effort-sent "@cf/qwen/qwen3.8-27b" "high")) "qwen3.8 answers 400 to high; it is asked medium")
    (is (equal "high" (workers-ai-effort-sent "@cf/moonshotai/kimi-k2.6" "medium")) "a rung below the lowest becomes the lowest")
    (is (equal "max" (workers-ai-effort-sent "@cf/zai-org/glm-5.3" "max")))
    (is (equal "low" (workers-ai-effort-sent "@cf/openai/gpt-oss-20b" "off")) "gpt-oss always reasons: off is its lowest rung")
    (is (equal "none" (workers-ai-effort-sent "@cf/deepseek-ai/deepseek-v4-flash-0731" "off")) "a model that takes none is told so")
    (is (null (workers-ai-effort-sent "@cf/zai-org/glm-4.7-flash" "high")) "a model whose catalog lists no levels gets no field")
    (is (null (workers-ai-effort-sent "@cf/meta/llama-4-scout-17b-16e-instruct" "high")) "and neither does one that does not reason")
    (is (null (workers-ai-effort-sent "@cf/openai/gpt-oss-120b" nil)) "no pick: the provider's own default")))

(deftest cloudflare-workers-ai-cell-leaves-other-models-alone ()
  (with-cell-stop ((cloudflare-workers-ai-start "account_id" "acc123"))
    (is (equal "high" (workers-ai-effort-sent "gpt-5" "high")) "a model off the roster is not shaped by this cell")))

;;; --- the listing ---------------------------------------------------------------------

(deftest cloudflare-workers-ai-cell-lists-its-roster ()
  (with-cell-stop ((cloudflare-workers-ai-start "account_id" "acc123"))
    (let ((asked nil))
      (with-stubbed-fdefinition (nle::http-fetch (url &key headers timeout proxy content binary)
                                 (declare (ignore headers timeout proxy content binary))
                                 (push url asked)
                                 (values nil "HTTP 405"))
        (multiple-value-bind (rows error) (nle::list-provider-models "cloudflare-workers-ai")
          (is (= 16 (length rows)))
          (is (equal "@cf/openai/gpt-oss-120b" (getf (first rows) :id)))
          (is (equal 128000 (getf (first rows) :context-window)))
          (is (null error))
          (is (null asked) "the roster answers without a request"))
        (multiple-value-bind (rows error) (nle::list-provider-models "cloudflare-workers-ai" :key "cf-test")
          (is rows)
          (is (search "HTTP 405" error) "a key check says why it asked nothing"))))))
