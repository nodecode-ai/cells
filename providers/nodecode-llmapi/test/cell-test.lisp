;;;; cell-test.lisp --- the llmapi cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, the gateway's catalog and credit
;;;; balance stubbed reads, and every chat round a stubbed dex:post: nothing
;;;; touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "llmapi" "LLMAPI-CELL-" :start nodecode-llmapi:start-cell)

(define-cell-lifecycle-tests "llmapi"
  (:hooks 'nle::models-catalog-table :credential 'nle::resolve-model-lane 'nle::walk-provider-stream)
  (:help :llmapi)
  (:command "llmapi")
  (:refused ("base_url" 5)))

;;; --- the catalog's own feed ---------------------------------------------------------

(defun feed-entry (id &key (context 200000) (tools t) (levels '("low" "medium" "high"))
                          (vision nil) (prompt 0.000003) (completion 0.000015))
  "One model of the gateway's catalog, shaped as GET /v1/models answers it."
  (nlk:json-object
   "id" id
   "kind" "chat"
   "context_length" context
   "reasoning_levels" (coerce (or levels #()) 'vector)
   "providers" (vector (nlk:json-object "provider" "anthropic" "tools" tools))
   "pricing" (nlk:json-object "prompt" prompt "completion" completion
                               "input_cache_read" 0.0000003)
   "architecture" (nlk:json-object
                   "input_modalities" (if vision #("text" "image") #("text"))
                   "output_modalities" #("text"))))

(defun feed-payload (&rest entries)
  "A GET /v1/models body carrying ENTRIES."
  (nlk:json-object "data" (coerce entries 'vector)))

(deftest llmapi-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((llmapi-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "llmapi")))
      (is (equal "LLM API" (nlk:json-value row :string "name")))
      (is (equal "https://api.llmapi.ai/v1" (nlk:json-value row :string "api")))
      (is (equal "@ai-sdk/openai-compatible" (nlk:json-value row :string "npm")) "the chat lane's package")
      (is (gethash "claude-sonnet-5-5" (nlk:json-value row :object "models")) "the featured list, while the catalog has said nothing")
      (is (equal "openai-completions" (nle::configured-provider-lane "llmapi")) "the chat lane drives it")
      (is (equal "https://api.llmapi.ai/v1/chat/completions"
                 (nle::lane-endpoint "llmapi" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "llmapi")))))

(deftest llmapi-cell-lists-what-the-gateway-names ()
  (with-cell-stop ((llmapi-start))
    (with-stubbed-fdefinition (nodecode-llmapi::fetch-entries (base timeout)
                               (list (feed-entry "claude-haiku-5-5" :vision t
                                        :levels '("none" "low" "medium" "high"))
                               (feed-entry "gemini-2-flash-lite" :context 32000)
                               (feed-entry "kimi-k2" :tools nil)))
      (nodecode-llmapi::refresh-feed t))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "llmapi"))
           (models (nlk:json-value row :object "models"))
           (claude (find "claude-haiku-5-5" (nodecode-llmapi::catalog-models)
                         :key #'nle::catalog-model-name :test #'equal)))
      (is (gethash "claude-haiku-5-5" models) "a chat model that takes tools is listed")
      (is (find "claude-haiku-5-5" (nodecode-llmapi::catalog-models) :key #'nle::catalog-model-name :test #'equal) "and the picker holds its row")
      (is (equal 200000 (nle::catalog-model-context claude)) "with the window the catalog published")
      (is (equal '("off" "low" "medium" "high") (nle::catalog-model-efforts claude)) "and its rungs, none spelled off")
      (is (equal '(3.0 15.0 0.3 nil) (nle::catalog-model-price claude)) "and its list price, per million tokens")
      (is (member "image" (coerce (nle::catalog-model-inputs claude) 'list) :test #'string=) "and that it reads images")
      (is (null (gethash "gemini-2-flash-lite" models)) "a 32K window is under the original's floor")
      (is (null (gethash "kimi-k2" models)) "a model with no tool calling is not a conversation")
      (is (null (gethash "claude-sonnet-5-5" models)) "the featured list gives way to the catalog"))))

(deftest llmapi-cell-keeps-the-last-catalog-when-a-read-fails ()
  (with-cell-stop ((llmapi-start))
    (with-stubbed-fdefinition (nodecode-llmapi::fetch-entries (base timeout)
                               (list (feed-entry "claude-opus-5-5")))
      (nodecode-llmapi::refresh-feed t))
    (with-stubbed-fdefinition (nodecode-llmapi::fetch-entries (base timeout) nil)
      (is (search "did not answer" (nodecode-llmapi::refresh-text)) "a failed read says so")
      (is (equal 1 (length (nodecode-llmapi::feed-models))) "and the catalog already held stands"))))

;;; --- the wire each model rides --------------------------------------------------------

(deftest llmapi-cell-sends-each-model-on-its-own-wire ()
  (with-cell-stop ((llmapi-start))
    (is (equal "anthropic" (nle::resolve-model-lane "llmapi" "claude-sonnet-5-5")) "Claude rides Messages")
    (is (equal "https://api.llmapi.ai/v1/messages"
               (nle::lane-endpoint "llmapi" "anthropic")) "at the gateway's own /messages")
    (is (equal "openai-responses" (nle::resolve-model-lane "llmapi" "gpt-5.5")) "GPT-5.5 rides Responses")
    (is (equal "openai-responses" (nle::resolve-model-lane "llmapi" "openai/gpt-6.1-sol")) "a prefixed id too")
    (is (equal "https://api.llmapi.ai/v1/responses"
               (nle::lane-endpoint "llmapi" "openai-responses")) "at /responses")
    (is (equal "openai-completions" (nle::resolve-model-lane "llmapi" "gemini-3.8-flash")) "Gemini rides chat")
    (is (equal "openai-completions" (nle::resolve-model-lane "llmapi" "gpt-4.1")) "so does GPT-4.1")
    (is (equal "openai-completions" (nle::resolve-model-lane "llmapi" "gpt-oss-120b")) "and gpt-oss, which names no version")
    (is (equal "anthropic" (nle::resolve-model-lane "anthropic" "claude-sonnet-5-5")) "another provider's lane is left to the core")))

;;; --- the key ---------------------------------------------------------------------------

(deftest llmapi-cell-reads-llmapi-api-key ()
  (with-cell-stop ((llmapi-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "LLMAPI_API_KEY") "sk-llmapi-env"))
        (let ((credential (nle::resolve-provider-credential "llmapi" :auth-path auth :probe t)))
          (is (equal "sk-llmapi-env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest llmapi-cell-never-sends-another-familys-key ()
  (with-cell-stop ((llmapi-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "llmapi" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

;;; --- one round ----------------------------------------------------------------------------

(defun llmapi-stream ()
  "One whole chat answer, `ok'."
  (make-truncated-sse-stream
   "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
   "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
   "[DONE]"))

(defun llmapi-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(deftest llmapi-cell-names-this-app-to-the-gateway ()
  (with-cell-stop ((llmapi-start))
    (let ((nle::*provider* "llmapi") (nle::*model* "gemini-3.8-flash") (nle::*api-key* "sk-llmapi-test")
          (nle::*endpoint* nil) (nle::*reasoning-effort* nil) (url nil) (headers nil))
      (with-stubbed-fdefinition (dex:post (asked &rest args)
                                 (setf url asked headers (getf args :headers))
                                 (values (llmapi-stream) 200))
        (nle::call-provider-streaming (user-context)))
      (is (equal "https://api.llmapi.ai/v1/chat/completions" url) "the chat wire at the gateway's address")
      (is (equal "nodecode" (llmapi-header headers "x-source")) "the app it comes from")
      (is (equal "nodecode-llmapi" (llmapi-header headers "x-llmapi-app-id")) "and the provider it is")
      (is (equal "Bearer sk-llmapi-test" (llmapi-header headers "authorization")) "with its own key"))))

(deftest llmapi-cell-leaves-another-provider-round-alone ()
  (with-cell-stop ((llmapi-start))
    (let ((seen nil)
          (lane-headers (list (cons "authorization" "Bearer sk-anthropic"))))
      (funcall (cell-hook "nodecode-llmapi" 'nle::walk-provider-stream)
               (lambda (fold &rest keys)
                 (declare (ignore fold))
                 (setf seen keys))
               'fold :config nil :headers lane-headers)
      (is (equal lane-headers (getf seen :headers)) "another provider's headers ride untouched")
      (is (null (llmapi-header (getf seen :headers) "x-llmapi-app-id")) "no attribution on another vendor's request"))))

;;; --- /llmapi ----------------------------------------------------------------------------------

(deftest llmapi-cell-usage-reads-the-credit-balance ()
  (with-cell-stop ((llmapi-start))
    (with-stubbed-fdefinitions
        ((nodecode-llmapi::credential-key () "sk-llmapi-test")
         (nodecode-llmapi::fetch-credits (base key timeout)
           (nlk:json-object "total_credits" 12.4 "credits_bonus" 2 "mode" "payg"
                            "monthly_cap" (nlk:json-object "limit" 50 "used" 12.4)
                            "organization" (nlk:json-object "name" "An Org"))))
      (let ((text (nodecode-llmapi::run-command "usage" nil)))
        (is (search "Credits balance: $12.40 (incl. $2.00 bonus)" text) "the balance, with its bonus")
        (is (search "pay as you go" text) "and the billing mode")
        (is (search "Monthly cap: $12.40 of $50.00 used" text) "and the cap")))))

(deftest llmapi-cell-usage-without-a-key-says-where-to-make-one ()
  (with-cell-stop ((llmapi-start))
    (with-stubbed-fdefinition (nodecode-llmapi::credential-key () nil)
      (let ((text (nodecode-llmapi::run-command "usage" nil)))
        (is (search "LLMAPI_API_KEY" text) "the variable to set")
        (is (search "app.llmapi.ai" text) "and where a key is made")))))

(deftest llmapi-cell-a-refused-balance-read-fails-open ()
  (with-cell-stop ((llmapi-start))
    (with-stubbed-fdefinitions
        ((nodecode-llmapi::credential-key () "sk-llmapi-test")
         (nodecode-llmapi::fetch-credits (base key timeout) nil))
      (is (search "could not be read" (nodecode-llmapi::run-command "usage" nil)) "says so, and answers"))))

(deftest llmapi-cell-status-names-the-address-and-the-key ()
  (with-cell-stop ((llmapi-start "base_url" "https://eu.example/v1"))
    (with-stubbed-fdefinition (nodecode-llmapi::credential-key () "sk-llmapi-test")
      (let ((text (nodecode-llmapi::run-command "" nil)))
        (is (search "https://eu.example/v1" text) "the configured address")
        (is (search "Key: set" text) "whether a key is there")
        (is (search "Messages" text) "and the wires it routes")))))
