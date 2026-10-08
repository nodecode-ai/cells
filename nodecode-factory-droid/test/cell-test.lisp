;;;; cell-test.lisp --- the factory-droid cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every wire and every sign-in exchange a stubbed
;;;; dex:post or dex:get, every poll interval a stubbed PAUSE: nothing touches
;;;; the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "factory-droid" "FACTORY-DROID-CELL-" :start nodecode-factory-droid:start-cell)

(define-cell-lifecycle-tests "factory-droid"
  (:hooks 'nle::models-catalog-table 'nle::list-provider-models 'nle::resolve-model-lane
          'nle::lane-endpoint :credential 'nle::request-body 'nle::responses-request-body
          'nle::anthropic-request-body 'nle::google-request-body 'nle::walk-provider-stream)
  (:command "factory-droid")
  (:refused ("base_url" 5)))

(defun factory-droid-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun factory-droid-now ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun factory-droid-jwt (&rest claims)
  "An unsigned JWT whose payload holds CLAIMS, alternating keys and values."
  (flet ((part (object)
           (string-right-trim "." (cl-base64:string-to-base64-string
                                   (nlk:encode-json-object object) :uri t))))
    (format nil "~a.~a.sig" (part (nlk:json-object "alg" "none"))
            (part (apply #'nlk:make-json-object claims)))))

(defun factory-droid-uuid-p (text)
  (and (stringp text) (cl-ppcre:scan "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" text)))

(defun factory-droid-auth (&key (region nil) (inference nil) (org "org_ext_1"))
  "auth.json text holding a signed-in Factory entry."
  (format nil "{\"oauth_tokens\":{\"factory-droid\":{\"access_token\":\"~a\",\"refresh_token\":\"r-1\",~
               \"expires_at\":~d~@[,\"region\":\"~a\"~]~@[,\"inference_region\":\"~a\"~]}}}"
          (factory-droid-jwt "sub" "user_1" "external_org_id" org "exp" (+ (factory-droid-now) 3600))
          (+ (factory-droid-now) 3600) region inference))

(defun factory-droid-stream (lane)
  "One short answer as LANE's wire streams it."
  (cond ((equal lane "anthropic")
         (make-truncated-sse-stream
          "{\"type\":\"message_start\",\"message\":{\"id\":\"m1\",\"model\":\"k\",\"usage\":{\"input_tokens\":1}}}"
          "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
          "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}"
          "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}"
          "{\"type\":\"message_stop\"}"))
        ((equal lane "openai-responses")
         (make-truncated-sse-stream
          "{\"type\":\"response.created\",\"response\":{\"id\":\"r1\",\"model\":\"m\"}}"
          "{\"type\":\"response.output_text.delta\",\"item_id\":\"m1\",\"delta\":\"ok\"}"
          "{\"type\":\"response.completed\",\"response\":{\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}}"))
        ((equal lane "google")
         (make-truncated-sse-stream
          "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"ok\"}]},\"finishReason\":\"STOP\"}]}"))
        (t
         (make-truncated-sse-stream
          "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
          "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
          "[DONE]"))))

(defmacro with-factory-droid-round ((url headers body) (model &key effort (auth '(factory-droid-auth)))
                                    &body forms)
  "FORMS with the cell started and one Factory round on MODEL at EFFORT
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them;
the credential is the sign-in AUTH (auth.json text) holds."
  `(with-cell-stop ((factory-droid-start))
     (with-temp-auth (auth-path ,auth)
       (let ((nle::*provider* "factory-droid") (nle::*model* ,model) (nle::*api-key* nil)
             (nle::*reasoning-effort* ,effort) (nle::*endpoint* nil)
             (nle::*auth-file-path* (pathname auth-path))
             (,url nil) (,headers nil) (,body nil))
         (declare (ignorable ,url ,headers ,body))
         (with-stubbed-fdefinition
             (dex:post (asked &rest args)
              (setf ,url asked ,headers (getf args :headers)
                    ,body (nlk:decode-json (getf args :content)))
              (values (factory-droid-stream (nle::resolve-model-lane "factory-droid" ,model)) 200))
           (nle::call-provider (user-context)))
         ,@forms))))

(deftest factory-droid-cell-puts-its-roster-in-the-catalog ()
  (with-cell-stop ((factory-droid-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "factory-droid"))
           (models (nlk:json-value row :object "models")))
      (is (equal "Factory Droid" (nlk:json-value row :string "name")))
      (is (= 48 (hash-table-count models)) "the roster discovery answers with no live flags")
      (is (gethash "kimi-k3" models))
      (is (null (gethash "claude-sonnet-5-5" models)) "a model behind a feature flag stays hidden")
      (is (null (gethash "claude-fable-5" models)) "and so does one behind an explicit opt-in")
      (loop for (model lane path) in '(("kimi-k3" "openai-completions" "/api/llm/o/v1/chat/completions")
                                       ("gpt-5.5" "openai-responses" "/api/llm/o/v1/responses")
                                       ("claude-opus-5" "anthropic" "/api/llm/a/v1/messages")
                                       ("gemini-3.8-flash" "google" "/api/llm/g/v1/generate"))
            do (is (equal lane (nle::resolve-model-lane "factory-droid" model)) model)
               (is (equal (concatenate 'string "https://api.factory.ai" path)
                          (nle::lane-endpoint "factory-droid" lane)))))
    (is (equal '("kimi-k3" "Kimi K3" 196608)
               (let ((row (find "kimi-k3" (nle::list-provider-models "factory-droid")
                                :key (lambda (row) (getf row :id)) :test #'equal)))
                 (list (getf row :id) (getf row :display) (getf row :context-window))))
        "the listing is the roster, asked of no endpoint")
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "factory-droid")))))

(deftest factory-droid-cell-says-a-connect-key-was-not-checked ()
  ;; The roster answered /connect's key check with no reason, which the core
  ;; reads as a key Factory took: any key read `works'. Factory is asked
  ;; nothing, so the verdict is unchecked, even where an asked endpoint would
  ;; have refused the key, and the words name the sign-in.
  (with-cell-stop ((factory-droid-start))
    (with-temp-file (nle::*provider-models-cache-path*)
      (let ((asked '()))
        (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                   (push url asked)
                                   (values "{\"error\":{\"type\":\"authentication_error\"}}" 401))
          (multiple-value-bind (verdict words) (nle::provider-key-check "factory-droid" "fk-wrong")
            (is (eq :unchecked verdict))
            (is (search "/factory-droid login" words)))
          (is (null asked) "nothing was asked")
          (multiple-value-bind (rows reason) (nle::list-provider-models "factory-droid")
            (is rows)
            (is (null reason) "the picker's listing, with no key, is the roster as before")))))))

(deftest factory-droid-cell-without-a-sign-in-says-so ()
  (with-cell-stop ((factory-droid-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (cdr (assoc name '(("OPENAI_API_KEY" . "sk-openai")
                                                      ("FACTORY_API_KEY" . "fk"))
                                               :test #'equal)))
          (is (eq :public (nle:credential-source
                           (nle::resolve-provider-credential "factory-droid" :auth-path auth :probe t)))
              "no key variable stands in for the sign-in")))))
  (with-cell-stop ((factory-droid-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*provider* "factory-droid") (nle::*model* "kimi-k3") (nle::*api-key* nil)
            (nle::*auth-file-path* (pathname auth)))
        (is (search "Run /factory-droid login"
                    (princ-to-string (signals-error nle::provider-error (nle::call-provider (user-context))))))))))

(deftest factory-droid-cell-chat-round-carries-factory-identity ()
  (with-factory-droid-round (url headers body) ("kimi-k3")
    (is (equal "https://api.factory.ai/api/llm/o/v1/chat/completions" url))
    (is (search "Bearer eyJ" (factory-droid-header headers "Authorization")) "the WorkOS token, as a bearer")
    (is (equal "factory-cli/0.230.0" (factory-droid-header headers "User-Agent")))
    (is (equal "0.230.0" (factory-droid-header headers "X-Client-Version")))
    (is (equal "cli" (factory-droid-header headers "X-Factory-Client")))
    (is (equal "org_ext_1" (factory-droid-header headers "X-Factory-Org-Id")) "the token's external org")
    (is (equal "fireworks" (factory-droid-header headers "x-api-provider")) "the rotation's first upstream")
    (is (equal "registry_default" (factory-droid-header headers "x-provider-routing-source")))
    (is (factory-droid-uuid-p (factory-droid-header headers "x-session-id")))
    (is (factory-droid-uuid-p (factory-droid-header headers "x-assistant-message-id")))
    (is (equal "application/json" (factory-droid-header headers "Accept")))
    (is (equal "6.25.0" (factory-droid-header headers "X-Stainless-Package-Version")))
    (is (equal "kimi-k3" (nlk:json-value body :string "model")))
    (let ((first (aref (nlk:json-value body :array "messages") 0)))
      (is (equal "system" (nlk:json-value first :string "role")))
      (is (equal "You are Droid, an AI software engineering agent built by Factory."
                 (nlk:json-value first :string "content"))
          "Droid's identity opens the system prompt"))
    (is (= 65536 (nlk:json-value body :integer "max_tokens")) "the model's own ceiling")
    (is (= 1 (nlk:json-value body :number "temperature")))
    (is (equal "high" (nlk:json-value body :string "reasoning_effort")) "the model's default level")
    (is (equal "preserved" (nlk:json-value body :string "reasoning_history")) "Fireworks keeps the history")
    (is (null (nlk:json-value body :any "prompt_cache_key"))))
  (with-factory-droid-round (url headers body) ("kimi-k3" :effort "off")
    (is (equal "none" (nlk:json-value body :string "reasoning_effort")))
    (is (null (nlk:json-value body :any "reasoning_history")))))

(deftest factory-droid-cell-names-the-session-it-runs-in ()
  (with-stubbed-fdefinition (nodecode-factory-droid::session-id () "s-1")
    (with-factory-droid-round (url headers body) ("kimi-k3")
      (is (equal (nodecode-factory-droid::deterministic-uuid "s-1")
                 (factory-droid-header headers "x-session-id"))
          "the same session is the same id at every round")
      (is (not (equal (factory-droid-header headers "x-session-id")
                      (factory-droid-header headers "x-assistant-message-id")))))))

(deftest factory-droid-cell-an-eu-account-goes-to-the-eu-host ()
  (with-factory-droid-round (url headers body) ("glm-5.2" :auth (factory-droid-auth :region "eu" :inference "eu"))
    (is (equal "https://api.eu.factory.ai/api/llm/o/v1/chat/completions" url))
    (is (equal "mistral" (factory-droid-header headers "x-api-provider")) "the EU's own upstream")
    (is (= 65536 (nlk:json-value body :integer "max_tokens")) "the EU's limit")
    (is (equal "high" (nlk:json-value body :string "reasoning_effort")))
    (is (null (nlk:json-value body :any "reasoning_history"))))
  (with-cell-stop ((factory-droid-start))
    (with-temp-auth (auth (factory-droid-auth :region "eu" :inference "eu"))
      (let ((nle::*provider* "factory-droid") (nle::*model* "kimi-k3") (nle::*api-key* nil)
            (nle::*auth-file-path* (pathname auth)))
        (is (search "unavailable in this account region"
                    (princ-to-string (signals-error nle::provider-error (nle::call-provider (user-context))))))))))

(deftest factory-droid-cell-says-a-region-refusal ()
  (with-cell-stop ((factory-droid-start))
    (with-temp-auth (auth (factory-droid-auth))
      (let ((nle::*api-key* nil) (nle::*auth-file-path* (pathname auth)))
        (let* ((config (nle::snapshot-effective-provider-config "factory-droid" "kimi-k3"))
               (caught (signals-error nle::provider-error
                         (nodecode-factory-droid::walk
                          (lambda (fold &rest keys)
                            (declare (ignore fold keys))
                            (error 'nle::provider-error :status 403 :detail "non-200 response"
                                                        :evidence-body "{\"error\":\"Model is not available in this region\"}"))
                          (lambda (&rest frame) (declare (ignore frame)))
                          :config config :headers '()))))
          (is (equal "kimi-k3 is not served from your network's region. Choose another model."
                     (and caught (nle::provider-error-note caught)))))))))

(deftest factory-droid-cell-responses-round-for-gpt ()
  (with-factory-droid-round (url headers body) ("gpt-5.5")
    (is (equal "https://api.factory.ai/api/llm/o/v1/responses" url))
    (is (equal "org-bHuLtG1fGmYk5YaOihAAXFBw" (factory-droid-header headers "OpenAI-Platform")))
    (is (uiop:string-prefix-p "You are Droid, an AI software engineering agent built by Factory."
                              (nlk:json-value body :string "instructions")))
    (is (equal "medium" (nlk:json-value body :string "reasoning" "effort")))
    (is (equal "auto" (nlk:json-value body :string "reasoning" "summary")))
    (is (equalp #("reasoning.encrypted_content") (nlk:json-value body :array "include")))
    (is (equal "low" (nlk:json-value body :string "text" "verbosity")))
    (is (equal (factory-droid-header headers "x-session-id") (nlk:json-value body :string "prompt_cache_key")))
    (is (equal (factory-droid-header headers "x-session-id") (nlk:json-value body :string "safety_identifier")))
    (is (equal "24h" (nlk:json-value body :string "prompt_cache_retention")))
    (is (null (nlk:json-value body :any "max_output_tokens")) "no output cap for GPT")
    (is (null (nlk:json-value body :any "temperature")))
    (is-present (tools (nlk:json-value body :array "tools")) "the round carries the core's tools"
      (is (every (lambda (tool) (multiple-value-bind (strict present) (gethash "strict" tool)
                                  (and present (null strict))))
                 tools)
          "every tool non-strict")
      (is (eq t (nlk:json-value body :boolean "parallel_tool_calls")))
      (is (equal "auto" (nlk:json-value body :string "tool_choice"))))))

(deftest factory-droid-cell-responses-round-for-grok ()
  (with-factory-droid-round (url headers body) ("grok-4.6" :effort "xhigh")
    (is (equal "xai" (factory-droid-header headers "x-api-provider")))
    (is (equal "session_lock" (factory-droid-header headers "x-provider-routing-source"))
        "an xAI route locks the session")
    (is (null (factory-droid-header headers "OpenAI-Platform")))
    (is (equal "xhigh" (nlk:json-value body :string "reasoning" "effort")))
    (is (null (nlk:json-value body :any "reasoning" "summary")) "no summary for Grok")
    (is (= 63356 (nlk:json-value body :integer "max_output_tokens")))
    (is (null (nlk:json-value body :any "text")))
    (is (null (nlk:json-value body :any "tool_choice")))))

(deftest factory-droid-cell-responses-body-with-tools ()
  (let ((body (nlk:json-object "model" "gpt-5.2" "instructions" "sys" "temperature" 0.5
                               "tools" (vector (nlk:json-object "type" "function" "name" "eval"))))
        (facts (list :model "gpt-5.2" :upstream "openai" :session "s" :effort nil)))
    (nodecode-factory-droid::responses-body body facts)
    (let ((tool (aref (nlk:json-value (nlk:decode-json (nlk:encode-json-object body)) :array "tools") 0)))
      (is (multiple-value-bind (strict present) (gethash "strict" tool) (and present (null strict)))
          "strict is stated on the wire, as false"))
    (is (eq t (gethash "parallel_tool_calls" body)))
    (is (equal "auto" (gethash "tool_choice" body)))
    (is (null (gethash "reasoning" body)) "off is no reasoning field")
    (is (null (gethash "prompt_cache_retention" body)) "gpt-5.2 keeps no 24h cache")
    (is (null (gethash "safety_identifier" body)))
    (is (null (gethash "temperature" body)))))

(deftest factory-droid-cell-messages-round-for-claude ()
  (with-factory-droid-round (url headers body) ("claude-opus-5")
    (is (equal "https://api.factory.ai/api/llm/a/v1/messages" url))
    (is (equal "placeholder" (factory-droid-header headers "x-api-key")) "the SDK's placeholder, not the token")
    (is (search "Bearer " (factory-droid-header headers "Authorization")))
    (is (equal "0.70.1" (factory-droid-header headers "X-Stainless-Package-Version")))
    (is (equal "600" (factory-droid-header headers "X-Stainless-Timeout")))
    (is (equal "2023-06-01" (factory-droid-header headers "anthropic-version")))
    (is (equal "anthropic" (factory-droid-header headers "x-api-provider")))
    (is (equal "You are Droid, an AI software engineering agent built by Factory."
               (nlk:json-value (aref (nlk:json-value body :array "system") 0) :string "text")))
    (is (= 2 (length (nlk:json-value body :array "system"))) "then the prompt, its own block")
    (is (= 128000 (nlk:json-value body :integer "max_tokens")))
    (is (equal "adaptive" (nlk:json-value body :string "thinking" "type")))
    (is (equal "summarized" (nlk:json-value body :string "thinking" "display")))
    (is (equal "high" (nlk:json-value body :string "output_config" "effort")))
    (is (null (nlk:json-value body :any "context_management")) "droid never sends it")
    (is-present (tools (nlk:json-value body :array "tools")) "the round carries the core's tools"
      (is (every (lambda (tool) (eq t (gethash "eager_input_streaming" tool))) tools))
      (is (search "fine-grained-tool-streaming-2025-05-14" (factory-droid-header headers "anthropic-beta"))))))

(deftest factory-droid-cell-messages-budget-models ()
  (with-factory-droid-round (url headers body) ("claude-opus-4-5-20251101")
    (is (null (nlk:json-value body :any "thinking")) "Opus 4.5 is off by default")
    (is (null (nlk:json-value body :any "output_config"))))
  (with-factory-droid-round (url headers body) ("claude-opus-4-5-20251101" :effort "high")
    (is (equal "enabled" (nlk:json-value body :string "thinking" "type")))
    (is (= 24576 (nlk:json-value body :integer "thinking" "budget_tokens")))
    (is (equal "high" (nlk:json-value body :string "output_config" "effort")))
    (is (search "effort-2025-11-24" (factory-droid-header headers "anthropic-beta"))))
  (with-factory-droid-round (url headers body) ("claude-sonnet-4-5-20250929" :effort "medium")
    (is (= 12288 (nlk:json-value body :integer "thinking" "budget_tokens")))
    (is (null (nlk:json-value body :any "output_config")))
    (is (search "interleaved-thinking-2025-05-14" (factory-droid-header headers "anthropic-beta"))))
  (with-factory-droid-round (url headers body) ("claude-opus-4-8-fast")
    (is (equal "fast" (nlk:json-value body :string "speed")))
    (is (search "fast-mode-2026-02-01" (factory-droid-header headers "anthropic-beta")))))

(deftest factory-droid-cell-strips-thinking-from-a-history-it-no-longer-leads ()
  (let ((led (vector (nlk:decode-json "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"a\"}]}")
                     (nlk:decode-json "{\"role\":\"assistant\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"t\"},{\"type\":\"text\",\"text\":\"b\"}]}")
                     (nlk:decode-json "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"c\"}]}")))
        (unled (vector (nlk:decode-json "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"a\"}]}")
                       (nlk:decode-json "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"b\"},{\"type\":\"thinking\",\"thinking\":\"t\"}]}")
                       (nlk:decode-json "{\"role\":\"user\",\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"x\"}]}"))))
    (is (not (nodecode-factory-droid::strip-thinking-p led)))
    (is (nodecode-factory-droid::strip-thinking-p unled))
    (let ((stripped (nodecode-factory-droid::without-thinking unled)))
      (is (= 1 (length (nlk:json-value (aref stripped 1) :array "content"))))
      (is (= 2 (length (nlk:json-value (aref unled 1) :array "content"))) "the history itself is not touched"))))

(deftest factory-droid-cell-gemini-round ()
  (with-factory-droid-round (url headers body) ("gemini-3.8-flash" :effort "medium")
    (is (equal "https://api.factory.ai/api/llm/g/v1/generate" url))
    (is (search "Bearer " (factory-droid-header headers "Authorization")))
    (is (null (factory-droid-header headers "x-goog-api-key")) "the lane's key header goes")
    (is (equal "*/*" (factory-droid-header headers "Accept")))
    (is (null (factory-droid-header headers "X-Stainless-Lang")))
    (is (equal "google" (factory-droid-header headers "x-api-provider")))
    (is (equal "gemini-3.8-flash" (nlk:json-value body :string "model")))
    (is (uiop:string-prefix-p "You are Droid, an AI software engineering agent built by Factory.
"
                              (nlk:json-value (aref (nlk:json-value body :array "systemInstruction" "parts") 0)
                                              :string "text")))
    (let ((generation (nlk:json-value body :object "generationConfig")))
      (is (= 1 (nlk:json-value generation :number "temperature")))
      (is (= 0.95 (nlk:json-value generation :number "topP")))
      (is (= 64 (nlk:json-value generation :number "topK")))
      (is (equal "MEDIUM" (nlk:json-value generation :string "thinkingConfig" "thinkingLevel")))
      (is (null (nlk:json-value generation :any "maxOutputTokens"))))
    (is (null (nlk:json-value body :any "toolConfig")))
    (is-present (declarations (nlk:json-value (aref (nlk:json-value body :array "tools") 0)
                                              :array "functionDeclarations"))
        "the round carries the core's tools"
      (is (nlk:json-value (aref declarations 0) :object "parameters") "under parameters, projected")
      (is (null (nlk:json-value (aref declarations 0) :any "parametersJsonSchema")))))
  (is (equal "HIGH" (nodecode-factory-droid::thinking-level "gemini-3.5-flash" "medium"))
      "a model without a medium level hears high")
  (is (equal "LOW" (nodecode-factory-droid::thinking-level "gemini-3.5-flash" "minimal"))))

(deftest factory-droid-cell-gemini-contents-and-names ()
  (let* ((contents (nlk:decode-json "[{\"role\":\"user\",\"parts\":[{\"text\":\"go\"}]},
{\"role\":\"model\",\"parts\":[{\"functionCall\":{\"name\":\"my.tool\",\"args\":{}}},{\"functionCall\":{\"name\":\"eval\",\"args\":{}},\"thoughtSignature\":\"sig\"}]},
{\"role\":\"user\",\"parts\":[{\"functionResponse\":{\"name\":\"my.tool\",\"response\":{\"name\":\"my.tool\",\"content\":\"done\"}}}]},
{\"role\":\"user\",\"parts\":[{\"functionResponse\":{\"name\":\"eval\",\"response\":{\"name\":\"eval\",\"content\":\"\"}}}]},
{\"role\":\"model\",\"parts\":[]}]"))
         (out (nodecode-factory-droid::factory-contents contents)))
    (is (= 3 (length out)) "the two tool results are one turn, the empty model turn goes")
    (let ((calls (nlk:json-value (aref out 1) :array "parts"))
          (results (nlk:json-value (aref out 2) :array "parts")))
      (is (equal "my_tool" (nlk:json-value (aref calls 0) :string "functionCall" "name")))
      (is (equal "skip_thought_signature_validator" (nlk:json-value (aref calls 0) :string "thoughtSignature")))
      (is (equal "sig" (nlk:json-value (aref calls 1) :string "thoughtSignature")))
      (is (equal "done" (nlk:json-value (aref results 0) :string "functionResponse" "response" "result")))
      (is (equal "Tool execution succeeded."
                 (nlk:json-value (aref results 1) :string "functionResponse" "response" "result")))))
  (is (= 73 (length (nodecode-factory-droid::wire-tool-name (make-string 70 :initial-element #\a))))
      "a long name is cut at 64 and tagged")
  (let* ((names (let ((table (make-hash-table :test 'equal))) (setf (gethash "my_tool" table) "my.tool") table))
         (seen nil)
         (fold (nodecode-factory-droid::named-back-fold (lambda (frame finish record)
                                                           (declare (ignore finish record))
                                                           (setf seen frame))
                                                         names)))
    (funcall fold (nlk:decode-json "{\"candidates\":[{\"content\":{\"parts\":[{\"functionCall\":{\"name\":\"my_tool\",\"args\":{}}}]}}]}")
             nil nil)
    (is (equal "my.tool" (nlk:json-value (aref (nlk:json-value (aref (nlk:json-value seen :array "candidates") 0)
                                                               :array "content" "parts")
                                               0)
                                         :string "functionCall" "name"))
        "a returned call dispatches to the tool that was advertised")))

(deftest factory-droid-cell-gemini-schema-projection ()
  (let ((schema (nodecode-factory-droid::gemini-schema
                 (nlk:decode-json "{\"type\":\"object\",\"additionalProperties\":false,\"properties\":{\"mode\":{\"const\":\"fast\"},\"n\":{\"type\":[\"integer\",\"null\"]},\"pick\":{\"anyOf\":[{\"type\":\"string\"},{\"type\":\"null\"}]},\"tags\":{\"items\":{\"enum\":[1,\"b\"]}}},\"required\":[\"mode\"]}"))))
    (is (null (nth-value 1 (gethash "additionalProperties" schema))) "an unknown keyword goes")
    (is (equalp #("fast") (nlk:json-value schema :array "properties" "mode" "enum")))
    (is (equal "string" (nlk:json-value schema :string "properties" "mode" "type")) "an enum is a string")
    (is (equal "integer" (nlk:json-value schema :string "properties" "n" "type")))
    (is (eq t (nlk:json-value schema :boolean "properties" "n" "nullable")))
    (is (equal "string" (nlk:json-value schema :string "properties" "pick" "type")))
    (is (eq t (nlk:json-value schema :boolean "properties" "pick" "nullable")))
    (is (equal "array" (nlk:json-value schema :string "properties" "tags" "type")))
    (is (equalp #("1" "b") (nlk:json-value schema :array "properties" "tags" "items" "enum")))
    (is (equalp #("mode") (nlk:json-value schema :array "required")))))

(deftest factory-droid-cell-leaves-other-providers-alone ()
  (with-cell-stop ((factory-droid-start))
    (let ((nle::*provider* "anthropic") (nle::*model* "claude-opus-5") (nle::*api-key* "k")
          (nle::*reasoning-effort* nil) (nle::*endpoint* nil) (headers nil) (body nil) (url nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (factory-droid-stream "anthropic") 200))
        (nle::call-provider (user-context)))
      (is (equal "https://api.anthropic.com/v1/messages" url))
      (is (equal "k" (factory-droid-header headers "x-api-key")))
      (is (null (factory-droid-header headers "X-Factory-Client")))
      (is (not (search "Droid" (nlk:encode-json-object body)))))))

;;; --- the sign-in ---------------------------------------------------------------

(defmacro with-factory-droid-sign-in ((requests pauses) (&rest posts) (&rest gets) &body forms)
  "FORMS with the cell started, a temp auth.json as the store, and every
sign-in exchange answered in order: POSTS by dex:post, GETS by dex:get, each
a (URL BODY STATUS). REQUESTS collects (METHOD URL CONTENT HEADERS), PAUSES
each poll interval the flow would have slept."
  `(with-cell-stop ((factory-droid-start))
     (with-temp-auth (auth "{\"api_keys\":{\"other\":{\"provider\":\"other\",\"key\":\"k\"}}}")
       (let ((nle::*auth-file-path* (pathname auth))
             (,requests '()) (,pauses '()) (posts (list ,@posts)) (gets (list ,@gets)))
         (flet ((next-answer (method url args)
                  (push (list method url (getf args :content) (getf args :headers)) ,requests)
                  (let ((answer (if (eq method :post) (pop posts) (pop gets))))
                    (assert (equal url (first answer)) () "asked ~a, scripted ~a" url (first answer))
                    (values (second answer) (third answer)))))
           (with-stubbed-fdefinitions
               ((dex:post (url &rest args) (next-answer :post url args))
                (dex:get (url &rest args) (next-answer :get url args))
                (nodecode-factory-droid::pause (seconds flow) (push seconds ,pauses)))
             ,@forms))))))

(defparameter +factory-droid-device+
  '("https://api.workos.com/user_management/authorize/device"
    "{\"device_code\":\"dc-f\",\"user_code\":\"FCTR-1234\",\"verification_uri\":\"https://login.factory.ai/device\",\"verification_uri_complete\":\"https://login.factory.ai/device?user_code=FCTR-1234\",\"expires_in\":300,\"interval\":5}"
    200))

(deftest factory-droid-cell-login-resolves-the-org-and-region ()
  (let* ((exp (+ (factory-droid-now) 3000))
         (access (factory-droid-jwt "sub" "user_1" "external_org_id" "org_ext_1" "email" "claim@example.com" "exp" exp)))
    (with-factory-droid-sign-in (requests pauses)
        (+factory-droid-device+
         (list "https://api.workos.com/user_management/authenticate" "{\"error\":\"authorization_pending\"}" 400)
         (list "https://api.workos.com/user_management/authenticate"
               (format nil "{\"access_token\":\"~a\",\"refresh_token\":\"r-1\",\"organization_id\":\"org_workos_1\",~
                            \"user\":{\"id\":\"user_1\",\"email\":\"dev@example.com\"}}" access)
               200))
        ((list "https://api.factory.ai/api/cli/whoami"
               "{\"orgId\":\"org_ext_1\",\"region\":\"eu\",\"inferenceRegion\":\"eu\"}" 200))
      (let ((text (cell-entry "nodecode-factory-droid" "factory-droid" "login")))
        (is (search "https://login.factory.ai/device?user_code=FCTR-1234" text))
        (is (search "FCTR-1234" text)))
      (is (await (:timeout 5) (cell-notice "nodecode-factory-droid")))
      (is (search "signed in as dev@example.com, eu region" (second (cell-notice "nodecode-factory-droid"))))
      (let ((entry (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "factory-droid")))
        (is (equal access (nlk:json-value entry :string "access_token")))
        (is (equal "r-1" (nlk:json-value entry :string "refresh_token")))
        (is (= exp (nlk:json-value entry :integer "expires_at")) "the token's own exp")
        (is (equal "dev@example.com" (nlk:json-value entry :string "email")) "the answer's user outranks the claim")
        (is (equal "user_1" (nlk:json-value entry :string "account_id")))
        (is (equal "org_ext_1" (nlk:json-value entry :string "org_id")))
        (is (equal "org_workos_1" (nlk:json-value entry :string "active_organization_id")))
        (is (equal "eu" (nlk:json-value entry :string "region")))
        (is (equal "eu" (nlk:json-value entry :string "inference_region"))))
      (destructuring-bind (device pending done whoami) (reverse requests)
        (is (equal "client_id=client_01HNM792M5G5G1A2THWPXKFMXB" (third device)))
        (is (equal "application/json" (factory-droid-header (fourth device) "Accept")))
        (is (search "device_code=dc-f" (third pending)))
        (is (search "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Adevice_code" (third done)))
        (is (eq :get (first whoami)))
        (is (equal "org_ext_1" (factory-droid-header (fourth whoami) "X-Factory-Org-Id")))
        (is (equal (format nil "Bearer ~a" access) (factory-droid-header (fourth whoami) "Authorization"))))
      (is (equal '(5) pauses)))))

(deftest factory-droid-cell-login-refuses-an-unidentified-account ()
  (with-factory-droid-sign-in (requests pauses)
      (+factory-droid-device+
       (list "https://api.workos.com/user_management/authenticate"
             (format nil "{\"access_token\":\"~a\",\"refresh_token\":\"r\"}" (factory-droid-jwt "sub" "u")) 200))
      ((list "https://api.factory.ai/api/cli/whoami" "{\"error\":\"nope\"}" 401))
    (cell-entry "nodecode-factory-droid" "factory-droid" "login")
    (is (await (:timeout 5) (cell-notice "nodecode-factory-droid")))
    (is (search "identity check failed (401)" (second (cell-notice "nodecode-factory-droid"))))
    (is (null (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "factory-droid")))))

(deftest factory-droid-cell-refreshes-with-the-organization ()
  (let ((access (factory-droid-jwt "sub" "user_1" "external_org_id" "org_ext_1" "exp" (+ (factory-droid-now) 3600))))
    (with-factory-droid-sign-in (requests pauses)
        ((list "https://api.workos.com/user_management/authenticate"
               (format nil "{\"access_token\":\"~a\",\"refresh_token\":\"r-2\",\"organization_id\":\"org_workos_1\"}" access)
               200))
        ((list "https://api.eu.factory.ai/api/cli/whoami" "boom" 500))
      (nodecode-factory-droid::save-entry
       (nlk:json-object "access_token" "old" "refresh_token" "r-1" "expires_at" (+ (factory-droid-now) 5)
                        "email" "dev@example.com" "account_id" "user_1" "org_id" "org_ext_1"
                        "active_organization_id" "org_workos_1" "region" "eu" "inference_region" "eu")
       nle::*auth-file-path*)
      (let ((nle::*api-key* nil))
        (is (eq :oauth (nle::provider-auth-state "factory-droid" :auth-path nle::*auth-file-path*)))
        (is (null requests) "a probe costs no network")
        (let ((credential (nle::resolve-provider-credential "factory-droid" :auth-path nle::*auth-file-path*)))
          (is (equal access (nle:credential-key credential)))
          (is (equal "eu" (getf (nle:credential-attributes credential) :region)))))
      (destructuring-bind (refresh whoami) (reverse requests)
        (is (equal "grant_type=refresh_token&client_id=client_01HNM792M5G5G1A2THWPXKFMXB&refresh_token=r-1&organization_id=org_workos_1"
                   (third refresh)))
        (is (equal "https://api.eu.factory.ai/api/cli/whoami" (second whoami)) "the stored region's host"))
      (let ((entry (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "factory-droid")))
        (is (equal access (nlk:json-value entry :string "access_token")))
        (is (equal "r-2" (nlk:json-value entry :string "refresh_token")))
        (is (equal "eu" (nlk:json-value entry :string "region")) "a failed whoami keeps the same org's scope")
        (is (equal "dev@example.com" (nlk:json-value entry :string "email")))))))

(deftest factory-droid-cell-status-and-logout ()
  (with-factory-droid-sign-in (requests pauses) () ()
    (is (search "not signed in" (cell-entry "nodecode-factory-droid" "factory-droid" "status")))
    (nodecode-factory-droid::save-entry
     (nlk:json-object "access_token" "tok" "refresh_token" "r" "expires_at" (+ (factory-droid-now) 600)
                      "email" "dev@example.com" "org_id" "org_ext_1")
     nle::*auth-file-path*)
    (is (search "signed in as dev@example.com (org org_ext_1)"
                (cell-entry "nodecode-factory-droid" "factory-droid" "")))
    (is (search "signed out" (cell-entry "nodecode-factory-droid" "factory-droid" "logout")))
    (is (null (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "factory-droid")))))
