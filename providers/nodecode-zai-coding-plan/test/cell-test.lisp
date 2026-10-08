;;;; cell-test.lisp --- the zai-coding-plan cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every wire a stubbed dex:post or dex:get: nothing
;;;; touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "zai-coding-plan" "ZAI-CODING-PLAN-CELL-" :start nodecode-zai-coding-plan:start-cell)

(define-cell-lifecycle-tests "zai-coding-plan"
  (:hooks 'nle::models-catalog-table :credential 'nle::resolve-model-lane 'nle::lane-endpoint
          'nle::anthropic-request-body 'nle::request-body 'nle::walk-provider-stream)
  (:command "zai-coding-plan")
  (:refused ("base_url" 5) ("anthropic_base_url" 5)))

(defun zai-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun zai-anthropic-stream ()
  "One whole Messages answer, `ok'."
  (make-truncated-sse-stream
   "{\"type\":\"message_start\",\"message\":{\"id\":\"m1\",\"model\":\"glm-5.3\",\"usage\":{\"input_tokens\":3}}}"
   "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
   "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}"
   "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}"
   "{\"type\":\"message_stop\"}"))

(defun zai-chat-stream ()
  "One whole chat answer, `ok'."
  (make-truncated-sse-stream
   "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
   "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
   "[DONE]"))

(defmacro with-zai-round ((url headers body) (provider model &key effort (lane :chat)) &body forms)
  "FORMS after one round of PROVIDER's MODEL at EFFORT on LANE (:chat or
:anthropic), the cell started: URL, HEADERS and BODY (decoded) as dex:post saw them."
  `(with-cell-stop ((zai-coding-plan-start))
     (let ((nle::*provider* ,provider) (nle::*model* ,model) (nle::*api-key* "sk-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* ,effort)
           (nle::*model-capability-memo* nil)
           (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values ,(if (eq lane :anthropic) '(zai-anthropic-stream) '(zai-chat-stream)) 200))
         ,(if (eq lane :anthropic)
              '(nle::call-anthropic-streaming (user-context))
              '(nle::call-provider-streaming (user-context))))
       ,@forms)))

;;; --- the catalog, the lanes, the addresses ------------------------------------------

(deftest zai-coding-plan-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((zai-coding-plan-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "zai-coding-plan")))
      (is (equal "Z.AI (GLM Coding Plan, Sign in)" (nlk:json-value row :string "name")))
      (is (equal "https://api.z.ai/api/coding/paas/v4" (nlk:json-value row :string "api")))
      (is (gethash "glm-5.3" (nlk:json-value row :object "models")) "omp's zai rows are listed")
      (is (gethash "glm-5.3-flash" (nlk:json-value row :object "models")))
      (is (equal "anthropic" (nle::resolve-model-lane "zai-coding-plan" "glm-5.3"))
          "a Messages row rides the anthropic lane")
      (is (equal "openai-completions" (nle::resolve-model-lane "zai-coding-plan" "glm-5.3-flash"))
          "GLM-5.3-Flash rides the chat lane")
      (is (equal "https://api.z.ai/api/anthropic/v1/messages"
                 (nle::lane-endpoint "zai-coding-plan" "anthropic")))
      (is (equal "https://api.z.ai/api/coding/paas/v4/chat/completions"
                 (nle::lane-endpoint "zai-coding-plan" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "zai-coding-plan"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest zai-coding-plan-cell-addresses-follow-the-section ()
  (with-cell-stop ((zai-coding-plan-start "base_url" "https://relay.example/chat"
                                          "anthropic_base_url" "https://relay.example/anthropic/v1/"))
    (is (equal "https://relay.example/chat"
               (nlk:json-value (nle::models-catalog-table) :string "zai-coding-plan" "api")))
    (is (equal "https://relay.example/anthropic/v1/messages"
               (nle::lane-endpoint "zai-coding-plan" "anthropic")))))

(deftest zai-coding-plan-cell-keeps-the-core-path-for-what-models-dev-knows ()
  ;; models.dev serves the plan on the chat lane alone; a model only it lists
  ;; rides that lane at its base with the key /connect saved, as before.
  (with-catalog-fixture (catalog "{\"zai-coding-plan\":{\"id\":\"zai-coding-plan\",\"name\":\"Z.AI Coding Plan\",\"npm\":\"@ai-sdk/openai-compatible\",\"api\":\"https://api.z.ai/api/coding/paas/v4\",\"env\":[\"ZHIPU_API_KEY\"],\"models\":{\"glm-5.3-highspeed\":{\"id\":\"glm-5.3-highspeed\",\"name\":\"GLM-5.3 Highspeed\",\"tool_call\":true,\"limit\":{\"context\":200000,\"output\":131072}}}}}")
    (with-temp-auth (auth "{\"api_keys\":{\"zai-coding-plan\":{\"provider\":\"zai-coding-plan\",\"key\":\"sk-saved\"}}}")
      (with-cell-stop ((zai-coding-plan-start))
        (is (gethash "glm-5.3-highspeed" (nlk:json-value (nle::models-catalog-table) :object "zai-coding-plan" "models"))
            "models.dev's own models are kept under omp's")
        (is (equal "openai-completions" (nle::resolve-model-lane "zai-coding-plan" "glm-5.3-highspeed")))
        (let ((nle::*provider* "zai-coding-plan") (nle::*model* "glm-5.3-highspeed") (nle::*api-key* nil)
              (nle::*endpoint* nil) (nle::*auth-file-path* auth) (seen-url nil) (seen-headers nil))
          (with-stubbed-fdefinition (dex:post (asked &rest args)
                                     (setf seen-url asked seen-headers (getf args :headers))
                                     (values (zai-chat-stream) 200))
            (nle::call-provider-streaming (user-context)))
          (is (equal "https://api.z.ai/api/coding/paas/v4/chat/completions" seen-url))
          (is (equal "Bearer sk-saved" (zai-header seen-headers "authorization"))))))))

;;; --- the credential -------------------------------------------------------------------

(deftest zai-coding-plan-cell-answers-the-minted-key ()
  (with-cell-stop ((zai-coding-plan-start))
    (with-temp-auth (auth "{\"oauth_tokens\":{\"zai-coding-plan\":{\"access_token\":\"id.secret\",\"email\":\"a@z.ai\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "ZAI_API_KEY") "sk-env"))
        (let ((credential (nle::resolve-provider-credential "zai-coding-plan" :auth-path auth :probe t)))
          (is (equal "id.secret" (nle:credential-key credential)) "the minted key outranks ZAI_API_KEY")
          (is (eq :oauth (nle:credential-source credential))))))))

(deftest zai-coding-plan-cell-reads-zai-api-key ()
  (with-cell-stop ((zai-coding-plan-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "ZAI_API_KEY") "sk-env"))
        (let ((credential (nle::resolve-provider-credential "zai-coding-plan" :auth-path auth :probe t)))
          (is (equal "sk-env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))
        (is (not (equal "sk-env" (nle:credential-key
                                  (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t))))
            "another provider's ladder never reads ZAI_API_KEY")))))

(deftest zai-coding-plan-cell-never-sends-another-familys-key ()
  (with-cell-stop ((zai-coding-plan-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (cond ((equal name "OPENAI_API_KEY") "sk-openai")
                                       ((equal name "ANTHROPIC_API_KEY") "sk-ant")))
        (let ((credential (nle::resolve-provider-credential "zai-coding-plan" :auth-path auth :probe t)))
          (is (not (member (nle:credential-key credential) '("sk-openai" "sk-ant") :test #'equal))
              "neither lane family's default variable reaches Z.AI")
          (is (eq :public (nle:credential-source credential))))))))

(deftest zai-coding-plan-cell-saved-key-outranks-the-sign-in ()
  (with-cell-stop ((zai-coding-plan-start))
    (with-temp-auth (auth "{\"api_keys\":{\"zai-coding-plan\":{\"provider\":\"zai-coding-plan\",\"key\":\"sk-saved\"}},\"oauth_tokens\":{\"zai-coding-plan\":{\"access_token\":\"id.secret\"}}}")
      (is (equal "sk-saved" (nle:credential-key
                             (nle::resolve-provider-credential "zai-coding-plan" :auth-path auth :probe t)))))))

;;; --- one round per wire ------------------------------------------------------------------

(deftest zai-coding-plan-cell-sends-a-messages-round-as-omp-does ()
  (with-zai-round (url headers body) ("zai-coding-plan" "glm-5.3" :effort "max" :lane :anthropic)
    (is (equal "https://api.z.ai/api/anthropic/v1/messages" url))
    (is (equal "Bearer sk-test" (zai-header headers "authorization")) "the key rides as a bearer")
    (is (null (zai-header headers "x-api-key")) "and not as x-api-key")
    (is (equal "glm-5.3" (nlk:json-value body :string "model")))
    (is (equal "enabled" (nlk:json-value body :string "thinking" "type")) "budget thinking, not adaptive")
    (is (eql 32768 (nlk:json-value body :integer "thinking" "budget_tokens")) "omp's max budget")
    (is (equal "summarized" (nlk:json-value body :string "thinking" "display")))
    (is (equal "max" (nlk:json-value body :string "output_config" "effort"))
        "GLM-5.3 names its effort beside the budget")))

(deftest zai-coding-plan-cell-a-budget-model-sends-no-effort ()
  (with-zai-round (url headers body) ("zai-coding-plan" "glm-4.7" :effort "xhigh" :lane :anthropic)
    (is (equal "enabled" (nlk:json-value body :string "thinking" "type")))
    (is (eql 32768 (nlk:json-value body :integer "thinking" "budget_tokens")))
    (is (null (nlk:json-value body :object "output_config")) "a budget-only model takes the block alone")))

(deftest zai-coding-plan-cell-sends-flash-on-the-chat-endpoint ()
  (with-zai-round (url headers body) ("zai-coding-plan" "glm-5.3-flash" :effort "high")
    (is (equal "https://api.z.ai/api/coding/paas/v4/chat/completions" url))
    (is (equal "Bearer sk-test" (zai-header headers "authorization")))
    (is (equal "glm-5.3-flash" (nlk:json-value body :string "model")))
    (is (equal "high" (nlk:json-value body :string "reasoning_effort")))
    (is (equal "enabled" (nlk:json-value body :string "thinking" "type")) "Z.AI's thinking switch")))

(deftest zai-coding-plan-cell-leaves-other-providers-alone ()
  (with-zai-round (url headers body) ("openai-completions" "glm-5.3" :effort "high")
    (is (null (nlk:json-value body :object "thinking")))
    (is (stringp url))))

;;; --- the sign-in ---------------------------------------------------------------------------

(defun zai-notice ()
  "The newest notice said, or NIL."
  (first (first (nlk:notice-log :limit 1))))

(deftest zai-coding-plan-cell-parses-what-is-pasted ()
  (flet ((parsed (text) (multiple-value-list (nodecode-zai-coding-plan::parse-callback-input text))))
    (is (equal '("c1" "s1") (parsed "zcode://zai-auth/callback?code=c1&state=s1")))
    (is (equal '("c 2" "s2") (parsed "?code=c+2&state=s2")))
    (is (equal '("c3" "s3") (parsed "c3#s3")))
    (is (equal '("c4" nil) (parsed "  c4 ")))))

(deftest zai-coding-plan-cell-signs-in-and-mints-a-key ()
  (with-cell-stop ((zai-coding-plan-start))
    (with-temp-auth (auth "{\"api_keys\":{\"other\":{\"provider\":\"other\",\"key\":\"k\"}}}")
      (let ((nle::*auth-file-path* auth) (posts '()) (gets '()))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name) nil)
             (dex:post (url &rest args)
              (push (list url (getf args :headers) (nlk:decode-json (getf args :content))) posts)
              (values (cond ((search "/oauth/token" url)
                             "{\"code\":0,\"data\":{\"zai\":{\"access_token\":\"oauth-tok\"},\"user\":{\"email\":\"a@z.ai\",\"id\":42}}}")
                            ((search "/auth/z/login" url) "{\"code\":200,\"data\":{\"access_token\":\"biz-tok\"}}")
                            ((search "/api_keys" url) "{\"code\":200,\"success\":true,\"data\":{\"apiKey\":\"key-id\"}}")
                            (t "{}"))
                      200))
             (dex:get (url &rest args)
              (push (list url (getf args :headers)) gets)
              (values (cond ((search "getCustomerInfo" url)
                             "{\"code\":200,\"data\":{\"organizations\":[{\"organizationId\":\"o2\",\"projects\":[]},{\"organizationId\":\"o1\",\"isDefault\":true,\"projects\":[{\"projectId\":\"p1\",\"isDefault\":true}]}]}}")
                            ((search "/copy/" url) "{\"code\":200,\"data\":{\"secretKey\":\"sec\"}}")
                            ((search "/api_keys" url) "{\"code\":200,\"data\":{\"list\":[{\"name\":\"zcode-api-key\",\"apiKey\":\"theirs\"}]}}")
                            (t "{}"))
                      200)))
          (let* ((answer (nodecode-zai-coding-plan::run-command "login" "s1"))
                 (address (subseq answer (search "https://" answer) (position #\Newline answer :start (search "https://" answer))))
                 (state (nodecode-zai-coding-plan::param
                         (nodecode-zai-coding-plan::query-params (nodecode-zai-coding-plan::url-query address))
                         "state")))
            (is (uiop:string-prefix-p "https://chat.z.ai/api/oauth/authorize?client_id=client_P8X5CMWmlaRO9gyO-KSqtg&response_type=code&redirect_uri=zcode%3A%2F%2Fzai-auth%2Fcallback&state=" address)
                "the command answers ZCode's authorize request at once")
            (is (= 32 (length state)))
            (is (search "another sign-in" (nodecode-zai-coding-plan::run-command "code zcode://zai-auth/callback?code=c&state=other" "s1"))
                "a paste from another sign-in is refused")
            (is (search "received" (nodecode-zai-coding-plan::run-command
                                    (format nil "code zcode://zai-auth/callback?code=the-code&state=~a" state) "s1")))
            (is (await (:timeout 10) (search "signed in" (or (zai-notice) ""))) "the outcome is a notice")
            (let ((token (find-if (lambda (post) (search "/oauth/token" (first post))) posts)))
              (is (equal "https://zcode.z.ai/api/v1/oauth/token" (first token)))
              (is (equal "zai" (nlk:json-value (third token) :string "provider")))
              (is (equal "the-code" (nlk:json-value (third token) :string "code")))
              (is (equal "zcode://zai-auth/callback" (nlk:json-value (third token) :string "redirect_uri")))
              (is (equal state (nlk:json-value (third token) :string "state"))))
            (let ((login (find-if (lambda (post) (search "/auth/z/login" (first post))) posts))
                  (create (find-if (lambda (post) (search "/api_keys" (first post))) posts)))
              (is (equal "oauth-tok" (nlk:json-value (third login) :string "token")))
              (is (equal "https://api.z.ai/api/biz/v1/organization/o1/projects/p1/api_keys" (first create))
                  "the default organization's default project")
              (is (equal "nodecode" (nlk:json-value (third create) :string "name")) "a key of Nodecode's own")
              (is (equal "Bearer biz-tok" (zai-header (second create) "Authorization"))))
            (is (find-if (lambda (get) (search "/api_keys/copy/key-id" (first get))) gets) "the secret is copied out")
            (let* ((stored (nlk:decode-json (uiop:read-file-string auth)))
                   (entry (nlk:json-value stored :object "oauth_tokens" "zai-coding-plan")))
              (is (equal "key-id.sec" (nlk:json-value entry :string "access_token")))
              (is (equal "a@z.ai" (nlk:json-value entry :string "email")))
              (is (equal "42" (nlk:json-value entry :string "account_id")))
              (is (null (nlk:json-value entry :any "expires_at")) "the key never expires")
              (is (equal "k" (nlk:json-value stored :string "api_keys" "other" "key")) "every other field is kept")
              (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat auth))))))
            (is (equal "key-id.sec" (nle:credential-key
                                     (nle::resolve-provider-credential "zai-coding-plan" :auth-path auth :probe t)))
                "the next round sends the minted key")
            (is (search "Signed out" (nodecode-zai-coding-plan::run-command "logout" "s1")))
            (is (null (nlk:json-value (nlk:decode-json (uiop:read-file-string auth))
                                      :object "oauth_tokens" "zai-coding-plan")))))))))

(deftest zai-coding-plan-cell-a-refused-exchange-is-said ()
  (with-cell-stop ((zai-coding-plan-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*auth-file-path* auth))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name) nil)
             (dex:post (url &rest args) (values "{\"code\":1001,\"msg\":\"bad code\"}" 200)))
          (let* ((answer (nodecode-zai-coding-plan::run-command "login" "s1"))
                 (state (nodecode-zai-coding-plan::flow-state nodecode-zai-coding-plan::*flow*)))
            (is (search "chat.z.ai" answer))
            (nodecode-zai-coding-plan::run-command (format nil "code c#~a" state) "s1")
            (is (await (:timeout 10) (search "failed" (or (zai-notice) ""))))
            (is (search "missing access token" (zai-notice)))
            (is (null (nlk:json-value (nlk:decode-json (uiop:read-file-string auth)) :object "oauth_tokens"))
                "nothing is kept")))))))
