;;;; cell-test.lisp --- the gitlab-duo cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every token variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every wire to GitLab a stubbed dex:post. The
;;;; sign-in's callback server is real, on a loopback port of the test's own,
;;;; and is dialled from here: nothing leaves the machine.

(in-package #:nodecode.test)

(define-test-slice "gitlab-duo" "GITLAB-DUO-CELL-" :start nodecode-gitlab-duo:start-cell)

(define-cell-lifecycle-tests "gitlab-duo"
  (:hooks 'nle::models-catalog-table :credential 'nle::resolve-model-lane 'nle::lane-endpoint
          'nle::anthropic-request-body 'nle::request-body 'nle::responses-request-body
          'nle::walk-provider-stream)
  (:command "gitlab-duo")
  (:refused ("gitlab_url" 5) ("gateway_url" 5)))

(defun gitlab-duo-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun gitlab-duo-now ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun gitlab-duo-stream (lane)
  "One whole answer, `ok', on LANE's wire."
  (ecase lane
    (:anthropic
     (make-truncated-sse-stream
      "{\"type\":\"message_start\",\"message\":{\"id\":\"m1\",\"model\":\"claude-opus-4-6\",\"usage\":{\"input_tokens\":3}}}"
      "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
      "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}"
      "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}"
      "{\"type\":\"message_stop\"}"))
    (:chat
     (make-truncated-sse-stream
      "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
      "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
      "[DONE]"))
    (:responses
     (make-truncated-sse-stream
      "{\"type\":\"response.created\",\"response\":{\"id\":\"r1\",\"model\":\"gpt-5-codex\"}}"
      "{\"type\":\"response.output_text.delta\",\"item_id\":\"m1\",\"output_index\":0,\"delta\":\"ok\"}"
      "{\"type\":\"response.completed\",\"response\":{\"id\":\"r1\",\"status\":\"completed\",\"usage\":{\"input_tokens\":3,\"output_tokens\":1}}}"))))

(defparameter *gitlab-duo-grant*
  "{\"token\":\"duo-grant\",\"headers\":{\"x-gitlab-realm\":\"saas\",\"x-gitlab-instance-id\":\"inst-1\"}}"
  "What the direct-access exchange answers.")

(defmacro with-gitlab-duo-rounds ((posts) &body forms)
  "FORMS with the cell started, the GitLab token glpat-test frozen into every
round, and dex:post answering the direct-access exchange and each round,
every request pushed onto POSTS as (URL HEADERS BODY)."
  `(with-cell-stop ((gitlab-duo-start))
     (let ((nle::*provider* "gitlab-duo") (nle::*api-key* "glpat-test") (nle::*endpoint* nil)
           (nle::*model-capability-memo* nil) (,posts '()))
       (with-stubbed-fdefinition
           (dex:post (url &rest args)
            (push (list url (getf args :headers) (ignore-errors (nlk:decode-json (getf args :content)))) ,posts)
            (cond ((search "direct_access" url) (values *gitlab-duo-grant* 200))
                  ((search "/anthropic/" url) (values (gitlab-duo-stream :anthropic) 200))
                  ((search "/responses" url) (values (gitlab-duo-stream :responses) 200))
                  (t (values (gitlab-duo-stream :chat) 200))))
         ,@forms))))

;;; --- the catalog, the routes, the addresses ---------------------------------------------

(deftest gitlab-duo-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((gitlab-duo-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "gitlab-duo")))
      (is (equal "GitLab Duo Non-Agentic" (nlk:json-value row :string "name")))
      (is (equal "https://cloud.gitlab.com/ai/v1/proxy/anthropic/v1" (nlk:json-value row :string "api")))
      (is (gethash "duo-chat-opus-4-6" (nlk:json-value row :object "models")))
      (is (equal "anthropic" (nle::resolve-model-lane "gitlab-duo" "duo-chat-opus-4-6")))
      (is (equal "anthropic" (nle::resolve-model-lane "gitlab-duo" "claude-sonnet-4-5-20250929")))
      (is (equal "openai-responses" (nle::resolve-model-lane "gitlab-duo" "duo-chat-gpt-5-codex")))
      (is (equal "openai-completions" (nle::resolve-model-lane "gitlab-duo" "duo-chat-gpt-5-1")))
      (is (equal "openai-completions" (nle::resolve-model-lane "gitlab-duo" "gpt-5.1-2025-11-13"))
          "omp's route, not the row's api, decides the wire")
      (is (equal "https://cloud.gitlab.com/ai/v1/proxy/anthropic/v1/messages"
                 (nle::lane-endpoint "gitlab-duo" "anthropic")))
      (is (equal "https://cloud.gitlab.com/ai/v1/proxy/openai/v1/responses"
                 (nle::lane-endpoint "gitlab-duo" "openai-responses")))
      (is (equal "https://cloud.gitlab.com/ai/v1/proxy/openai/v1/chat/completions"
                 (nle::lane-endpoint "gitlab-duo" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "gitlab-duo")))))

(deftest gitlab-duo-cell-maps-aliases-to-upstream-models ()
  (is (equal "claude-opus-4-6" (nodecode-gitlab-duo::upstream-id "duo-chat-opus-4-6")))
  (is (equal "gpt-5.2-2025-12-11" (nodecode-gitlab-duo::upstream-id "duo-chat-gpt-5-2")))
  (is (equal "claude-haiku-4-5-20251001" (nodecode-gitlab-duo::upstream-id "claude-haiku-4-5-20251001"))
      "an upstream id is asked for as it is")
  (is (null (nodecode-gitlab-duo::upstream-id "duo-chat-unknown"))))

;;; --- the credential ----------------------------------------------------------------------

(deftest gitlab-duo-cell-credential-ladder ()
  (with-cell-stop ((gitlab-duo-start))
    (with-temp-auth (auth (format nil "{\"oauth_tokens\":{\"gitlab-duo\":{\"access_token\":\"gl-oauth\",\"refresh_token\":\"r1\",\"expires_at\":~d}}}"
                                  (+ (gitlab-duo-now) 3600)))
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "GITLAB_TOKEN") "glpat-env"))
        (let ((credential (nle::resolve-provider-credential "gitlab-duo" :auth-path auth :probe t)))
          (is (equal "gl-oauth" (nle:credential-key credential)) "the sign-in outranks GITLAB_TOKEN")
          (is (eq :oauth (nle:credential-source credential))))))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "GITLAB_TOKEN") "glpat-env"))
        (is (equal "glpat-env" (nle:credential-key
                                (nle::resolve-provider-credential "gitlab-duo" :auth-path auth :probe t)))))
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "ANTHROPIC_API_KEY") "sk-ant"))
        (let ((credential (nle::resolve-provider-credential "gitlab-duo" :auth-path auth :probe t)))
          (is (not (equal "sk-ant" (nle:credential-key credential)))
              "another provider's key never goes to GitLab")
          (is (eq :public (nle:credential-source credential))))))))

(deftest gitlab-duo-cell-refreshes-a-token-about-to-expire ()
  (with-cell-stop ((gitlab-duo-start))
    (let ((now (gitlab-duo-now)) (posts '()))
      (with-temp-auth (auth (format nil "{\"api_keys\":{\"x\":{\"provider\":\"x\",\"key\":\"k\"}},\"oauth_tokens\":{\"gitlab-duo\":{\"access_token\":\"old\",\"refresh_token\":\"r1\",\"expires_at\":~d}}}"
                                    (+ now 30)))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name) nil)
             (dex:post (url &rest args)
              (push (list url (getf args :headers) (getf args :content)) posts)
              (values (format nil "{\"access_token\":\"new\",\"refresh_token\":\"r2\",\"expires_in\":7200,\"created_at\":~d}" now)
                      200)))
          (let ((credential (nle::resolve-provider-credential "gitlab-duo" :auth-path auth)))
            (is (equal "new" (nle:credential-key credential)) "a token within a minute of expiry is refreshed first"))
          (let ((post (first posts)))
            (is (= 1 (length posts)))
            (is (equal "https://gitlab.com/oauth/token" (first post)))
            (is (equal "application/x-www-form-urlencoded" (gitlab-duo-header (second post) "Content-Type")))
            (is (equal "grant_type=refresh_token&client_id=da4edff2e6ebd2bc3208611e2768bc1c1dd7be791dc5ff26ca34ca9ee44f7d4b&refresh_token=r1"
                       (third post))))
          (let* ((stored (nlk:decode-json (uiop:read-file-string auth)))
                 (entry (nlk:json-value stored :object "oauth_tokens" "gitlab-duo")))
            (is (equal "new" (nlk:json-value entry :string "access_token")))
            (is (equal "r2" (nlk:json-value entry :string "refresh_token")) "the rotated refresh token is kept")
            (is (eql (+ now 7200 -300) (nlk:json-value entry :integer "expires_at"))
                "created_at plus expires_in, five minutes inside")
            (is (equal "k" (nlk:json-value stored :string "api_keys" "x" "key"))))
          (is (equal "new" (nle:credential-key (nle::resolve-provider-credential "gitlab-duo" :auth-path auth)))
              "and the fresh token is not refreshed again")
          (is (= 1 (length posts))))))))

(deftest gitlab-duo-cell-a-failed-refresh-says-sign-in-again ()
  (with-cell-stop ((gitlab-duo-start))
    (with-temp-auth (auth (format nil "{\"oauth_tokens\":{\"gitlab-duo\":{\"access_token\":\"old\",\"refresh_token\":\"r1\",\"expires_at\":~d}}}"
                                  (- (gitlab-duo-now) 10)))
      (with-stubbed-fdefinitions
          ((nle::credential-env (name) nil)
           (dex:post (url &rest args) (values "{\"error\":\"invalid_grant\"}" 400)))
        (let ((refusal (handler-case (progn (nle::resolve-provider-credential "gitlab-duo" :auth-path auth) nil)
                         (nle::credential-error (condition) (princ-to-string condition)))))
          (is (and refusal (search "/gitlab-duo login" refusal)))
          (is (search "invalid_grant" refusal))
          (is (search "sign in again"
                      (or (second (assoc "nodecode-gitlab-duo" (nodecode.kernel::standing-notices) :test #'string=)) ""))
              "and it stands where the model reads it, until a sign-in"))))))

(deftest gitlab-duo-cell-never-sends-another-familys-key ()
  (with-cell-stop ((gitlab-duo-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (cond ((equal name "OPENAI_API_KEY") "sk-openai")
                                       ((equal name "ANTHROPIC_API_KEY") "sk-ant")))
        (let ((credential (nle::resolve-provider-credential "gitlab-duo" :auth-path auth :probe t)))
          (is (not (member (nle:credential-key credential) '("sk-openai" "sk-ant") :test #'equal))
              "no lane family's default variable reaches GitLab")
          (is (eq :public (nle:credential-source credential))))))))

(deftest gitlab-duo-cell-a-probe-refreshes-nothing ()
  (with-cell-stop ((gitlab-duo-start))
    (with-temp-auth (auth (format nil "{\"oauth_tokens\":{\"gitlab-duo\":{\"access_token\":\"old\",\"refresh_token\":\"r1\",\"expires_at\":~d}}}"
                                  (- (gitlab-duo-now) 10)))
      (with-stubbed-fdefinition (dex:post (url &rest args) (error "a probe touched the network"))
        (is (equal "old" (nle:credential-key (nle::resolve-provider-credential "gitlab-duo" :auth-path auth :probe t)))
            "where the credential comes from is answered from the store alone")))))

;;; --- one round per wire -------------------------------------------------------------------

(deftest gitlab-duo-cell-sends-a-claude-round-through-the-anthropic-proxy ()
  (with-gitlab-duo-rounds (posts)
    (let ((nle::*model* "duo-chat-opus-4-6") (nle::*reasoning-effort* "xhigh"))
      (nle::call-anthropic-streaming (user-context)))
    (destructuring-bind (round exchange) posts
      (is (equal "https://gitlab.com/api/v4/ai/third_party_agents/direct_access" (first exchange)))
      (is (equal "Bearer glpat-test" (gitlab-duo-header (second exchange) "Authorization"))
          "the GitLab token buys the grant")
      (is (eq t (nlk:json-value (third exchange) :any "feature_flags" "DuoAgentPlatformNext")))
      (is (equal "https://cloud.gitlab.com/ai/v1/proxy/anthropic/v1/messages" (first round)))
      (is (equal "Bearer duo-grant" (gitlab-duo-header (second round) "authorization"))
          "the round sends the grant, not the GitLab token")
      (is (null (gitlab-duo-header (second round) "x-api-key")))
      (is (equal "saas" (gitlab-duo-header (second round) "x-gitlab-realm")) "with the gateway's headers")
      (is (equal "inst-1" (gitlab-duo-header (second round) "x-gitlab-instance-id")))
      (is (equal "2023-06-01" (gitlab-duo-header (second round) "anthropic-version")))
      (is (equal "claude-opus-4-6" (nlk:json-value (third round) :string "model")) "the upstream model")
      (is (equal "enabled" (nlk:json-value (third round) :string "thinking" "type")))
      (is (eql 32768 (nlk:json-value (third round) :integer "thinking" "budget_tokens")))
      (is (null (nlk:json-value (third round) :object "output_config")) "a budget-mode row sends no effort"))))

(deftest gitlab-duo-cell-sends-a-gpt-round-through-the-openai-proxy ()
  (with-gitlab-duo-rounds (posts)
    (let ((nle::*model* "duo-chat-gpt-5-1") (nle::*reasoning-effort* "high") (nle::*temperature* 0.5))
      (nle::call-provider-streaming (user-context))
      (nle::call-provider-streaming (user-context)))
    (is (= 1 (count-if (lambda (post) (search "direct_access" (first post))) posts))
        "the grant is reused for the next round")
    (let ((round (first posts)))
      (is (equal "https://cloud.gitlab.com/ai/v1/proxy/openai/v1/chat/completions" (first round)))
      (is (equal "Bearer duo-grant" (gitlab-duo-header (second round) "authorization")))
      (is (equal "gpt-5.1-2025-11-13" (nlk:json-value (third round) :string "model")))
      (is (equal "high" (nlk:json-value (third round) :string "reasoning_effort")))
      (is (null (nth-value 1 (gethash "temperature" (third round)))) "a GPT-5 front takes no sampling"))))

(deftest gitlab-duo-cell-sends-a-codex-round-on-the-responses-wire ()
  (with-gitlab-duo-rounds (posts)
    (let ((nle::*model* "duo-chat-gpt-5-codex"))
      (nle::call-responses-streaming (user-context)))
    (let ((round (first posts)))
      (is (equal "https://cloud.gitlab.com/ai/v1/proxy/openai/v1/responses" (first round)))
      (is (equal "Bearer duo-grant" (gitlab-duo-header (second round) "authorization")))
      (is (equal "gpt-5-codex" (nlk:json-value (third round) :string "model"))))))

(deftest gitlab-duo-cell-a-refused-grant-is-a-provider-error ()
  (with-cell-stop ((gitlab-duo-start))
    (let ((nle::*provider* "gitlab-duo") (nle::*model* "duo-chat-opus-4-6") (nle::*api-key* "glpat-test")
          (nle::*endpoint* nil))
      (with-stubbed-fdefinition (dex:post (url &rest args) (values "{\"message\":\"403 Forbidden\"}" 403))
        (let ((condition (handler-case (progn (nle::call-anthropic-streaming (user-context)) nil)
                           (nle::provider-error (it) it))))
          (is (eql 403 (and condition (nle::provider-error-status condition))))
          (is (search "GitLab Duo access denied" (nle::provider-error-detail condition))))))))

(deftest gitlab-duo-cell-leaves-other-providers-alone ()
  (with-cell-stop ((gitlab-duo-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "duo-chat-gpt-5-1") (nle::*api-key* "k")
          (nle::*endpoint* nil) (seen nil))
      (with-stubbed-fdefinition (dex:post (url &rest args)
                                 (push (list url (nlk:decode-json (getf args :content))) seen)
                                 (values (gitlab-duo-stream :chat) 200))
        (nle::call-provider-streaming (user-context)))
      (is (= 1 (length seen)) "no direct-access exchange")
      (is (equal "duo-chat-gpt-5-1" (nlk:json-value (second (first seen)) :string "model"))))))

;;; --- the sign-in -------------------------------------------------------------------------

(defun gitlab-duo-notice ()
  "The newest notice said, or NIL."
  (first (first (nlk:notice-log :limit 1))))

(defun gitlab-duo-local-get (port target)
  "The raw HTTP response 127.0.0.1:PORT answers GET TARGET with."
  (let* ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream socket)))
    (unwind-protect
         (progn
           (write-sequence (sb-ext:string-to-octets
                            (format nil "GET ~a HTTP/1.1~c~cHost: localhost~c~c~c~c"
                                    target #\Return #\Newline #\Return #\Newline #\Return #\Newline)
                            :external-format :latin-1)
                           stream)
           (force-output stream)
           (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
             (loop for byte = (read-byte stream nil nil) while byte do (vector-push-extend byte octets))
             (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8))) :external-format :utf-8)))
      (usocket:socket-close socket))))

(deftest gitlab-duo-cell-signs-in-through-the-callback ()
  (with-cell-stop ((gitlab-duo-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*auth-file-path* auth) (posts '()) (now (gitlab-duo-now))
            (nodecode-gitlab-duo::*callback-port* 0))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name) nil)
             (dex:post (url &rest args)
              (push (list url (getf args :headers) (getf args :content)) posts)
              (values (format nil "{\"access_token\":\"gl-at\",\"refresh_token\":\"gl-rt\",\"token_type\":\"Bearer\",\"expires_in\":7200,\"created_at\":~d}" now)
                      200)))
          (nodecode-gitlab-duo::run-command "login" "s1")
          (let* ((flow nodecode-gitlab-duo::*flow*)
                 (redirect (nodecode-gitlab-duo::flow-redirect-uri flow))
                 (port (parse-integer redirect :start (length "http://localhost:") :junk-allowed t)))
            (is (equal (format nil "http://localhost:~d/callback" port) redirect))
            (is (equal (format nil "https://gitlab.com/oauth/authorize?client_id=da4edff2e6ebd2bc3208611e2768bc1c1dd7be791dc5ff26ca34ca9ee44f7d4b&response_type=code&redirect_uri=http%3A%2F%2Flocalhost%3A~d%2Fcallback&scope=api&code_challenge=~a&code_challenge_method=S256&state=~a"
                               port (nodecode-gitlab-duo::flow-challenge flow) (nodecode-gitlab-duo::flow-state flow))
                       (nodecode-gitlab-duo::flow-url flow))
                "GitLab's standard authorize request with PKCE")
            (is (uiop:string-prefix-p "HTTP/1.1 500" (gitlab-duo-local-get port "/callback?code=forged&state=other"))
                "a callback with another state is refused")
            (is (null posts) "and exchanges nothing")
            (is (uiop:string-prefix-p "HTTP/1.1 200"
                                      (gitlab-duo-local-get port (format nil "/callback?code=gl-code&state=~a"
                                                                         (nodecode-gitlab-duo::flow-state flow)))))
            (is (await (:timeout 10) (search "signed in" (or (gitlab-duo-notice) ""))))
            (let ((post (first posts)))
              (is (equal "https://gitlab.com/oauth/token" (first post)))
              (is (equal (format nil "grant_type=authorization_code&client_id=da4edff2e6ebd2bc3208611e2768bc1c1dd7be791dc5ff26ca34ca9ee44f7d4b&code=gl-code&redirect_uri=http%3A%2F%2Flocalhost%3A~d%2Fcallback&code_verifier=~a"
                                 port (nodecode-gitlab-duo::flow-verifier flow))
                         (third post))))
            (let ((entry (nlk:json-value (nlk:decode-json (uiop:read-file-string auth)) :object "oauth_tokens" "gitlab-duo")))
              (is (equal "gl-at" (nlk:json-value entry :string "access_token")))
              (is (equal "gl-rt" (nlk:json-value entry :string "refresh_token")))
              (is (eql (+ now 7200 -300) (nlk:json-value entry :integer "expires_at"))))
            (is (search "Signed in" (nodecode-gitlab-duo::run-command "status" "s1")))
            (is (search "Signed out" (nodecode-gitlab-duo::run-command "logout" "s1")))))))))

(deftest gitlab-duo-cell-an-own-redirect-is-pasted ()
  ;; GITLAB_REDIRECT_URI naming another host: nothing listens, the code is pasted
  (with-cell-stop ((gitlab-duo-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*auth-file-path* auth))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name)
              (cond ((equal name "GITLAB_REDIRECT_URI") "https://example.test/oauth/done")
                    ((equal name "GITLAB_CLIENT_ID") "my-app")))
             (dex:post (url &rest args)
              (values "{\"access_token\":\"gl-at\",\"refresh_token\":\"gl-rt\",\"expires_in\":7200}" 200)))
          (let* ((answer (nodecode-gitlab-duo::run-command "login" "s1"))
                 (flow nodecode-gitlab-duo::*flow*))
            (is (null (nodecode-gitlab-duo::flow-sockets flow)) "nothing listens")
            (is (search "client_id=my-app" answer))
            (is (search "redirect_uri=https%3A%2F%2Fexample.test%2Foauth%2Fdone" answer))
            (is (search "received"
                        (nodecode-gitlab-duo::run-command
                         (format nil "code https://example.test/oauth/done?code=c1&state=~a"
                                 (nodecode-gitlab-duo::flow-state flow))
                         "s1")))
            (is (await (:timeout 10) (search "signed in" (or (gitlab-duo-notice) ""))))))))))
