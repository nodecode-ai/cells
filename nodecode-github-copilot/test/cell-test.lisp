;;;; cell-test.lisp --- the github-copilot cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every exchange with GitHub or Copilot a stubbed
;;;; dex:post or dex:get: nothing touches the network, the environment or the
;;;; operator's files.

(in-package #:nodecode.test)

(define-test-slice "github-copilot" "GITHUB-COPILOT-CELL-" :start nodecode-github-copilot:start-cell)

(define-cell-lifecycle-tests "github-copilot"
  (:hooks 'nle::models-catalog-table :credential 'nle::resolve-model-lane 'nle::lane-endpoint
          'nle::anthropic-request-body 'nle::walk-provider-stream 'nle::list-provider-models)
  (:command "github-copilot")
  (:refused ("base_url" 5)))

;;; --- fixtures -------------------------------------------------------------------

(defun copilot-said (fragment)
  "Whether a notice said lately carries FRAGMENT."
  (some (lambda (entry) (search fragment (first entry))) (nlk:notice-log :limit 50)))

(defun copilot-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun copilot-signed-in (&key (token "gho_signed") (expires 4000000000) enterprise endpoint)
  "An auth.json text holding a Copilot sign-in."
  (shasht:write-json
   (nlk:json-object "oauth_tokens"
                    (nlk:json-object "github-copilot"
                                     (nlk:json-object "access_token" token "refresh_token" token
                                                      "expires_at" expires
                                                      :opt "enterprise_url" enterprise
                                                      :opt "api_endpoint" endpoint)))
   nil))

(defun copilot-stream (lane)
  "A canned stream of the answer `ok' on LANE's wire."
  (cond ((equal lane "anthropic")
         (make-truncated-sse-stream
          "{\"type\":\"message_start\",\"message\":{\"id\":\"m\",\"model\":\"x\",\"usage\":{\"input_tokens\":3}}}"
          "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
          "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}"
          "{\"type\":\"content_block_stop\",\"index\":0}"
          "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}"
          "{\"type\":\"message_stop\"}"))
        ((equal lane "openai-responses")
         (make-truncated-sse-stream
          "{\"type\":\"response.output_text.delta\",\"item_id\":\"m\",\"delta\":\"ok\"}"
          "{\"type\":\"response.completed\",\"response\":{}}"))
        (t (make-truncated-sse-stream
            "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
            "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
            "[DONE]"))))

(defun copilot-round (context)
  "One round of CONTEXT on the lane its frozen config names, the way the turn
loop runs it: the lane's message."
  (let ((config (nle::compiled-turn-context-provider-config context)))
    (funcall (nle::provider-lane-stream-symbol
              (nle::find-lane-by-name (nle::effective-provider-config-lane config)))
             context)))

(defmacro with-copilot-round ((url headers body &key (auth '(copilot-signed-in)) (context '(user-context)))
                              model &body forms)
  "FORMS with the cell started, signed in as AUTH says, and one round of MODEL
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-cell-stop ((github-copilot-start))
     (with-temp-auth (auth ,auth)
       (let ((nle::*provider* "github-copilot") (nle::*model* ,model) (nle::*api-key* nil)
             (nle::*endpoint* nil) (nle::*auth-file-path* auth) (,url nil) (,headers nil) (,body nil))
         (declare (ignorable ,url ,headers ,body))
         (with-stubbed-fdefinition (nle::credential-env (name) nil)
           (with-stubbed-fdefinition
               (dex:post (asked &rest args)
                (setf ,url asked ,headers (getf args :headers)
                      ,body (nlk:decode-json (getf args :content)))
                (values (copilot-stream (nodecode-github-copilot::model-lane ,model)) 200))
             (copilot-round ,context)))
         ,@forms))))

;;; --- the catalog and the wires -----------------------------------------------------

(deftest github-copilot-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((github-copilot-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "github-copilot")))
      (is (equal "GitHub Copilot" (nlk:json-value row :string "name")))
      (is (equal "https://api.githubcopilot.com" (nlk:json-value row :string "api")))
      (is (gethash "claude-sonnet-4.5" (nlk:json-value row :object "models")) "the bundled models are listed")
      (is (equal "anthropic" (nle::resolve-model-lane "github-copilot" "claude-sonnet-4.5")) "Claude on Messages")
      (is (equal "openai-responses" (nle::resolve-model-lane "github-copilot" "gpt-5.5")) "GPT-5 on Responses")
      (is (equal "openai-completions" (nle::resolve-model-lane "github-copilot" "gpt-4.1")) "the rest on chat")
      (is (equal "anthropic" (nle::resolve-model-lane "github-copilot" "claude-opus-9")) "an unlisted Claude by omp's route")
      (is (equal "https://api.githubcopilot.com/v1/messages"
                 (nle::lane-endpoint "github-copilot" "anthropic")))
      (is (equal "https://api.githubcopilot.com/responses"
                 (nle::lane-endpoint "github-copilot" "openai-responses")))
      (is (equal "https://api.githubcopilot.com/chat/completions"
                 (nle::lane-endpoint "github-copilot" "openai-completions")))
      (is (equal "https://api.anthropic.com/v1/messages" (nle::lane-endpoint "anthropic" "anthropic"))
          "another provider's Messages address is its own"))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "github-copilot"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest github-copilot-cell-base-follows-the-section ()
  (with-cell-stop ((github-copilot-start "base_url" "https://relay.example"))
    (is (equal "https://relay.example" (nlk:json-value (nle::models-catalog-table) :string "github-copilot" "api")))
    (is (equal "https://relay.example/v1/messages" (nle::lane-endpoint "github-copilot" "anthropic")))))

;;; --- the credential --------------------------------------------------------------------

(deftest github-copilot-cell-reads-copilot-github-token ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "COPILOT_GITHUB_TOKEN") "ghp_env"))
        (let ((credential (nle::resolve-provider-credential "github-copilot" :auth-path auth :probe t)))
          (is (equal "ghp_env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))
        (is (not (equal "ghp_env"
                        (nle:credential-key (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t))))
            "another provider's ladder never reads COPILOT_GITHUB_TOKEN")))))

(deftest github-copilot-cell-asks-github-where-an-env-token-is-served ()
  (with-cell-stop ((github-copilot-start))
    (clrhash nodecode-github-copilot::*env-endpoints*)
    (with-temp-auth (auth "{}")
      (let ((asked '()))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name) (and (equal name "COPILOT_GITHUB_TOKEN") "ghp_env"))
             (dex:get (url &rest args)
              (push (list url (getf args :headers)) asked)
              (values "{\"endpoints\":{\"api\":\"https://api.individual.githubcopilot.com/\"}}" 200)))
          (let ((credential (nle::resolve-provider-credential
                             "github-copilot" :auth-path auth
                                              :endpoint "https://api.githubcopilot.com/chat/completions")))
            (is (equal "https://api.individual.githubcopilot.com/chat/completions"
                       (getf (nle:credential-attributes credential) :endpoint))
                "the plan's own host"))
          (nle::resolve-provider-credential "github-copilot" :auth-path auth
                                                             :endpoint "https://api.githubcopilot.com/responses")
          (nle::resolve-provider-credential "github-copilot" :auth-path auth :probe t))
        (is (= 1 (length asked)) "asked once per token, and never on a probe")
        (is (equal "https://api.github.com/copilot_internal/user" (first (first asked))))
        (is (equal "token ghp_env" (copilot-header (second (first asked)) "Authorization")))))))

(deftest github-copilot-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth "{\"api_keys\":{\"github-copilot\":{\"provider\":\"github-copilot\",\"key\":\"ghp_saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "COPILOT_GITHUB_TOKEN") "ghp_env"))
        (is (equal "ghp_saved"
                   (nle:credential-key (nle::resolve-provider-credential "github-copilot" :auth-path auth :probe t))))))))

(deftest github-copilot-cell-sign-in-answers-before-the-variable ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth (copilot-signed-in :endpoint "https://api.business.githubcopilot.com"))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "COPILOT_GITHUB_TOKEN") "ghp_env"))
        (let ((credential (nle::resolve-provider-credential
                           "github-copilot" :auth-path auth
                                            :endpoint "https://api.githubcopilot.com/v1/messages")))
          (is (equal "gho_signed" (nle:credential-key credential)))
          (is (eq :oauth (nle:credential-source credential)))
          (is (equal "https://api.business.githubcopilot.com/v1/messages"
                     (getf (nle:credential-attributes credential) :endpoint))
              "the plan host the sign-in learned"))
        (is (eq :oauth (nle::provider-auth-state "github-copilot" :auth-path auth)))))
    (with-temp-auth (auth (copilot-signed-in :enterprise "acme.ghe.com"))
      (let ((credential (nle::resolve-provider-credential
                         "github-copilot" :auth-path auth
                                          :endpoint "https://api.githubcopilot.com/responses")))
        (is (equal "https://copilot-api.acme.ghe.com/responses"
                   (getf (nle:credential-attributes credential) :endpoint))
            "an Enterprise sign-in is served at copilot-api.<domain>")))))

(deftest github-copilot-cell-never-sends-another-familys-key ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (member name '("OPENAI_API_KEY" "ANTHROPIC_API_KEY") :test #'equal) "sk-other"))
        (let ((credential (nle::resolve-provider-credential "github-copilot" :auth-path auth :probe t)))
          (is (not (equal "sk-other" (nle:credential-key credential)))
              "the chat family's default variable never reaches Copilot")
          (is (eq :public (nle:credential-source credential))))
        (let ((credential (nle::resolve-provider-credential
                           "github-copilot" :auth-path auth :endpoint "https://api.githubcopilot.com/v1/messages")))
          (is (eq :public (nle:credential-source credential)) "nor does Anthropic's, on a round"))))))

(deftest github-copilot-cell-lends-no-other-key ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (is (eq :none (nle::provider-auth-state "github-copilot" :auth-path auth))
            "OPENAI_API_KEY is not a Copilot credential")))))

(deftest github-copilot-cell-refreshes-a-sign-in-that-is-due ()
  ;; omp stamps a Copilot sign-in ten years out; one that came due is
  ;; refreshed the way omp's hook does, without the network
  (with-cell-stop ((github-copilot-start))
    (let ((soon (+ (nodecode-github-copilot::unix-now) 30)))
      (with-temp-auth (auth (copilot-signed-in :expires soon :endpoint "https://api.individual.githubcopilot.com"))
        (with-stubbed-fdefinitions ((dex:get (url &rest args) (error "no network: ~a" url))
                                    (dex:post (url &rest args) (error "no network: ~a" url)))
          (nle::resolve-provider-credential "github-copilot" :auth-path auth :probe t)
          (is (= soon (nlk:json-value (nle::read-auth-file auth) :integer "oauth_tokens" "github-copilot" "expires_at"))
              "a probe writes nothing")
          (let ((credential (nle::resolve-provider-credential
                             "github-copilot" :auth-path auth
                                              :endpoint "https://api.githubcopilot.com/responses")))
            (is (equal "gho_signed" (nle:credential-key credential)) "the GitHub token stays the token")))
        (let ((entry (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "github-copilot")))
          (is (null (nth-value 1 (gethash "expires_at" entry))) "written back with no expiry")
          (is (equal "gho_signed" (nlk:json-value entry :string "access_token")))
          (is (equal "https://api.individual.githubcopilot.com" (nlk:json-value entry :string "api_endpoint"))
              "and the plan host kept"))))))

;;; --- the sign-in ---------------------------------------------------------------------------

(defmacro with-device-flow ((posts gets &key (token "gho_new")) &body body)
  "BODY with GitHub's device flow and Copilot's policy endpoint stubbed: the
first token poll pending, the second granting TOKEN; POSTS and GETS collect
(URL HEADERS CONTENT) in order."
  `(let ((,posts '()) (,gets '()) (polls 0))
     (with-saved-globals ((nodecode-github-copilot::*poll-floor* 0.01)
                          (nodecode-github-copilot::*poll-scale* 0.01))
       (with-stubbed-fdefinitions
           ((dex:post (url &rest args)
             (push (list url (getf args :headers) (getf args :content)) ,posts)
             (cond ((search "/login/device/code" url)
                    (values "{\"device_code\":\"dev-1\",\"user_code\":\"ABCD-1234\",\"verification_uri\":\"https://github.com/login/device\",\"interval\":5,\"expires_in\":900}" 200))
                   ((search "/login/oauth/access_token" url)
                    (if (= 1 (incf polls))
                        (values "{\"error\":\"authorization_pending\"}" 200)
                        (values ,(format nil "{\"access_token\":~s,\"token_type\":\"bearer\"}" token) 200)))
                   ((search "/policy" url) (values "{}" 200))
                   (t (values "{}" 404))))
            (dex:get (url &rest args)
             (push (list url (getf args :headers)) ,gets)
             (values "{\"endpoints\":{\"api\":\"https://api.individual.githubcopilot.com\"}}" 200)))
         ,@body))))

(defun copilot-form (content)
  "The urlencoded form CONTENT as an alist."
  (quri:url-decode-params content))

(deftest github-copilot-cell-device-sign-in-keeps-the-token ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth "{\"api_keys\":{\"other\":{\"provider\":\"other\",\"key\":\"k\"}}}")
      (with-device-flow (posts gets)
        (let ((answer (let ((nle::*auth-file-path* auth))
                        (cell-entry "nodecode-github-copilot" "github-copilot" "login"))))
          (is (search "https://github.com/login/device" answer) "the page to open")
          (is (search "ABCD-1234" answer) "and the code to type")
          (is (await (:timeout 10) (copilot-said "github-copilot: signed in (served at"))
              "the outcome is said once")
          (is (null (cell-notice "nodecode-github-copilot")) "and does not stand")
          (let* ((auth-json (nle::read-auth-file auth))
                 (entry (nlk:json-value auth-json :object "oauth_tokens" "github-copilot")))
            (is (equal "gho_new" (nlk:json-value entry :string "access_token")))
            (is (equal "gho_new" (nlk:json-value entry :string "refresh_token")))
            (is (null (nth-value 1 (gethash "expires_at" entry))) "a GitHub token keeps no expiry")
            (is (equal "https://api.individual.githubcopilot.com" (nlk:json-value entry :string "api_endpoint")))
            (is (null (nlk:json-value entry :string "enterprise_url")))
            (is (equal "k" (nlk:json-value auth-json :string "api_keys" "other" "key")) "every other field kept")
            (is (equal "600" (format nil "~o" (logand #o777 (sb-posix:stat-mode (sb-posix:stat auth)))))
                "written 0600"))
          (let* ((ordered (reverse posts))
                 (device (first ordered))
                 (poll (second ordered)))
            (is (equal "https://github.com/login/device/code" (first device)))
            (is (equal "Ov23li8tweQw6odWQebz" (cdr (assoc "client_id" (copilot-form (third device)) :test #'equal)))
                "public GitHub signs in through the OpenCode app")
            (is (equal "read:user" (cdr (assoc "scope" (copilot-form (third device)) :test #'equal))))
            (is (equal "copilot-developer-action/0.0.1" (copilot-header (second device) "User-Agent")))
            (is (equal "https://github.com/login/oauth/access_token" (first poll)))
            (is (equal "urn:ietf:params:oauth:grant-type:device_code"
                       (cdr (assoc "grant_type" (copilot-form (third poll)) :test #'equal))))
            (is (equal "dev-1" (cdr (assoc "device_code" (copilot-form (third poll)) :test #'equal))))
            (let ((policies (remove-if-not (lambda (post) (search "/policy" (first post))) ordered)))
              (is (= (length nodecode-github-copilot::+models+) (length policies)) "every bundled model's policy")
              (is (find "https://api.individual.githubcopilot.com/models/claude-sonnet-4.5/policy" policies
                        :key #'first :test #'equal)
                  "at the plan's host")
              (let ((headers (second (first policies))))
                (is (equal "Bearer gho_new" (copilot-header headers "Authorization")))
                (is (equal "copilot-chat" (copilot-header headers "Copilot-Integration-Id")))
                (is (equal "chat-policy" (copilot-header headers "Openai-Intent")))
                (is (equal "2026-08-01" (copilot-header headers "X-GitHub-Api-Version"))))
              (is (equal "{\"state\":\"enabled\"}" (third (first policies))))))
          (is (equal "https://api.github.com/copilot_internal/user" (first (first gets))))
          (is (search "signed in" (let ((nle::*auth-file-path* auth))
                                    (cell-entry "nodecode-github-copilot" "github-copilot" "status")))))))))

(deftest github-copilot-cell-enterprise-sign-in-names-its-domain ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth "{}")
      (with-device-flow (posts gets :token "ghu_ent")
        (let ((answer (let ((nle::*auth-file-path* auth))
                        (cell-entry "nodecode-github-copilot" "github-copilot" "login https://acme.ghe.com/"))))
          (is (search "acme.ghe.com" answer))
          (is (await (:timeout 10)
                (nlk:json-value (ignore-errors (nle::read-auth-file auth)) :string "oauth_tokens" "github-copilot" "access_token")))
          (let ((device (car (last posts))))
            (is (equal "https://acme.ghe.com/login/device/code" (first device)))
            (is (equal "Ov23ctDVkRmgkPke0Mmm" (cdr (assoc "client_id" (copilot-form (third device)) :test #'equal)))
                "an Enterprise instance keeps the Copilot CLI's app"))
          (is (equal "acme.ghe.com" (nlk:json-value (nle::read-auth-file auth) :string
                                                    "oauth_tokens" "github-copilot" "enterprise_url")))
          (is (await (:timeout 10) (copilot-said "github-copilot: signed in to acme.ghe.com"))))))))

(deftest github-copilot-cell-says-a-refused-sign-in ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth "{}")
      (with-saved-globals ((nodecode-github-copilot::*poll-floor* 0.01)
                           (nodecode-github-copilot::*poll-scale* 0.01))
        (with-stubbed-fdefinition
            (dex:post (url &rest args)
             (if (search "/login/device/code" url)
                 (values "{\"device_code\":\"d\",\"user_code\":\"U\",\"verification_uri\":\"https://github.com/login/device\",\"interval\":1,\"expires_in\":60}" 200)
                 (values "{\"error\":\"access_denied\",\"error_description\":\"the user said no\"}" 200)))
          (let ((nle::*auth-file-path* auth))
            (cell-entry "nodecode-github-copilot" "github-copilot" "login"))
          (is (await (:timeout 10)
                (search "the user said no" (or (second (cell-notice "nodecode-github-copilot")) "")))
              "the refusal in GitHub's words")
          (is (null (nlk:json-value (ignore-errors (nle::read-auth-file auth)) :object "oauth_tokens"))
              "and nothing kept"))))))

(deftest github-copilot-cell-logout-forgets-the-sign-in ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth (copilot-signed-in))
      (let ((nle::*auth-file-path* auth))
        (is (search "signed out" (cell-entry "nodecode-github-copilot" "github-copilot" "logout")))
        (is (null (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "github-copilot")))
        (is (search "not signed in" (cell-entry "nodecode-github-copilot" "github-copilot" "status")))))))

;;; --- one round per wire -------------------------------------------------------------------

(deftest github-copilot-cell-sends-a-claude-round-to-messages ()
  (with-copilot-round (url headers body) "claude-sonnet-4.5"
    (is (equal "https://api.githubcopilot.com/v1/messages" url))
    (is (equal "Bearer gho_signed" (copilot-header headers "Authorization")) "the GitHub token as bearer")
    (is (null (copilot-header headers "x-api-key")) "no Anthropic key header")
    (is (null (copilot-header headers "anthropic-beta")) "no beta: Copilot refuses them")
    (is (equal "2023-06-01" (copilot-header headers "anthropic-version")))
    (is (equal "copilot/1.0.82" (copilot-header headers "User-Agent")))
    (is (equal "copilot-chat" (copilot-header headers "Copilot-Integration-Id")) "the chat identity first")
    (is (equal "user" (copilot-header headers "X-Initiator")))
    (is (equal "conversation-user" (copilot-header headers "X-Interaction-Type")))
    (is (equal "2026-08-01" (copilot-header headers "X-GitHub-Api-Version")))
    (is (equal "claude-sonnet-4.5" (nlk:json-value body :string "model")))))

(deftest github-copilot-cell-sends-a-gpt-5-round-to-responses ()
  (with-copilot-round (url headers body :auth (copilot-signed-in :endpoint "https://api.individual.githubcopilot.com"))
      "gpt-5.5"
    (is (equal "https://api.individual.githubcopilot.com/responses" url) "at the plan's host")
    (is (equal "Bearer gho_signed" (copilot-header headers "Authorization")))
    (is (equal "conversation-agent" (copilot-header headers "Openai-Intent")))
    (is (equal "gpt-5.5" (nlk:json-value body :string "model")))))

(deftest github-copilot-cell-sends-a-gpt-4-round-to-chat ()
  (with-copilot-round (url headers body :auth (copilot-signed-in :enterprise "acme.ghe.com")) "gpt-4.1"
    (is (equal "https://copilot-api.acme.ghe.com/chat/completions" url))
    (is (equal "copilot-developer-cli" (copilot-header headers "Copilot-Integration-Id"))
        "Enterprise keeps the CLI identity")
    (is (equal "gpt-4.1" (nlk:json-value body :string "model")))))

(deftest github-copilot-cell-marks-a-round-after-a-tool-as-the-agent-s ()
  (with-copilot-round (url headers body
                       :context (compiled-context
                                 (list (nlk:json-object "role" "user" "content" "read it")
                                       (nlk:json-object "role" "assistant" "content" :null
                                                        "tool_calls" (vector (nlk:json-object
                                                                              "id" "c1" "type" "function"
                                                                              "function" (nlk:json-object "name" "eval" "arguments" "{}"))))
                                       (nlk:json-object "role" "tool" "tool_call_id" "c1" "content" "done"))))
      "claude-sonnet-4.5"
    (is (equal "agent" (copilot-header headers "X-Initiator")) "a tool result's round is not billed as the operator's")
    (is (equal "conversation-agent" (copilot-header headers "X-Interaction-Type")))))

(deftest github-copilot-cell-retries-a-refused-chat-identity-as-the-cli ()
  (with-cell-stop ((github-copilot-start))
    (clrhash nodecode-github-copilot::*working-identity*)
    (with-temp-auth (auth (copilot-signed-in :token "gho_biz"))
      (let ((nle::*provider* "github-copilot") (nle::*model* "gpt-4.1") (nle::*api-key* nil)
            (nle::*endpoint* nil) (nle::*auth-file-path* auth) (identities '()))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name) nil)
             (dex:post (url &rest args)
              (let ((id (copilot-header (getf args :headers) "Copilot-Integration-Id")))
                (push id identities)
                (if (equal id "copilot-chat")
                    (error 'dex:http-request-forbidden :status 403 :body "{\"error\":{\"message\":\"forbidden\"}}"
                                                       :headers (make-hash-table :test #'equal) :uri (quri:uri url)
                                                       :method :post)
                    (values (copilot-stream "openai-completions") 200)))))
          (is (equal "ok" (nlk:json-value (copilot-round (user-context)) :string "content")))
          (is (equal '("copilot-chat" "copilot-developer-cli") (reverse identities)) "one retry, as the CLI")
          (setf identities '())
          (copilot-round (user-context))
          (is (equal '("copilot-developer-cli") identities) "what worked is where the next round starts"))))))

(deftest github-copilot-cell-says-it-is-not-signed-in ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*provider* "github-copilot") (nle::*model* "gpt-4.1") (nle::*api-key* nil)
            (nle::*endpoint* nil) (nle::*auth-file-path* auth) (posted nil))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name) nil)
             (dex:post (url &rest args) (setf posted t) (values (copilot-stream "openai-completions") 200)))
          (let ((refusal (signals-error nle::provider-config-error (copilot-round (user-context)))))
            (when (typep refusal 'nle::provider-error)
              (is (search "/github-copilot login" (nle::provider-error-detail refusal)))))
          (is (null posted) "nothing is sent"))))))

(deftest github-copilot-cell-leaves-other-providers-alone ()
  (with-cell-stop ((github-copilot-start))
    (let ((nle::*provider* "anthropic") (nle::*model* "claude-sonnet-4-5") (nle::*api-key* "sk-ant")
          (nle::*endpoint* nil) (url nil) (headers nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked headers (getf args :headers))
           (values (copilot-stream "anthropic") 200))
        (nle::call-anthropic-streaming (user-context)))
      (is (equal "https://api.anthropic.com/v1/messages" url))
      (is (equal "sk-ant" (copilot-header headers "x-api-key")) "Anthropic's own key header")
      (is (null (copilot-header headers "Copilot-Integration-Id")) "and no Copilot identity"))))

(deftest github-copilot-cell-lists-the-account-s-chat-models ()
  (with-cell-stop ((github-copilot-start))
    (with-temp-auth (auth (copilot-signed-in :endpoint "https://api.individual.githubcopilot.com"))
      (let ((nle::*auth-file-path* auth) (nle::*api-key* nil) (asked nil))
        (with-stubbed-fdefinition
            (dex:get (url &rest args)
             (setf asked (list url (getf args :headers)))
             (values "{\"data\":[{\"id\":\"gpt-5.5\",\"name\":\"GPT-5.5\",\"capabilities\":{\"type\":\"chat\",\"limits\":{\"max_context_window_tokens\":400000}}},{\"id\":\"text-embedding-3-small\",\"capabilities\":{\"type\":\"embeddings\"}}]}" 200))
          (multiple-value-bind (rows error) (nle::list-provider-models "github-copilot")
            (is (null error) error)
            (is (equal '("gpt-5.5") (mapcar (lambda (row) (getf row :id)) rows)) "chat models only")
            (is (eql 400000 (getf (first rows) :context-window)))))
        (is (equal "https://api.individual.githubcopilot.com/models" (first asked)) "at the account's host")
        (is (equal "Bearer gho_signed" (copilot-header (second asked) "Authorization")))
        (is (equal "copilot-developer-cli" (copilot-header (second asked) "Copilot-Integration-Id"))
            "the CLI identity, which listing unlocks more with")
        (is (equal "user" (copilot-header (second asked) "X-Initiator")))))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) nil)
        (let ((nle::*auth-file-path* auth) (nle::*api-key* nil))
          (is (search "not signed in" (nth-value 1 (nle::list-provider-models "github-copilot")))
              "nothing is asked without a sign-in"))))))
