;;;; cell-test.lisp --- the codex-auth cell against a scripted store.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store here is a temp auth.json the test writes and names through
;;;; :auth-path, and every wire is a stubbed dex:post — the core suite's own
;;;; seams (WITH-TEMP-AUTH, WITH-STUBBED-FDEFINITION). Nothing touches the
;;;; network and nothing reads the operator's own auth.json.

(in-package #:nodecode.test)

(define-test-slice "codex-auth" "CODEX-AUTH-CELL-" :start nodecode-codex-auth:start-cell)

(define-cell-lifecycle-tests "codex-auth"
  (:hooks :credential)
  (:refused ("endpoint" 5)))

;;; --- the fixtures -----------------------------------------------------------

(defparameter +openai-store+
  (concatenate 'string
               "{\"oauth_tokens\":{\"openai-responses\":{\"provider\":\"openai-responses\","
               "\"access_token\":\"codex-tok\",\"account_id\":\"acct-7\"}}}")
  "One bare-id oauth_tokens entry for the openai-responses lane.")

(defmacro with-codex-credential ((credential store &rest keys) &body body)
  "BODY with the cell started and CREDENTIAL what the openai-responses lane
resolves, with KEYS, over a scratch auth.json holding STORE, named AUTH."
  `(with-cell-stop ((codex-auth-start))
     (with-temp-auth (auth ,store)
       (let ((,credential (nle::resolve-provider-credential "openai-responses" :auth-path auth
                                                            ,@keys)))
         ,@body))))

(defun codex-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun capture-responses-request (&aux (url nil) (headers nil))
  "One responses round with dex:post stubbed: (values URL HEADERS)."
  ;; The lane runs for real, the socket does not: the same seam the core
  ;; suite's codex test used.
  (with-stubbed-fdefinition
      (dex:post (asked &rest args)
       (setf url asked headers (getf args :headers))
       (values (make-truncated-sse-stream
                "{\"type\":\"response.output_text.delta\",\"item_id\":\"m\",\"delta\":\"ok\"}"
                "{\"type\":\"response.completed\",\"response\":{}}")
               200))
    (nle::call-responses-streaming (user-context)))
  (values url headers))

;;; --- what the point answers -------------------------------------------------

(deftest codex-auth-cell-answers-an-oauth-token-for-an-openai-lane ()
  (with-codex-credential (credential +openai-store+ :endpoint "https://api.openai.com/v1/responses")
    (let ((attributes (nle:credential-attributes credential)))
      (is (equal "codex-tok" (nle:credential-key credential)) "the access token is the key")
      (is (eq :oauth (nle:credential-source credential)) "and it came from the OAuth tier")
      (is (equal (getf attributes :endpoint)
                 "https://chatgpt.com/backend-api/codex/responses") "the Codex backend")
      (is (equal "codex_cli_rs"
                 (codex-header (getf attributes :headers) "originator")))
      (is (equal "acct-7"
                 (codex-header (getf attributes :headers) "chatgpt-account-id")))
      (is (equal "codex_oauth" (getf attributes :cache-key)))
      ;; and the freeze carries all of it: the request path reads the config
      (nlk:bind ((nle::*provider* "openai-responses") (nle::*api-key* nil)
                 (nle::*auth-file-path* auth)
                 (config (nle::compiled-turn-context-provider-config (user-context))))
        (is (equal "codex-tok" config.api-key))
        (is (equal "codex_oauth" (nle::credential-attribute config :cache-key)))))))

(deftest codex-auth-cell-reads-a-profiled-entry-and-its-jwt-account ()
  ;; openai:work is how a second ChatGPT login rides beside the first: the
  ;; entry matches by the provider id before the colon, and the account comes
  ;; off the token when the entry names none.
  (let* ((payload "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct-42\"}}")
         (encoded (string-right-trim "." (cl-base64:string-to-base64-string payload :uri t)))
         (jwt (format nil "h.~a.s" encoded)))
    (with-codex-credential (credential (format nil "{\"oauth_tokens\":{\"openai-responses:work\":{\"provider\":\"openai-responses:work\",\"access_token\":\"profiled-tok\",\"id_token\":\"~a\"}}}" jwt)
                            :endpoint "https://api.openai.com/v1/responses")
      (let ((headers (getf (nle:credential-attributes credential) :headers)))
        (is (equal "profiled-tok" (nle:credential-key credential)))
        (is (equal "acct-42" (codex-header headers "chatgpt-account-id")) "the token's claim")))))

(deftest codex-auth-cell-honors-an-explicit-endpoint ()
  ;; An operator who pointed the provider somewhere else meant it: the token
  ;; still rides, the address does not move.
  (with-codex-credential (credential +openai-store+ :endpoint "https://proxy.example/v1/responses")
    (let ((attributes (nle:credential-attributes credential)))
      (is (equal "codex-tok" (nle:credential-key credential)))
      (is (null (getf attributes :endpoint)) "no endpoint override")
      (is (equal "codex_cli_rs" (codex-header (getf attributes :headers) "originator")) "still sent"))))

(deftest codex-auth-cell-refuses-an-ambiguous-store (with-cell-stop ((codex-auth-start)))
  ;; Two profiles and no bare id: a decisive failure, never a pick.
  (with-temp-auth (auth (concatenate
                         'string
                         "{\"oauth_tokens\":{"
                         "\"openai-responses:work\":{\"provider\":\"openai-responses:work\",\"access_token\":\"t1\"},"
                         "\"openai-responses:home\":{\"provider\":\"openai-responses:home\",\"access_token\":\"t2\"}}}"))
    (is (signals-error nle:credential-error
          (nle::resolve-provider-credential "openai-responses" :auth-path auth)))))

(deftest codex-auth-cell-declines-outside-the-openai-family (with-cell-stop ((codex-auth-start)))
  ;; A store credential for a lane this cell says nothing about: NEXT runs,
  ;; and the chain ends where it would have without the folder at all.
  (with-temp-auth (auth "{\"oauth_tokens\":{\"anthropic\":{\"provider\":\"anthropic\",\"access_token\":\"claude-tok\"}}}")
    (with-stubbed-fdefinition (nle::credential-env (name)
                               nil)
      (let ((credential (nle::resolve-provider-credential "anthropic" :auth-path auth
                                                         :endpoint "https://api.anthropic.com/v1/messages")))
        (is-shape credential (nle:credential-key "public") (nle:credential-source eq :public)
          (nle:credential-attributes null))))))

(deftest codex-auth-cell-stays-behind-api-keys ()
  ;; The ladder's order is the kernel's: an explicit api_keys entry wins, and
  ;; no ChatGPT transport rides an api key.
  (with-codex-credential (credential (concatenate
                                      'string
                                      "{\"api_keys\":{\"openai-responses\":{\"provider\":\"openai-responses\",\"key\":\"api-key-wins\"}},"
                                      "\"oauth_tokens\":{\"openai-responses\":{\"provider\":\"openai-responses\",\"access_token\":\"codex-tok\"}}}"))
    (is-shape credential (nle:credential-key "api-key-wins") (nle:credential-source eq :api-key)
      (nle:credential-attributes null))))

(deftest codex-auth-cell-is-what-the-provider-auth-state-reads ()
  (with-temp-auth (auth +openai-store+)
    (with-stubbed-fdefinition (nle::credential-env (name)
                               nil)
      (is (eq :none (nle::provider-auth-state "openai-responses" :auth-path auth)) "alone")
      (with-cell-stop ((codex-auth-start))
        (is (eq :oauth (nle::provider-auth-state "openai-responses" :auth-path auth)) "with it")))))

;;; --- the wire ---------------------------------------------------------------

(deftest codex-auth-cell-shapes-the-openai-responses-request ()
  (let ((nle::*provider* "openai-responses")
        (nle::*api-key* nil))
    (with-temp-auth (auth +openai-store+)
      (let ((nle::*auth-file-path* auth))
        (nlk:bind (((url headers) (with-cell-stop ((codex-auth-start))
                                    (capture-responses-request))))
          (is (equal "https://chatgpt.com/backend-api/codex/responses" url) "the login's address")
          (is (equal "Bearer codex-tok" (codex-header headers "authorization")))
          (is (equal "codex_cli_rs" (codex-header headers "originator")))
          (is (equal "acct-7" (codex-header headers "chatgpt-account-id"))))
        ;; Stopped: the store's OAuth entry belongs to nobody, and the kernel
        ;; carries none of Codex — the OpenAI address, no originator, the
        ;; keyless bearer.
        (nlk:bind (((url headers) (capture-responses-request)))
          (is (equal "https://api.openai.com/v1/responses" url) "the address it froze")
          (is (equal "Bearer public" (codex-header headers "authorization")))
          (is (null (codex-header headers "originator")) "no header the kernel never knew"))))))
