;;;; cell-test.lisp --- the openai-codex cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every token endpoint and every round a
;;;; stubbed dex:post, every key variable a stubbed NLE::CREDENTIAL-ENV. The
;;;; sign-in's callback listens on a free loopback port the test dials
;;;; itself: nothing reaches OpenAI, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "openai-codex" "OPENAI-CODEX-CELL-" :start nodecode-openai-codex:start-cell)

(define-cell-lifecycle-tests "openai-codex"
  (:hooks 'nle::models-catalog-table :credential 'nle::responses-request-body 'nle::walk-provider-stream
          'nle::note-body-wire)
  (:command "openai-codex")
  (:running (is (nle::find-lane-by-name "openai-codex" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "openai-codex" nil)) "and taken back out"))
  (:refused ("base_url" 5) ("originator" 5)))

;;; --- fixtures --------------------------------------------------------------------

(defun oc-jwt (claims)
  "A JWT whose payload is the JSON text CLAIMS, unsigned."
  (format nil "h.~a.s" (string-right-trim "." (cl-base64:string-to-base64-string claims :uri t))))

(defparameter +oc-access+
  (oc-jwt (concatenate 'string
                       "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct-7\","
                       "\"chatgpt_plan_type\":\"Pro\",\"chatgpt_data_residency\":\"eu\"},"
                       "\"https://api.openai.com/profile\":{\"email\":\"Op@Example.com\"}}"))
  "An access token naming account acct-7, plan pro, residency eu, email op@example.com.")

(defun oc-now () (nodecode-openai-codex::unix-seconds))

(defun oc-store (&key (access +oc-access+) (expires-in 3600) (extra ""))
  "auth.json text holding one openai-codex sign-in, EXTRA spliced before it."
  (format nil "{~a\"oauth_tokens\":{\"openai-codex\":{\"provider\":\"openai-codex\",\"access_token\":\"~a\",~
               \"refresh_token\":\"rt-1\",\"expires_at\":~d,\"account_id\":\"acct-7\",\"email\":\"op@example.com\",~
               \"org_id\":\"acct-7\",\"org_name\":\"pro\",\"installation_id\":\"inst-1\"}}}"
          extra access (+ (oc-now) expires-in)))

(defun oc-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun oc-entry (auth)
  "The openai-codex entry of the auth.json at AUTH."
  (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "openai-codex"))

(defun oc-form (content)
  "The x-www-form-urlencoded CONTENT as an alist."
  (quri:url-decode-params content))

(defun oc-token-answer (&key (access +oc-access+) (refresh "rt-2"))
  "The token endpoint's answer: ACCESS, REFRESH, an hour, an id token."
  (format nil "{\"access_token\":\"~a\",\"refresh_token\":\"~a\",\"id_token\":\"~a\",\"expires_in\":3600}"
          access refresh (oc-jwt "{}")))

(defun oc-get (port target)
  "GET TARGET from 127.0.0.1:PORT over a raw socket: the answer's status code."
  (let ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket))
               (bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
           (write-sequence (sb-ext:string-to-octets
                            (format nil "GET ~a HTTP/1.1~c~cHost: localhost~c~c~c~c"
                                    target #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
                            :external-format :latin-1)
                           stream)
           (finish-output stream)
           (loop for byte = (read-byte stream nil nil) while byte do (vector-push-extend byte bytes))
           (parse-integer (sb-ext:octets-to-string bytes :external-format :latin-1) :start 9 :end 12))
      (usocket:socket-close socket))))

(defun oc-query (url name)
  "The query parameter NAME of URL."
  (cdr (assoc name (quri:uri-query-params (quri:uri url)) :test #'equal)))

(defun oc-login-url (text)
  "The authorization address a /openai-codex login answer names."
  (find-if (lambda (line) (uiop:string-prefix-p "https://" line))
           (uiop:split-string text :separator '(#\Newline))))

(defmacro with-oc-login ((auth url posts &key (store "{}")) &body body)
  "BODY with the cell started, a sign-in begun on a free port against a temp
auth.json AUTH holding STORE, URL its authorization address and POSTS the
(URL . FORM) pairs the token endpoint saw; the endpoint answers a token."
  `(with-cell-stop ((openai-codex-start))
     (with-temp-auth (,auth ,store)
       (let ((nle::*auth-file-path* ,auth)
             (nodecode-openai-codex::*callback-port* 0)
             (,posts '()))
         (with-stubbed-fdefinition (dex:post (asked &rest args)
                                    (push (cons asked (oc-form (getf args :content))) ,posts)
                                    (values (oc-token-answer) 200))
           (let ((,url (oc-login-url (cell-entry "nodecode-openai-codex" "openai-codex" "login"))))
             (declare (ignorable ,url))
             ,@body))))))

(defun oc-await-login ()
  "Wait for the running sign-in's thread to finish."
  (alexandria:when-let (login nodecode-openai-codex::*login*)
    (bt2:join-thread (nodecode-openai-codex::login-thread login))))

;;; --- the catalog -----------------------------------------------------------------

(deftest openai-codex-cell-puts-its-row-and-lane-in-the-catalog ()
  (with-cell-stop ((openai-codex-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "openai-codex"))
           (models (nlk:json-value row :object "models")))
      (is (equal "ChatGPT Plus/Pro (Codex Subscription)" (nlk:json-value row :string "name")))
      (is (equal "https://chatgpt.com/backend-api/codex" (nlk:json-value row :string "api")))
      (is (gethash "gpt-6.1-sol" models) "the bundled models are listed")
      (is (nle::catalog-model-turn-p (gethash "gpt-6.1-sol" models)))
      (is (not (nle::catalog-model-turn-p (gethash "gpt-image-2" models))) "an image model is no turn's")
      (is (equal "openai-codex" (nle::configured-provider-lane "openai-codex")) "its own lane drives it")
      (is (equal "https://chatgpt.com/backend-api/codex/responses"
                 (nle::lane-endpoint "openai-codex" "openai-codex"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "openai-codex"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest openai-codex-cell-base-follows-the-section ()
  (with-cell-stop ((openai-codex-start "base_url" "https://relay.example/backend-api/codex/responses/"))
    (is (equal "https://relay.example/backend-api/codex"
               (nlk:json-value (nle::models-catalog-table) :string "openai-codex" "api"))
        "omp's three spellings of the base end at one /codex")))

;;; --- the token's claims and the authorization address ---------------------------

(deftest openai-codex-cell-reads-the-token-claims ()
  (multiple-value-bind (account email plan) (nodecode-openai-codex::token-profile +oc-access+)
    (is (equal "acct-7" account))
    (is (equal "op@example.com" email) "the email lower-cased")
    (is (equal "pro" plan)))
  (is (equal "eu" (nodecode-openai-codex::token-residency +oc-access+)))
  (is (equal "acct-9" (nodecode-openai-codex::token-profile
                       "not-a-jwt"
                       (oc-jwt "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct-9\"}}")))
      "the id token answers when the access token does not")
  (is (null (nodecode-openai-codex::token-profile "a.b")) "a malformed token names nobody"))

(deftest openai-codex-cell-builds-omps-authorization-address ()
  (with-cell-stop ((openai-codex-start))
    (multiple-value-bind (verifier challenge) (nodecode-openai-codex::pkce)
      (is (= 128 (length verifier)) "96 random bytes, base64url")
      (is (equal challenge (nodecode-openai-codex::base64url (nodecode-openai-codex::sha256 verifier))))
      (let ((url (nodecode-openai-codex::authorize-url "st-1" challenge "http://localhost:1455/auth/callback")))
        (is (uiop:string-prefix-p "https://auth.openai.com/oauth/authorize?client_id=app_EMoamEEZ73f0CkXaXp7hrann&" url))
        (is (equal "http://localhost:1455/auth/callback" (oc-query url "redirect_uri")))
        (is (equal "openid profile email offline_access api.connectors.read api.connectors.invoke"
                   (oc-query url "scope")))
        (is (equal "S256" (oc-query url "code_challenge_method")))
        (is (equal challenge (oc-query url "code_challenge")))
        (is (equal "st-1" (oc-query url "state")))
        (is (equal "true" (oc-query url "codex_cli_simplified_flow")))
        (is (equal "codex_cli_rs" (oc-query url "originator")))))
    (is (equal "http://localhost:1455/auth/callback" (nodecode-openai-codex::redirect-uri 1455))
        "the one redirect OpenAI allowlists")))

(deftest openai-codex-cell-pkce-challenge-is-rfc-7636 ()
  ;; RFC 7636 appendix B's verifier and challenge
  (is (equal "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
             (nodecode-openai-codex::base64url
              (nodecode-openai-codex::sha256 "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")))))

;;; --- the sign-in -----------------------------------------------------------------

(deftest openai-codex-cell-signs-in-through-the-callback ()
  (with-oc-login (auth url posts :store "{\"api_keys\":{\"openai\":{\"provider\":\"openai\",\"key\":\"sk-kept\"}}}")
    (let ((redirect (oc-query url "redirect_uri")))
      (is (ppcre:scan "^http://localhost:\\d+/auth/callback$" redirect))
      (let ((port (parse-integer redirect :start 17 :end (position #\/ redirect :start 17))))
        (is (= 404 (oc-get port "/elsewhere")) "only the callback path answers")
        (is (= 500 (oc-get port "/auth/callback?code=c-1&state=forged")) "a forged state is refused")
        (is (= 200 (oc-get port (format nil "/auth/callback?code=c-1&state=~a" (oc-query url "state"))))))
      (oc-await-login)
      (is (= 1 (length posts)) "one exchange")
      (destructuring-bind (asked . form) (first posts)
        (is (equal "https://auth.openai.com/oauth/token" asked))
        (is (equal "authorization_code" (cdr (assoc "grant_type" form :test #'equal))))
        (is (equal "app_EMoamEEZ73f0CkXaXp7hrann" (cdr (assoc "client_id" form :test #'equal))))
        (is (equal "c-1" (cdr (assoc "code" form :test #'equal))))
        (is (equal redirect (cdr (assoc "redirect_uri" form :test #'equal))))
        (is (equal (oc-query url "code_challenge")
                   (nodecode-openai-codex::base64url
                    (nodecode-openai-codex::sha256 (cdr (assoc "code_verifier" form :test #'equal)))))
            "the verifier is the one the challenge was made of"))
      (let ((entry (oc-entry auth)))
        (is (equal +oc-access+ (nlk:json-value entry :string "access_token")))
        (is (equal "rt-2" (nlk:json-value entry :string "refresh_token")))
        (is (<= (abs (- (nlk:json-value entry :integer "expires_at") (+ (oc-now) 3600))) 5)
            "expires_at is epoch seconds")
        (is (equal "acct-7" (nlk:json-value entry :string "account_id")))
        (is (equal "op@example.com" (nlk:json-value entry :string "email")))
        (is (equal "acct-7" (nlk:json-value entry :string "org_id")))
        (is (equal "pro" (nlk:json-value entry :string "org_name")))
        (is (= 36 (length (nlk:json-value entry :string "installation_id")))))
      (is (equal "sk-kept" (nle::auth-api-key (nle::read-auth-file auth) "openai")) "every other field kept")
      (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring auth))))))
      (is (search "signed in as op@example.com (pro)" (second (cell-notice "nodecode-openai-codex")))))))

(deftest openai-codex-cell-takes-a-pasted-address ()
  (with-oc-login (auth url posts)
    (is (search "another sign-in"
                (cell-entry "nodecode-openai-codex" "openai-codex"
                            "code http://localhost:1455/auth/callback?code=c-9&state=other")))
    (is (search "code received"
                (cell-entry "nodecode-openai-codex" "openai-codex"
                            (format nil "code http://localhost:1455/auth/callback?code=c-9&state=~a"
                                    (oc-query url "state")))))
    (oc-await-login)
    (is (equal "c-9" (cdr (assoc "code" (cdr (first posts)) :test #'equal))))
    (is (equal +oc-access+ (nlk:json-value (oc-entry auth) :string "access_token")))))

(deftest openai-codex-cell-refuses-a-busy-callback-port ()
  (let ((held (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect
         (with-cell-stop ((openai-codex-start))
           (let ((nodecode-openai-codex::*callback-port* (usocket:get-local-port held)))
             (is (search "is in use" (cell-entry "nodecode-openai-codex" "openai-codex" "login"))
                 "no other port: OpenAI allowlists exactly one")
             (is (null nodecode-openai-codex::*login*))))
      (usocket:socket-close held))))

(deftest openai-codex-cell-logs-out-and-says-its-status ()
  (with-cell-stop ((openai-codex-start))
    (with-temp-auth (auth (oc-store))
      (let ((nle::*auth-file-path* auth))
        (is (search "signed in as op@example.com (pro)" (cell-entry "nodecode-openai-codex" "openai-codex" "status")))
        (is (search "signed out" (cell-entry "nodecode-openai-codex" "openai-codex" "logout")))
        (is (null (oc-entry auth)))
        (is (search "not signed in" (cell-entry "nodecode-openai-codex" "openai-codex" "")))))))

;;; --- the credential ------------------------------------------------------------------

(deftest openai-codex-cell-answers-the-sign-in-with-its-headers ()
  (with-cell-stop ((openai-codex-start))
    (with-temp-auth (auth (oc-store))
      (let* ((credential (nle::resolve-provider-credential "openai-codex" :auth-path auth
                                                                          :endpoint "https://chatgpt.com/backend-api/codex/responses"))
             (headers (getf (nle:credential-attributes credential) :headers)))
        (is (equal +oc-access+ (nle:credential-key credential)))
        (is (eq :oauth (nle:credential-source credential)))
        (is (equal "acct-7" (oc-header headers "chatgpt-account-id")))
        (is (equal "eu" (oc-header headers "x-openai-internal-codex-residency")))
        (is (equal "inst-1" (getf (nle:credential-attributes credential) :installation-id))))
      (with-stubbed-fdefinition (nle::credential-env (name) nil)
        (is (eq :oauth (nle::provider-auth-state "openai-codex" :auth-path auth)))))))

(deftest openai-codex-cell-refreshes-an-expiring-token ()
  (with-cell-stop ((openai-codex-start))
    (with-temp-auth (auth (oc-store :access "old-token" :expires-in 30))
      (let ((posts '())
            (fresh (oc-jwt "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct-7\"}}")))
        (with-stubbed-fdefinition (dex:post (asked &rest args)
                                   (push (cons asked (oc-form (getf args :content))) posts)
                                   (values (oc-token-answer :access fresh :refresh "rt-3") 200))
          (is (equal "old-token" (nle:credential-key (nle::resolve-provider-credential "openai-codex" :auth-path auth :probe t)))
              "a probe reads the store as it is")
          (is (null posts) "and never dials")
          (is (equal fresh (nle:credential-key (nle::resolve-provider-credential
                                                "openai-codex" :auth-path auth
                                                               :endpoint "https://chatgpt.com/backend-api/codex/responses")))
              "a round's resolution refreshes first"))
        (is (= 1 (length posts)))
        (destructuring-bind (asked . form) (first posts)
          (is (equal "https://auth.openai.com/oauth/token" asked))
          (is (equal "refresh_token" (cdr (assoc "grant_type" form :test #'equal))))
          (is (equal "rt-1" (cdr (assoc "refresh_token" form :test #'equal))))
          (is (equal "app_EMoamEEZ73f0CkXaXp7hrann" (cdr (assoc "client_id" form :test #'equal)))))
        (let ((entry (oc-entry auth)))
          (is (equal fresh (nlk:json-value entry :string "access_token")) "written back")
          (is (equal "rt-3" (nlk:json-value entry :string "refresh_token")) "the rotated refresh token")
          (is (> (nlk:json-value entry :integer "expires_at") (+ (oc-now) 3000)))
          (is (equal "pro" (nlk:json-value entry :string "org_name")) "the sign-in's plan kept")
          (is (equal "inst-1" (nlk:json-value entry :string "installation_id"))))))))

(deftest openai-codex-cell-fails-clearly-when-an-expired-token-cannot-refresh ()
  (with-cell-stop ((openai-codex-start))
    (with-temp-auth (auth (oc-store :access "old-token" :expires-in -10))
      (with-stubbed-fdefinition (dex:post (asked &rest args)
                                 (values "{\"error\":\"invalid_grant\",\"error_description\":\"Refresh token expired\"}" 400))
        (let ((condition (signals-error nle:credential-error
                           (nle::resolve-provider-credential "openai-codex" :auth-path auth
                                                                            :endpoint "https://chatgpt.com/x"))))
          (is (search "invalid_grant: Refresh token expired" (princ-to-string condition)))
          (is (search "/openai-codex login" (princ-to-string condition))))))))

(deftest openai-codex-cell-reads-the-token-variable-and-else-nothing ()
  (with-cell-stop ((openai-codex-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_CODEX_OAUTH_TOKEN") +oc-access+))
        (let ((credential (nle::resolve-provider-credential "openai-codex" :auth-path auth :probe t)))
          (is (equal +oc-access+ (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name) nil)
        (is (eq :none (nle::provider-auth-state "openai-codex" :auth-path auth))
            "no sign-in, no variable: nothing, and no fall into a ladder that knows no such family")
        (is (search "/openai-codex login"
                    (princ-to-string (signals-error nle:credential-error
                                       (nle::resolve-provider-credential "openai-codex" :auth-path auth
                                                                                        :endpoint "https://chatgpt.com/x"))))
            "and a round is told what to do")))))

(deftest openai-codex-cell-is-not-an-openai-family-lane ()
  ;; nodecode-codex-auth's rule, registered first so it runs outermost: it
  ;; answers any openai-family lane from oauth_tokens, unrefreshed. This lane
  ;; is not one, so the refreshing answer is this cell's.
  (nle:hook :credential "codex-auth-stand-in"
            (lambda (op next)
              (let ((entry (nlk:json-value (getf op :auth) :object "oauth_tokens" (getf op :provider))))
                (if (and entry (eq (getf op :family) :openai))
                    (nle:make-credential (gethash "access_token" entry) :oauth)
                    (funcall next op)))))
  (unwind-protect
       (with-cell-stop ((openai-codex-start))
         (with-temp-auth (auth (oc-store :access "old-token" :expires-in 30))
           (with-stubbed-fdefinition (dex:post (asked &rest args)
                                      (values (oc-token-answer :access "new-token") 200))
             (is (equal "new-token" (nle:credential-key (nle::resolve-provider-credential
                                                         "openai-codex" :auth-path auth
                                                                        :endpoint "https://chatgpt.com/x")))))))
    (nle:unhook :credential "codex-auth-stand-in")))

;;; --- one round -------------------------------------------------------------------

(defparameter +oc-stream+
  (list "{\"type\":\"response.created\",\"response\":{\"id\":\"resp_1\",\"model\":\"gpt-6.1-sol\"}}"
        "{\"type\":\"response.output_text.delta\",\"item_id\":\"msg_1\",\"delta\":\"ok\"}"
        "{\"type\":\"response.done\",\"response\":{\"usage\":{\"input_tokens\":5,\"output_tokens\":1}}}")
  "A Codex stream that ends on response.done.")

(defparameter +oc-completed+
  (list "{\"type\":\"response.output_text.delta\",\"item_id\":\"m\",\"delta\":\"ok\"}"
        "{\"type\":\"response.completed\",\"response\":{}}")
  "A Responses stream as the OpenAI API ends one.")

(defun oc-round (context &key (stream +oc-stream+))
  "One round of CONTEXT with dex:post stubbed to answer STREAM:
(values MESSAGE URL HEADERS BODY)."
  (let ((url nil) (headers nil) (body nil))
    (with-stubbed-fdefinition (dex:post (asked &rest args)
                               (setf url asked headers (getf args :headers)
                                     body (nlk:decode-json (getf args :content)))
                               (values (apply #'make-truncated-sse-stream stream) 200))
      (values (nle::call-responses-streaming context) url headers body))))

(defmacro with-oc-round ((&key (model "gpt-6.1-sol")) &body body)
  "BODY with the cell started, signed in, and MODEL selected on openai-codex."
  `(with-cell-stop ((openai-codex-start))
     (with-temp-auth (auth (oc-store))
       (let ((nle::*auth-file-path* auth) (nle::*provider* "openai-codex") (nle::*model* ,model)
             (nle::*api-key* nil) (nle::*endpoint* nil))
         ,@body))))

(deftest openai-codex-cell-sends-a-round-the-codex-way ()
  (with-oc-round ()
    (multiple-value-bind (message url headers body) (oc-round (user-context "hi"))
      (is (equal "ok" (nlk:json-value message :string "content")) "response.done ends the answer")
      (is (equal "https://chatgpt.com/backend-api/codex/responses" url))
      (is (equal (format nil "Bearer ~a" +oc-access+) (oc-header headers "authorization")))
      (is (equal "acct-7" (oc-header headers "chatgpt-account-id")))
      (is (equal "eu" (oc-header headers "x-openai-internal-codex-residency")))
      (is (equal "codex_cli_rs" (oc-header headers "originator")))
      (is (equal "responses=experimental" (oc-header headers "OpenAI-Beta")))
      (is (equal "0.159.0" (oc-header headers "version")))
      (is (equal "model=gpt-6.1-sol" (oc-header headers "x-codex-routing-hint")))
      (is (equal "text/event-stream" (oc-header headers "accept")))
      (is (oc-header headers "thread-id"))
      (is (equal "gpt-6.1-sol" (nlk:json-value body :string "model")))
      (is (eq nil (gethash "store" body)) "store false")
      (is (nlk:json-value body :string "instructions") "the system prompt rides as instructions")
      (is (null (nth-value 1 (gethash "max_output_tokens" body))) "no output cap: the backend refuses one")
      (is (equalp #("reasoning.encrypted_content") (gethash "include" body)))
      (let ((metadata (nlk:json-value body :object "client_metadata")))
        (is (equal "inst-1" (nlk:json-value metadata :string "x-codex-installation-id")))
        (is (equal (oc-header headers "thread-id") (nlk:json-value metadata :string "thread_id"))
            "the body's identity is the headers'")
        (is (equal (oc-header headers "x-codex-turn-metadata")
                   (nlk:json-value metadata :string "x-codex-turn-metadata")))
        (is (equal "turn" (nlk:json-value (nlk:decode-json (nlk:json-value metadata :string "x-codex-turn-metadata"))
                                          :string "request_kind")))))))

(deftest openai-codex-cell-sends-no-item-id-and-keeps-the-cores-memo ()
  (with-oc-round ()
    (let* ((assistant (nlk:json-object "role" "assistant" "content" "earlier answer"
                                       "reasoning_items" (vector (nlk:json-object "id" "rs_1" "text" "thought"
                                                                                  "encrypted_content" "enc-1"))))
           (messages (list (nle::message "user" "first") assistant (nle::message "user" "second"))))
      (flet ((reasoning (body)
               (find "reasoning" (nlk:json-value body :array "input")
                     :key (lambda (item) (nlk:json-value item :string "type")) :test #'equal)))
        (dotimes (round 2)
          (let ((item (reasoning (nth-value 3 (oc-round (compiled-context messages))))))
            (is (equal "enc-1" (nlk:json-value item :string "encrypted_content"))
                (format nil "round ~d replays the reasoning" round))
            (is (null (nth-value 1 (gethash "id" item))) (format nil "round ~d sends no item id" round))))
        (let ((nle::*provider* "openai-responses") (nle::*model* "gpt-6.1-sol"))
          (is (equal "rs_1" (nlk:json-value (reasoning (nth-value 3 (oc-round (compiled-context messages)
                                                                              :stream +oc-completed+)))
                                            :string "id"))
              "another lane still sends the item as the core built it"))))))

(deftest openai-codex-cell-says-off-and-fails-a-failed-response ()
  (with-oc-round ()
    (let ((nle::*reasoning-effort* "off"))
      (is (equal "none" (nlk:json-value (nth-value 3 (oc-round (user-context))) :string "reasoning" "effort"))
          "an effort turned off is sent as none")))
  (with-oc-round ()
    (let ((nle::*max-stream-retries* 0) (nle::*max-request-retries* 0))
      (let ((condition (signals-error nle::provider-error
                         (oc-round (user-context)
                                   :stream (list "{\"type\":\"response.failed\",\"response\":{\"status\":\"failed\",\"error\":{\"code\":\"usage_not_included\",\"message\":\"Your plan does not include this model\"}}}")))))
        (is (search "Your plan does not include this model" (nle::provider-error-detail condition)))
        (is (eql 400 (nle::provider-error-status condition)) "a code no retry can clear")))))

(deftest openai-codex-cell-leaves-other-lanes-alone ()
  (with-cell-stop ((openai-codex-start))
    (let ((nle::*provider* "openai-responses") (nle::*model* "gpt-6.1-sol") (nle::*api-key* "sk-openai")
          (nle::*endpoint* nil))
      (multiple-value-bind (message url headers body)
          (oc-round (user-context) :stream +oc-completed+)
        (declare (ignore message))
        (is (equal "https://api.openai.com/v1/responses" url))
        (is (equal "Bearer sk-openai" (oc-header headers "authorization")))
        (is (null (oc-header headers "originator")))
        (is (null (nth-value 1 (gethash "client_metadata" body))))))))

(deftest openai-codex-cell-sends-the-turns-routing-token-back ()
  ;; The backend names a sticky-routing token for the turn and its models
  ;; etag in its response headers; the turn's next request carries both.
  (with-oc-round ()
    (let ((sent '()))
      (with-stubbed-fdefinition (dex:post (asked &rest args)
                                 (push (getf args :headers) sent)
                                 (let ((answer (make-hash-table :test 'equal)))
                                   (setf (gethash "content-type" answer) "text/event-stream"
                                         (gethash "x-codex-turn-state" answer) (format nil "ts-~d" (length sent))
                                         (gethash "x-models-etag" answer) "etag-1")
                                   (values (apply #'make-truncated-sse-stream +oc-stream+) 200 answer)))
        (dotimes (round 3) (nle::call-responses-streaming (user-context "hi"))))
      (destructuring-bind (third second first) sent
        (is (null (oc-header first "x-codex-turn-state")) "the first request has none to send")
        (is (equal "ts-1" (oc-header second "x-codex-turn-state")))
        (is (equal "ts-1" (oc-header third "x-codex-turn-state")) "the first token a turn is given stays")
        (is (equal "etag-1" (oc-header second "x-models-etag")))))))
