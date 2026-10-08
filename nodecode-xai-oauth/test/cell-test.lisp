;;;; cell-test.lisp --- the xai-oauth cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every wire and every sign-in exchange a stubbed
;;;; dex:post or dex:get, every poll interval a stubbed PAUSE: nothing touches
;;;; the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "xai-oauth" "XAI-OAUTH-CELL-" :start nodecode-xai-oauth:start-cell)

(define-cell-lifecycle-tests "xai-oauth"
  (:hooks 'nle::models-catalog-table :credential 'nle::responses-request-body 'nle::walk-provider-stream)
  (:command "xai-oauth")
  (:refused ("base_url" 5)))

(defun xai-oauth-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun xai-oauth-jwt (&rest claims)
  "An unsigned JWT whose payload holds CLAIMS, alternating keys and values."
  (flet ((part (object)
           (string-right-trim "." (cl-base64:string-to-base64-string
                                   (nlk:encode-json-object object) :uri t))))
    (format nil "~a.~a.sig" (part (nlk:json-object "alg" "none"))
            (part (apply #'nlk:make-json-object claims)))))

(defun xai-oauth-now ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun xai-oauth-responses-stream ()
  "One short Responses answer, as the wire streams it."
  (make-truncated-sse-stream
   "{\"type\":\"response.created\",\"response\":{\"id\":\"r1\",\"model\":\"grok\"}}"
   "{\"type\":\"response.output_text.delta\",\"item_id\":\"m1\",\"delta\":\"ok\"}"
   "{\"type\":\"response.completed\",\"response\":{\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}}"))

(defmacro with-xai-oauth-round ((url headers body) (model &key effort (key "tok-test") auth
                                                              (provider "xai-oauth"))
                                &body forms)
  "FORMS with the cell started and one round on PROVIDER's MODEL at EFFORT
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them.
KEY is the configured key; NIL resolves the credential from AUTH, a temp
auth.json."
  `(with-cell-stop ((xai-oauth-start))
     (let ((nle::*provider* ,provider) (nle::*model* ,model) (nle::*api-key* ,key)
           (nle::*reasoning-effort* ,effort) (nle::*endpoint* nil)
           (nle::*auth-file-path* (or ,auth nle::*auth-file-path*))
           (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (xai-oauth-responses-stream) 200))
         (nle::call-provider (user-context)))
       ,@forms)))

(deftest xai-oauth-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((xai-oauth-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "xai-oauth")))
      (is (equal "xAI Grok OAuth (SuperGrok or X Premium+)" (nlk:json-value row :string "name")))
      (is (equal "https://api.x.ai/v1" (nlk:json-value row :string "api")))
      (is (gethash "grok-4.6" (nlk:json-value row :object "models")) "the bundled models are listed")
      (is (equal "openai-responses" (nle::configured-provider-lane "xai-oauth"))
          "the Responses lane drives it")
      (is (equal "https://api.x.ai/v1/responses" (nle::lane-endpoint "xai-oauth" "openai-responses"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "xai-oauth"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest xai-oauth-cell-base-follows-the-section ()
  (with-cell-stop ((xai-oauth-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "xai-oauth" "api")))))

(deftest xai-oauth-cell-reads-its-own-token-variable-only ()
  (with-cell-stop ((xai-oauth-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "XAI_OAUTH_TOKEN") "tok-env"))
          (let ((credential (nle::resolve-provider-credential "xai-oauth" :auth-path auth :probe t)))
            (is (equal "tok-env" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (cdr (assoc name '(("OPENAI_API_KEY" . "sk-openai")
                                                      ("XAI_API_KEY" . "xai-key"))
                                               :test #'equal)))
          (is (eq :public (nle:credential-source
                           (nle::resolve-provider-credential "xai-oauth" :auth-path auth :probe t)))
              "neither an OpenAI key nor an xAI API key stands in for the sign-in"))))))

(deftest xai-oauth-cell-saved-key-outranks-the-sign-in ()
  (with-cell-stop ((xai-oauth-start))
    (with-temp-auth (auth (format nil "{\"api_keys\":{\"xai-oauth\":{\"provider\":\"xai-oauth\",\"key\":\"saved\"}},~
                                      \"oauth_tokens\":{\"xai-oauth\":{\"access_token\":\"tok\",\"expires_at\":~d}}}"
                                  (+ (xai-oauth-now) 3600)))
      (let ((nle::*api-key* nil))
        (is (equal "saved"
                   (nle:credential-key (nle::resolve-provider-credential "xai-oauth" :auth-path auth :probe t))))))))

(deftest xai-oauth-cell-round-sends-the-token-and-the-grok-dialect ()
  (with-temp-auth (auth (format nil "{\"oauth_tokens\":{\"xai-oauth\":{\"access_token\":\"tok-1\",~
                                     \"refresh_token\":\"r-1\",\"expires_at\":~d}}}"
                                (+ (xai-oauth-now) 3600)))
    (with-xai-oauth-round (url headers body) ("grok-4.5" :effort "xhigh" :key nil :auth auth)
      (is (equal "https://api.x.ai/v1/responses" url))
      (is (equal "Bearer tok-1" (xai-oauth-header headers "authorization")) "the stored token, as a bearer")
      (is (equal "grok-4.5" (nlk:json-value body :string "model")))
      (is (equal "high" (nlk:json-value body :string "reasoning" "effort")) "xhigh is high on grok-4.5")
      (is (null (nlk:json-value body :any "reasoning" "summary")) "and no summary is asked")
      (is (equalp #("reasoning.encrypted_content") (nlk:json-value body :array "include"))))))

(deftest xai-oauth-cell-xhigh-stays-where-the-model-takes-it ()
  (with-xai-oauth-round (url headers body) ("grok-4.6" :effort "xhigh")
    (is (equal "xhigh" (nlk:json-value body :string "reasoning" "effort"))))
  (with-xai-oauth-round (url headers body) ("grok-4.6" :effort "minimal")
    (is (equal "low" (nlk:json-value body :string "reasoning" "effort")))))

(deftest xai-oauth-cell-a-self-reasoning-model-is-asked-no-effort ()
  (with-xai-oauth-round (url headers body) ("grok-build" :effort "high")
    (is (null (nlk:json-value body :any "reasoning")) "grok-build refuses reasoning.effort")
    (is (equalp #("reasoning.encrypted_content") (nlk:json-value body :array "include"))
        "but its reasoning is still kept for the next round"))
  (with-xai-oauth-round (url headers body) ("grok-4.20-0309-non-reasoning" :effort "high")
    (is (null (nlk:json-value body :any "reasoning")))
    (is (null (nlk:json-value body :any "include")) "a model that does not reason is asked nothing")))

(deftest xai-oauth-cell-names-the-conversation ()
  (with-stubbed-fdefinition (nodecode-xai-oauth::session-id () "s-42")
    (with-xai-oauth-round (url headers body) ("grok-4.6")
      (is (equal "s-42" (xai-oauth-header headers "x-grok-conv-id"))))))

(deftest xai-oauth-cell-flattens-a-bare-required-root-union ()
  (let* ((schema (nlk:decode-json "{\"type\":\"object\",\"properties\":{\"a\":{\"type\":\"string\"},\"b\":{\"type\":\"string\"}},\"anyOf\":[{\"required\":[\"a\"]},{\"required\":[\"b\"],\"description\":\"or b\"}]}"))
         (flat (nodecode-xai-oauth::flattened-parameters schema)))
    (is (null (nlk:json-value flat :any "anyOf")) "the union of bare required keys goes")
    (is (nlk:json-value flat :object "properties" "a") "and the object stays")
    (is (nlk:json-value schema :array "anyOf") "the caller's schema is not touched"))
  (let ((typed (nlk:decode-json "{\"type\":\"object\",\"oneOf\":[{\"type\":\"object\",\"required\":[\"a\"]}]}")))
    (is (eq typed (nodecode-xai-oauth::flattened-parameters typed)) "a union that says more is kept")))

(deftest xai-oauth-cell-leaves-other-providers-alone ()
  (with-stubbed-fdefinition (nodecode-xai-oauth::session-id () "s-42")
    (with-xai-oauth-round (url headers body) ("gpt-6" :effort "xhigh" :provider "openai-responses")
      (is (equal "xhigh" (nlk:json-value body :string "reasoning" "effort")))
      (is (null (xai-oauth-header headers "x-grok-conv-id"))))))

(defmacro with-xai-oauth-sign-in ((requests pauses) (&rest posts) (&rest gets) &body forms)
  "FORMS with the cell started, a temp auth.json as the store, and every
sign-in exchange answered in order: POSTS by dex:post, GETS by dex:get, each
a (URL BODY STATUS). REQUESTS collects (METHOD URL CONTENT HEADERS), PAUSES
each poll interval the flow would have slept."
  `(with-cell-stop ((xai-oauth-start))
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
                (nodecode-xai-oauth::pause (seconds flow) (push seconds ,pauses)))
             ,@forms))))))

(defparameter +xai-oauth-discovery+
  '("https://auth.x.ai/.well-known/openid-configuration"
    "{\"issuer\":\"https://auth.x.ai\",\"token_endpoint\":\"https://auth.x.ai/oauth2/token\"}" 200))

(deftest xai-oauth-cell-login-discovers-polls-and-stores-the-token ()
  (let ((access (xai-oauth-jwt "sub" "acct-7" "exp" (+ (xai-oauth-now) 3600))))
    (with-xai-oauth-sign-in (requests pauses)
        ((list "https://auth.x.ai/oauth2/device/code"
               "{\"user_code\":\"WXYZ-0000\",\"device_code\":\"dc-x\",\"verification_uri\":\"https://accounts.x.ai/device\",\"verification_uri_complete\":\"https://accounts.x.ai/device?code=WXYZ-0000\",\"interval\":5,\"expires_in\":900}"
               200)
         (list "https://auth.x.ai/oauth2/token" "{\"error\":\"authorization_pending\"}" 400)
         (list "https://auth.x.ai/oauth2/token"
               (format nil "{\"access_token\":\"~a\",\"refresh_token\":\"r-x\",\"expires_in\":3600}" access) 200))
        (+xai-oauth-discovery+
         (list "https://auth.x.ai/oauth2/userinfo" "{\"sub\":\"acct-7\",\"email\":\"grok@example.com\"}" 200))
      (let ((text (cell-entry "nodecode-xai-oauth" "xai-oauth" "login")))
        (is (search "https://accounts.x.ai/device?code=WXYZ-0000" text))
        (is (search "WXYZ-0000" text)))
      (is (await (:timeout 5) (cell-notice "nodecode-xai-oauth")) "the outcome comes as a notice")
      (is (search "signed in as grok@example.com" (second (cell-notice "nodecode-xai-oauth"))))
      (let ((entry (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "xai-oauth")))
        (is (equal access (nlk:json-value entry :string "access_token")))
        (is (equal "r-x" (nlk:json-value entry :string "refresh_token")))
        (is (<= (abs (- (nlk:json-value entry :integer "expires_at") (+ (xai-oauth-now) 3600))) 5))
        (is (equal "acct-7" (nlk:json-value entry :string "account_id")))
        (is (equal "grok@example.com" (nlk:json-value entry :string "email"))))
      (is (equal "k" (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :string "api_keys" "other" "key")))
      (destructuring-bind (device discovery pending done userinfo) (reverse requests)
        (is (search "client_id=b1a00492-073a-47ea-816f-4c329264a828" (third device)))
        (is (search "scope=openid%20profile%20email%20offline_access%20grok-cli%3Aaccess%20api%3Aaccess"
                    (third device)))
        (is (equal "application/json" (xai-oauth-header (fourth device) "Accept")))
        (is (eq :get (first discovery)))
        (is (search "device_code=dc-x" (third pending)))
        (is (equal "application/json" (xai-oauth-header (fourth done) "Accept")))
        (is (equal (format nil "Bearer ~a" access) (xai-oauth-header (fourth userinfo) "Authorization"))))
      (is (equal '(5) pauses)))))

(deftest xai-oauth-cell-refuses-a-token-endpoint-off-x-ai ()
  (with-xai-oauth-sign-in (requests pauses)
      ((list "https://auth.x.ai/oauth2/device/code"
             "{\"user_code\":\"C\",\"device_code\":\"d\",\"verification_uri\":\"https://accounts.x.ai/device\"}" 200))
      ((list "https://auth.x.ai/.well-known/openid-configuration"
             "{\"token_endpoint\":\"https://evil.example/token\"}" 200))
    (is (search "Invalid xAI token_endpoint" (cell-entry "nodecode-xai-oauth" "xai-oauth" "login"))
        "no refresh token is ever sent off x.ai")
    (is (null nodecode-xai-oauth::*flow*) "and nothing polls")))

(deftest xai-oauth-cell-refreshes-at-the-discovered-endpoint ()
  (with-xai-oauth-sign-in (requests pauses)
      ((list "https://auth.x.ai/oauth2/token" "{\"access_token\":\"tok-2\",\"refresh_token\":\"r-2\",\"expires_in\":3600}" 200))
      (+xai-oauth-discovery+
       (list "https://auth.x.ai/oauth2/userinfo" "{}" 401))
    (nodecode-xai-oauth::save-entry
     (nlk:json-object "access_token" "tok-1" "refresh_token" "r-1"
                      "expires_at" (+ (xai-oauth-now) 10) "account_id" "acct-7" "email" "grok@example.com")
     nle::*auth-file-path*)
    (let ((nle::*api-key* nil))
      (is (eq :oauth (nle::provider-auth-state "xai-oauth" :auth-path nle::*auth-file-path*)))
      (is (null requests) "a probe costs no network")
      (is (equal "tok-2" (nle:credential-key
                          (nle::resolve-provider-credential "xai-oauth" :auth-path nle::*auth-file-path*)))))
    (destructuring-bind (discovery refresh userinfo) (reverse requests)
      (is (eq :get (first discovery)))
      (is (equal "grant_type=refresh_token&client_id=b1a00492-073a-47ea-816f-4c329264a828&refresh_token=r-1"
                 (third refresh)))
      (is (null (xai-oauth-header (fourth refresh) "Accept")) "a refresh carries no Accept header, as omp's")
      (is (equal "https://auth.x.ai/oauth2/userinfo" (second userinfo))))
    (let ((entry (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "xai-oauth")))
      (is (equal "tok-2" (nlk:json-value entry :string "access_token")))
      (is (equal "r-2" (nlk:json-value entry :string "refresh_token")) "a rotated refresh token is kept")
      (is (equal "grok@example.com" (nlk:json-value entry :string "email"))
          "a failed userinfo keeps the identity the store had"))))

(deftest xai-oauth-cell-status-and-logout ()
  (with-xai-oauth-sign-in (requests pauses) () ()
    (is (search "not signed in" (cell-entry "nodecode-xai-oauth" "xai-oauth" "status")))
    (nodecode-xai-oauth::save-entry
     (nlk:json-object "access_token" "tok" "refresh_token" "r" "expires_at" (+ (xai-oauth-now) 600)
                      "email" "grok@example.com")
     nle::*auth-file-path*)
    (is (search "signed in as grok@example.com" (cell-entry "nodecode-xai-oauth" "xai-oauth" "")))
    (is (search "signed out" (cell-entry "nodecode-xai-oauth" "xai-oauth" "logout")))
    (is (null (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "xai-oauth")))))
