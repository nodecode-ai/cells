;;;; cell-test.lisp --- the snowflake cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post,
;;;; every token exchange a stubbed dex:request, the sign-in's callback a
;;;; free loopback port the test dials: nothing touches the network, the
;;;; environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "snowflake" "SNOWFLAKE-CELL-" :start nodecode-snowflake:start-cell)

(define-cell-lifecycle-tests "snowflake"
  (:hooks 'nle::models-catalog-table 'nle::list-provider-models 'nle::resolve-model-lane
          :credential 'nle::request-body 'nle::walk-provider-stream)
  (:command "snowflake")
  (:refused ("account" 5)))

(defun sf-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun sf-now () (nodecode-snowflake::unix-seconds))

(defun sf-store (&key (access "sf-access") (refresh "sf-refresh") (expires-in 3600)
                      (account "https://myorg-myaccount.snowflakecomputing.com"))
  "auth.json text holding one snowflake sign-in."
  (format nil "{\"api_keys\":{\"openai\":{\"provider\":\"openai\",\"key\":\"sk-kept\"}},~
               \"oauth_tokens\":{\"snowflake\":{\"provider\":\"snowflake\",\"access_token\":\"~a\",~
               \"refresh_token\":\"~a\",\"expires_at\":~d,\"account_url\":\"~a\"}}}"
          access refresh (+ (sf-now) expires-in) account))

(defun sf-entry (auth)
  "The snowflake entry of the auth.json at AUTH."
  (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "snowflake"))

(defun sf-form (content)
  "The x-www-form-urlencoded CONTENT as an alist."
  (quri:url-decode-params content))

(defun sf-field (form name)
  (cdr (assoc name form :test #'equal)))

(defun sf-stream (lane)
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

(defmacro with-sf-round ((url headers body &key (auth "{}") (env ''()) (section ''()) key) model &body forms)
  "FORMS with the cell started on SECTION and one round of MODEL captured:
URL, HEADERS and BODY (the decoded request) as dex:post saw them; the store
is AUTH, the environment ENV (an alist), the config tier's key KEY."
  `(with-stubbed-fdefinition (nle::credential-env (name) (cdr (assoc name ,env :test #'equal)))
     (with-cell-stop ((apply #'snowflake-start ,section))
       (with-temp-auth (auth-path ,auth)
         (let ((nle::*provider* "snowflake") (nle::*model* ,model) (nle::*api-key* ,key)
               (nle::*endpoint* nil) (nle::*auth-file-path* (pathname auth-path))
               (,url nil) (,headers nil) (,body nil))
           (declare (ignorable ,url ,headers ,body))
           (with-stubbed-fdefinition
               (dex:post (asked &rest args)
                (setf ,url asked ,headers (getf args :headers)
                      ,body (nlk:decode-json (getf args :content)))
                (values (sf-stream (nle::resolve-model-lane "snowflake" ,model)) 200))
             (nle::call-provider (user-context)))
           ,@forms)))))

;;; --- the catalog and the account ---------------------------------------------------

(deftest snowflake-cell-puts-its-row-in-the-catalog ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((snowflake-start))
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "snowflake")))
        (is (equal "Snowflake Cortex" (nlk:json-value row :string "name")))
        (is (equal "https://snowflake-account.invalid/api/v2/cortex/v1" (nlk:json-value row :string "api"))
            "no account named: omp's placeholder, which never resolves")
        (is (= 15 (hash-table-count (nlk:json-value row :object "models"))))
        (is (equal "anthropic" (nle::resolve-model-lane "snowflake" "claude-opus-5-5")))
        (is (equal "openai-completions" (nle::resolve-model-lane "snowflake" "openai-gpt-5.1")))
        (is (equal "https://snowflake-account.invalid/api/v2/cortex/v1/messages"
                   (nle::lane-endpoint "snowflake" "anthropic")))
        (is (find "openai-gpt-5" (nle::list-provider-models "snowflake")
                  :key (lambda (row) (getf row :id)) :test #'equal)
            "the listing is the roster, asked of no endpoint"))
      (funcall stop)
      (setf stop nil)
      (is (null (nlk:json-value (nle::models-catalog-table) :object "snowflake"))))))

(deftest snowflake-cell-says-a-connect-pat-was-not-checked ()
  ;; The roster answered /connect's key check with no reason, which the core
  ;; reads as a PAT Snowflake took: any PAT read `works'. Snowflake is asked
  ;; nothing, so the verdict is unchecked, even where an asked endpoint would
  ;; have refused the PAT.
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((snowflake-start "account" "myorg-myaccount"))
      (with-temp-file (nle::*provider-models-cache-path*)
        (let ((asked '()))
          (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                     (push url asked)
                                     (values "{\"error\":{\"code\":\"390303\"}}" 401))
            (multiple-value-bind (verdict words) (nle::provider-key-check "snowflake" "pat-wrong")
              (is (eq :unchecked verdict))
              (is (search "first turn tries the token" words)))
            (is (null asked) "nothing was asked")
            (multiple-value-bind (rows reason) (nle::list-provider-models "snowflake")
              (is rows)
              (is (null reason) "the picker's listing, with no key, is the roster as before"))))))))

(deftest snowflake-cell-base-follows-the-account ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((snowflake-start "account" "MyOrg_MyAccount"))
      (is (equal "https://myorg-myaccount.snowflakecomputing.com/api/v2/cortex/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "snowflake" "api"))))))

(deftest snowflake-cell-normalizes-an-account-as-omp-does ()
  (with-cell-stop ((snowflake-start))
    (loop for (input expected) in '(("myorg-myaccount" "https://myorg-myaccount.snowflakecomputing.com")
                                    ("MyOrg_MyAccount" "https://myorg-myaccount.snowflakecomputing.com")
                                    ("https://myorg-myaccount.snowflakecomputing.com:443/console"
                                     "https://myorg-myaccount.snowflakecomputing.com")
                                    ("https://app.snowflake.com/myorg/myaccount/#/worksheets"
                                     "https://myorg-myaccount.snowflakecomputing.com")
                                    ("xy12345.us-east-2.aws" "https://xy12345.us-east-2.aws.snowflakecomputing.com"))
          do (is (equal expected (nodecode-snowflake::normalize-account-url input)) input))
    (loop for (input words) in '(("" "account is required")
                                 ("http://myorg-myaccount.snowflakecomputing.com" "must use https")
                                 ("https://evil.example.com" "account identifier")
                                 ("myorg/x" "account identifier")
                                 ("myorg.snowflakecomputing.cn" "China-region")
                                 ("https://app.snowflake.com/us-east-1/xy12345" "Legacy Snowsight"))
          do (let ((condition (handler-case (progn (nodecode-snowflake::normalize-account-url input) nil)
                                (nodecode-snowflake::snowflake-error (condition) condition))))
               (is (and condition (search words (princ-to-string condition))) input)))))

;;; --- a PAT --------------------------------------------------------------------------

(deftest snowflake-cell-sends-a-pat-to-the-account-on-the-messages-wire ()
  (with-sf-round (url headers body :env '(("SNOWFLAKE_PAT" . "pat-1")
                                          ("SNOWFLAKE_ACCOUNT" . "myorg-myaccount")
                                          ("ANTHROPIC_API_KEY" . "sk-ant")))
      "claude-opus-5-5"
    (is (equal "https://myorg-myaccount.snowflakecomputing.com/api/v2/cortex/v1/messages" url))
    (is (equal "Bearer pat-1" (sf-header headers "Authorization")))
    (is (null (sf-header headers "x-api-key")) "a non-Anthropic host gets the key as a bearer only")
    (is (equal "claude-opus-5-5" (nlk:json-value body :string "model")))))

(deftest snowflake-cell-sends-a-gpt-model-on-the-chat-wire ()
  (with-sf-round (url headers body :env '(("SNOWFLAKE_PAT" . "pat-1")) :section '("account" "myorg-myaccount"))
      "openai-gpt-5.1"
    (is (equal "https://myorg-myaccount.snowflakecomputing.com/api/v2/cortex/v1/chat/completions" url))
    (is (equal "Bearer pat-1" (sf-header headers "Authorization")))
    (is (equal "openai-gpt-5.1" (nlk:json-value body :string "model")))
    (is (null (nlk:json-value body :any "max_tokens")) "max_tokens is a hard error on Cortex")
    (is (null (nlk:json-value body :any "prompt_cache_key")))))

(deftest snowflake-cell-saved-pat-outranks-the-variable ()
  (with-cell-stop ((snowflake-start))
    (with-temp-auth (auth "{\"api_keys\":{\"snowflake\":{\"provider\":\"snowflake\",\"key\":\"pat-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "SNOWFLAKE_PAT") "pat-env"))
        (let ((nle::*api-key* nil))
          (is (equal "pat-saved"
                     (nle:credential-key (nle::resolve-provider-credential "snowflake" :auth-path auth :probe t)))))))))

(deftest snowflake-cell-never-sends-another-familys-key ()
  (with-cell-stop ((snowflake-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (cdr (assoc name '(("ANTHROPIC_API_KEY" . "sk-ant") ("OPENAI_API_KEY" . "sk-oai"))
                                             :test #'equal)))
        (let ((nle::*api-key* nil))
          (is (eq :public (nle:credential-source
                           (nle::resolve-provider-credential "snowflake" :auth-path auth :probe t)))))))))

(deftest snowflake-cell-refuses-a-pat-with-no-account ()
  (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "SNOWFLAKE_PAT") "pat-1"))
    (with-cell-stop ((snowflake-start))
      (with-temp-auth (auth "{}")
        (let ((nle::*provider* "snowflake") (nle::*model* "claude-opus-5-5") (nle::*api-key* nil)
              (nle::*endpoint* nil) (nle::*auth-file-path* (pathname auth)) (posted nil))
          (with-stubbed-fdefinition (dex:post (&rest args) (setf posted t) (values nil 500))
            (let ((condition (handler-case (progn (nle::call-anthropic-streaming (user-context)) nil)
                               (nle::provider-error (condition) condition))))
              (is (typep condition 'nle::provider-config-error))
              (is (search "account is required" (nle::provider-error-detail condition)))
              (is (not posted) "nothing goes to the placeholder"))))))))

(deftest snowflake-cell-reads-omps-structured-key ()
  (with-sf-round (url headers body
                      :key "{\"token\":\"tok-9\",\"enterpriseUrl\":\"https://other-acct.snowflakecomputing.com\"}"
                      :section '("account" "myorg-myaccount"))
      "claude-sonnet-5"
    (is (equal "https://other-acct.snowflakecomputing.com/api/v2/cortex/v1/messages" url)
        "the key's account outranks the section's")
    (is (equal "Bearer tok-9" (sf-header headers "Authorization")))))

;;; --- the sign-in ------------------------------------------------------------------------

(deftest snowflake-cell-sends-the-sign-in-to-its-account ()
  (with-sf-round (url headers body :auth (sf-store) :section '("account" "elsewhere")
                      :env '(("SNOWFLAKE_PAT" . "pat-1")))
      "claude-haiku-4-5"
    (is (equal "https://myorg-myaccount.snowflakecomputing.com/api/v2/cortex/v1/messages" url)
        "the sign-in's account, not the section's")
    (is (equal "Bearer sf-access" (sf-header headers "Authorization")) "the sign-in outranks SNOWFLAKE_PAT")))

(defun sf-token-answer (&key (access "sf-new") (refresh "sf-refresh-2"))
  (format nil "{\"access_token\":\"~a\",~@[\"refresh_token\":\"~a\",~]\"expires_in\":600,\"token_type\":\"Bearer\"}"
          access refresh))

(deftest snowflake-cell-refreshes-an-expiring-token ()
  (with-cell-stop ((snowflake-start))
    (with-temp-auth (auth (sf-store :access "sf-old" :expires-in 30))
      (let ((posts '()))
        (with-stubbed-fdefinition (dex:request (asked &rest args)
                                   (push (list asked (sf-form (getf args :content)) (getf args :max-redirects)) posts)
                                   (values (sf-token-answer) 200))
          (is (equal "sf-old" (nle:credential-key (nle::resolve-provider-credential "snowflake" :auth-path auth :probe t)))
              "a probe reads the store as it is")
          (is (null posts) "and never dials")
          (is (equal "sf-new" (nle:credential-key (nle::resolve-provider-credential
                                                   "snowflake" :auth-path auth :endpoint "https://x/messages")))))
        (is (= 1 (length posts)))
        (destructuring-bind (asked form redirects) (first posts)
          (is (equal "https://myorg-myaccount.snowflakecomputing.com/oauth/token-request" asked))
          (is (equal "refresh_token" (sf-field form "grant_type")))
          (is (equal "sf-refresh" (sf-field form "refresh_token")))
          (is (equal "LOCAL_APPLICATION" (sf-field form "client_id")))
          (is (eql 0 redirects) "no redirect is followed"))
        (let ((entry (sf-entry auth)))
          (is (equal "sf-new" (nlk:json-value entry :string "access_token")) "written back")
          (is (equal "sf-refresh-2" (nlk:json-value entry :string "refresh_token")))
          (is (equal "https://myorg-myaccount.snowflakecomputing.com" (nlk:json-value entry :string "account_url")))
          (is (<= (abs (- (nlk:json-value entry :integer "expires_at") (+ (sf-now) 600))) 5)))
        (is (equal "sk-kept" (nle::auth-api-key (nle::read-auth-file auth) "openai")) "every other field kept")))))

(deftest snowflake-cell-keeps-a-token-without-a-refresh-until-it-expires ()
  (with-cell-stop ((snowflake-start))
    (with-stubbed-fdefinition (dex:request (&rest args) (values (sf-token-answer) 200))
      (with-temp-auth (auth (sf-store :access "sf-live" :refresh "" :expires-in 30))
        (is (equal "sf-live" (nle:credential-key (nle::resolve-provider-credential
                                                  "snowflake" :auth-path auth :endpoint "https://x/messages")))
            "no refresh token: the token stands while it is good"))
      (with-temp-auth (auth (sf-store :access "sf-dead" :refresh "" :expires-in -10))
        (let ((condition (signals-error nle:credential-error
                           (nle::resolve-provider-credential "snowflake" :auth-path auth :endpoint "https://x/messages"))))
          (is (search "did not issue a refresh token" (princ-to-string condition))))))))

(defun sf-get (port target)
  "GET TARGET from 127.0.0.1:PORT over a raw socket: the answer's status code."
  (let ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket))
               (bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
           (write-sequence (sb-ext:string-to-octets
                            (format nil "GET ~a HTTP/1.1~c~cHost: 127.0.0.1~c~c~c~c"
                                    target #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
                            :external-format :latin-1)
                           stream)
           (finish-output stream)
           (loop for byte = (read-byte stream nil nil) while byte do (vector-push-extend byte bytes))
           (parse-integer (sb-ext:octets-to-string bytes :external-format :latin-1) :start 9 :end 12))
      (usocket:socket-close socket))))

(defun sf-query (url name)
  (cdr (assoc name (quri:uri-query-params (quri:uri url)) :test #'equal)))

(defun sf-login-url (text)
  (find-if (lambda (line) (uiop:string-prefix-p "https://" line))
           (uiop:split-string text :separator '(#\Newline))))

(defun sf-await-login ()
  (alexandria:when-let (login nodecode-snowflake::*login*)
    (bt2:join-thread (nodecode-snowflake::login-thread login))))

(defmacro with-sf-login ((auth url posts &key (account "myorg-myaccount")) &body body)
  "BODY with the cell started, a sign-in to ACCOUNT begun on a free port
against a temp auth.json AUTH, URL its authorization address and POSTS the
(URL FORM) pairs the token endpoint saw; the endpoint answers a token."
  `(with-cell-stop ((snowflake-start))
     (with-temp-auth (,auth "{\"api_keys\":{\"openai\":{\"provider\":\"openai\",\"key\":\"sk-kept\"}}}")
       (let ((nle::*auth-file-path* ,auth)
             (nodecode-snowflake::*callback-port* 0)
             (,posts '()))
         (with-stubbed-fdefinition (dex:request (asked &rest args)
                                    (push (list asked (sf-form (getf args :content))) ,posts)
                                    (values (sf-token-answer :access "sf-signed" :refresh "sf-r") 200))
           (let ((,url (sf-login-url (cell-entry "nodecode-snowflake" "snowflake"
                                                 (format nil "login ~a" ,account)))))
             (declare (ignorable ,url))
             ,@body))))))

(deftest snowflake-cell-signs-in-through-the-callback ()
  (with-sf-login (auth url posts)
    (is (uiop:string-prefix-p "https://myorg-myaccount.snowflakecomputing.com/oauth/authorize?" url))
    (is (equal "LOCAL_APPLICATION" (sf-query url "client_id")))
    (is (equal "code" (sf-query url "response_type")))
    (is (equal "refresh_token" (sf-query url "scope")))
    (is (equal "S256" (sf-query url "code_challenge_method")))
    (let* ((redirect (sf-query url "redirect_uri"))
           (port (parse-integer redirect :start 17 :end (position #\/ redirect :start 17))))
      (is (ppcre:scan "^http://127\\.0\\.0\\.1:\\d+/$" redirect) "the loopback root, as omp asks")
      (is (= 404 (sf-get port "/elsewhere")) "only the callback path answers")
      (is (= 500 (sf-get port "/?code=c-1&state=forged")) "a forged state is refused")
      (is (= 200 (sf-get port (format nil "/?code=c-1&state=~a" (sf-query url "state")))))
      (sf-await-login)
      (is (= 1 (length posts)) "one exchange")
      (destructuring-bind (asked form) (first posts)
        (is (equal "https://myorg-myaccount.snowflakecomputing.com/oauth/token-request" asked))
        (is (equal "authorization_code" (sf-field form "grant_type")))
        (is (equal "c-1" (sf-field form "code")))
        (is (equal redirect (sf-field form "redirect_uri")))
        (is (equal "LOCAL_APPLICATION" (sf-field form "client_id")))
        (is (equal (sf-query url "code_challenge")
                   (nodecode-snowflake::base64url (nodecode-snowflake::sha256 (sf-field form "code_verifier"))))
            "the verifier is the one the challenge was made of")))
    (let ((entry (sf-entry auth)))
      (is (equal "sf-signed" (nlk:json-value entry :string "access_token")))
      (is (equal "sf-r" (nlk:json-value entry :string "refresh_token")))
      (is (equal "https://myorg-myaccount.snowflakecomputing.com" (nlk:json-value entry :string "account_url")))
      (is (<= (abs (- (nlk:json-value entry :integer "expires_at") (+ (sf-now) 600))) 5) "epoch seconds"))
    (is (equal "sk-kept" (nle::auth-api-key (nle::read-auth-file auth) "openai")) "every other field kept")
    (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring auth))))))
    (is (search "signed in to https://myorg-myaccount.snowflakecomputing.com"
                (cell-entry "nodecode-snowflake" "snowflake" "status")))))

(deftest snowflake-cell-takes-a-pasted-address ()
  (with-sf-login (auth url posts)
    (is (search "another sign-in"
                (cell-entry "nodecode-snowflake" "snowflake" "code http://127.0.0.1:54551/?code=c-9&state=other")))
    (is (search "code received"
                (cell-entry "nodecode-snowflake" "snowflake"
                            (format nil "code http://127.0.0.1:54551/?code=c-9&state=~a" (sf-query url "state")))))
    (sf-await-login)
    (is (equal "c-9" (sf-field (second (first posts)) "code")))
    (is (equal "sf-signed" (nlk:json-value (sf-entry auth) :string "access_token")))))

(deftest snowflake-cell-falls-back-from-a-busy-callback-port ()
  (let ((held (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect
         (with-cell-stop ((snowflake-start))
           (with-temp-auth (auth "{}")
             (let ((nle::*auth-file-path* auth)
                   (nodecode-snowflake::*callback-port* (usocket:get-local-port held)))
               (let ((url (sf-login-url (cell-entry "nodecode-snowflake" "snowflake" "login myorg-myaccount"))))
                 (is url "the sign-in still starts")
                 (is (not (search (format nil ":~d/" (usocket:get-local-port held)) (sf-query url "redirect_uri")))
                     "on another port, as omp's callback falls back"))
               (cell-entry "nodecode-snowflake" "snowflake" "logout"))))
      (usocket:socket-close held))))

(deftest snowflake-cell-refuses-a-login-to-a-foreign-host ()
  (with-cell-stop ((snowflake-start))
    (is (search "account identifier"
                (cell-entry "nodecode-snowflake" "snowflake" "login https://evil.example.com")))
    (is (null nodecode-snowflake::*login*))))

(deftest snowflake-cell-logs-out ()
  (with-cell-stop ((snowflake-start))
    (with-temp-auth (auth (sf-store))
      (let ((nle::*auth-file-path* auth))
        (is (search "signed out" (cell-entry "nodecode-snowflake" "snowflake" "logout")))
        (is (null (sf-entry auth)))
        (is (equal "sk-kept" (nle::auth-api-key (nle::read-auth-file auth) "openai")))
        (is (search "not signed in" (cell-entry "nodecode-snowflake" "snowflake" "")))))))

(deftest snowflake-cell-leaves-other-providers-alone ()
  (with-cell-stop ((snowflake-start "account" "myorg-myaccount"))
    (let ((nle::*provider* "anthropic") (nle::*model* "claude-opus-5") (nle::*api-key* "sk-ant")
          (nle::*endpoint* nil) (url nil) (headers nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked headers (getf args :headers))
           (values (sf-stream "anthropic") 200))
        (nle::call-provider (user-context)))
      (is (equal "https://api.anthropic.com/v1/messages" url))
      (is (equal "sk-ant" (sf-header headers "x-api-key"))))))
