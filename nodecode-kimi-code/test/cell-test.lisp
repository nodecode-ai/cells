;;;; cell-test.lisp --- the kimi-code cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every wire and every sign-in exchange a stubbed
;;;; dex:post, every poll interval a stubbed PAUSE: nothing touches the
;;;; network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "kimi-code" "KIMI-CODE-CELL-" :start nodecode-kimi-code:start-cell)

(define-cell-lifecycle-tests "kimi-code"
  (:hooks 'nle::models-catalog-table :credential 'nle::list-provider-models 'nle::anthropic-request-body
          'nle::walk-provider-stream)
  (:command "kimi-code")
  (:refused ("base_url" 5)))

(defun kimi-code-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun kimi-code-jwt (&rest claims)
  "An unsigned JWT whose payload holds CLAIMS, alternating keys and values."
  (flet ((part (object)
           (string-right-trim "." (cl-base64:string-to-base64-string
                                   (nlk:encode-json-object object) :uri t))))
    (format nil "~a.~a.sig" (part (nlk:json-object "alg" "none"))
            (part (apply #'nlk:make-json-object claims)))))

(defun kimi-code-now ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun kimi-code-messages-stream ()
  "One short Messages answer, as the Anthropic wire streams it."
  (make-truncated-sse-stream
   "{\"type\":\"message_start\",\"message\":{\"id\":\"m1\",\"model\":\"k\",\"usage\":{\"input_tokens\":1}}}"
   "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
   "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}"
   "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}"
   "{\"type\":\"message_stop\"}"))

(defmacro with-kimi-code-round ((url headers body) (model &key effort (key "sk-test") auth) &body forms)
  "FORMS with the cell started and one round on kimi-code MODEL at EFFORT
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them.
KEY is the configured key; NIL resolves the credential from AUTH, a temp
auth.json."
  `(with-cell-stop ((kimi-code-start))
     (let ((nle::*provider* "kimi-code") (nle::*model* ,model) (nle::*api-key* ,key)
           (nle::*reasoning-effort* ,effort) (nle::*endpoint* nil)
           (nle::*auth-file-path* (or ,auth nle::*auth-file-path*))
           (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (kimi-code-messages-stream) 200))
         (nle::call-provider (user-context)))
       ,@forms)))

(deftest kimi-code-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((kimi-code-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "kimi-code")))
      (is (equal "Kimi Code" (nlk:json-value row :string "name")))
      (is (equal "https://api.kimi.com/coding/v1" (nlk:json-value row :string "api")))
      (is (gethash "kimi-for-coding" (nlk:json-value row :object "models")) "the bundled models are listed")
      (is (equal "anthropic" (nle::configured-provider-lane "kimi-code"))
          "the Messages lane drives it, the wire omp declares for every bundled model")
      (is (equal "https://api.kimi.com/coding/v1/messages"
                 (nle::lane-endpoint "kimi-code" "anthropic"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "kimi-code"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest kimi-code-cell-base-follows-the-section ()
  (with-cell-stop ((kimi-code-start "base_url" "https://relay.example/coding/v1"))
    (is (equal "https://relay.example/coding/v1"
               (nlk:json-value (nle::models-catalog-table) :string "kimi-code" "api")))))

(deftest kimi-code-cell-reads-the-key-variables-and-never-the-family-default ()
  (with-cell-stop ((kimi-code-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "KIMI_API_KEY") "sk-kimi"))
          (let ((credential (nle::resolve-provider-credential "kimi-code" :auth-path auth :probe t)))
            (is (equal "sk-kimi" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "ANTHROPIC_API_KEY") "sk-ant"))
          (is (eq :public (nle:credential-source
                           (nle::resolve-provider-credential "kimi-code" :auth-path auth :probe t)))
              "an Anthropic key is never sent to Kimi")
          (is (equal "sk-ant" (nle:credential-key
                               (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t)))
              "and Anthropic's own ladder is untouched"))))))

(deftest kimi-code-cell-saved-key-outranks-the-sign-in ()
  (with-cell-stop ((kimi-code-start))
    (with-temp-auth (auth (format nil "{\"api_keys\":{\"kimi-code\":{\"provider\":\"kimi-code\",\"key\":\"sk-saved\"}},~
                                      \"oauth_tokens\":{\"kimi-code\":{\"access_token\":\"tok\",\"expires_at\":~d}}}"
                                  (+ (kimi-code-now) 3600)))
      (let ((nle::*api-key* nil))
        (is (equal "sk-saved"
                   (nle:credential-key (nle::resolve-provider-credential "kimi-code" :auth-path auth :probe t))))))))

(deftest kimi-code-cell-round-carries-the-token-as-a-bearer-and-the-fingerprint ()
  (with-temp-auth (auth (format nil "{\"oauth_tokens\":{\"kimi-code\":{\"access_token\":\"tok-1\",~
                                     \"refresh_token\":\"r-1\",\"expires_at\":~d}}}"
                                (+ (kimi-code-now) 3600)))
    (with-kimi-code-round (url headers body) ("kimi-for-coding" :effort "high" :key nil :auth auth)
      (is (equal "https://api.kimi.com/coding/v1/messages" url))
      (is (equal "Bearer tok-1" (kimi-code-header headers "Authorization")) "the stored token, as a bearer")
      (is (null (kimi-code-header headers "x-api-key")) "and no x-api-key beside it")
      (is (equal "KimiCLI/18.8.3" (kimi-code-header headers "User-Agent")))
      (is (equal "kimi_cli" (kimi-code-header headers "X-Msh-Platform")))
      (is (plusp (length (kimi-code-header headers "X-Msh-Device-Id"))))
      (is (equal "cli" (kimi-code-header headers "x-app")))
      (is (equal "kimi-for-coding" (nlk:json-value body :string "model")))
      (is (equal "adaptive" (nlk:json-value body :string "thinking" "type"))
          "Kimi's own models think adaptively")
      (is (null (nlk:json-value body :any "thinking" "budget_tokens")))
      (is (equal "high" (nlk:json-value body :string "output_config" "effort")))
      (is (equal "all" (nlk:json-value (aref (nlk:json-value body :array "context_management" "edits") 0)
                                       :string "keep"))
          "every replayed thinking block is kept")
      (is (search "context-management-2025-06-27" (or (kimi-code-header headers "anthropic-beta") "")))
      (is (search "interleaved-thinking-2025-05-14" (or (kimi-code-header headers "anthropic-beta") ""))))))

(deftest kimi-code-cell-names-the-session ()
  (with-stubbed-fdefinition (nodecode-kimi-code::session-id () "s-7")
    (with-kimi-code-round (url headers body) ("k3")
      (is (equal "s-7" (kimi-code-header headers "X-Claude-Code-Session-Id"))))))

(deftest kimi-code-cell-k3-never-asks-for-thinking-off ()
  (with-kimi-code-round (url headers body) ("k3")
    (is (equal "adaptive" (nlk:json-value body :string "thinking" "type")))
    (is (equal "low" (nlk:json-value body :string "output_config" "effort"))
        "off on K3 is its lowest rung")))

(deftest kimi-code-cell-minimal-is-low-and-off-pins-low ()
  (with-kimi-code-round (url headers body) ("kimi-for-coding" :effort "minimal")
    (is (equal "low" (nlk:json-value body :string "output_config" "effort"))))
  (with-kimi-code-round (url headers body) ("kimi-for-coding")
    (is (null (nlk:json-value body :any "thinking")) "off is no thinking field")
    (is (equal "low" (nlk:json-value body :string "output_config" "effort")))
    (is (null (nlk:json-value body :any "context_management")))))

(deftest kimi-code-cell-a-budget-model-keeps-its-budget ()
  (with-kimi-code-round (url headers body) ("kimi-k2.5" :effort "high")
    (is (equal "enabled" (nlk:json-value body :string "thinking" "type")))
    (is (nlk:json-value body :integer "thinking" "budget_tokens"))
    (is (nlk:json-value body :object "context_management"))))

(deftest kimi-code-cell-lists-models-as-omp-asks ()
  (with-cell-stop ((kimi-code-start))
    (with-temp-auth (auth (format nil "{\"oauth_tokens\":{\"kimi-code\":{\"access_token\":\"tok-1\",\"expires_at\":~d}}}"
                                  (+ (kimi-code-now) 3600)))
      (let ((nle::*api-key* nil) (nle::*auth-file-path* (pathname auth)) (asked nil))
        (with-stubbed-fdefinition (nle::http-fetch (url &rest args)
                                   (setf asked (list url (getf args :headers)))
                                   (values "{\"data\":[{\"id\":\"kimi-for-coding\",\"display_name\":\"K2.8\",\"context_length\":262144}]}" 200))
          (let ((rows (nle::list-provider-models "kimi-code")))
            (is (equal '("kimi-for-coding") (mapcar (lambda (row) (getf row :id)) rows)))
            (is (eql 262144 (getf (first rows) :context-window)))))
        (destructuring-bind (url headers) asked
          (is (equal "https://api.kimi.com/coding/v1/models" url))
          (is (equal "Bearer tok-1" (kimi-code-header headers "Authorization")))
          (is (equal "KimiCLI/1.0" (kimi-code-header headers "User-Agent"))))))))

(deftest kimi-code-cell-leaves-other-providers-alone ()
  (with-cell-stop ((kimi-code-start))
    (let ((nle::*provider* "anthropic") (nle::*model* "claude-sonnet-5") (nle::*api-key* "k")
          (nle::*reasoning-effort* nil) (nle::*endpoint* nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (kimi-code-messages-stream) 200))
        (nle::call-provider (user-context)))
      (is (equal "k" (kimi-code-header headers "x-api-key")))
      (is (null (kimi-code-header headers "X-Msh-Platform")))
      (is (null (nlk:json-value body :any "context_management"))))))

(defmacro with-kimi-code-sign-in ((requests pauses) answers &body forms)
  "FORMS with the cell started, a temp auth.json as the store, and every
sign-in exchange answered by ANSWERS, a list of (URL-SUFFIX BODY STATUS)
taken in order: REQUESTS collects (URL CONTENT HEADERS), PAUSES each poll
interval the flow would have slept."
  `(with-cell-stop ((kimi-code-start))
     (with-temp-auth (auth "{\"api_keys\":{\"other\":{\"provider\":\"other\",\"key\":\"k\"}}}")
       (let ((nle::*auth-file-path* (pathname auth))
             (,requests '()) (,pauses '()) (answers (list ,@answers)))
         (with-stubbed-fdefinitions
             ((dex:post (url &rest args)
                        (push (list url (getf args :content) (getf args :headers)) ,requests)
                        (let ((answer (pop answers)))
                          (assert (uiop:string-suffix-p url (first answer)) ()
                                  "asked ~a, scripted ~a" url (first answer))
                          (values (second answer) (third answer))))
              (nodecode-kimi-code::pause (seconds flow) (push seconds ,pauses)))
           ,@forms)))))

(deftest kimi-code-cell-login-polls-and-stores-the-token ()
  (let ((access (kimi-code-jwt "user_id" "u-42" "sub" "s-1")))
    (with-kimi-code-sign-in (requests pauses)
        ((list "/api/oauth/device_authorization"
               "{\"user_code\":\"ABCD-1234\",\"device_code\":\"dc-1\",\"verification_uri\":\"https://www.kimi.com/code/authorize_device\",\"verification_uri_complete\":\"https://www.kimi.com/code/authorize_device?user_code=ABCD-1234\",\"interval\":5,\"expires_in\":600}"
               200)
         (list "/api/oauth/token" "{\"error\":\"authorization_pending\"}" 400)
         (list "/api/oauth/token"
               (format nil "{\"access_token\":\"~a\",\"refresh_token\":\"r-1\",\"expires_in\":3600,\"token_type\":\"Bearer\"}" access)
               200))
      (let ((text (cell-entry "nodecode-kimi-code" "kimi-code" "login")))
        (is (search "https://www.kimi.com/code/authorize_device?user_code=ABCD-1234" text)
            "the answer names the address at once")
        (is (search "ABCD-1234" text)))
      (is (await (:timeout 5) (cell-notice "nodecode-kimi-code")) "the outcome comes as a notice")
      (is (search "signed in as u-42" (second (cell-notice "nodecode-kimi-code"))))
      (let ((entry (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "kimi-code")))
        (is (equal access (nlk:json-value entry :string "access_token")))
        (is (equal "r-1" (nlk:json-value entry :string "refresh_token")))
        (is (<= (abs (- (nlk:json-value entry :integer "expires_at") (+ (kimi-code-now) 3600))) 5)
            "expires_at is epoch seconds")
        (is (equal "u-42" (nlk:json-value entry :string "account_id")) "the user_id claim names the account"))
      (is (equal "k" (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :string "api_keys" "other" "key"))
          "every other field of the store is kept")
      (destructuring-bind (device first-poll second-poll) (reverse requests)
        (is (equal "https://auth.kimi.com/api/oauth/device_authorization" (first device)))
        (is (equal "client_id=17e5f671-d194-4dfb-9706-5516cb48c098" (second device)))
        (is (equal "KimiCLI/18.8.3" (kimi-code-header (third device) "User-Agent"))
            "the sign-in carries the fingerprint too")
        (is (search "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Adevice_code" (second first-poll)))
        (is (search "device_code=dc-1" (second second-poll))))
      (is (equal '(5) pauses) "one wait, at the interval the device answer named"))))

(deftest kimi-code-cell-login-slows-down-and-says-a-refusal ()
  (with-kimi-code-sign-in (requests pauses)
      ((list "/api/oauth/device_authorization"
             "{\"user_code\":\"C\",\"device_code\":\"d\",\"verification_uri\":\"https://k/d\"}" 200)
       (list "/api/oauth/token" "{\"error\":\"slow_down\"}" 400)
       (list "/api/oauth/token" "{\"error\":\"access_denied\"}" 400))
    (cell-entry "nodecode-kimi-code" "kimi-code" "login")
    (is (await (:timeout 5) (cell-notice "nodecode-kimi-code")))
    (is (search "denied" (second (cell-notice "nodecode-kimi-code"))))
    (is (eq :warning (third (cell-notice "nodecode-kimi-code"))))
    (is (equal '(10) pauses) "slow_down adds five seconds to the default five")
    (is (null (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "kimi-code"))
        "and nothing is stored")))

(deftest kimi-code-cell-refreshes-an-expiring-token-and-writes-it-back ()
  (with-kimi-code-sign-in (requests pauses)
      ((list "/api/oauth/token" "{\"access_token\":\"tok-2\",\"expires_in\":3600}" 200))
    (nodecode-kimi-code::save-entry
     (nlk:json-object "access_token" "tok-1" "refresh_token" "r-1"
                      "expires_at" (+ (kimi-code-now) 10) "account_id" "u-42")
     nle::*auth-file-path*)
    (let ((nle::*api-key* nil))
      (is (eq :oauth (nle::provider-auth-state "kimi-code" :auth-path nle::*auth-file-path*)))
      (is (null requests) "a probe answers the stored token and costs no network")
      (is (equal "tok-2" (nle:credential-key
                          (nle::resolve-provider-credential "kimi-code" :auth-path nle::*auth-file-path*)))))
    (destructuring-bind (url content headers) (first requests)
      (is (equal "https://auth.kimi.com/api/oauth/token" url))
      (is (equal "grant_type=refresh_token&client_id=17e5f671-d194-4dfb-9706-5516cb48c098&refresh_token=r-1"
                 content))
      (is (equal "kimi_cli" (kimi-code-header headers "X-Msh-Platform"))))
    (let ((entry (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "kimi-code")))
      (is (equal "tok-2" (nlk:json-value entry :string "access_token")))
      (is (equal "r-1" (nlk:json-value entry :string "refresh_token")) "an unrotated refresh token is kept")
      (is (equal "u-42" (nlk:json-value entry :string "account_id")) "and so is the account")
      (is (> (nlk:json-value entry :integer "expires_at") (+ (kimi-code-now) 3000))))))

(deftest kimi-code-cell-status-and-logout ()
  (with-kimi-code-sign-in (requests pauses) ()
    (is (search "not signed in" (cell-entry "nodecode-kimi-code" "kimi-code" "status")))
    (nodecode-kimi-code::save-entry
     (nlk:json-object "access_token" "tok" "refresh_token" "r" "expires_at" (+ (kimi-code-now) 600)
                      "account_id" "u-42")
     nle::*auth-file-path*)
    (is (search "signed in as u-42" (cell-entry "nodecode-kimi-code" "kimi-code" "")))
    (is (search "signed out" (cell-entry "nodecode-kimi-code" "kimi-code" "logout")))
    (is (null (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "kimi-code")))
    (is (equal "k" (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :string "api_keys" "other" "key")))))
