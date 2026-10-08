;;;; cell-test.lisp --- the muse-code cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every wire and every sign-in exchange a stubbed
;;;; dex:post, every poll interval a stubbed PAUSE: nothing touches the
;;;; network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "muse-code" "MUSE-CODE-CELL-" :start nodecode-muse-code:start-cell)

(define-cell-lifecycle-tests "muse-code"
  (:hooks 'nle::models-catalog-table :credential 'nle::list-provider-models 'nle::responses-request-body
          'nle::walk-provider-stream)
  (:command "muse-code")
  (:refused ("base_url" 5)))

(defun muse-code-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun muse-code-responses-stream ()
  "One short Responses answer, as the wire streams it."
  (make-truncated-sse-stream
   "{\"type\":\"response.created\",\"response\":{\"id\":\"r1\",\"model\":\"muse\"}}"
   "{\"type\":\"response.output_text.delta\",\"item_id\":\"m1\",\"delta\":\"ok\"}"
   "{\"type\":\"response.completed\",\"response\":{\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}}"))

(defmacro with-muse-code-round ((url headers body) (model &key (key "key-test") auth (provider "muse-code"))
                                &body forms)
  "FORMS with the cell started and one round on PROVIDER's MODEL captured:
URL, HEADERS and BODY (the decoded request) as dex:post saw them. KEY is the
configured key; NIL resolves the credential from AUTH, a temp auth.json."
  `(with-cell-stop ((muse-code-start))
     (let ((nle::*provider* ,provider) (nle::*model* ,model) (nle::*api-key* ,key)
           (nle::*reasoning-effort* "high") (nle::*tool-choice* "auto") (nle::*endpoint* nil)
           (nle::*auth-file-path* (or ,auth nle::*auth-file-path*))
           (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (muse-code-responses-stream) 200))
         (nle::call-provider (user-context)))
       ,@forms)))

(deftest muse-code-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((muse-code-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "muse-code")))
      (is (equal "Muse Code (Subscription)" (nlk:json-value row :string "name")))
      (is (equal "https://api.meta.ai/v1" (nlk:json-value row :string "api")))
      (is (gethash "muse-spark-1.3" (nlk:json-value row :object "models")) "the bundled models are listed")
      (is (equal "openai-responses" (nle::configured-provider-lane "muse-code")))
      (is (equal "https://api.meta.ai/v1/responses" (nle::lane-endpoint "muse-code" "openai-responses"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "muse-code")))))

(deftest muse-code-cell-base-follows-the-section ()
  (with-cell-stop ((muse-code-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "muse-code" "api")))))

(deftest muse-code-cell-spends-the-minted-key-never-the-account-token ()
  (with-cell-stop ((muse-code-start))
    (with-temp-auth (auth "{\"oauth_tokens\":{\"muse-code\":{\"access_token\":\"acct-token\",\"api_key\":\"mk-1\",\"account_id\":\"u1\"}}}")
      (let ((nle::*api-key* nil))
        (let ((credential (nle::resolve-provider-credential "muse-code" :auth-path auth :probe t)))
          (is (equal "mk-1" (nle:credential-key credential)))
          (is (eq :oauth (nle:credential-source credential))))))
    (with-temp-auth (auth "{}")
      (let ((nle::*api-key* nil))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (is (eq :public (nle:credential-source
                           (nle::resolve-provider-credential "muse-code" :auth-path auth :probe t)))
              "an OpenAI key is never sent to Meta"))))))

(deftest muse-code-cell-saved-key-outranks-the-sign-in ()
  (with-cell-stop ((muse-code-start))
    (with-temp-auth (auth "{\"api_keys\":{\"muse-code\":{\"provider\":\"muse-code\",\"key\":\"saved\"}},\"oauth_tokens\":{\"muse-code\":{\"access_token\":\"a\",\"api_key\":\"mk\"}}}")
      (let ((nle::*api-key* nil))
        (is (equal "saved"
                   (nle:credential-key (nle::resolve-provider-credential "muse-code" :auth-path auth :probe t))))))))

(deftest muse-code-cell-round-sends-the-key-and-the-api-version ()
  (with-temp-auth (auth "{\"oauth_tokens\":{\"muse-code\":{\"access_token\":\"acct-token\",\"api_key\":\"mk-1\"}}}")
    (with-muse-code-round (url headers body) ("muse-spark-1.3" :key nil :auth auth)
      (is (equal "https://api.meta.ai/v1/responses" url))
      (is (equal "Bearer mk-1" (muse-code-header headers "authorization")) "the minted key, as a bearer")
      (is (equal "1.0.0" (muse-code-header headers "x-api-version")))
      (is (equal "muse-spark-1.3" (nlk:json-value body :string "model")))
      (is (equal "high" (nlk:json-value body :string "reasoning" "effort")))
      (is (null (nlk:json-value body :any "tool_choice")) "Meta refuses tool_choice")
      (is (eq nil (gethash "store" body)) "and nothing is stored on Meta's side"))))

(deftest muse-code-cell-drops-tool-choice ()
  (let ((body (nlk:json-object "model" "m" "tool_choice" "auto" "tools" #())))
    (is (null (nth-value 1 (gethash "tool_choice" (nodecode-muse-code::muse-body body)))))))

(deftest muse-code-cell-lists-models-as-omp-asks ()
  (with-cell-stop ((muse-code-start))
    (with-temp-auth (auth "{\"oauth_tokens\":{\"muse-code\":{\"access_token\":\"acct\",\"api_key\":\"mk-1\"}}}")
      (let ((nle::*api-key* nil) (nle::*auth-file-path* (pathname auth)) (asked nil))
        (with-stubbed-fdefinition (nle::http-fetch (url &rest args)
                                   (setf asked (list url (getf args :headers)))
                                   (values "{\"data\":[{\"id\":\"muse-spark-1.4\"}]}" 200))
          (is (equal '("muse-spark-1.4")
                     (mapcar (lambda (row) (getf row :id)) (nle::list-provider-models "muse-code")))))
        (destructuring-bind (url headers) asked
          (is (equal "https://api.meta.ai/v1/models" url))
          (is (equal "Bearer mk-1" (muse-code-header headers "Authorization")))
          (is (equal "1.0.0" (muse-code-header headers "x-api-version"))))))))

(deftest muse-code-cell-leaves-other-providers-alone ()
  (with-muse-code-round (url headers body) ("gpt-6" :provider "openai-responses")
    (is (null (muse-code-header headers "x-api-version")))))

(defmacro with-muse-code-sign-in ((requests pauses) (&rest answers) &body forms)
  "FORMS with the cell started, a temp auth.json as the store, and every
sign-in exchange answered by ANSWERS, a list of (URL BODY STATUS) taken in
order: REQUESTS collects (URL CONTENT HEADERS), PAUSES each poll interval the
flow would have slept."
  `(with-cell-stop ((muse-code-start))
     (with-temp-auth (auth "{\"api_keys\":{\"other\":{\"provider\":\"other\",\"key\":\"k\"}}}")
       (let ((nle::*auth-file-path* (pathname auth))
             (,requests '()) (,pauses '()) (answers (list ,@answers)))
         (with-stubbed-fdefinitions
             ((dex:post (url &rest args)
                        (push (list url (getf args :content) (getf args :headers)) ,requests)
                        (let ((answer (pop answers)))
                          (assert (equal url (first answer)) () "asked ~a, scripted ~a" url (first answer))
                          (values (second answer) (third answer))))
              (nodecode-muse-code::pause (seconds flow) (push seconds ,pauses)))
           ,@forms)))))

(defparameter +muse-code-device+
  '("https://auth.meta.com/oidc/device/authorization/"
    "{\"user_code\":\"MUSE-1\",\"device_code\":\"dc-m\",\"verification_uri\":\"https://meta.ai/device\",\"verification_uri_complete\":\"https://meta.ai/device?code=MUSE-1\",\"interval\":5,\"expires_in\":600}"
    200))

(deftest muse-code-cell-login-polls-mints-the-key-and-stores-it ()
  (with-muse-code-sign-in (requests pauses)
      (+muse-code-device+
       (list "https://auth.meta.com/oidc/device/token/" "{\"error\":\"authorization_pending\"}" 400)
       (list "https://auth.meta.com/oidc/device/token/" "{\"access_token\":\"acct-token\",\"token_type\":\"Bearer\"}" 200)
       (list "https://api.meta.ai/muse-code/key"
             "{\"api_key\":\"mk-new\",\"user_email\":\"Muse@Example.com\",\"user_id\":\"m-9\",\"is_subs_active\":true}" 200))
    (let ((text (cell-entry "nodecode-muse-code" "muse-code" "login")))
      (is (search "https://meta.ai/device?code=MUSE-1" text))
      (is (search "MUSE-1" text)))
    (is (await (:timeout 5) (cell-notice "nodecode-muse-code")))
    (is (search "signed in as muse@example.com" (second (cell-notice "nodecode-muse-code"))))
    (let ((entry (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "muse-code")))
      (is (equal "acct-token" (nlk:json-value entry :string "access_token")))
      (is (equal "mk-new" (nlk:json-value entry :string "api_key")))
      (is (equal "m-9" (nlk:json-value entry :string "account_id")))
      (is (equal "muse@example.com" (nlk:json-value entry :string "email")))
      (is (null (nth-value 1 (gethash "expires_at" entry))) "the sign-in never expires"))
    (destructuring-bind (device pending done mint) (reverse requests)
      (is (equal "client_id=1031625952748946" (second device)))
      (is (equal "1.0.0" (muse-code-header (third device) "x-api-version")))
      (is (search "device_code=dc-m" (second pending)))
      (is (equal "application/json" (muse-code-header (third done) "Accept")))
      (is (equal "Bearer acct-token" (muse-code-header (third mint) "Authorization"))
          "the account token is spent on the key exchange")
      (is (eq t (nlk:json-value (nlk:decode-json (second mint)) :boolean "onboard"))))
    (is (equal '(5) pauses))))

(deftest muse-code-cell-login-says-an-inactive-subscription ()
  (with-muse-code-sign-in (requests pauses)
      (+muse-code-device+
       (list "https://auth.meta.com/oidc/device/token/" "{\"access_token\":\"acct-token\"}" 200)
       (list "https://api.meta.ai/muse-code/key" "{\"is_subs_active\":false}" 200))
    (cell-entry "nodecode-muse-code" "muse-code" "login")
    (is (await (:timeout 5) (cell-notice "nodecode-muse-code")))
    (is (search "subscription is inactive" (second (cell-notice "nodecode-muse-code"))))
    (is (null (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "muse-code")))))

(deftest muse-code-cell-login-says-where-to-pay ()
  (with-muse-code-sign-in (requests pauses)
      (+muse-code-device+
       (list "https://auth.meta.com/oidc/device/token/" "{\"access_token\":\"acct-token\"}" 200)
       (list "https://api.meta.ai/muse-code/key"
             "{\"require_payment\":true,\"action_url\":\"https://meta.ai/subscribe\"}" 200))
    (cell-entry "nodecode-muse-code" "muse-code" "login")
    (is (await (:timeout 5) (cell-notice "nodecode-muse-code")))
    (is (search "subscription is required: https://meta.ai/subscribe"
                (second (cell-notice "nodecode-muse-code"))))))

(deftest muse-code-cell-status-and-logout ()
  (with-muse-code-sign-in (requests pauses) ()
    (is (search "not signed in" (cell-entry "nodecode-muse-code" "muse-code" "status")))
    (nodecode-muse-code::save-entry
     (nlk:json-object "access_token" "a" "api_key" "mk" "account_id" "m-9" "email" "muse@example.com")
     nle::*auth-file-path*)
    (is (search "signed in as muse@example.com" (cell-entry "nodecode-muse-code" "muse-code" "")))
    (is (search "signed out" (cell-entry "nodecode-muse-code" "muse-code" "logout")))
    (is (null (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "oauth_tokens" "muse-code")))
    (is (equal "k" (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :string "api_keys" "other" "key")))))
