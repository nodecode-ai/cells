;;;; cell-test.lisp --- the openai-codex-device cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every OpenAI endpoint and every round a
;;;; stubbed dex:post, every wait between two polls a stubbed PAUSE: nothing
;;;; reaches OpenAI, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "openai-codex-device" "OPENAI-CODEX-DEVICE-CELL-"
  :start nodecode-openai-codex-device:start-cell)

(define-cell-lifecycle-tests "openai-codex-device"
  (:hooks 'nle::models-catalog-table :credential 'nle::responses-request-body 'nle::walk-provider-stream
          'nle::note-body-wire)
  (:command "openai-codex-device")
  (:running (is (nle::find-lane-by-name "openai-codex-device" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "openai-codex-device" nil)) "and taken back out"))
  (:refused ("base_url" 5) ("originator" 5)))

;;; --- fixtures --------------------------------------------------------------------

(defun ocd-jwt (claims)
  "A JWT whose payload is the JSON text CLAIMS, unsigned."
  (format nil "h.~a.s" (string-right-trim "." (cl-base64:string-to-base64-string claims :uri t))))

(defparameter +ocd-access+
  (ocd-jwt (concatenate 'string
                        "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct-7\",\"chatgpt_plan_type\":\"plus\"},"
                        "\"https://api.openai.com/profile\":{\"email\":\"op@example.com\"}}"))
  "An access token naming account acct-7, plan plus, email op@example.com.")

(defun ocd-now () (nodecode-openai-codex-device::unix-seconds))

(defun ocd-store (&key (access +ocd-access+) (expires-in 3600))
  "auth.json text holding one openai-codex sign-in."
  (format nil "{\"oauth_tokens\":{\"openai-codex\":{\"provider\":\"openai-codex\",\"access_token\":\"~a\",~
               \"refresh_token\":\"rt-1\",\"expires_at\":~d,\"account_id\":\"acct-7\",\"email\":\"op@example.com\",~
               \"org_id\":\"acct-7\",\"org_name\":\"plus\",\"installation_id\":\"inst-1\"}}}"
          access (+ (ocd-now) expires-in)))

(defun ocd-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun ocd-tokens (auth)
  "The oauth_tokens object of the auth.json at AUTH."
  (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens"))

(defun ocd-await-login ()
  "Wait for the running sign-in's thread to finish."
  (alexandria:when-let (login nodecode-openai-codex-device::*login*)
    (bt2:join-thread (nodecode-openai-codex-device::login-thread login))))

(defmacro with-ocd-login ((auth answer posts &key (polls ''(403 200)) (pending "{}")) &body body)
  "BODY with the cell started and a device sign-in begun against a temp
auth.json AUTH, ANSWER what /openai-codex-device login said and POSTS the
(URL HEADERS CONTENT) OpenAI saw, newest first. The token poll answers the
statuses POLLS in turn, the code and verifier once one is 200; PENDING is
the body of a poll still waiting."
  `(with-cell-stop ((openai-codex-device-start))
     (with-temp-auth (,auth "{}")
       (let ((nle::*auth-file-path* ,auth) (,posts '()) (polls ,polls))
         (with-stubbed-fdefinitions
             ((nodecode-openai-codex-device::pause (seconds) nil)
              (dex:post (asked &rest args)
               (push (list asked (getf args :headers) (getf args :content)) ,posts)
               (cond ((search "usercode" asked)
                      (values "{\"device_auth_id\":\"dev-1\",\"user_code\":\"ABCD-1234\",\"interval\":\"2\"}" 200))
                     ((search "deviceauth/token" asked)
                      (let ((status (or (pop polls) 200)))
                        (if (= status 200)
                            (values "{\"authorization_code\":\"ac-1\",\"code_verifier\":\"cv-1\"}" 200)
                            (values ,pending status))))
                     (t (values (format nil "{\"access_token\":\"~a\",\"refresh_token\":\"rt-2\",\"id_token\":\"~a\",\"expires_in\":3600}"
                                        +ocd-access+ (ocd-jwt "{}"))
                                200)))))
           (let ((,answer (cell-entry "nodecode-openai-codex-device" "openai-codex-device" "login")))
             (declare (ignorable ,answer))
             (ocd-await-login)
             ,@body))))))

;;; --- the catalog -----------------------------------------------------------------

(deftest openai-codex-device-cell-serves-openai-codex-on-its-own-lane ()
  (with-cell-stop ((openai-codex-device-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "openai-codex"))
           (models (nlk:json-value row :object "models")))
      (is (equal "nodecode-openai-codex-device" (nlk:json-value row :string "npm")))
      (is (equal "https://chatgpt.com/backend-api/codex" (nlk:json-value row :string "api")))
      (is (gethash "gpt-6.1-sol" models))
      (is (equal "openai-codex-device" (nle::configured-provider-lane "openai-codex"))
          "alone, the provider rides this cell's lane")
      (is (equal "https://chatgpt.com/backend-api/codex/responses"
                 (nle::lane-endpoint "openai-codex" "openai-codex-device"))))))

;;; --- the sign-in -----------------------------------------------------------------

(deftest openai-codex-device-cell-signs-in-with-a-device-code ()
  (with-ocd-login (auth answer posts)
    (is (search "https://auth.openai.com/codex/device" answer))
    (is (search "ABCD-1234" answer) "the code to type")
    (is (search "every 5 seconds" answer) "OpenAI's interval and the margin")
    (let ((sent (reverse posts)))
      (is (= 4 (length sent)) "a code, a poll still waiting, a poll answered, one exchange")
      (destructuring-bind (asked headers content) (first sent)
        (is (equal "https://auth.openai.com/api/accounts/deviceauth/usercode" asked))
        (is (equal "application/json" (ocd-header headers "content-type")))
        (is (equal "app_EMoamEEZ73f0CkXaXp7hrann" (nlk:json-value (nlk:decode-json content) :string "client_id"))))
      (destructuring-bind (asked headers content) (second sent)
        (declare (ignore headers))
        (is (equal "https://auth.openai.com/api/accounts/deviceauth/token" asked))
        (is (equal "dev-1" (nlk:json-value (nlk:decode-json content) :string "device_auth_id")))
        (is (equal "ABCD-1234" (nlk:json-value (nlk:decode-json content) :string "user_code"))))
      (destructuring-bind (asked headers content) (fourth sent)
        (declare (ignore headers))
        (let ((form (quri:url-decode-params content)))
          (is (equal "https://auth.openai.com/oauth/token" asked))
          (is (equal "authorization_code" (cdr (assoc "grant_type" form :test #'equal))))
          (is (equal "ac-1" (cdr (assoc "code" form :test #'equal))))
          (is (equal "cv-1" (cdr (assoc "code_verifier" form :test #'equal))) "the verifier OpenAI made")
          (is (equal "https://auth.openai.com/deviceauth/callback" (cdr (assoc "redirect_uri" form :test #'equal)))))))
    (let ((tokens (ocd-tokens auth)))
      (is (null (gethash "openai-codex-device" tokens)) "not under a name of its own")
      (let ((entry (gethash "openai-codex" tokens)))
        (is (equal +ocd-access+ (nlk:json-value entry :string "access_token")) "but as openai-codex, as omp stores it")
        (is (equal "rt-2" (nlk:json-value entry :string "refresh_token")))
        (is (<= (abs (- (nlk:json-value entry :integer "expires_at") (+ (ocd-now) 3600))) 5))
        (is (equal "acct-7" (nlk:json-value entry :string "account_id")))
        (is (equal "op@example.com" (nlk:json-value entry :string "email")))
        (is (equal "plus" (nlk:json-value entry :string "org_name")))
        (is (= 36 (length (nlk:json-value entry :string "installation_id"))))))
    (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring auth))))))
    (is (search "signed in as op@example.com (plus)" (second (cell-notice "nodecode-openai-codex-device"))))))

(deftest openai-codex-device-cell-says-a-failed-poll ()
  (with-ocd-login (auth answer posts :polls '(403 500))
    (is (null (ocd-tokens auth)) "nothing kept")
    (is (search "sign-in failed: device token polling failed: 500"
                (second (cell-notice "nodecode-openai-codex-device"))))))

(deftest openai-codex-device-cell-refuses-a-refused-initiation ()
  (with-cell-stop ((openai-codex-device-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*auth-file-path* auth))
        (with-stubbed-fdefinition (dex:post (asked &rest args) (values "{}" 429))
          (is (search "device authorization initiation failed: 429"
                      (cell-entry "nodecode-openai-codex-device" "openai-codex-device" "login")))
          (is (null nodecode-openai-codex-device::*login*)))))))

;;; --- the credential and the refresh ------------------------------------------------------

(deftest openai-codex-device-cell-refreshes-an-expiring-token ()
  (with-cell-stop ((openai-codex-device-start))
    (with-temp-auth (auth (ocd-store :access "old-token" :expires-in 30))
      (let ((posts '()))
        (with-stubbed-fdefinition (dex:post (asked &rest args)
                                   (push (cons asked (quri:url-decode-params (getf args :content))) posts)
                                   (values (format nil "{\"access_token\":\"new-token\",\"refresh_token\":\"rt-3\",\"expires_in\":3600}") 200))
          (is (equal "old-token" (nle:credential-key (nle::resolve-provider-credential "openai-codex" :auth-path auth :probe t))))
          (is (null posts) "a probe never dials")
          (let ((credential (nle::resolve-provider-credential "openai-codex" :auth-path auth
                                                                             :endpoint "https://chatgpt.com/backend-api/codex/responses")))
            (is (equal "new-token" (nle:credential-key credential)))
            (is (eq :oauth (nle:credential-source credential)))
            (is (equal "acct-7" (ocd-header (getf (nle:credential-attributes credential) :headers) "chatgpt-account-id"))
                "the saved account, when the token's claims name none")))
        (destructuring-bind (asked . form) (first posts)
          (is (equal "https://auth.openai.com/oauth/token" asked))
          (is (equal "refresh_token" (cdr (assoc "grant_type" form :test #'equal))))
          (is (equal "rt-1" (cdr (assoc "refresh_token" form :test #'equal))))
          (is (equal "app_EMoamEEZ73f0CkXaXp7hrann" (cdr (assoc "client_id" form :test #'equal)))))
        (let ((entry (gethash "openai-codex" (ocd-tokens auth))))
          (is (equal "new-token" (nlk:json-value entry :string "access_token")))
          (is (equal "rt-3" (nlk:json-value entry :string "refresh_token")))
          (is (equal "plus" (nlk:json-value entry :string "org_name")) "the sign-in's plan kept"))))))

;;; --- one round -------------------------------------------------------------------

(defun ocd-round (context)
  "One round of CONTEXT with dex:post stubbed: (values MESSAGE URL HEADERS BODY)."
  (let ((url nil) (headers nil) (body nil))
    (with-stubbed-fdefinition (dex:post (asked &rest args)
                               (setf url asked headers (getf args :headers)
                                     body (nlk:decode-json (getf args :content)))
                               (values (make-truncated-sse-stream
                                        "{\"type\":\"response.output_text.delta\",\"item_id\":\"m\",\"delta\":\"ok\"}"
                                        "{\"type\":\"response.done\",\"response\":{}}")
                                       200))
      (values (nle::call-responses-streaming context) url headers body))))

(defmacro with-ocd-round (&body body)
  "BODY with the cell started, signed in, and gpt-6.1-sol selected on openai-codex."
  `(with-cell-stop ((openai-codex-device-start))
     (with-temp-auth (auth (ocd-store))
       (let ((nle::*auth-file-path* auth) (nle::*provider* "openai-codex") (nle::*model* "gpt-6.1-sol")
             (nle::*api-key* nil) (nle::*endpoint* nil))
         ,@body))))

(deftest openai-codex-device-cell-sends-a-round-the-codex-way ()
  (with-ocd-round
    (multiple-value-bind (message url headers body) (ocd-round (user-context "hi"))
      (is (equal "ok" (nlk:json-value message :string "content")))
      (is (equal "https://chatgpt.com/backend-api/codex/responses" url))
      (is (equal (format nil "Bearer ~a" +ocd-access+) (ocd-header headers "authorization")))
      (is (equal "acct-7" (ocd-header headers "chatgpt-account-id")))
      (is (equal "codex_cli_rs" (ocd-header headers "originator")))
      (is (equal "0.159.0" (ocd-header headers "version")))
      (is (equal "model=gpt-6.1-sol" (ocd-header headers "x-codex-routing-hint")))
      (is (null (nth-value 1 (gethash "max_output_tokens" body))))
      (is (equal "inst-1" (nlk:json-value body :string "client_metadata" "x-codex-installation-id"))))))

(deftest openai-codex-device-cell-leaves-the-browser-cells-lane-alone ()
  ;; nodecode-openai-codex registers a lane named openai-codex; while it runs
  ;; the provider rides that lane, and only that cell shapes the round.
  (with-ocd-round
    (nle::register-provider-lane
     (nle::make-provider-lane :name "openai-codex" :stream-symbol 'nle::call-responses-streaming
                              :family :openai-codex :reasoning-carry :text :path "/responses"
                              :default-endpoint "https://chatgpt.com/backend-api/codex/responses"))
    (unwind-protect
         (let ((headers nil) (body nil))
           (is (equal "openai-codex" (nle::configured-provider-lane "openai-codex")))
           (with-stubbed-fdefinition (dex:post (asked &rest args)
                                      (setf headers (getf args :headers)
                                            body (nlk:decode-json (getf args :content)))
                                      (values (make-truncated-sse-stream
                                               "{\"type\":\"response.completed\",\"response\":{}}")
                                              200))
             (nle::call-responses-streaming (user-context "hi")))
           (is headers "the round went out")
           (is (null (ocd-header headers "originator")) "this cell added no header")
           (is (null (nth-value 1 (gethash "client_metadata" body))) "and shaped no body"))
      (setf nle::*provider-lanes*
            (remove "openai-codex" nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))))
