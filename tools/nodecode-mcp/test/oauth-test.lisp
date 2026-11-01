;;;; oauth-test.lisp --- OAuth sign-in against a local resource and auth server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One clack app is both the protected MCP endpoint (POST /mcp, a 401 and a
;;;; Bearer challenge without a live token) and its authorization server:
;;;; protected-resource and server metadata, dynamic registration, /authorize
;;;; (which approves at once and redirects to the client's loopback, as a
;;;; person who says yes would), /token with PKCE, resource and client checks
;;;; and rotating refresh tokens, and /revoke. Every request it takes is an
;;;; event the gates read, so a gate checks what reached the server. The
;;;; browser is the test itself: it visits the sign-in's address and follows
;;;; the redirect to the cell's listener.

(in-package #:nodecode.test)

(defstruct (mcp-oauth-fixture (:copier nil))
  port
  handler
  (lock (bt2:make-lock :name "mcp-oauth-fixture"))
  (events '() :type list)               ; plists, newest first
  (mode :dcr)                           ; :dcr :cimd :both :confidential
  (ttl 3600)
  (prm-p t)                             ; serves protected-resource metadata
  (issuer-path "")
  (tamper-p nil)                        ; /authorize answers another state
  (clients (make-hash-table :test #'equal))
  (codes (make-hash-table :test #'equal))
  (access (make-hash-table :test #'equal))
  (refresh (make-hash-table :test #'equal)))

(defun mcp-oauth-origin (fixture)
  (format nil "http://127.0.0.1:~d" (mcp-oauth-fixture-port fixture)))

(defun mcp-oauth-issuer (fixture)
  (concatenate 'string (mcp-oauth-origin fixture)
               (string-right-trim "/" (mcp-oauth-fixture-issuer-path fixture))))

(defun mcp-oauth-resource (fixture)
  (format nil "~a/mcp" (mcp-oauth-origin fixture)))

(defun mcp-oauth-note (fixture &rest event)
  (bt2:with-lock-held ((mcp-oauth-fixture-lock fixture))
    (push event (mcp-oauth-fixture-events fixture))))

(defun mcp-oauth-events (fixture &optional kind)
  "What reached FIXTURE, oldest first; only KIND's events when given."
  (bt2:with-lock-held ((mcp-oauth-fixture-lock fixture))
    (remove-if-not (lambda (event) (or (null kind) (eq kind (getf event :ev))))
                   (reverse (mcp-oauth-fixture-events fixture)))))

(defun mcp-oauth-grants (fixture grant)
  (count-if (lambda (event) (and (getf event :ok) (equal grant (getf event :grant))))
            (mcp-oauth-events fixture :token)))

(defun mcp-oauth-json (status object &rest headers)
  (list status (list* :content-type "application/json" headers)
        (list (nlk:encode-json-object object))))

(defun mcp-oauth-body (env)
  (let* ((length (or (getf env :content-length) 0))
         (buffer (make-array length :element-type '(unsigned-byte 8))))
    (when (and (getf env :raw-body) (plusp length))
      (read-sequence buffer (getf env :raw-body)))
    (flexi-streams:octets-to-string buffer :external-format :utf-8)))

(defun mcp-oauth-s256 (verifier)
  (mcp::base64url (mcp::sha256-octets verifier)))

(defun mcp-oauth-server-metadata (fixture &aux (issuer (mcp-oauth-issuer fixture))
                                                (mode (mcp-oauth-fixture-mode fixture)))
  (let ((metadata (nlk:json-object
                   "issuer" issuer
                   "authorization_endpoint" (format nil "~a/authorize" issuer)
                   "token_endpoint" (format nil "~a/token" issuer)
                   "revocation_endpoint" (format nil "~a/revoke" issuer)
                   "response_types_supported" (vector "code")
                   "code_challenge_methods_supported" (vector "S256")
                   "token_endpoint_auth_methods_supported"
                   (if (eq mode :confidential)
                       (vector "client_secret_basic" "client_secret_post")
                       (vector "none")))))
    (when (member mode '(:dcr :both :confidential))
      (setf (gethash "registration_endpoint" metadata) (format nil "~a/register" issuer)))
    (when (member mode '(:cimd :both))
      (setf (gethash "client_id_metadata_document_supported" metadata) t))
    metadata))

(defun mcp-oauth-challenge (fixture error)
  (format nil "Bearer realm=\"mcp\"~@[, error=\"~a\"~]~:[~*~;, resource_metadata=\"~a/.well-known/oauth-protected-resource/mcp\"~], scope=\"mcp:tools\""
          error (mcp-oauth-fixture-prm-p fixture) (mcp-oauth-origin fixture)))

(defun mcp-oauth-authorize (fixture query)
  (flet ((q (name) (cdr (assoc name query :test #'string=))))
    (let* ((client-id (or (q "client_id") ""))
           (redirect (or (q "redirect_uri") ""))
           (mode (mcp-oauth-fixture-mode fixture))
           (client (gethash client-id (mcp-oauth-fixture-clients fixture)))
           (why (cond ((not (equal (q "response_type") "code")) "response_type")
                      ((not (and (equal (q "code_challenge_method") "S256") (q "code_challenge"))) "pkce")
                      ((null (q "state")) "state")
                      ((uiop:string-prefix-p "https://" client-id)
                       (cond ((not (member mode '(:cimd :both))) "cimd not supported")
                             ((not (equal client-id mcp::+client-metadata-url+)) "not the document")
                             ;; The document lists http://127.0.0.1/callback: any port (RFC 8252 7.3).
                             ((not (ppcre:scan "^http://127\\.0\\.0\\.1:\\d+/callback$" redirect)) "redirect")))
                      (client (unless (member redirect (coerce (gethash "redirect_uris" client) 'list)
                                              :test #'equal)
                                "redirect_uri not registered"))
                      (t "unknown client"))))
      (mcp-oauth-note fixture :ev :authorize :ok (null why) :why why :client-id client-id
                              :resource (q "resource") :scope (q "scope")
                              :ccm (q "code_challenge_method"))
      (if why
          (mcp-oauth-json 400 (nlk:json-object "error" "invalid_request" "error_description" why))
          (let ((code (format nil "code-~a" (mcp::fresh-nonce))))
            (setf (gethash code (mcp-oauth-fixture-codes fixture))
                  (list :client-id client-id :redirect redirect :challenge (q "code_challenge")
                        :resource (q "resource") :scope (or (q "scope") "mcp:tools")))
            (list 302 (list :location
                            (format nil "~a~:[?~;&~]~a" redirect (find #\? redirect)
                                    (quri:url-encode-params
                                     (list (cons "code" code)
                                           (cons "state" (if (mcp-oauth-fixture-tamper-p fixture)
                                                             (format nil "tampered-~a" (q "state"))
                                                             (q "state")))
                                           (cons "iss" (mcp-oauth-issuer fixture))))))
                  '("")))))))

(defun mcp-oauth-register (fixture body)
  (let* ((metadata (ignore-errors (cell-json body)))
         (uris (and (hash-table-p metadata) (gethash "redirect_uris" metadata))))
    (mcp-oauth-note fixture :ev :register :ok (and uris t)
                            :auth-method (and (hash-table-p metadata)
                                              (gethash "token_endpoint_auth_method" metadata)))
    (if (not uris)
        (mcp-oauth-json 400 (nlk:json-object "error" "invalid_redirect_uri"))
        (let ((client-id (format nil "dcr-~a" (mcp::fresh-nonce))))
          (setf (gethash "client_id" metadata) client-id)
          (if (eq (mcp-oauth-fixture-mode fixture) :confidential)
              (setf (gethash "client_secret" metadata) (format nil "cs-~a" (mcp::fresh-nonce))
                    (gethash "token_endpoint_auth_method" metadata) "client_secret_basic")
              (setf (gethash "token_endpoint_auth_method" metadata) "none"))
          (setf (gethash client-id (mcp-oauth-fixture-clients fixture)) metadata)
          (mcp-oauth-json 201 metadata)))))

(defun mcp-oauth-client-auth (fixture headers form)
  "(values CLIENT-ID HOW), or NIL and why the client is not who it says."
  (let ((authorization (or (gethash "authorization" headers) ""))
        (clients (mcp-oauth-fixture-clients fixture))
        (id (cdr (assoc "client_id" form :test #'string=))))
    (cond ((uiop:string-prefix-p "Basic " authorization)
           (let* ((pair (ignore-errors (cl-base64:base64-string-to-string (subseq authorization 6))))
                  (colon (and pair (position #\: pair)))
                  (user (and colon (subseq pair 0 colon)))
                  (client (and user (gethash user clients))))
             (if (and client (equal (gethash "client_secret" client) (subseq pair (1+ colon))))
                 (values user "basic")
                 (values nil "basic secret"))))
          ((assoc "client_secret" form :test #'string=)
           (let ((client (gethash id clients)))
             (if (and client (equal (gethash "client_secret" client)
                                    (cdr (assoc "client_secret" form :test #'string=))))
                 (values id "post")
                 (values nil "post secret"))))
          ((eq (mcp-oauth-fixture-mode fixture) :confidential) (values nil "no client authentication"))
          (id (values id "none"))
          (t (values nil "no client_id")))))

(defun mcp-oauth-token (fixture headers body)
  (let* ((form (quri:url-decode-params body))
         (grant (cdr (assoc "grant_type" form :test #'string=)))
         (resource (cdr (assoc "resource" form :test #'string=))))
    (flet ((f (name) (cdr (assoc name form :test #'string=)))
           (refuse (why &optional (error "invalid_grant"))
             (mcp-oauth-note fixture :ev :token :ok nil :why why :grant grant :resource resource)
             (mcp-oauth-json 400 (nlk:json-object "error" error "error_description" why))))
      (multiple-value-bind (client-id how) (mcp-oauth-client-auth fixture headers form)
        (if (null client-id)
            (refuse how "invalid_client")
            (let ((scope nil) (pkce nil))
              (cond
                ((equal grant "authorization_code")
                 (let ((code (gethash (f "code") (mcp-oauth-fixture-codes fixture))))
                   (remhash (f "code") (mcp-oauth-fixture-codes fixture))
                   (cond ((null code) (return-from mcp-oauth-token (refuse "code")))
                         ((not (equal (getf code :client-id) client-id)) (return-from mcp-oauth-token (refuse "client")))
                         ((not (equal (getf code :redirect) (f "redirect_uri"))) (return-from mcp-oauth-token (refuse "redirect_uri")))
                         ((not (equal (getf code :challenge) (mcp-oauth-s256 (or (f "code_verifier") ""))))
                          (return-from mcp-oauth-token (refuse "pkce"))))
                   (setf scope (getf code :scope) pkce "ok")))
                ((equal grant "refresh_token")
                 (let ((kept (gethash (f "refresh_token") (mcp-oauth-fixture-refresh fixture))))
                   ;; Rotation: a refresh token serves once.
                   (remhash (f "refresh_token") (mcp-oauth-fixture-refresh fixture))
                   (unless (and kept (equal (getf kept :client-id) client-id))
                     (return-from mcp-oauth-token (refuse "refresh_token")))
                   (setf scope (getf kept :scope))))
                (t (return-from mcp-oauth-token (refuse "grant_type" "unsupported_grant_type"))))
              (let ((access (format nil "at-~a" (mcp::fresh-nonce)))
                    (refresh (format nil "rt-~a" (mcp::fresh-nonce))))
                (bt2:with-lock-held ((mcp-oauth-fixture-lock fixture))
                  (setf (gethash access (mcp-oauth-fixture-access fixture))
                        (list :client-id client-id :expires (+ (get-universal-time) (mcp-oauth-fixture-ttl fixture)))
                        (gethash refresh (mcp-oauth-fixture-refresh fixture))
                        (list :client-id client-id :scope scope)))
                (mcp-oauth-note fixture :ev :token :ok t :grant grant :client-auth how :resource resource
                                        :pkce pkce :access access :refresh refresh)
                (mcp-oauth-json 200 (nlk:json-object "access_token" access "token_type" "Bearer"
                                                     "expires_in" (mcp-oauth-fixture-ttl fixture)
                                                     "refresh_token" refresh "scope" scope)))))))))

(defun mcp-oauth-mcp (fixture env headers body)
  (let* ((authorization (or (gethash "authorization" headers) ""))
         (token (and (uiop:string-prefix-p "Bearer " authorization) (subseq authorization 7)))
         (grant (and token (bt2:with-lock-held ((mcp-oauth-fixture-lock fixture))
                             (gethash token (mcp-oauth-fixture-access fixture)))))
         (message (ignore-errors (cell-json body)))
         (state (cond ((null token) :missing)
                      ((null grant) :invalid)
                      ((< (getf grant :expires) (get-universal-time)) :expired)
                      (t :ok))))
    (mcp-oauth-note fixture :ev :mcp :auth state
                            :method (and (hash-table-p message) (gethash "method" message)))
    (if (eq state :ok)
        (mcp-fixture-reply (make-mcp-http-fixture) env message)
        (mcp-oauth-json 401 (nlk:json-object "error" (if token "invalid_token" "unauthorized"))
                        :www-authenticate (mcp-oauth-challenge fixture (and token "invalid_token"))))))

(defun mcp-oauth-app (fixture)
  (lambda (env &aux (method (getf env :request-method))
                    (path (getf env :path-info))
                    (issuer-path (string-right-trim "/" (mcp-oauth-fixture-issuer-path fixture))))
    (handler-case
        (cond
          ((and (eq method :get) (member path '("/.well-known/oauth-protected-resource/mcp"
                                               "/.well-known/oauth-protected-resource")
                                         :test #'string=))
           (mcp-oauth-note fixture :ev :prm :served (mcp-oauth-fixture-prm-p fixture))
           (if (mcp-oauth-fixture-prm-p fixture)
               (mcp-oauth-json 200 (nlk:json-object "resource" (mcp-oauth-resource fixture)
                                                    "authorization_servers" (vector (mcp-oauth-issuer fixture))
                                                    "scopes_supported" (vector "mcp:tools")))
               (mcp-oauth-json 404 (nlk:json-object "error" "not_found"))))
          ((and (eq method :get) (or (uiop:string-prefix-p "/.well-known/oauth-authorization-server" path)
                                     (search "openid-configuration" path)))
           (let ((served (string= path (format nil "/.well-known/oauth-authorization-server~a" issuer-path))))
             (mcp-oauth-note fixture :ev :asm :served served :path path)
             (if served
                 (mcp-oauth-json 200 (mcp-oauth-server-metadata fixture))
                 (mcp-oauth-json 404 (nlk:json-object "error" "not_found")))))
          ((and (eq method :get) (string= path (format nil "~a/authorize" issuer-path)))
           (mcp-oauth-authorize fixture (quri:url-decode-params (or (getf env :query-string) ""))))
          ((and (eq method :post) (string= path "/mcp"))
           (mcp-oauth-mcp fixture env (getf env :headers) (mcp-oauth-body env)))
          ((and (eq method :post) (string= path (format nil "~a/register" issuer-path))
                (member (mcp-oauth-fixture-mode fixture) '(:dcr :both :confidential)))
           (mcp-oauth-register fixture (mcp-oauth-body env)))
          ((and (eq method :post) (string= path (format nil "~a/token" issuer-path)))
           (mcp-oauth-token fixture (getf env :headers) (mcp-oauth-body env)))
          ((and (eq method :post) (string= path (format nil "~a/revoke" issuer-path)))
           (let ((token (cdr (assoc "token" (quri:url-decode-params (mcp-oauth-body env)) :test #'string=))))
             (bt2:with-lock-held ((mcp-oauth-fixture-lock fixture))
               (remhash token (mcp-oauth-fixture-access fixture))
               (remhash token (mcp-oauth-fixture-refresh fixture)))
             (mcp-oauth-note fixture :ev :revoke)
             (list 200 '(:content-type "text/plain") '(""))))
          ((eq method :delete) (list 200 '(:content-type "text/plain") '("bye")))
          (t (mcp-oauth-note fixture :ev :unknown :path path)
             (mcp-oauth-json 404 (nlk:json-object "error" "not_found"))))
      (error (condition)
        (list 500 '(:content-type "text/plain") (list (format nil "fixture error: ~a" condition)))))))

(defun mcp-start-oauth-fixture (&rest options)
  (let ((fixture (apply #'make-mcp-oauth-fixture options)))
    (setf (values (mcp-oauth-fixture-handler fixture) (mcp-oauth-fixture-port fixture))
          (nle:serve-local (mcp-oauth-app fixture)))
    fixture))

(defmacro with-mcp-oauth-fixture ((var &rest options) &body body)
  `(let ((,var (mcp-start-oauth-fixture ,@options)))
     (unwind-protect (progn ,@body)
       (nle:stop-clack-handler (mcp-oauth-fixture-handler ,var)))))

(defmacro with-mcp-oauth-store (&body body)
  "BODY with the OAuth store in a fresh temp directory and no browser."
  `(with-temp-directory (oauth-directory "mcp-oauth")
     (with-saved-globals (mcp::*oauth-store-path* mcp::*open-browser-p*)
       (setf mcp::*oauth-store-path* (merge-pathnames "oauth.json" oauth-directory)
             mcp::*open-browser-p* nil)
       ,@body)))

(defun mcp-oauth-entry (fixture &rest pairs)
  (apply #'mcp-entry "url" (mcp-oauth-resource fixture) "timeout_ms" 5000 pairs))

(defun mcp-sign-in-address (name)
  (let ((flow (bt2:with-lock-held (mcp::*sign-ins-lock*) (gethash name mcp::*sign-ins*))))
    (and flow (mcp::sign-in-address flow))))

(defun mcp-browse (address &key (follow t))
  "Visit ADDRESS as a person who approves. => the status of the loopback
callback's page once the authorization's redirect is followed, or (FOLLOW
NIL) the address the browser lands on."
  (flet ((get-page (url &rest options)
           (handler-case (multiple-value-bind (body status headers)
                             (apply #'dex:get url :use-connection-pool nil :keep-alive nil options)
                           (declare (ignore body))
                           (values status headers))
             (dex:http-request-failed (condition)
               (values (dex:response-status condition) (dex:response-headers condition))))))
    (multiple-value-bind (status headers) (get-page address :max-redirects 0)
      (let ((location (and (hash-table-p headers) (gethash "location" headers))))
        (cond ((not (eql status 302)) status)
              ((not follow) location)
              (t (values (get-page location))))))))

(defun mcp-await-state (name state &key (timeout 10) &aux (server (mcp-server name)))
  (await (:timeout timeout) (and (eq state server.state) server.settled-p state)))

(defun mcp-file-mode (path)
  (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring path)))))

(defun mcp-kept-tokens (fixture)
  "Every token FIXTURE issued."
  (loop for event in (mcp-oauth-events fixture :token)
        when (getf event :access) collect (getf event :access)
        when (getf event :refresh) collect (getf event :refresh)))

(defun mcp-oauth-sign-in (name)
  "Sign NAME in as a person does: /mcp login, the browser, the redirect. =>
the answer /mcp login gave."
  (let ((answer (mcp-slash (format nil "login ~a" name))))
    (mcp-browse (mcp-sign-in-address name))
    answer))

;;; --- units -------------------------------------------------------------------

(deftest mcp-cell-oauth-pkce-is-rfc-7636-s256 ()
  (is (equal "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
             (mcp::base64url (mcp::sha256-octets "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")))
      "RFC 7636 appendix B's verifier gives its challenge")
  (multiple-value-bind (verifier challenge) (mcp::pkce-pair)
    (is (<= 43 (length verifier) 128) "a verifier is 43 to 128 characters")
    (is (equal challenge (mcp::base64url (mcp::sha256-octets verifier))))))

(deftest mcp-cell-oauth-reads-a-bearer-challenge ()
  (let ((challenge (mcp::parse-www-authenticate
                    "Bearer realm=\"mcp\", error=\"invalid_token\", resource_metadata=\"https://x.example/.well-known/oauth-protected-resource/mcp\", scope=\"a b\"")))
    (is (equal "https://x.example/.well-known/oauth-protected-resource/mcp" (getf challenge :resource-metadata)))
    (is (equal "a b" (getf challenge :scope)))
    (is (equal "invalid_token" (getf challenge :error))))
  (is (null (mcp::parse-www-authenticate "Basic realm=\"x\"")) "another scheme is no challenge")
  (is (null (getf (mcp::parse-www-authenticate "Bearer scope=\"\"") :scope)) "an empty value is absent")
  (is (equal "insufficient_scope" (getf (mcp::parse-www-authenticate "Bearer error=insufficient_scope") :error))
      "an unquoted value reads"))

(deftest mcp-cell-oauth-discovery-paths ()
  (is (equal '("https://as.example/.well-known/oauth-authorization-server/tenant1"
               "https://as.example/.well-known/openid-configuration/tenant1"
               "https://as.example/tenant1/.well-known/openid-configuration")
             (mcp::authorization-server-candidates "https://as.example/tenant1"))
      "an issuer with a path: RFC 8414's insertion, then OpenID's two")
  (is (equal '("https://as.example/.well-known/oauth-authorization-server"
               "https://as.example/.well-known/openid-configuration")
             (mcp::authorization-server-candidates "https://as.example/")))
  (is (equal '("https://mcp.example/.well-known/oauth-protected-resource/v1/mcp"
               "https://mcp.example/.well-known/oauth-protected-resource")
             (mcp::resource-metadata-candidates "https://mcp.example/v1/mcp")))
  (is (equal "https://mcp.example/v1"
             (mcp::select-resource "https://mcp.example/v1/mcp"
                                   (nlk:json-object "resource" "https://mcp.example/v1")))
      "a resource covering the server's path is the one asked for")
  (is (search "does not cover"
              (refusal-text mcp::oauth-failure
                (mcp::select-resource "https://mcp.example/mcp"
                                      (nlk:json-object "resource" "https://evil.example/mcp"))))
      "a resource on another origin is refused")
  (is (search "is not https" (refusal-text mcp::oauth-failure
                               (mcp::secure-endpoint "http://as.example/token" "token endpoint")))
      "a token endpoint over plain http to another machine is refused")
  (is (equal "http://127.0.0.1:9/token" (mcp::secure-endpoint "http://127.0.0.1:9/token" "token endpoint"))
      "loopback http is fine"))

(deftest mcp-cell-oauth-entry-settings ()
  (flet ((spec (&rest pairs) (mcp::parse-server-entry "r" (apply #'mcp-entry "url" "https://h/mcp" pairs))))
    (is (null (mcp::server-spec-oauth (spec))) "an entry without oauth signs in by default")
    (is (mcp::oauth-enabled-p (spec)))
    (is (eq :off (mcp::server-spec-oauth (spec "oauth" nil))) "oauth: false turns it off")
    (is (not (mcp::oauth-enabled-p (spec "oauth" nil))))
    (is (not (mcp::oauth-enabled-p (spec "headers" (mcp-entry "Authorization" "Bearer x"))))
        "its own Authorization header turns it off")
    (let ((settings (mcp::server-spec-oauth (spec "oauth" (mcp-entry "client_id" "abc" "scope" "read"
                                                                       "callback_port" 33418
                                                                       "client_registration" "dcr")))))
      (is (equal "abc" (getf settings :client-id)))
      (is (equal "read" (getf settings :scope)))
      (is (eql 33418 (getf settings :callback-port)))
      (is (eq :dcr (getf settings :client-registration))))
    (is (search "client_registration" (mcp::server-spec-refusal (spec "oauth" (mcp-entry "client_registration" "magic"))))
        "an unknown registration mode refuses the entry")
    (is (search "oauth must be" (mcp::server-spec-refusal (spec "oauth" "yes"))))))

;;; --- the sign-in, end to end --------------------------------------------------

(deftest mcp-cell-oauth-a-401-names-the-sign-in ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (is (eq :error (mcp-wait-ready "remote")) "a 401 fails the connect")
        (is (search "remote needs a sign-in: /mcp login remote" (mcp::server-error-text (mcp-server "remote")))
            "the error names the command that signs in")
        (is (search "/mcp login NAME" (mcp-slash "status")) "/mcp status offers login")
        (is (null (mcp-oauth-events fixture :prm)) "a connect never starts a sign-in on its own")))))

(deftest mcp-cell-oauth-signs-in-by-registration ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture :mode :dcr)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (let ((answer (mcp-slash "login remote")))
          (is (search "Sign in to remote in a browser: http://127.0.0.1:" answer) "the answer is the address to open")
          (is (search "/mcp login remote ADDRESS" answer) "and how to paste an answer back")
          (is (eql 200 (mcp-browse (mcp-sign-in-address "remote"))) "the loopback takes the browser's answer"))
        (is (eq :ready (mcp-await-state "remote" :ready)) "signed in, the server connects")
        (is (equal "echo: hi" (mcp:call "remote" "echo" :message "hi")) "and its tools answer")
        (let ((prm (mcp-oauth-events fixture :prm))
              (register (first (mcp-oauth-events fixture :register)))
              (authorize (first (mcp-oauth-events fixture :authorize)))
              (code (find "authorization_code" (mcp-oauth-events fixture :token)
                          :key (lambda (event) (getf event :grant)) :test #'equal)))
          (is (and prm (getf (first prm) :served)) "the 401's resource metadata is read")
          (is (equal "none" (getf register :auth-method)) "it registers as a public client")
          (is (getf authorize :ok) "the authorization request is taken")
          (is (equal "S256" (getf authorize :ccm)) "with PKCE S256")
          (is (equal "mcp:tools" (getf authorize :scope)) "asking the challenge's scope")
          (is (equal (mcp-oauth-resource fixture) (getf authorize :resource)) "and the RFC 8707 resource")
          (is (and code (getf code :ok) (equal "ok" (getf code :pkce))) "the code exchange proves the verifier")
          (is (equal (mcp-oauth-resource fixture) (getf code :resource)) "for the same resource"))
        (let ((store mcp::*oauth-store-path*))
          (is (= #o600 (mcp-file-mode store)) "the kept sign-in is the owner's alone")
          (dolist (token (mcp-kept-tokens fixture))
            (is (not (search token (mcp:status))) "no token in the status")
            (is (not (search token (mcp-slash "status"))) "nor in /mcp status")))))))

(deftest mcp-cell-oauth-prefers-the-metadata-document ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture :mode :both)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-oauth-sign-in "remote")
        (is (eq :ready (mcp-await-state "remote" :ready)))
        (is (null (mcp-oauth-events fixture :register)) "offered both, it does not register")
        (is (equal "https://nodecode.ai/oauth/client.json"
                   (getf (first (mcp-oauth-events fixture :authorize)) :client-id))
            "the client_id is the metadata document's URL")))))

(deftest mcp-cell-oauth-refreshes-a-token-about-to-expire ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture :ttl 2)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-oauth-sign-in "remote")
        (is (eq :ready (mcp-await-state "remote" :ready)))
        (sleep 2.5)
        (is (equal "echo: later" (mcp:call "remote" "echo" :message "later")) "an expired token is refreshed")
        (is (plusp (mcp-oauth-grants fixture "refresh_token")) "by the refresh grant")
        (is (notany (lambda (event) (eq :expired (getf event :auth))) (mcp-oauth-events fixture :mcp))
            "before it is sent: the server never sees it expired")
        (is (= 1 (length (mcp-oauth-events fixture :authorize))) "and never a second browser")))))

(deftest mcp-cell-oauth-refreshes-a-refused-token-and-sends-again ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-oauth-sign-in "remote")
        (is (eq :ready (mcp-await-state "remote" :ready)))
        ;; The server forgets every access token while the client still holds its own as good.
        (bt2:with-lock-held ((mcp-oauth-fixture-lock fixture)) (clrhash (mcp-oauth-fixture-access fixture)))
        (let ((before (mcp-oauth-grants fixture "refresh_token")))
          (is (equal "echo: retried" (mcp:call "remote" "echo" :message "retried"))
              "a refused token is refreshed and the call sent again")
          (is (= (1+ before) (mcp-oauth-grants fixture "refresh_token")) "with one refresh")
          (is (find :invalid (mcp-oauth-events fixture :mcp) :key (lambda (event) (getf event :auth)))
              "after the server refused the old one"))
        (is (= 1 (length (mcp-oauth-events fixture :authorize))) "and never a second browser")))))

(deftest mcp-cell-oauth-a-dead-grant-asks-for-a-sign-in ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-oauth-sign-in "remote")
        (is (eq :ready (mcp-await-state "remote" :ready)))
        (bt2:with-lock-held ((mcp-oauth-fixture-lock fixture))
          (clrhash (mcp-oauth-fixture-access fixture))
          (clrhash (mcp-oauth-fixture-refresh fixture)))
        (is (search "needs a sign-in: /mcp login remote"
                    (refusal-text mcp:mcp-offline (mcp:call "remote" "echo" :message "x")))
            "a refresh the server refuses ends in the sign-in it needs")
        (is (null (gethash "tokens" (mcp::stored-state (mcp::state-key "remote" (mcp-oauth-resource fixture)))))
            "and the dead tokens are forgotten")))))

(deftest mcp-cell-oauth-refuses-an-answer-of-another-state ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture :tamper-p t)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-slash "login remote")
        (is (eql 400 (mcp-browse (mcp-sign-in-address "remote"))) "the listener refuses it")
        (sleep 0.3)
        (is (zerop (mcp-oauth-grants fixture "authorization_code")) "and exchanges no code")
        (is (mcp-sign-in-address "remote") "the sign-in still waits for its own answer")))))

(deftest mcp-cell-oauth-a-pasted-address-finishes-the-sign-in ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-slash "login remote")
        (let ((landed (mcp-browse (mcp-sign-in-address "remote") :follow nil)))
          (is (search "/callback?code=" landed) "the browser lands on the loopback address")
          (is (search "another sign-in" (mcp:login "remote" (ppcre:regex-replace "state=[^&]+" landed "state=forged")))
              "an address of another sign-in is refused")
          (is (equal "signed in to remote; connecting" (mcp:login "remote" landed))
              "the address pasted back signs in"))
        (is (eq :ready (mcp-await-state "remote" :ready)))
        (is (equal "echo: pasted" (mcp:call "remote" "echo" :message "pasted")))))))

(deftest mcp-cell-oauth-authenticates-a-registered-secret ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture :mode :confidential)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-oauth-sign-in "remote")
        (is (eq :ready (mcp-await-state "remote" :ready)) "a registration that issues a secret signs in")
        (is (equal "basic" (getf (find "authorization_code" (mcp-oauth-events fixture :token)
                                       :key (lambda (event) (getf event :grant)) :test #'equal)
                                 :client-auth))
            "the token request authenticates with the issued secret")))))

(deftest mcp-cell-oauth-an-issuer-with-a-path-and-no-resource-metadata ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture :issuer-path "/tenant1")
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-oauth-sign-in "remote")
        (is (eq :ready (mcp-await-state "remote" :ready)) "an issuer with a path signs in")))
    (with-mcp-oauth-fixture (fixture :prm-p nil)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-oauth-sign-in "remote")
        (is (eq :ready (mcp-await-state "remote" :ready))
            "a server with no resource metadata is its own authorization server")
        (is (null (getf (first (mcp-oauth-events fixture :authorize)) :resource))
            "and no resource is asked for")))))

(deftest mcp-cell-oauth-a-fresh-start-reconnects-from-the-store ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture :ttl 1)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-oauth-sign-in "remote")
        (is (eq :ready (mcp-await-state "remote" :ready))))
      (sleep 1.5)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (is (eq :ready (mcp-wait-ready "remote")) "a new start connects from the kept sign-in")
        (is (= 1 (length (mcp-oauth-events fixture :authorize))) "with no browser")))))

(deftest mcp-cell-oauth-logout-forgets-and-revokes ()
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture))
        (mcp-wait-ready "remote")
        (mcp-oauth-sign-in "remote")
        (is (eq :ready (mcp-await-state "remote" :ready)))
        (is (equal "MCP: remote: signed out; its tokens are forgotten" (mcp-slash "logout remote")))
        (is (mcp-oauth-events fixture :revoke) "the tokens are revoked")
        (is (null (mcp::stored-state (mcp::state-key "remote" (mcp-oauth-resource fixture)))) "and forgotten")
        (is (eq :error (mcp-await-state "remote" :error)) "the server asks for a sign-in again")
        (is (equal "MCP: remote: no sign-in is kept" (mcp-slash "logout remote")))))))

(deftest mcp-cell-oauth-off-keeps-a-401-a-401 ()
  ;; An OAuth error body is not a JSON-RPC answer: a 401 with one failed the
  ;; handshake as `no answer to initialize' before.
  (with-mcp-oauth-store
    (with-mcp-oauth-fixture (fixture)
      (with-mcp-cell ("remote" (mcp-oauth-entry fixture "oauth" nil))
        (is (eq :error (mcp-wait-ready "remote")))
        (is (search "HTTP 401" (mcp::server-error-text (mcp-server "remote"))) "the status is named")
        (is (search "oauth: false" (mcp-slash "login remote")) "and login says why it does not sign in")
        (is (null (mcp-oauth-events fixture :prm)) "no discovery starts")))))
