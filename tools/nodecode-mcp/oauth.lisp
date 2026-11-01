;;;; oauth.lisp --- OAuth sign-in for remote MCP servers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from pi (https://github.com/earendil-works/pi, MIT): the sign-in
;;;; of packages/coding-agent/src/extensions/mcp/oauth.ts at 908aa7b and the
;;;; flow of packages/mcp/src/oauth/ (discovery.ts, flow.ts), which pi adapted
;;;; from the MCP TypeScript SDK's src/client/auth.ts. The client half of the
;;;; MCP authorization spec:
;;;;
;;;;   1. A request without a usable token gets 401 and a Bearer challenge
;;;;      naming the server's protected-resource metadata (RFC 9728); without
;;;;      one, the well-known paths under the server's own URL are read.
;;;;   2. That document names the authorization server, whose metadata comes
;;;;      from RFC 8414's or OpenID's well-known paths, its issuer checked. A
;;;;      server with no resource metadata at all is its own authorization
;;;;      server (the 2025-03-26 rule), with /authorize, /token and /register
;;;;      at its origin when it publishes no metadata either.
;;;;   3. The client: a configured client_id; else this client's Client ID
;;;;      Metadata Document -- https://nodecode.ai/oauth/client.json as the
;;;;      client_id -- when the server takes one from a public client; else
;;;;      RFC 7591 dynamic registration, kept and reused while its redirect
;;;;      still serves.
;;;;   4. The authorization code grant with PKCE S256, the RFC 8707 resource
;;;;      and a loopback redirect (RFC 8252, any free port at /callback); the
;;;;      answer's state checked, and its iss (RFC 9207) whenever it carries
;;;;      one or the server says it sends one.
;;;;   5. Tokens kept per server, sent as Authorization: Bearer, refreshed
;;;;      before they expire and after a 401, the request then sent again.
;;;;
;;;; A connection never starts a browser on its own: it sends the kept token,
;;;; refreshes it, and when neither works fails with SIGN-IN-REQUIRED, which
;;;; names /mcp login NAME. A sign-in answers the address to open at once,
;;;; hands it to a browser here when there is one ($BROWSER, else the
;;;; platform's opener on a desktop), and finishes on a thread of its own when
;;;; the browser comes back to the loopback listener -- or when the address the
;;;; browser landed on is pasted back, for a browser on another machine.
;;;;
;;;; pi also serves one metadata document per MCP server (<id>/client.json,
;;;; with a redirect path of the server's own) for authorization servers that
;;;; send no iss; nodecode.ai serves the one document, so every server gets
;;;; it, and an answer is bound to its sign-in by the state and the listener.
;;;;
;;;; mcp/oauth.json under the home keeps the state, one member per server
;;;; keyed by name and URL, written atomically at mode 0600. Tokens and
;;;; secrets are never printed: a condition names the step and the server's
;;;; error code, never a value.

(in-package #:nodecode-mcp)

;;; --- settings -----------------------------------------------------------------

(defparameter +client-metadata-url+ "https://nodecode.ai/oauth/client.json"
  "This client's Client ID Metadata Document: its URL is the client_id, and
it lists http://127.0.0.1/callback and http://localhost/callback, any port.")

(defparameter +client-name+ "Nodecode"
  "The client_name a dynamic registration asks for.")

(defparameter +callback-host+ "127.0.0.1")

(defparameter +callback-path+ "/callback")

(defparameter *oauth-request-seconds* 15
  "Bound on each request to an authorization server or a metadata URL, so an
unresponsive one holds a refresh, a connect or a sign-in only this long.")

(defparameter *sign-in-prepare-seconds* 30
  "Bound on what a sign-in does before it can answer the address to open:
discovery and a registration.")

(defparameter *sign-in-wait-seconds* 600
  "How long a sign-in waits for the browser's answer, or a pasted one.")

(defparameter *paste-wait-seconds* 30
  "How long a pasted answer waits for the code exchange to say how it went.")

(defparameter *refresh-skew-seconds* 30
  "An access token this close to its expiry is refreshed before it is sent;
half its lifetime, for one that lives shorter than twice this.")

(nlk:define-startup-parameter *oauth-store-path*
  (nlk:home "mcp/oauth.json")
  "The kept OAuth state: one member per server, keyed by name and URL.")

;;; --- conditions -----------------------------------------------------------------

(nlk:define-error oauth-failure (error) (detail (code :initform nil))
  (:report (lambda (condition stream) (format stream "~a" (slot-value condition 'detail))))
  (:documentation "A step of the OAuth flow failed. DETAIL names the step and
what the server said, never a token or a secret; CODE is the OAuth error code
it answered (invalid_grant, ...), or NIL."))

(defun oauth-fail (control &rest arguments)
  (error 'oauth-failure :detail (apply #'format nil control arguments)))

(defun failure-code (condition)
  (and (typep condition 'oauth-failure) (slot-value condition 'code)))

(define-condition sign-in-required (transport-closed) ()
  (:documentation "The server wants a sign-in this client cannot make on its
own: no token, or one that neither the kept refresh token nor a retry
repairs. DETAIL names /mcp login, and why a refresh did not do."))

(defun sign-in-required (name &key step-up why)
  (error 'sign-in-required
         :detail (format nil "~a needs a sign-in~:[~; with more scope~]: /mcp login ~a~@[ (~a)~]"
                         name step-up name why)))

;;; --- JSON and URLs --------------------------------------------------------------

(defun json-text (object &rest keys)
  "The non-empty string at KEYS under OBJECT, or NIL."
  (let ((value (and (hash-table-p object) (apply #'nlk:json-value object :string keys))))
    (and (stringp value) (plusp (length value)) value)))

(defun json-strings (object key)
  "The strings of the array at KEY in OBJECT."
  (let ((value (and (hash-table-p object) (gethash key object))))
    (and (vectorp value) (not (stringp value))
         (loop for item across value when (stringp item) collect item))))

(defun json-true-p (object key)
  (and (hash-table-p object) (eq t (gethash key object))))

(defun unix-now ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun without-fragment (url)
  (subseq url 0 (or (position #\# url) (length url))))

(defun without-query (url)
  (subseq url 0 (or (position #\? url) (position #\# url) (length url))))

(defun url-origin (url &aux (uri (quri:uri url)))
  "URL's scheme, host and port, a default port left out: what a well-known
path hangs off."
  (let ((scheme (string-downcase (or (quri:uri-scheme uri) "")))
        (port (quri:uri-port uri)))
    (format nil "~a://~(~a~)~@[:~d~]" scheme (quri:uri-host uri)
            (and port (not (eql port (if (string= scheme "https") 443 80))) port))))

(defun url-path (url)
  "URL's path, / for none."
  (let ((path (quri:uri-path (quri:uri url))))
    (if (or (null path) (zerop (length path))) "/" path)))

(defun path-suffix (path)
  "PATH without its trailing slash, \"\" for the root: what follows
/.well-known/KIND for a URL with a path."
  (string-right-trim "/" path))

(defun loopback-host-p (host)
  (member (string-downcase (or host "")) '("localhost" "127.0.0.1" "[::1]" "::1") :test #'string=))

(defun http-url-p (url)
  (let ((uri (ignore-errors (quri:uri url))))
    (and uri (member (quri:uri-scheme uri) '("http" "https") :test #'equalp)
         (quri:uri-host uri) t)))

(defun secure-endpoint (url what)
  "URL, once it is https or http on loopback: a code, a verifier or a secret
never crosses plain http to another machine."
  (let ((uri (ignore-errors (quri:uri url))))
    (unless (and uri (or (equalp (quri:uri-scheme uri) "https")
                         (and (equalp (quri:uri-scheme uri) "http")
                              (loopback-host-p (quri:uri-host uri)))))
      (oauth-fail "the ~a ~a is not https" what (without-query url)))
    url))

(defun with-query (url params)
  "URL with PARAMS, an alist, added to its query."
  (format nil "~a~:[?~;&~]~a" url (find #\? url) (quri:url-encode-params params)))

(defun merge-scopes (&rest scopes)
  "The words of SCOPES, each once, in order, as one scope; NIL for none."
  (let ((words (remove-duplicates (loop for scope in scopes append (nlk:split-words (or scope "")))
                                  :test #'string= :from-end t)))
    (and words (format nil "~{~a~^ ~}" words))))

;;; --- requests to an authorization server -------------------------------------------

(defun octets-text (body)
  (cond ((stringp body) body)
        ((typep body '(vector (unsigned-byte 8)))
         (sb-ext:octets-to-string body :external-format '(:utf-8 :replacement #\?)))
        (t "")))

(defun oauth-request (method url &key headers content)
  "(values TEXT STATUS HEADERS) of one request to URL, a refused status
answered like any other; OAUTH-FAILURE when the server cannot be reached
within *OAUTH-REQUEST-SECONDS*."
  (let ((seconds *oauth-request-seconds*))
    (handler-case
        ;; Dexador's read timeout does not cover cl+ssl's handshake wait.
        (sb-sys:with-deadline (:seconds (+ seconds 2))
          (multiple-value-bind (body status response-headers)
              (dex:request url :method method :headers headers :content content
                               :force-string t :max-redirects (if (eq method :get) 5 0)
                               :use-connection-pool nil :keep-alive nil
                               :connect-timeout seconds :read-timeout seconds)
            (values (octets-text body) status response-headers)))
      (dex:http-request-failed (condition)
        (values (octets-text (ignore-errors (dex:response-body condition)))
                (dex:response-status condition)
                (ignore-errors (dex:response-headers condition))))
      (sb-sys:deadline-timeout ()
        (oauth-fail "~a did not answer within ~d s" (without-query url) seconds))
      (error (condition)
        (oauth-fail "~a could not be reached: ~a" (without-query url)
                    (nlk:one-line (princ-to-string condition) :cap 200))))))

(defun fetch-metadata (url)
  "(values OBJECT STATUS) of the JSON document at URL: OBJECT is NIL when the
answer is not a 2xx JSON object."
  (multiple-value-bind (text status)
      (oauth-request :get url :headers `(("accept" . "application/json")
                                         ("mcp-protocol-version" . ,+protocol-version+)))
    (values (and (<= 200 status 299)
                 (let ((value (ignore-errors (nlk:decode-json text))))
                   (and (hash-table-p value) value)))
            status)))

(defun discovery-miss-p (status)
  "Whether STATUS says the document is not at this URL, so discovery tries
the next one."
  (or (<= 400 status 499) (= status 502)))

;;; --- discovery ------------------------------------------------------------------

(defun challenge-field (header name)
  "The value of the auth-param NAME in HEADER, quoted or not; NIL when it is
absent or empty."
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings
       (format nil "(?i)(?:^|[,\\s])~a=(?:\"([^\"]*)\"|([^\\s,]+))" name) header)
    (and match
         (let ((value (or (aref groups 0) (aref groups 1))))
           (and value (plusp (length value)) value)))))

(defun parse-www-authenticate (header)
  "The Bearer (or DPoP) challenge in HEADER as a plist :RESOURCE-METADATA
:SCOPE :ERROR :ERROR-DESCRIPTION; NIL for any other scheme, or none."
  (when (and (stringp header) (plusp (length header)))
    (let ((scheme (string-downcase (or (first (nlk:split-words header)) ""))))
      (when (member scheme '("bearer" "dpop") :test #'string=)
        (list :resource-metadata (let ((url (challenge-field header "resource_metadata")))
                                   (and url (http-url-p url) url))
              :scope (challenge-field header "scope")
              :error (challenge-field header "error")
              :error-description (challenge-field header "error_description"))))))

(defun resource-metadata-candidates (server-url)
  (let ((origin (url-origin server-url))
        (suffix (path-suffix (url-path server-url))))
    (if (plusp (length suffix))
        (list (format nil "~a/.well-known/oauth-protected-resource~a" origin suffix)
              (format nil "~a/.well-known/oauth-protected-resource" origin))
        (list (format nil "~a/.well-known/oauth-protected-resource" origin)))))

(defun discover-resource-metadata (server-url &optional resource-metadata-url)
  "SERVER-URL's protected-resource metadata (RFC 9728), from the URL its
challenge named, else from the well-known paths under its own; NIL when it
publishes none."
  (dolist (url (if resource-metadata-url
                   (list resource-metadata-url)
                   (resource-metadata-candidates server-url))
               nil)
    (multiple-value-bind (object status) (fetch-metadata url)
      (cond ((json-text object "resource") (return object))
            ((not (discovery-miss-p status)) (return nil))))))

(defun authorization-server-candidates (issuer)
  "Where ISSUER's metadata may be: RFC 8414's path insertion, then OpenID's,
then OpenID's appended path."
  (let ((origin (url-origin issuer))
        (path (path-suffix (url-path issuer))))
    (append (list (format nil "~a/.well-known/oauth-authorization-server~a" origin path)
                  (format nil "~a/.well-known/openid-configuration~a" origin path))
            (when (plusp (length path))
              (list (format nil "~a~a/.well-known/openid-configuration" origin path))))))

(defun same-issuer-p (a b)
  ;; A bare origin parses with a trailing slash: compare without one on either side.
  (and a b (string= (string-right-trim "/" a) (string-right-trim "/" b))))

(defun checked-server-metadata (object url)
  (unless (and (json-text object "issuer") (json-text object "authorization_endpoint")
               (json-text object "token_endpoint"))
    (oauth-fail "the authorization server metadata at ~a names no issuer, authorization or token endpoint"
                (without-query url)))
  object)

(defun discover-server-metadata (issuer)
  "ISSUER's authorization server metadata (RFC 8414, else OpenID discovery),
its issuer checked against ISSUER; NIL when it publishes none."
  (dolist (url (authorization-server-candidates issuer) nil)
    (multiple-value-bind (object status) (fetch-metadata url)
      (cond (object
             (checked-server-metadata object url)
             (unless (same-issuer-p (json-text object "issuer") issuer)
               (oauth-fail "the authorization server metadata at ~a names the issuer ~a, not ~a"
                           url (json-text object "issuer") issuer))
             (return object))
            ((discovery-miss-p status))
            ((<= 200 status 299)
             (oauth-fail "the authorization server metadata at ~a is not a JSON object" url))
            (t (oauth-fail "HTTP ~d loading the authorization server metadata from ~a" status url))))))

(defun select-resource (server-url resource-metadata)
  "The resource a token is asked for (RFC 8707): the one SERVER-URL's
metadata names, once it covers SERVER-URL; NIL without metadata."
  (when resource-metadata
    (let ((resource (json-text resource-metadata "resource"))
          (requested (without-fragment server-url)))
      (flet ((folder (url &aux (path (url-path url)))
               (if (uiop:string-suffix-p path "/") path (concatenate 'string path "/"))))
        (unless (and (string= (url-origin requested) (url-origin resource))
                     (uiop:string-prefix-p (folder resource) (folder requested)))
          (oauth-fail "the protected resource ~a does not cover the server ~a" resource requested)))
      resource)))

(defun discover (server-url &key resource-metadata-url auth-server-metadata-url)
  "Where SERVER-URL signs in, as the JSON object kept beside its tokens:
issuer, metadata (absent when the server publishes none), resource and the
resource's scopes_supported."
  (let ((resource-metadata (discover-resource-metadata server-url resource-metadata-url))
        (issuer nil)
        (metadata nil))
    (if auth-server-metadata-url
        ;; Configured, so trusted as written: its issuer is not compared.
        (multiple-value-bind (object status) (fetch-metadata (secure-endpoint auth-server-metadata-url "metadata URL"))
          (unless object
            (oauth-fail "HTTP ~d loading the authorization server metadata from ~a" status
                        auth-server-metadata-url))
          (setf metadata (checked-server-metadata object auth-server-metadata-url)
                issuer (json-text object "issuer")))
        (setf issuer (or (first (json-strings resource-metadata "authorization_servers"))
                         (format nil "~a/" (url-origin server-url)))
              metadata (discover-server-metadata issuer)))
    (nlk:json-object "issuer" issuer
                     :opt "metadata" metadata
                     :opt "resource" (select-resource server-url resource-metadata)
                     :opt "scopes_supported" (let ((scopes (json-strings resource-metadata "scopes_supported")))
                                               (and scopes (coerce scopes 'vector))))))

(defun endpoint (discovery key default-path)
  "The authorization server's KEY endpoint; a server that publishes no
metadata has it at DEFAULT-PATH under the issuer's origin."
  (if (hash-table-p (gethash "metadata" discovery))
      (json-text discovery "metadata" key)
      (format nil "~a~a" (url-origin (gethash "issuer" discovery)) default-path)))

;;; --- PKCE and randomness --------------------------------------------------------------

(defun base64url (octets)
  "OCTETS as unpadded base64url."
  (string-right-trim "." (cl-base64:usb8-array-to-base64-string
                          (coerce octets '(simple-array (unsigned-byte 8) (*))) :uri t)))

(defun sha256-octets (text)
  "The SHA-256 digest of TEXT's UTF-8 bytes."
  (let ((hex (subseq (nlk:sha256-text text) 7)))
    (coerce (loop for at from 0 below 64 by 2
                  collect (parse-integer hex :start at :end (+ at 2) :radix 16))
            '(simple-array (unsigned-byte 8) (*)))))

(defun pkce-pair ()
  "(values VERIFIER CHALLENGE): 32 random bytes as base64url, and its S256."
  (let ((verifier (base64url (nlk:random-bytes 32))))
    (values verifier (base64url (sha256-octets verifier)))))

(defun fresh-nonce ()
  (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes 24) 'list)))

;;; --- the store ----------------------------------------------------------------------

(defvar *oauth-store-lock* (bt2:make-recursive-lock :name "mcp-oauth-store")
  "Held across one read-modify-write of the store.")

(defun state-key (name url)
  "The store's key for the server NAME at URL: a name can be reused for
another server, and two names can share a URL, each with its own account."
  (format nil "~a|~a" name (without-fragment url)))

(defun read-oauth-store ()
  (let ((path *oauth-store-path*))
    (if (probe-file path)
        (let ((value (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))
          ;; A store that cannot be read is never overwritten: it holds other servers' sign-ins.
          (unless (hash-table-p value)
            (oauth-fail "~a is not a JSON object; move it aside to sign in again"
                        (uiop:native-namestring path)))
          value)
        (make-hash-table :test #'equal))))

(defun stored-state (key)
  "KEY's kept state, a JSON object, or NIL."
  (bt2:with-recursive-lock-held (*oauth-store-lock*)
    (let ((state (gethash key (read-oauth-store))))
      (and (hash-table-p state) state))))

(defun update-stored-state (key function)
  "Call FUNCTION on KEY's kept state -- a fresh object when none -- and keep
what it answers as KEY's state; NIL forgets it. => what FUNCTION answered."
  (bt2:with-recursive-lock-held (*oauth-store-lock*)
    (let* ((store (read-oauth-store))
           (current (gethash key store))
           (next (funcall function (if (hash-table-p current) current (make-hash-table :test #'equal)))))
      (if next (setf (gethash key store) next) (remhash key store))
      (nlk:write-file-atomically *oauth-store-path* (shasht:write-json store nil)
                                 :mode #o600 :directory-mode #o700)
      next)))

(defun kept-tokens (answer &key refresh-token scope)
  "The token endpoint's ANSWER as kept: a refresh that names no new refresh
token keeps REFRESH-TOKEN, and one that names no scope keeps SCOPE (RFC 6749
5.1 and 6); expires_in becomes an absolute expires_at."
  (let* ((raw (gethash "expires_in" answer))
         (expires-in (if (stringp raw) (ignore-errors (parse-integer raw)) raw))
         (now (unix-now)))
    (nlk:json-object "access_token" (json-text answer "access_token")
                     :opt "token_type" (json-text answer "token_type")
                     :opt "refresh_token" (or (json-text answer "refresh_token") refresh-token)
                     :opt "scope" (or (json-text answer "scope") scope)
                     :opt "issued_at" (and (realp expires-in) (plusp expires-in) now)
                     :opt "expires_at" (and (realp expires-in) (plusp expires-in)
                                            (+ now (floor expires-in))))))

(defun tokens-expiring-p (tokens)
  "Whether TOKENS' access token is about to expire, or has."
  (let ((at (gethash "expires_at" tokens))
        (issued (gethash "issued_at" tokens)))
    (and (realp at)
         (<= (- at (min *refresh-skew-seconds*
                        (if (realp issued) (/ (- at issued) 2) *refresh-skew-seconds*)))
             (unix-now)))))

;;; --- the client ------------------------------------------------------------------

(defun configured-client (settings)
  "The client the server's oauth settings name, its secret resolved, or NIL."
  (nlk:when-let (id (getf settings :client-id))
    (nlk:json-object "client_id" id "via" "config"
                     :opt "client_secret" (nlk:when-let (secret (getf settings :client-secret))
                                            (resolve-references secret "oauth.client_secret")))))

(defun state-client (state settings)
  "The client STATE's tokens belong to: the configured one (its secret is
never kept), else the kept registration or document."
  (or (configured-client settings) (and state (gethash "client" state))))

(defun client-document-p (metadata)
  "Whether METADATA takes a Client ID Metadata Document from a public client."
  (and (json-true-p metadata "client_id_metadata_document_supported")
       (member "none" (json-strings metadata "token_endpoint_auth_methods_supported") :test #'string=)))

(defun register-client (discovery redirect-uri scope)
  "Register this client by RFC 7591 as a public native client with
REDIRECT-URI: the registration, marked as one."
  (let ((url (or (endpoint discovery "registration_endpoint" "/register")
                 (oauth-fail "the authorization server offers no dynamic client registration; set oauth.client_id for this server"))))
    (secure-endpoint url "registration endpoint")
    (multiple-value-bind (text status)
        (oauth-request :post url
                       :headers '(("accept" . "application/json") ("content-type" . "application/json"))
                       :content (nlk:encode-json-object
                                 (nlk:json-object "client_name" +client-name+
                                                  "redirect_uris" (vector redirect-uri)
                                                  "grant_types" (vector "authorization_code" "refresh_token")
                                                  "response_types" (vector "code")
                                                  "token_endpoint_auth_method" "none"
                                                  "application_type" "native"
                                                  :opt "scope" scope)))
      (let ((value (ignore-errors (nlk:decode-json text))))
        (unless (<= 200 status 299)
          (oauth-fail "dynamic client registration was refused: HTTP ~d~@[ ~a~]" status
                      (json-text value "error")))
        (unless (json-text value "client_id")
          (oauth-fail "the dynamic client registration answered no client_id"))
        (setf (gethash "via" value) "dcr")
        (unless (json-strings value "redirect_uris")
          (setf (gethash "redirect_uris" value) (vector redirect-uri)))
        value))))

(defun choose-client (key settings discovery redirect-uri scope)
  "The client a sign-in uses: the configured one; else a kept registration
whose redirect still serves; else this client's metadata document when the
server takes one (oauth.client_registration auto or cimd); else a new
registration, kept at once and with no tokens of the old client beside it."
  (let* ((metadata (gethash "metadata" discovery))
         (mode (or (getf settings :client-registration) :auto))
         (state (stored-state key))
         (kept (and state (gethash "client" state))))
    (cond
      ((configured-client settings))
      ((and (equal (json-text kept "via") "dcr") (not (eq mode :cimd))
            (member redirect-uri (json-strings kept "redirect_uris") :test #'string=))
       kept)
      ((and (not (eq mode :dcr)) (client-document-p metadata))
       (nlk:json-object "client_id" +client-metadata-url+ "via" "cimd"))
      ((eq mode :cimd)
       (oauth-fail "the authorization server takes no Client ID Metadata Document from a public client; remove oauth.client_registration \"cimd\""))
      (t (let ((client (register-client discovery redirect-uri scope)))
           (update-stored-state key (lambda (kept-state)
                                      (setf (gethash "client" kept-state) client)
                                      (remhash "tokens" kept-state)
                                      kept-state))
           client)))))

(defun client-authentication (client discovery params headers)
  "(values PARAMS HEADERS) carrying CLIENT's authentication to the token or
revocation endpoint: the method the registration names when the server
takes it, else the best the server lists."
  (let* ((supported (json-strings (gethash "metadata" discovery) "token_endpoint_auth_methods_supported"))
         (hinted (json-text client "token_endpoint_auth_method"))
         (id (json-text client "client_id"))
         (secret (json-text client "client_secret"))
         (method (cond ((and hinted (member hinted '("client_secret_basic" "client_secret_post" "none") :test #'string=)
                             (or (null supported) (member hinted supported :test #'string=)))
                        hinted)
                       ((null supported) (if secret "client_secret_basic" "none"))
                       ((and secret (member "client_secret_basic" supported :test #'string=)) "client_secret_basic")
                       ((and secret (member "client_secret_post" supported :test #'string=)) "client_secret_post")
                       ((member "none" supported :test #'string=) "none")
                       (t (if secret "client_secret_post" "none")))))
    (cond ((string= method "client_secret_basic")
           (unless secret (oauth-fail "the client authenticates with client_secret_basic but has no secret"))
           (values params
                   (cons (cons "authorization"
                               (format nil "Basic ~a" (cl-base64:string-to-base64-string
                                                       (format nil "~a:~a" id secret))))
                         headers)))
          (t (values (append params (list (cons "client_id" id))
                             (and (string= method "client_secret_post") secret
                                  (list (cons "client_secret" secret))))
                     headers)))))

(defun token-request (discovery client params)
  "POST PARAMS (an alist), the resource and CLIENT's authentication to the
token endpoint: the answer, once it carries an access token."
  (let ((url (secure-endpoint (or (endpoint discovery "token_endpoint" "/token")
                                  (oauth-fail "the authorization server names no token endpoint"))
                              "token endpoint"))
        (resource (json-text discovery "resource")))
    (multiple-value-bind (params headers)
        (client-authentication client discovery
                               (append params (and resource (list (cons "resource" resource))))
                               '(("accept" . "application/json")
                                 ("content-type" . "application/x-www-form-urlencoded")))
      (multiple-value-bind (text status)
          (oauth-request :post url :headers headers :content (quri:url-encode-params params))
        (let ((value (ignore-errors (nlk:decode-json text))))
          ;; Servers report OAuth errors under any status: the body first.
          (nlk:when-let (code (json-text value "error"))
            (error 'oauth-failure
                   :code code
                   :detail (format nil "the token endpoint refused: ~a~@[ - ~a~]" code
                                   (nlk:when-let (text (json-text value "error_description"))
                                     (nlk:one-line text :cap 200)))))
          (unless (<= 200 status 299)
            (error 'oauth-failure :code "server_error"
                                  :detail (format nil "the token endpoint answered HTTP ~d" status)))
          (unless (json-text value "access_token")
            (oauth-fail "the token endpoint's answer carries no access_token"))
          value)))))

;;; --- tokens on a connection ----------------------------------------------------------

(defstruct (oauth-binding (:copier nil))
  "What an http transport signs in with: the server's name, URL, store key
and oauth settings."
  name url key (settings '() :type list))

(defvar *key-locks* (make-hash-table :test #'equal))
(defvar *key-locks-lock* (bt2:make-lock :name "mcp-oauth-keys"))

(defun key-lock (key)
  "The lock a refresh of KEY's tokens holds, one per server."
  (bt2:with-lock-held (*key-locks-lock*)
    (or (gethash key *key-locks*)
        (setf (gethash key *key-locks*) (bt2:make-lock :name "mcp-oauth-refresh")))))

(defvar *challenges* (make-hash-table :test #'equal)
  "The last Bearer challenge each server answered, by name: what its next
sign-in asks for.")
(defvar *challenges-lock* (bt2:make-lock :name "mcp-oauth-challenges"))

(defun note-challenge (name challenge)
  (bt2:with-lock-held (*challenges-lock*) (setf (gethash name *challenges*) challenge)))

(defun last-challenge (name)
  (bt2:with-lock-held (*challenges-lock*) (values (gethash name *challenges*))))

(defun spec-binding (spec)
  "The binding an http transport to SPEC signs in with, or NIL when SPEC
sends its own Authorization header or says oauth: false."
  (and (oauth-enabled-p spec)
       (make-oauth-binding :name (server-spec-name spec) :url (server-spec-url spec)
                           :key (state-key (server-spec-name spec) (server-spec-url spec))
                           :settings (let ((settings (server-spec-oauth spec)))
                                       (if (listp settings) settings '())))))

(defun kept-discovery (key url settings)
  "KEY's kept discovery, else a fresh one, kept."
  (let ((discovery (let ((state (stored-state key))) (and state (gethash "discovery" state)))))
    (if (hash-table-p discovery)
        discovery
        (let ((fresh (discover url :auth-server-metadata-url (getf settings :auth-server-metadata-url))))
          (update-stored-state key (lambda (state) (setf (gethash "discovery" state) fresh) state))
          fresh))))

(defun refresh-tokens (key url settings stale-token)
  "Replace STALE-TOKEN, the access token that is expiring or was refused. =>
T when KEY now keeps another access token -- refreshed here, or meanwhile by
another request or a sign-in -- else (values NIL WHY). Held under KEY's lock
from reading the tokens to keeping the new ones: a server that rotates
refresh tokens takes the grant back from a second refresh with the old one."
  (bt2:with-lock-held ((key-lock key))
    (let* ((state (stored-state key))
           (tokens (and state (gethash "tokens" state)))
           (current (json-text tokens "access_token"))
           (refresh-token (json-text tokens "refresh_token"))
           (client (and refresh-token (state-client state settings))))
      (cond ((and current (not (equal current stale-token))) t)
            ((null refresh-token) (values nil (and current "no refresh token is kept")))
            ((null client) (values nil "no client is kept"))
            (t (handler-case
                   (let ((answer (token-request (kept-discovery key url settings) client
                                                (list (cons "grant_type" "refresh_token")
                                                      (cons "refresh_token" refresh-token)))))
                     (update-stored-state key (lambda (kept)
                                                (setf (gethash "tokens" kept)
                                                      (kept-tokens answer :refresh-token refresh-token
                                                                          :scope (json-text tokens "scope")))
                                                kept))
                     t)
                 (oauth-failure (condition)
                   ;; A refused grant or client never refreshes: forget the tokens.
                   (when (member (failure-code condition) '("invalid_grant" "invalid_client" "unauthorized_client")
                                 :test #'equal)
                     (update-stored-state key (lambda (kept) (remhash "tokens" kept) kept)))
                   (values nil (format nil "refreshing its token failed: ~a" condition)))))))))

(defun binding-token (binding)
  "The access token a request sends: the kept one, refreshed first when it
is about to expire; NIL without one. A refresh that fails sends the old
token, and the server's 401 decides."
  (let* ((key (oauth-binding-key binding))
         (state (stored-state key))
         (tokens (and state (gethash "tokens" state)))
         (token (json-text tokens "access_token")))
    (if (and token (json-text tokens "refresh_token") (tokens-expiring-p tokens))
        (progn (ignore-errors (refresh-tokens key (oauth-binding-url binding)
                                              (oauth-binding-settings binding) token))
               (or (json-text (stored-state key) "tokens" "access_token") token))
        token)))

(defun binding-kept-token (binding)
  "The kept access token as it is, never refreshed: what a session's DELETE
sends on the way out."
  (ignore-errors (json-text (stored-state (oauth-binding-key binding)) "tokens" "access_token")))

(defun binding-unauthorized (binding used-token header status)
  "After STATUS -- 401, or 403 -- to a request that sent USED-TOKEN (NIL for
none), HEADER its WWW-Authenticate: T when the request may go again with
another token; NIL when the server asked for no OAuth (no Bearer challenge,
nothing kept) or the 403 is not about scope, so the status stands; else
SIGN-IN-REQUIRED."
  (let* ((name (oauth-binding-name binding))
         (key (oauth-binding-key binding))
         (challenge (parse-www-authenticate header)))
    (when challenge (note-challenge name challenge))
    (cond ((equal (getf challenge :error) "insufficient_scope")
           ;; A refresh keeps the granted scope: more needs a sign-in.
           (sign-in-required name :step-up t))
          ((/= status 401) nil)
          ((and (null challenge) (null used-token) (null (stored-state key))) nil)
          (t (multiple-value-bind (refreshed why)
                 (refresh-tokens key (oauth-binding-url binding) (oauth-binding-settings binding) used-token)
               (or refreshed (sign-in-required name :why why)))))))

;;; --- opening a browser -------------------------------------------------------------

(defun browser-command (url)
  "The command that shows URL in a browser here, or NIL: $BROWSER (its first
entry; a %s in it stands for URL), else the platform's opener -- on Linux and
the BSDs only where a desktop is, since an ssh session has none."
  (let ((browser (string-trim " " (or (uiop:getenv "BROWSER") ""))))
    (cond ((plusp (length browser))
           (let ((words (nlk:split-words (first (uiop:split-string browser :separator ":")))))
             (and words
                  (if (member "%s" words :test #'string=)
                      (substitute url "%s" words :test #'string=)
                      (append words (list url))))))
          #+darwin (t (list "open" url))
          #+win32 (t (list "rundll32" "url.dll,FileProtocolHandler" url))
          #-(or darwin win32)
          ((some (lambda (variable &aux (value (uiop:getenv variable)))
                   (and value (plusp (length value))))
                 '("WAYLAND_DISPLAY" "DISPLAY"))
           (list "xdg-open" url)))))

(defvar *open-browser-p* t
  "Whether a sign-in hands its address to a browser here; the tests say no.")

(defun open-in-browser (url)
  "Hand URL to a browser here, detached. => T when one started."
  (let ((command (and *open-browser-p* (browser-command url))))
    (and command
         (nlk:spawn-detached command (nlk:cache-path "nodecode/open-url.log"))
         t)))

;;; --- the sign-in --------------------------------------------------------------------

(defstruct (sign-in (:copier nil))
  name url key (settings '() :type list)
  listener port redirect-uri
  state verifier client discovery scope address
  (answer nil)                          ; (CODE . ISS) once it arrived
  (failure nil)                         ; why it can never arrive
  (outcome nil)                         ; :SIGNED-IN, or the failure's text
  (cancelled nil)
  on-done
  thread)

(defvar *sign-ins* (make-hash-table :test #'equal)
  "The sign-in in progress per server name.")
(defvar *sign-ins-lock* (bt2:make-lock :name "mcp-sign-ins"))

(defun open-callback-listener (port required)
  "(values LISTENER PORT) on 127.0.0.1: at PORT, or at a free one when PORT
is NIL, or taken and not REQUIRED."
  (flet ((listen-at (port)
           (usocket:socket-listen +callback-host+ port :reuse-address t :backlog 8
                                                       :element-type '(unsigned-byte 8))))
    (let ((listener (if port
                        (handler-case (listen-at port)
                          (usocket:address-in-use-error ()
                            (if required
                                (oauth-fail "the callback port ~d is taken" port)
                                (listen-at 0))))
                        (listen-at 0))))
      (values listener (usocket:get-local-port listener)))))

(defun close-callback-listener (flow)
  (nlk:when-let (listener (sign-in-listener flow))
    (setf (sign-in-listener flow) nil)
    (ignore-errors (usocket:socket-close listener))))

(defun read-request-line (stream)
  "The request line of the HTTP request on STREAM, its head read through."
  (let ((buffer (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (vector-push-extend byte buffer)
          until (or (> (fill-pointer buffer) 65536)
                    (let ((end (fill-pointer buffer)))
                      (and (>= end 4)
                           (= 13 (aref buffer (- end 4))) (= 10 (aref buffer (- end 3)))
                           (= 13 (aref buffer (- end 2))) (= 10 (aref buffer (- end 1)))))))
    (let ((text (sb-ext:octets-to-string buffer :external-format :latin-1)))
      (subseq text 0 (or (search (format nil "~c~c" #\Return #\Linefeed) text) (length text))))))

(defun html-escape (text)
  (with-output-to-string (out)
    (loop for char across text
          do (case char
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char char out))))))

(defun respond-page (stream status text)
  "Answer STATUS with a page saying TEXT, and close the exchange."
  (let ((octets (sb-ext:string-to-octets
                 (format nil "<!doctype html><meta charset=utf-8><title>Nodecode</title><p>~a</p>"
                         (html-escape text))
                 :external-format :utf-8))
        (crlf (format nil "~c~c" #\Return #\Linefeed)))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "HTTP/1.1 ~d ~a~aContent-Type: text/html; charset=utf-8~aContent-Length: ~d~aConnection: close~a~a"
                             status (case status (200 "OK") (400 "Bad Request") (t "Not Found"))
                             crlf crlf (length octets) crlf crlf crlf)
                     :external-format :latin-1)
                    stream)
    (write-sequence octets stream)
    (finish-output stream)))

(defun redirect-answer (target)
  "(values PATH CODE STATE ISS ERROR) a redirect TARGET carries: an address
the browser landed on, or a request target's path and query."
  (let* ((target (string-trim '(#\Space #\Tab #\Return #\Newline #\< #\>) target))
         (scheme (search "://" target))
         ;; An address pasted without its scheme (127.0.0.1:PORT/callback?...) still has its path.
         (start (cond (scheme (or (position #\/ target :start (+ scheme 3)) (length target)))
                      ((uiop:string-prefix-p "/" target) 0)
                      (t (or (position #\/ target) (length target)))))
         (mark (position #\? target))
         (end (or mark (position #\# target) (length target)))
         (query (if mark (subseq target (1+ mark) (or (position #\# target :start mark) (length target))) ""))
         (params (ignore-errors (quri:url-decode-params query))))
    (flet ((param (name)
             (let ((value (cdr (assoc name params :test #'string=))))
               (and (stringp value) (plusp (length value)) value))))
      (values (if (< start end) (subseq target start end) "/")
              (param "code") (param "state") (param "iss")
              (nlk:when-let (error (param "error"))
                (or (param "error_description") error))))))

(defun redirect-path (flow)
  (url-path (sign-in-redirect-uri flow)))

(defun serve-callback (flow)
  "Serve one connection waiting on FLOW's listener: the browser's redirect
answers the flow. => T when one was served."
  (let ((listener (sign-in-listener flow)))
    (when (and listener (usocket:wait-for-input listener :timeout 0.1 :ready-only t))
      (let ((connection (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
        (unwind-protect
             ;; A connection that says nothing is dropped after ten seconds; a
             ;; deadline is no ERROR, so it is caught here, not by the caller.
             (handler-case
                 (sb-sys:with-deadline (:seconds 10)
               (let* ((stream (usocket:socket-stream connection))
                      (target (or (second (nlk:split-words (read-request-line stream))) "")))
                 (multiple-value-bind (path code state iss error) (redirect-answer target)
                   (cond ((not (equal path (redirect-path flow)))
                          (respond-page stream 404 "Not found."))
                         ((not (equal state (sign-in-state flow)))
                          (respond-page stream 400 "This answer is not for the sign-in Nodecode started. You can close this tab."))
                         (error
                          (setf (sign-in-failure flow)
                                (format nil "the authorization server refused the sign-in: ~a"
                                        (nlk:one-line error :cap 200)))
                          (respond-page stream 200 (format nil "The sign-in was refused: ~a. You can close this tab."
                                                           (nlk:one-line error :cap 200))))
                         (code
                          (setf (sign-in-answer flow) (cons code iss))
                          (respond-page stream 200 (format nil "Nodecode got the answer and finishes signing in to ~a. You can close this tab."
                                                           (sign-in-name flow))))
                         (t (respond-page stream 400 "No authorization code arrived. You can close this tab."))))))
               (sb-sys:deadline-timeout () nil))
          (ignore-errors (usocket:socket-close connection))))
      t)))

(defun await-answer (flow)
  "Serve FLOW's listener until its answer arrives: (values CODE ISS)."
  (let ((deadline (+ (now-seconds) *sign-in-wait-seconds*)))
    (loop
      (when (sign-in-cancelled flow)
        (error 'oauth-failure :code "cancelled" :detail "the sign-in was cancelled"))
      (nlk:when-let (failure (sign-in-failure flow)) (oauth-fail "~a" failure))
      (nlk:when-let (answer (sign-in-answer flow)) (return (values (car answer) (cdr answer))))
      (when (> (now-seconds) deadline)
        (oauth-fail "no answer from the browser within ~d minutes" (round *sign-in-wait-seconds* 60)))
      (unless (ignore-errors (serve-callback flow))
        (sleep 0.05)))))

(defun check-issuer (flow iss)
  "RFC 9207: a code from another authorization server is never sent to this
one. Without metadata there is no issuer to compare, so any iss is refused."
  (let* ((metadata (gethash "metadata" (sign-in-discovery flow)))
         (issuer (json-text metadata "issuer")))
    (when (or iss (json-true-p metadata "authorization_response_iss_parameter_supported"))
      (unless (and iss issuer (string= iss issuer))
        (oauth-fail "the answer names the issuer ~:[nobody~;~:*~a~], not ~a" iss (or issuer "this server's"))))))

(defun keep-sign-in (flow answer)
  "Keep what FLOW signed in with: its client (a configured one without its
secret), its discovery and ANSWER's tokens."
  (let ((client (sign-in-client flow)))
    (update-stored-state (sign-in-key flow)
                         (lambda (state)
                           (setf (gethash "client" state)
                                 (if (equal (json-text client "via") "config")
                                     (nlk:json-object "client_id" (json-text client "client_id") "via" "config")
                                     client)
                                 (gethash "discovery" state) (sign-in-discovery flow)
                                 (gethash "tokens" state) (kept-tokens answer :scope (sign-in-scope flow)))
                           state))))

(defun finish-sign-in (flow)
  "The background half of a sign-in: wait for the answer, exchange its code,
keep the tokens, run the flow's ON-DONE (the server's reconnect), and say
how it went in a notice."
  (let ((name (sign-in-name flow)))
    (handler-case
        (multiple-value-bind (code iss) (await-answer flow)
          (close-callback-listener flow)
          (check-issuer flow iss)
          (let ((answer (token-request (sign-in-discovery flow) (sign-in-client flow)
                                       (list (cons "grant_type" "authorization_code")
                                             (cons "code" code)
                                             (cons "code_verifier" (sign-in-verifier flow))
                                             (cons "redirect_uri" (sign-in-redirect-uri flow))))))
            (when (sign-in-cancelled flow)
              (error 'oauth-failure :code "cancelled" :detail "the sign-in was cancelled"))
            (keep-sign-in flow answer)
            (setf (sign-in-outcome flow) :signed-in)
            (ignore-errors (nle:notice (format nil "MCP: signed in to ~a; connecting" name)))
            (nlk:when-let (done (sign-in-on-done flow))
              (ignore-errors (funcall done)))))
      (error (condition)
        (setf (sign-in-outcome flow) (princ-to-string condition))
        (unless (or (sign-in-cancelled flow) (equal (failure-code condition) "cancelled"))
          (ignore-errors (nle:notice (format nil "MCP: sign-in to ~a failed: ~a" name condition)
                                     :level :warning))))))
  (close-callback-listener flow)
  (bt2:with-lock-held (*sign-ins-lock*)
    (when (eq (gethash (sign-in-name flow) *sign-ins*) flow)
      (remhash (sign-in-name flow) *sign-ins*))))

(defun cancel-sign-in (name)
  "End NAME's sign-in in progress, if any."
  (let ((flow (bt2:with-lock-held (*sign-ins-lock*)
                (prog1 (gethash name *sign-ins*) (remhash name *sign-ins*)))))
    (when flow
      (setf (sign-in-cancelled flow) t)
      (let ((thread (sign-in-thread flow)))
        (when (and thread (not (eq thread (bt2:current-thread))))
          (loop repeat 40 while (bt2:thread-alive-p thread) do (sleep 0.05))))
      (close-callback-listener flow)
      t)))

(defun cancel-all-sign-ins ()
  (dolist (name (bt2:with-lock-held (*sign-ins-lock*)
                  (loop for name being the hash-keys of *sign-ins* collect name)))
    (cancel-sign-in name)))

(defun registered-port (client)
  "The port of a kept registration's redirect, which a new sign-in reuses so
the registration still serves."
  (and (equal (json-text client "via") "dcr")
       (nlk:when-let (uri (first (json-strings client "redirect_uris")))
         (ignore-errors (quri:uri-port (quri:uri uri))))))

(defun start-sign-in (name url settings &key on-done)
  "Begin signing in to the MCP server NAME at URL. => :REFRESHED when the
kept refresh token already brought fresh tokens (ON-DONE has run), else
(values ADDRESS REDIRECT-URI OPENED): the address to open, where the answer
is awaited, and whether a browser here was handed it. The rest runs on a
thread of its own, which runs ON-DONE once the tokens are kept."
  (cancel-sign-in name)
  (let* ((key (state-key name url))
         (challenge (last-challenge name))
         (step-up (equal (getf challenge :error) "insufficient_scope"))
         (state (stored-state key))
         (tokens (and state (gethash "tokens" state))))
    ;; A kept refresh token signs in again without a browser, unless the
    ;; server asked for more scope than it grants.
    (when (and (not step-up) (json-text tokens "refresh_token")
               (refresh-tokens key url settings (json-text tokens "access_token")))
      (when on-done (funcall on-done))
      (return-from start-sign-in :refreshed))
    (let ((listener nil) (handed-off nil))
      (unwind-protect
           (handler-case
               (sb-sys:with-deadline (:seconds *sign-in-prepare-seconds*)
                 (let* ((discovery (discover url :resource-metadata-url (getf challenge :resource-metadata)
                                                 :auth-server-metadata-url (getf settings :auth-server-metadata-url)))
                        (scope (or (merge-scopes (getf settings :scope)
                                                 (if step-up
                                                     (merge-scopes (json-text tokens "scope") (getf challenge :scope))
                                                     (getf challenge :scope)))
                                   (merge-scopes (format nil "~{~a~^ ~}" (json-strings discovery "scopes_supported")))))
                        (fixed-port (getf settings :callback-port))
                        (port nil))
                   (setf (values listener port)
                         (open-callback-listener (or fixed-port
                                                     (registered-port (and state (gethash "client" state))))
                                                 fixed-port))
                   (let* ((redirect-uri (format nil "http://~a:~d~a" +callback-host+ port +callback-path+))
                          (client (choose-client key settings discovery redirect-uri scope)))
                     (multiple-value-bind (verifier challenge-text) (pkce-pair)
                       (let* ((oauth-state (fresh-nonce))
                              (address (authorization-address discovery client redirect-uri challenge-text
                                                              oauth-state scope))
                              (flow (make-sign-in :name name :url url :key key :settings settings
                                                  :listener listener :port port :redirect-uri redirect-uri
                                                  :state oauth-state :verifier verifier :client client
                                                  :discovery discovery :scope scope :address address
                                                  :on-done on-done)))
                         (bt2:with-lock-held (*sign-ins-lock*) (setf (gethash name *sign-ins*) flow))
                         (setf (sign-in-thread flow)
                               (nlk:spawn (format nil "mcp-sign-in-~a" name) (finish-sign-in flow))
                               handed-off t)
                         (values address redirect-uri (open-in-browser address)))))))
             (sb-sys:deadline-timeout ()
               (oauth-fail "the sign-in could not be prepared within ~d s" *sign-in-prepare-seconds*)))
        (unless handed-off
          (when listener (ignore-errors (usocket:socket-close listener))))))))

(defun authorization-address (discovery client redirect-uri challenge state scope)
  "The address the operator opens: the authorization request with PKCE S256,
the state, the scope and the resource."
  (let ((metadata (gethash "metadata" discovery))
        (url (endpoint discovery "authorization_endpoint" "/authorize")))
    (when (and (gethash "response_types_supported" metadata)
               (not (member "code" (json-strings metadata "response_types_supported") :test #'string=)))
      (oauth-fail "the authorization server hands out no authorization codes"))
    (let ((methods (json-strings metadata "code_challenge_methods_supported")))
      (when (and methods (not (member "S256" methods :test #'string=)))
        (oauth-fail "the authorization server does not take PKCE S256")))
    ;; The address goes to the platform's opener, which also starts files and other schemes.
    (unless (and url (http-url-p url))
      (oauth-fail "the authorization endpoint ~a is not an http(s) address" url))
    (with-query url (append (list (cons "response_type" "code")
                                  (cons "client_id" (json-text client "client_id"))
                                  (cons "code_challenge" challenge)
                                  (cons "code_challenge_method" "S256")
                                  (cons "redirect_uri" redirect-uri)
                                  (cons "state" state))
                            (and scope (list (cons "scope" scope)))
                            (and scope (member "offline_access" (nlk:split-words scope) :test #'string=)
                                 (list (cons "prompt" "consent")))
                            (nlk:when-let (resource (json-text discovery "resource"))
                              (list (cons "resource" resource)))))))

(defun paste-sign-in (name text)
  "Hand NAME's sign-in in progress TEXT, the address the browser landed on,
and wait for the code exchange. => what came of it, one line."
  (let ((flow (bt2:with-lock-held (*sign-ins-lock*) (gethash name *sign-ins*))))
    (if (null flow)
        (format nil "no sign-in to ~a is waiting; /mcp login ~a starts one" name name)
        (multiple-value-bind (path code state iss error) (redirect-answer (or text ""))
          (cond ((not (equal path (redirect-path flow)))
                 (format nil "that is not the address the sign-in to ~a waits for (~a?code=...)"
                         name (sign-in-redirect-uri flow)))
                ((not (equal state (sign-in-state flow)))
                 "that address belongs to another sign-in")
                (error (setf (sign-in-failure flow)
                             (format nil "the authorization server refused the sign-in: ~a" (nlk:one-line error :cap 200)))
                       (format nil "the authorization server refused the sign-in to ~a: ~a" name
                               (nlk:one-line error :cap 200)))
                ((null code) "that address carries no authorization code")
                (t (setf (sign-in-answer flow) (cons code iss))
                   (loop repeat (* 10 *paste-wait-seconds*)
                         until (sign-in-outcome flow) do (sleep 0.1))
                   (let ((outcome (sign-in-outcome flow)))
                     (cond ((eq outcome :signed-in) (format nil "signed in to ~a; connecting" name))
                           (outcome (format nil "the sign-in to ~a failed: ~a" name outcome))
                           (t (format nil "the answer reached the sign-in to ~a; it is still finishing" name))))))))))

(defun revoke-tokens (state settings)
  "Revoke STATE's tokens where the server offers RFC 7009, as best it can."
  (let* ((discovery (gethash "discovery" state))
         (url (and (hash-table-p discovery) (json-text discovery "metadata" "revocation_endpoint")))
         (tokens (gethash "tokens" state))
         (client (state-client state settings))
         (*oauth-request-seconds* 5))
    (when (and url client (hash-table-p tokens))
      (secure-endpoint url "revocation endpoint")
      (dolist (kind '("refresh_token" "access_token"))
        (nlk:when-let (token (json-text tokens kind))
          (multiple-value-bind (params headers)
              (client-authentication client discovery
                                     (list (cons "token" token) (cons "token_type_hint" kind))
                                     '(("content-type" . "application/x-www-form-urlencoded")))
            (ignore-errors (oauth-request :post url :headers headers
                                                    :content (quri:url-encode-params params)))))))))

(defun forget-sign-in (name url settings)
  "Forget what the server NAME at URL keeps -- its client and tokens --
revoking the tokens where the server offers that. => T when something was
kept."
  (cancel-sign-in name)
  (let* ((key (state-key name url))
         (state (stored-state key)))
    (when state
      (ignore-errors (revoke-tokens state settings))
      (update-stored-state key (constantly nil))
      t)))
